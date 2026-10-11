/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/repository/market_series_identity_reader.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/service/import_service.hpp"
#include "ores.marketdata.core/service/series_identity_lister.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/database_helper.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <cctype>
#include <stdexcept>
#include <string>
#include <vector>

using namespace ores::logging;

namespace {

const std::string_view test_suite("ores.marketdata.core.tests");
const std::string tags("[service][series_identity_lister]");

using ores::marketdata::messaging::list_series_identities_request;
using ores::marketdata::messaging::listed_series;
using ores::marketdata::repository::market_series_identity_reader;
using ores::marketdata::repository::market_series_repository;
using ores::marketdata::service::series_identity_lister;
using ores::testing::database_helper;
using ores::testing::scoped_database_helper;

/// A curve id no earlier run used, so the series is this test's alone.
std::string fresh_curve_id() {
    auto tag = boost::uuids::to_string(boost::uuids::random_generator{}());
    tag.erase(std::remove(tag.begin(), tag.end(), '-'), tag.end());
    return tag;
}

ores::marketdata::domain::market_series series_at(const ores::utility::uuid::tenant_id& tenant,
                                                  const std::string& user,
                                                  const boost::uuids::uuid& party,
                                                  const std::string& uri) {
    ores::marketdata::domain::market_series s;
    s.id = boost::uuids::random_generator{}();
    s.version = 0;
    s.tenant_id = tenant;
    s.party_id = party;
    s.oresmd_uri = uri;
    s.series_subclass = "yield";
    s.derivation_kind = "OBSERVED";
    s.derivation_config_id = boost::uuids::nil_uuid();
    s.derivation_config_version = 0;
    s.modified_by = user;
    s.performed_by = user;
    s.change_reason_code = "system.test";
    s.change_commentary = "series identity lister test";
    return s;
}

ores::marketdata::domain::market_series
series_at(database_helper& h, const boost::uuids::uuid& party, const std::string& uri) {
    return series_at(h.tenant_id(), h.db_user(), party, uri);
}

std::string zero_uri(const std::string& curve_id) {
    return "oresmd://ir/EUR?type=series&instrument=zero&quote=rate&curve_id=" + curve_id +
           "&day_counter=A365";
}

/// The listed series whose curve id is @p curve_id, whoever owns them.
std::vector<listed_series> with_curve(const std::vector<listed_series>& all,
                                      const std::string& curve_id) {
    std::vector<listed_series> found;
    for (const auto& s : all) {
        for (const auto& f : s.identity.fields) {
            if (f.name == "curve_id" && f.text == curve_id)
                found.push_back(s);
        }
    }
    return found;
}

/// Two operational parties of the test tenant, and a context for each. Row level
/// security shows a session only the rows of the parties it may see.
struct two_parties {
    boost::uuids::uuid a;
    boost::uuids::uuid b;
    ores::database::context a_context;
    ores::database::context b_context;
};

two_parties make_two_parties(scoped_database_helper& h) {
    auto& ctx = h.context();
    const auto tenant = h.tenant_id();
    ores::refdata::repository::party_repository repo;

    boost::uuids::uuid system_party{};
    for (const auto& p : repo.read_latest(ctx)) {
        if (p.tenant_id == tenant && p.party_category == "System")
            system_party = p.id;
    }
    REQUIRE(system_party != boost::uuids::uuid{});

    auto generation = ores::testing::make_generation_context(h);
    const auto make_party = [&] {
        auto party = ores::refdata::generators::generate_synthetic_party(generation);
        party.party_category = "Operational";
        party.parent_party_id = system_party;
        party.change_reason_code = "system.test";
        repo.write(ctx, party);
        return party.id;
    };
    const auto a = make_party();
    const auto b = make_party();
    return {a, b, ctx.with_party(tenant, a, {a}, ""), ctx.with_party(tenant, b, {b}, "")};
}

}

TEST_CASE("a_listed_identity_names_the_series_it_was_listed_for", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto party = boost::uuids::random_generator{}();
    const auto curve = fresh_curve_id();
    const auto written = series_at(h, party, zero_uri(curve));
    market_series_repository{}.write(h.context(), written);

    const auto listed = series_identity_lister::list(h.context(), list_series_identities_request{});

    REQUIRE(listed.success);
    const auto mine = with_curve(listed.series, curve);
    REQUIRE(mine.size() == 1);
    CHECK(mine[0].series_id == boost::uuids::to_string(written.id));
    CHECK(mine[0].identity.asset == "ir");
    CHECK(mine[0].identity.scope == "EUR");
    CHECK(mine[0].identity.instrument_type == "zero");
    CHECK(mine[0].identity.quote_type == "rate");
    CHECK(mine[0].identity.party_id == boost::uuids::to_string(party));

    // The identity is what the curve reads take: resolving it finds the series.
    const auto resolved = market_series_identity_reader::read(h.context(), mine[0].identity);
    REQUIRE(resolved.size() == 1);
    CHECK(resolved[0].id == written.id);
}

TEST_CASE("a_party_is_listed_its_own_series_and_not_another_partys", tags) {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    const auto parties = make_two_parties(h);
    const auto curve = fresh_curve_id();
    // The same identity held by two parties is two series.
    market_series_repository{}.write(
        parties.a_context, series_at(h.tenant_id(), h.db_user(), parties.a, zero_uri(curve)));
    market_series_repository{}.write(
        parties.b_context, series_at(h.tenant_id(), h.db_user(), parties.b, zero_uri(curve)));

    // A caller acting as the first party is shown its own series and not the
    // second party's.
    const auto as_a =
        series_identity_lister::list(parties.a_context, list_series_identities_request{});
    const auto seen_by_a = with_curve(as_a.series, curve);
    REQUIRE(seen_by_a.size() == 1);
    CHECK(seen_by_a[0].identity.party_id == boost::uuids::to_string(parties.a));

    const auto as_b =
        series_identity_lister::list(parties.b_context, list_series_identities_request{});
    const auto seen_by_b = with_curve(as_b.series, curve);
    REQUIRE(seen_by_b.size() == 1);
    CHECK(seen_by_b[0].identity.party_id == boost::uuids::to_string(parties.b));
}

TEST_CASE("naming_a_party_narrows_the_list_to_it", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto first = boost::uuids::random_generator{}();
    const auto second = boost::uuids::random_generator{}();
    const auto curve = fresh_curve_id();
    market_series_repository{}.write(h.context(), series_at(h, first, zero_uri(curve)));
    market_series_repository{}.write(h.context(), series_at(h, second, zero_uri(curve)));

    const auto all = series_identity_lister::list(h.context(), list_series_identities_request{});
    REQUIRE(all.success);
    CHECK(with_curve(all.series, curve).size() == 2);

    list_series_identities_request narrowed;
    narrowed.party_id = boost::uuids::to_string(second);
    const auto only_second = series_identity_lister::list(h.context(), narrowed);
    const auto found = with_curve(only_second.series, curve);
    REQUIRE(found.size() == 1);
    CHECK(found[0].identity.party_id == boost::uuids::to_string(second));
}

TEST_CASE("a_party_is_named_whatever_way_its_uuid_is_spelled", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto party = boost::uuids::random_generator{}();
    const auto curve = fresh_curve_id();
    market_series_repository{}.write(h.context(), series_at(h, party, zero_uri(curve)));

    // Capitals and braces name the same party as the canonical spelling.
    auto capitals = boost::uuids::to_string(party);
    std::ranges::transform(capitals, capitals.begin(), [](unsigned char c) {
        return static_cast<char>(std::toupper(c));
    });
    for (const auto& spelling : {capitals, "{" + boost::uuids::to_string(party) + "}"}) {
        list_series_identities_request request;
        request.party_id = spelling;
        const auto listed = series_identity_lister::list(h.context(), request);
        CHECK(with_curve(listed.series, curve).size() == 1);
    }
}

TEST_CASE("a_party_that_is_no_uuid_is_refused_and_not_answered_with_nothing", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    list_series_identities_request request;
    request.party_id = "acme";
    CHECK_THROWS_AS(series_identity_lister::list(h.context(), request), std::invalid_argument);
}

TEST_CASE("a_fixing_series_is_not_listed", tags) {
    auto lg(make_logger(test_suite));

    database_helper h;
    const auto party = boost::uuids::random_generator{}();
    const auto name = fresh_curve_id();
    market_series_repository{}.write(
        h.context(),
        series_at(h, party, "oresmd://ir/EUR?type=fixing&index=ibor&name=" + name + "&tenor=6M"));

    const auto listed = series_identity_lister::list(h.context(), list_series_identities_request{});

    REQUIRE(listed.success);
    for (const auto& s : listed.series) {
        for (const auto& f : s.identity.fields)
            CHECK(f.text != name);
    }
}

TEST_CASE("a_series_lists_the_axes_its_shape_declares", tags) {
    auto lg(make_logger(test_suite));

    // A currency pair no other suite reads, because the tests share one tenant and
    // a point written here would show in another suite's surface.
    database_helper h;
    ores::nats::service::nats_client auth_nats;
    ores::marketdata::service::import_service svc(h.context(), auth_nats);
    ores::marketdata::messaging::import_market_data_request req;
    req.market_data_content = "20170910 FX_OPTION/RATE_LNVOL/SGD/THB/1Y/ATM 0.080\n"
                              "20170910 FX_OPTION/RATE_LNVOL/SGD/THB/2Y/25RR 0.011\n";
    req.source = "test.series_identity_lister";
    REQUIRE(svc.import(req).success);

    const auto curve = fresh_curve_id();
    market_series_repository{}.write(
        h.context(), series_at(h, boost::uuids::random_generator{}(), zero_uri(curve)));

    const auto listed = series_identity_lister::list(h.context(), list_series_identities_request{});
    REQUIRE(listed.success);

    bool found_surface = false;
    for (const auto& s : listed.series) {
        if (s.identity.instrument_type == "fx_option" && s.identity.scope == "SGD") {
            found_surface = true;
            CHECK(s.axes == std::vector<std::string>{"expiry", "strike_label"});
        }
    }
    CHECK(found_surface);

    // A series that declares no shape lists no axes.
    const auto plain = with_curve(listed.series, curve);
    REQUIRE(plain.size() == 1);
    CHECK(plain[0].axes.empty());
}
