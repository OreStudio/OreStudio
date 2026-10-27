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
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/service/import_service.hpp"
#include "ores.marketdata.core/service/series_identity_lister.hpp"
#include "ores.nats/service/nats_client.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include "test_suite.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

using namespace ores::logging;

namespace {

using ores::marketdata::messaging::import_market_data_request;
using ores::marketdata::messaging::list_series_identities_request;
using ores::marketdata::messaging::listed_series;
using ores::marketdata::service::import_service;
using ores::marketdata::service::series_identity_lister;
using ores::testing::scoped_database_helper;

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

/// A surface on a pair no other suite reads, with a coordinate on each axis.
const std::string surface = "20170910 FX_OPTION/RATE_LNVOL/SGD/MXN/1Y/ATM 0.080\n"
                            "20170910 FX_OPTION/RATE_LNVOL/SGD/MXN/2Y/25RR 0.011\n";

ores::marketdata::messaging::import_market_data_response
import_as(const ores::database::context& ctx) {
    ores::nats::service::nats_client auth_nats;
    import_service svc(ctx, auth_nats);
    import_market_data_request req;
    req.market_data_content = surface;
    req.source = "test.import_party_series";
    return svc.import(req);
}

std::vector<listed_series> surfaces_seen_by(const ores::database::context& ctx) {
    std::vector<listed_series> found;
    const auto listed = series_identity_lister::list(ctx, list_series_identities_request{});
    for (const auto& s : listed.series) {
        if (s.identity.scope == "SGD" && s.identity.instrument_type == "fx_option")
            found.push_back(s);
    }
    return found;
}

}

TEST_CASE("two_parties_each_import_the_same_surface_into_series_of_their_own", "[service][import_service][party]") {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    const auto parties = make_two_parties(h);

    const auto first = import_as(parties.a_context);
    INFO(first.message);
    REQUIRE(first.success);

    // The second party imports the same file. Before the fix its series were found by
    // the URI across the tenant, so it reused the first party's series and was refused
    // when it wrote axes the first party's row owns.
    const auto second = import_as(parties.b_context);
    INFO(second.message);
    REQUIRE(second.success);
    CHECK(second.series_count == first.series_count);

    const auto seen_by_a = surfaces_seen_by(parties.a_context);
    const auto seen_by_b = surfaces_seen_by(parties.b_context);
    REQUIRE(seen_by_a.size() == 1);
    REQUIRE(seen_by_b.size() == 1);
    CHECK(seen_by_a[0].series_id != seen_by_b[0].series_id);
    CHECK(seen_by_a[0].identity.party_id == boost::uuids::to_string(parties.a));
    CHECK(seen_by_b[0].identity.party_id == boost::uuids::to_string(parties.b));
    // Each party declared its own shape.
    CHECK(seen_by_a[0].axes == std::vector<std::string>{"expiry", "strike_label"});
    CHECK(seen_by_b[0].axes == std::vector<std::string>{"expiry", "strike_label"});
}

TEST_CASE("a_party_importing_again_updates_its_own_series_and_adds_none", "[service][import_service][party]") {
    auto lg(make_logger(test_suite));

    scoped_database_helper h;
    const auto parties = make_two_parties(h);

    const auto first = import_as(parties.a_context);
    REQUIRE(first.success);
    const auto again = import_as(parties.a_context);
    INFO(again.message);
    REQUIRE(again.success);

    // The series already exists for the party, so a second import creates no series.
    CHECK(again.series_count == 0);
    CHECK(surfaces_seen_by(parties.a_context).size() == 1);
}
