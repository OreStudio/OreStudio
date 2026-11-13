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
#include "ores.marketdata.api/domain/market_observation.hpp"
#include "ores.marketdata.api/domain/market_series.hpp"
#include "ores.marketdata.api/domain/series_axis.hpp"
#include "ores.marketdata.api/domain/series_axis_value.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.core/repository/market_observation_repository.hpp"
#include "ores.marketdata.core/repository/market_series_repository.hpp"
#include "ores.marketdata.core/repository/series_axis_repository.hpp"
#include "ores.marketdata.core/repository/series_axis_value_repository.hpp"
#include "ores.marketdata.service/app/instrument_catalogue.hpp"
#include "ores.marketdata.service/app/rate_ladder_reader.hpp"
#include "ores.refdata.api/generators/party_generator.hpp"
#include "ores.refdata.core/repository/party_repository.hpp"
#include "ores.testing/make_generation_context.hpp"
#include "ores.testing/scoped_database_helper.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>
#include <vector>

constexpr const char* catalogue_read_tags = "[instrument_catalogue][database]";

namespace {

using ores::marketdata::messaging::list_instruments_request;
using ores::marketdata::service::app::get_rate_ladder;
using ores::marketdata::service::app::list_instruments;
using ores::marketdata::messaging::get_rate_ladder_request;

/**
 * @brief Two providers carrying one AU swaption-style ladder identity, with the
 * shape and the quotes the catalogue reads.
 *
 * The identity is a rate ladder: one series URI, one term axis, and two series
 * rows that differ only in the provider that carries them. That is the case the
 * catalogue exists for: two providers' quotes of one ladder are one instrument
 * with two sources, never two instruments.
 */
struct catalogue_fixture {
    catalogue_fixture()
        : gctx(ores::testing::make_generation_context(h))
        , party_id(uuid_gen()) {
        write_party();
        party = h.context().with_party(h.tenant_id(), party_id, {party_id}, h.db_user());
        for (const auto* provider : {"bloomberg", "demo"}) {
            const auto id = write_series(provider);
            write_shape(id);
            series_ids.push_back(id);
        }
        write_quotes();
    }

    ores::marketdata::messaging::list_instruments_request request() const {
        list_instruments_request req;
        req.party_id = boost::uuids::to_string(party_id);
        return req;
    }

    get_rate_ladder_request ladder_request() const {
        get_rate_ladder_request req;
        req.party_id = boost::uuids::to_string(party_id);
        req.currency = "AUD";
        req.instrument_type = "ir_swap";
        req.forward_start = "0D";
        return req;
    }

    ores::testing::scoped_database_helper h;
    ores::utility::generation::generation_context gctx;
    boost::uuids::random_generator uuid_gen;
    boost::uuids::uuid party_id;
    std::optional<ores::database::context> party;
    std::vector<boost::uuids::uuid> series_ids;

private:
    static const std::string& ladder_uri() {
        static const std::string uri =
            "oresmd://ir/AUD?type=series&instrument=ir_swap&quote=rate"
            "&fwd_start=0D&tenor=1D";
        return uri;
    }

    /// The datum spelling of one rung: no `type=series`, and the term coordinate.
    static std::string datum_uri(const std::string& term) {
        return "oresmd://ir/AUD?instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=" +
               term;
    }

    void write_party() {
        ores::refdata::repository::party_repository repo;
        auto p = ores::refdata::generators::generate_synthetic_party(gctx);
        p.id = party_id;
        p.version = 0;
        p.is_registration_default = false;
        // The tenant already holds its root party, and the schema allows one.
        for (const auto& e : repo.read_latest(h.context()))
            if (e.tenant_id == h.tenant_id() && !e.parent_party_id) {
                p.parent_party_id = e.id;
                break;
            }
        p.change_reason_code = "system.test";
        p.change_commentary = "instrument catalogue test";
        repo.write(h.context(), p);
    }

    boost::uuids::uuid write_series(const std::string& provider) {
        ores::marketdata::domain::market_series s;
        s.id = uuid_gen();
        s.version = 0;
        s.tenant_id = h.tenant_id();
        s.party_id = party_id;
        s.oresmd_uri = ladder_uri();
        s.series_subclass = "yield";
        s.provider_code = provider;
        s.derivation_kind = "OBSERVED";
        s.derivation_config_id = boost::uuids::nil_uuid();
        s.derivation_config_version = 0;
        s.modified_by = h.db_user();
        s.performed_by = h.db_user();
        s.change_reason_code = "system.test";
        s.change_commentary = "instrument catalogue test";
        ores::marketdata::repository::market_series_repository{}.write(h.context(), s);
        return s.id;
    }

    /// The declared shape: one term axis, three rungs, in draw order.
    void write_shape(const boost::uuids::uuid& series_id) {
        ores::marketdata::repository::series_axis_repository axis_repo;
        ores::marketdata::repository::series_axis_value_repository value_repo;

        ores::marketdata::domain::series_axis axis;
        axis.tenant_id = h.tenant_id();
        axis.series_id = series_id;
        axis.axis_field = "term";
        axis.party_id = party_id;
        axis.sequence = 0;
        axis_repo.write(h.context(), axis);

        int sequence = 0;
        for (const auto* value : {"1Y", "2Y", "5Y"}) {
            ores::marketdata::domain::series_axis_value v;
            v.tenant_id = h.tenant_id();
            v.series_id = series_id;
            v.axis_field = "term";
            v.value = value;
            v.party_id = party_id;
            v.sequence = sequence++;
            value_repo.write(h.context(), v);
        }
    }

    /// One quote at each of two rungs, per provider, so freshness has a source.
    void write_quotes() {
        const auto now = std::chrono::system_clock::now();
        ores::marketdata::repository::market_observation_repository repo;
        for (const auto& series_id : series_ids)
            for (const auto* term : {"1Y", "2Y"}) {
                ores::marketdata::domain::market_observation o;
                o.id = uuid_gen();
                o.tenant_id = h.tenant_id();
                o.party_id = party_id;
                o.series_id = series_id;
                o.observation_datetime = now;
                o.oresmd_uri = datum_uri(term);
                o.value = "0.0432";
                o.source = "instrument catalogue test";
                repo.insert(*party, {o});
            }
    }
};

}

TEST_CASE("two_providers_quotes_of_one_ladder_are_one_instrument_with_two_sources",
          catalogue_read_tags) {
    catalogue_fixture f;

    const auto resp = list_instruments(*f.party, f.request());

    REQUIRE(resp.success);
    REQUIRE(resp.instruments.size() == 1);
    const auto& instrument = resp.instruments.front();
    CHECK(instrument.kind == "ladder");
    CHECK(instrument.sources.size() == 2);
    CHECK(instrument.currency == "AUD");
    CHECK(instrument.forward_start == "0D");
    CHECK(instrument.name.find("AUD") != std::string::npos);
}

TEST_CASE("the_catalogue_states_the_axes_in_the_order_the_shape_declares",
          catalogue_read_tags) {
    catalogue_fixture f;

    const auto resp = list_instruments(*f.party, f.request());

    REQUIRE(resp.success);
    REQUIRE(resp.instruments.size() == 1);
    const auto& axes = resp.instruments.front().axes;
    REQUIRE(axes.size() == 1);
    CHECK(axes.front().field == "term");
    CHECK(axes.front().values == std::vector<std::string>{"1Y", "2Y", "5Y"});
}

TEST_CASE("a_source_states_its_own_instant_and_the_servers_verdict", catalogue_read_tags) {
    catalogue_fixture f;

    const auto resp = list_instruments(*f.party, f.request());

    REQUIRE(resp.success);
    REQUIRE(resp.instruments.size() == 1);
    for (const auto& source : resp.instruments.front().sources) {
        CHECK(source.provider_code.empty() == false);
        CHECK(source.as_of != std::chrono::system_clock::time_point{});
        CHECK(source.age_seconds >= 0);
        CHECK(source.stale == false);
    }
    CHECK(resp.instruments.front().stale == false);
}

TEST_CASE("a_party_that_may_not_read_the_asked_party_is_refused", catalogue_read_tags) {
    catalogue_fixture f;

    auto req = f.request();
    req.party_id = boost::uuids::to_string(f.uuid_gen());
    const auto resp = list_instruments(*f.party, req);

    CHECK(resp.success == false);
    CHECK(resp.message.empty() == false);
}

TEST_CASE("a_query_that_matches_nothing_answers_an_empty_page_not_a_failure",
          catalogue_read_tags) {
    catalogue_fixture f;

    auto req = f.request();
    req.query = "zzz-no-such-instrument";
    const auto resp = list_instruments(*f.party, req);

    CHECK(resp.success == true);
    CHECK(resp.instruments.empty());
    CHECK(resp.total == 0);
}

TEST_CASE("a_fixing_only_query_answers_an_empty_page_when_no_fixing_is_held",
          catalogue_read_tags) {
    catalogue_fixture f;

    auto req = f.request();
    req.kinds = {"fixing"};
    const auto resp = list_instruments(*f.party, req);

    // The fixture writes no fixing series, so the read has nothing to refuse;
    // it answers an honest empty page for a kind it holds none of.
    CHECK(resp.success == true);
    CHECK(resp.total == 0);
}

TEST_CASE("the_ladder_answers_the_rungs_it_can_date_and_names_the_ones_it_cannot",
          catalogue_read_tags) {
    catalogue_fixture f;

    const auto resp = get_rate_ladder(*f.party, f.ladder_request());

    REQUIRE(resp.success);
    REQUIRE(resp.rungs.empty() == false);
    CHECK(resp.provider_count == 2);
    // The seeded RATES_SPOT_FORWARD convention resolves 2Y but carries no row
    // for 1Y, so the answer holds the rung it can date and names the one it
    // cannot rather than dropping it in silence.
    CHECK(resp.message.find("1Y") != std::string::npos);
    for (const auto& rung : resp.rungs) {
        CHECK(rung.quotes.size() == 2);
        CHECK(rung.consensus.method == "median");
        CHECK(rung.consensus.providers_used == 2);
        // Two quotes: the median is their mean.
        const auto a = rung.quotes[0].value;
        const auto b = rung.quotes[1].value;
        CHECK(rung.consensus.value == (a + b) / 2.0);
    }
}

TEST_CASE("the_ladder_orders_its_rungs_by_maturity", catalogue_read_tags) {
    catalogue_fixture f;

    const auto resp = get_rate_ladder(*f.party, f.ladder_request());

    REQUIRE(resp.success);
    REQUIRE(resp.rungs.empty() == false);
    for (std::size_t i = 1; i < resp.rungs.size(); ++i)
        CHECK(resp.rungs[i - 1].maturity_date < resp.rungs[i].maturity_date);
    CHECK(resp.rungs.front().maturity_date > std::chrono::year_month_day{
              std::chrono::floor<std::chrono::days>(std::chrono::system_clock::now())});
}

TEST_CASE("a_ladder_no_provider_quotes_is_refused_in_words", catalogue_read_tags) {
    catalogue_fixture f;

    auto req = f.ladder_request();
    req.currency = "ZZZ";
    const auto resp = get_rate_ladder(*f.party, req);

    CHECK(resp.success == false);
    CHECK(resp.message.empty() == false);
}
