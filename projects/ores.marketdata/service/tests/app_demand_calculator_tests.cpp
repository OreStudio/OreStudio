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
#include "ores.marketdata.service/app/demand_calculator.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <string>

namespace {

const std::string tags("[app][demand_calculator]");

using namespace std::chrono_literals;
using ores::marketdata::service::app::compute_demand;
using ores::marketdata::service::app::demand_catalogue;
using ores::marketdata::service::app::demand_policy;

const std::string eur_usd = "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD";
const std::string gbp_usd = "oresmd://fx/GBP?type=quote&instrument=fx_spot&quote=rate&ccy=USD";

struct world {
    demand_catalogue c;
    boost::uuids::random_generator gen;
    std::chrono::system_clock::time_point now = std::chrono::system_clock::now();
    ores::marketdata::domain::market_data_source sim;
    ores::marketdata::domain::market_data_source live;

    world() {
        sim.id = gen();
        sim.name = "bloomberg-sim";
        sim.provider_code = "bloomberg";
        live.id = gen();
        live.name = "bloomberg-live";
        live.provider_code = "bloomberg";
        c.sources = {sim, live};
        instrument("bloomberg", "EURUSD Curncy", eur_usd);
        instrument("bloomberg", "GBPUSD Curncy", gbp_usd);
        instrument("lseg", "EUR=", eur_usd);
    }

    void instrument(const std::string& provider, const std::string& id, const std::string& uri) {
        ores::marketdata::domain::market_data_vendor_instrument v;
        v.id = gen();
        v.provider_code = provider;
        v.id_scheme = provider == "lseg" ? "ric" : "bbg_ticker";
        v.external_id = id;
        v.field = provider == "lseg" ? "BID" : "PX_LAST";
        ores::marketdata::domain::market_data_mapping m;
        m.id = gen();
        m.vendor_instrument_id = v.id;
        m.oresmd_uri = uri;
        m.effective_from = now - 24h;
        c.instruments.push_back(v);
        c.mappings.push_back(m);
    }

    boost::uuids::uuid party_routed_to(const ores::marketdata::domain::market_data_source& source,
                                       bool subscribed = true,
                                       bool derived_only = false) {
        const auto party = gen();
        ores::marketdata::domain::route r;
        r.id = gen();
        r.party_id = party;
        r.provider_code = source.provider_code;
        r.source_id = source.id;
        c.routes.push_back(r);
        if (subscribed) {
            ores::marketdata::domain::subscription s;
            s.id = gen();
            s.party_id = party;
            s.provider_code = source.provider_code;
            s.display = !derived_only;
            s.derived = derived_only;
            c.subscriptions.push_back(s);
        }
        return party;
    }
};

}

TEST_CASE("the_demand_of_an_adapter_is_the_mapped_ids_of_its_provider", tags) {
    world w;
    w.party_routed_to(w.sim);

    const auto result = compute_demand(w.c, w.now);

    REQUIRE(result.by_adapter.contains("bloomberg-sim"));
    const auto& ids = result.by_adapter.at("bloomberg-sim");
    REQUIRE(ids.size() == 2);
    CHECK(ids[0].external_id == "EURUSD Curncy");
    CHECK(ids[1].external_id == "GBPUSD Curncy");
    CHECK(result.refused.empty());
}

TEST_CASE("the_demand_holds_no_id_of_another_provider", tags) {
    world w;
    w.party_routed_to(w.sim);

    const auto result = compute_demand(w.c, w.now);

    for (const auto& id : result.by_adapter.at("bloomberg-sim"))
        CHECK(id.id_scheme == "bbg_ticker");
}

TEST_CASE("the_demand_is_the_union_over_the_parties_and_holds_no_id_twice", tags) {
    world w;
    w.party_routed_to(w.sim);
    w.party_routed_to(w.sim);

    const auto result = compute_demand(w.c, w.now);

    CHECK(result.by_adapter.at("bloomberg-sim").size() == 2);
}

TEST_CASE("parties_of_two_sources_give_each_adapter_its_own_set", tags) {
    world w;
    w.party_routed_to(w.sim);
    w.party_routed_to(w.live);

    const auto result = compute_demand(w.c, w.now);

    const auto& sim = result.by_adapter.at("bloomberg-sim");
    const auto& live = result.by_adapter.at("bloomberg-live");
    REQUIRE(sim.size() == live.size());
    for (std::size_t i = 0; i < sim.size(); ++i)
        CHECK(sim[i].external_id == live[i].external_id);
    CHECK(result.by_adapter.size() == 2);
}

TEST_CASE("a_route_without_a_subscription_is_refused_as_not_entitled", tags) {
    world w;
    const auto party = w.party_routed_to(w.sim, false);

    const auto result = compute_demand(w.c, w.now);

    REQUIRE(result.refused.size() == 1);
    CHECK(result.refused.front().party_id == party);
    CHECK(result.refused.front().adapter_id == "bloomberg-sim");
    CHECK(result.refused.front().reason == "not_entitled");
    REQUIRE(result.by_adapter.contains("bloomberg-sim"));
    CHECK(result.by_adapter.at("bloomberg-sim").empty());
}

TEST_CASE("a_subscription_with_only_the_derived_right_still_entitles_a_party", tags) {
    world w;
    w.party_routed_to(w.sim, true, true);

    const auto result = compute_demand(w.c, w.now);

    CHECK(result.refused.empty());
    CHECK(result.by_adapter.at("bloomberg-sim").size() == 2);
}

TEST_CASE("a_subscription_with_no_right_does_not_entitle_a_party", tags) {
    world w;
    w.party_routed_to(w.sim);
    w.c.subscriptions.front().display = false;

    const auto result = compute_demand(w.c, w.now);

    CHECK(result.refused.size() == 1);
}

TEST_CASE("a_mapping_that_does_not_hold_now_adds_no_id", tags) {
    world w;
    w.party_routed_to(w.sim);
    w.c.mappings.front().effective_to = w.now - 1h;

    const auto result = compute_demand(w.c, w.now);

    REQUIRE(result.by_adapter.at("bloomberg-sim").size() == 1);
    CHECK(result.by_adapter.at("bloomberg-sim").front().external_id == "GBPUSD Curncy");
}

TEST_CASE("a_party_restricted_to_an_item_demands_only_its_ids", tags) {
    world w;
    w.party_routed_to(w.sim);
    demand_policy policy;
    policy.requirements = [](const auto&, const auto&) {
        return std::optional<std::set<std::string>>(std::set<std::string>{eur_usd});
    };

    const auto result = compute_demand(w.c, w.now, policy);

    REQUIRE(result.by_adapter.at("bloomberg-sim").size() == 1);
    CHECK(result.by_adapter.at("bloomberg-sim").front().external_id == "EURUSD Curncy");
}

TEST_CASE("the_inputs_of_a_composite_join_the_demand", tags) {
    world w;
    w.party_routed_to(w.sim);
    demand_policy policy;
    policy.requirements = [](const auto&, const auto&) {
        return std::optional<std::set<std::string>>(std::set<std::string>{
            "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=CHF"});
    };
    policy.composite_inputs = [](const std::string&) {
        return std::vector<std::string>{eur_usd, gbp_usd};
    };

    const auto result = compute_demand(w.c, w.now, policy);

    CHECK(result.by_adapter.at("bloomberg-sim").size() == 2);
}

TEST_CASE("a_route_to_a_source_the_catalogue_lacks_adds_nothing", tags) {
    world w;
    w.party_routed_to(w.sim);
    w.c.routes.front().source_id = w.gen();

    const auto result = compute_demand(w.c, w.now);

    CHECK(result.by_adapter.empty());
}
