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
#include "ores.marketdata.service/app/tick_plan.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

namespace {

const std::string tags("[app][tick_plan]");

using ores::marketdata::domain::feed_binding;
using ores::marketdata::messaging::market_tick;
using ores::marketdata::service::app::plan_tick;
using ores::marketdata::service::app::tick_drop;

const std::string eur_usd_spot = "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD";

market_tick make_tick(std::string uri, std::string value) {
    market_tick t;
    t.oresmd_uri = std::move(uri);
    t.value = std::move(value);
    t.source = "synthetic.eurusd";
    return t;
}

feed_binding make_binding(const std::string& tenant, const std::string& party) {
    const boost::uuids::string_generator uuid;
    feed_binding b;
    b.tenant_id = ores::utility::uuid::tenant_id::from_string(tenant).value();
    b.party_id = uuid(party);
    b.source_name = "synthetic.eurusd";
    return b;
}

}

TEST_CASE("a_tick_from_an_unbound_source_is_dropped", tags) {
    const auto plan = plan_tick(make_tick(eur_usd_spot, "1.0845"), {});

    REQUIRE_FALSE(plan.has_value());
    CHECK(plan.error() == tick_drop::unbound_source);
}

TEST_CASE("a_tick_whose_uri_names_no_datum_is_dropped", tags) {
    const std::vector<feed_binding> bindings{make_binding("11111111-1111-1111-1111-111111111111",
                                                          "22222222-2222-2222-2222-222222222222")};

    const auto plan = plan_tick(make_tick("oresmd://fx/EUR?type=quote", "1.0845"), bindings);

    REQUIRE_FALSE(plan.has_value());
    CHECK(plan.error() == tick_drop::unnameable_datum);
}

TEST_CASE("a_tick_whose_value_is_not_a_number_is_dropped", tags) {
    const std::vector<feed_binding> bindings{make_binding("11111111-1111-1111-1111-111111111111",
                                                          "22222222-2222-2222-2222-222222222222")};

    const auto plan = plan_tick(make_tick(eur_usd_spot, "one point oh"), bindings);

    REQUIRE_FALSE(plan.has_value());
    CHECK(plan.error() == tick_drop::not_a_number);
}

TEST_CASE("a_tick_from_a_source_with_several_bindings_has_one_target_each", tags) {
    const std::vector<feed_binding> bindings{make_binding("11111111-1111-1111-1111-111111111111",
                                                          "22222222-2222-2222-2222-222222222222"),
                                             make_binding("33333333-3333-3333-3333-333333333333",
                                                          "44444444-4444-4444-4444-444444444444")};

    const auto plan = plan_tick(make_tick(eur_usd_spot, "1.0845"), bindings);

    REQUIRE(plan.has_value());
    CHECK(plan->value == 1.0845);
    REQUIRE(plan->targets.size() == 2);
    CHECK(plan->targets[0].binding == bindings[0]);
    CHECK(plan->targets[1].binding == bindings[1]);
    CHECK(plan->targets[0].subject == "marketdata.v1.tick.11111111-1111-1111-1111-111111111111."
                                      "22222222-2222-2222-2222-222222222222.fx.rate.eur.usd");
    CHECK(plan->targets[1].subject == "marketdata.v1.tick.33333333-3333-3333-3333-333333333333."
                                      "44444444-4444-4444-4444-444444444444.fx.rate.eur.usd");
}
