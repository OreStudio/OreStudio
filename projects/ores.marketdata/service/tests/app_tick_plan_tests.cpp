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
#include "ores.marketdata.api/domain/tick_subjects.hpp"
#include "ores.marketdata.service/app/tick_plan.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

namespace {

using ores::marketdata::service::app::consumer;
using ores::marketdata::messaging::market_tick;
using ores::marketdata::service::app::make_targets;
using ores::marketdata::service::app::name_datum;
using ores::marketdata::service::app::plan_tick;
using ores::marketdata::service::app::tick_drop;

const std::string app_tick_plan_eur_usd_spot =
    "oresmd://fx/EUR?type=quote&instrument=fx_spot&quote=rate&ccy=USD";

market_tick make_tick(std::string uri, std::string value) {
    market_tick t;
    t.oresmd_uri = std::move(uri);
    t.value = std::move(value);
    t.source = "synthetic.eurusd";
    return t;
}

consumer app_tick_plan_make_owner(const std::string& tenant, const std::string& party) {
    const boost::uuids::string_generator uuid;
    consumer c;
    c.tenant_id = ores::utility::uuid::tenant_id::from_string(tenant).value();
    c.party_id = uuid(party);
    c.source_name = "synthetic.eurusd";
    return c;
}

}

TEST_CASE("a_tick_from_a_source_no_route_names_is_dropped", "[app][tick_plan]") {
    const auto plan = plan_tick(make_tick(app_tick_plan_eur_usd_spot, "1.0845"), {});

    REQUIRE_FALSE(plan.has_value());
    CHECK(plan.error() == tick_drop::unbound_source);
}

TEST_CASE("a_tick_whose_uri_names_no_datum_is_dropped", "[app][tick_plan]") {
    const std::vector<consumer> consumers{app_tick_plan_make_owner(
        "11111111-1111-1111-1111-111111111111", "22222222-2222-2222-2222-222222222222")};

    const auto plan = plan_tick(make_tick("oresmd://fx/EUR?type=quote", "1.0845"), consumers);

    REQUIRE_FALSE(plan.has_value());
    CHECK(plan.error() == tick_drop::unnameable_datum);
}

TEST_CASE("a_tick_spelled_another_way_is_dropped_so_a_datum_has_one_subject", "[app][tick_plan]") {
    const std::vector<consumer> consumers{app_tick_plan_make_owner(
        "11111111-1111-1111-1111-111111111111", "22222222-2222-2222-2222-222222222222")};

    const auto plan = plan_tick(
        make_tick("oresmd://fx/EUR?instrument=fx_spot&type=quote&quote=rate&ccy=USD", "1.0845"),
        consumers);

    REQUIRE_FALSE(plan.has_value());
    CHECK(plan.error() == tick_drop::unnameable_datum);
}

TEST_CASE("a_tick_whose_value_is_not_a_number_is_dropped", "[app][tick_plan]") {
    const std::vector<consumer> consumers{app_tick_plan_make_owner(
        "11111111-1111-1111-1111-111111111111", "22222222-2222-2222-2222-222222222222")};

    const auto plan = plan_tick(make_tick(app_tick_plan_eur_usd_spot, "one point oh"), consumers);

    REQUIRE_FALSE(plan.has_value());
    CHECK(plan.error() == tick_drop::not_a_number);
}

TEST_CASE("a_tick_from_a_source_with_several_consumers_has_one_target_each", "[app][tick_plan]") {
    const std::vector<consumer> consumers{
        app_tick_plan_make_owner("11111111-1111-1111-1111-111111111111",
                                 "22222222-2222-2222-2222-222222222222"),
        app_tick_plan_make_owner("33333333-3333-3333-3333-333333333333",
                                 "44444444-4444-4444-4444-444444444444")};

    const auto plan = plan_tick(make_tick(app_tick_plan_eur_usd_spot, "1.0845"), consumers);

    REQUIRE(plan.has_value());
    CHECK(plan->value == 1.0845);
    REQUIRE(plan->targets->size() == 2);
    CHECK((*plan->targets)[0].owner == consumers[0]);
    CHECK((*plan->targets)[1].owner == consumers[1]);
    const auto token = ores::marketdata::domain::subject_token(app_tick_plan_eur_usd_spot);
    CHECK((*plan->targets)[0].subject ==
          "marketdata.v1.ops.market_tick.11111111-1111-1111-1111-111111111111."
          "22222222-2222-2222-2222-222222222222." +
              token);
    CHECK((*plan->targets)[1].subject ==
          "marketdata.v1.ops.market_tick.33333333-3333-3333-3333-333333333333."
          "44444444-4444-4444-4444-444444444444." +
              token);
}

TEST_CASE("a_uri_is_named_once_with_its_canonical_spelling_and_the_uri_of_its_series",
          "[app][tick_plan]") {
    const auto named = name_datum(app_tick_plan_eur_usd_spot);

    REQUIRE(named.has_value());
    CHECK((*named)->uri == app_tick_plan_eur_usd_spot);
    CHECK((*named)->series_uri ==
          "oresmd://fx/EUR?type=series&instrument=fx_spot&quote=rate&ccy=USD");
}

TEST_CASE("a_uri_that_names_no_datum_is_not_named", "[app][tick_plan]") {
    const auto named = name_datum("oresmd://fx/EUR?type=quote");

    REQUIRE_FALSE(named.has_value());
    CHECK(named.error() == tick_drop::unnameable_datum);
}

TEST_CASE("a_plan_uses_the_named_datum_and_the_targets_it_is_given", "[app][tick_plan]") {
    const std::vector<consumer> consumers{app_tick_plan_make_owner(
        "11111111-1111-1111-1111-111111111111", "22222222-2222-2222-2222-222222222222")};
    const auto named = name_datum(app_tick_plan_eur_usd_spot).value();
    const auto targets = make_targets(consumers, *named);

    const auto plan =
        plan_tick(make_tick(app_tick_plan_eur_usd_spot, "1.0845"), consumers, named, targets);

    REQUIRE(plan.has_value());
    CHECK(plan->named == named);
    CHECK(plan->targets == targets);
    CHECK((*targets)[0].tenant_text == "11111111-1111-1111-1111-111111111111");
    CHECK((*targets)[0].party_text == "22222222-2222-2222-2222-222222222222");
}

TEST_CASE("a_plan_given_a_named_datum_still_refuses_a_value_that_is_no_number",
          "[app][tick_plan]") {
    const std::vector<consumer> consumers{app_tick_plan_make_owner(
        "11111111-1111-1111-1111-111111111111", "22222222-2222-2222-2222-222222222222")};

    const auto plan = plan_tick(make_tick(app_tick_plan_eur_usd_spot, "one point oh"),
                                consumers,
                                name_datum(app_tick_plan_eur_usd_spot).value());

    REQUIRE_FALSE(plan.has_value());
    CHECK(plan.error() == tick_drop::not_a_number);
}
