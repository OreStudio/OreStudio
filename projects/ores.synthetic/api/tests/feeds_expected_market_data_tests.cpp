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
#include "ores.synthetic.api/feeds/expected_market_data.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <set>
#include <string>
#include <vector>

namespace {

const std::string expected_market_data_tags("[feeds][expected_market_data]");

using namespace ores::synthetic::feed;
using ores::marketdata::messaging::demanded_route;

const std::string gbp_quote(
    "oresmd://ir/GBP?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=3M&term=1Y");
const std::string gbp_fixing("oresmd://ir/GBP?type=fixing&index=ibor&name=LIBOR&tenor=3M");
const std::string usd_quote(
    "oresmd://ir/USD?type=quote&instrument=ir_swap&quote=rate&fwd_start=0D&tenor=1D&term=1Y");
const std::string usd_fixing("oresmd://ir/USD?type=fixing&index=ibor&name=SOFR");

demanded_route route(std::string provider, std::vector<std::string> items, bool entitled = true) {
    demanded_route r;
    r.provider_code = std::move(provider);
    r.entitled = entitled;
    r.items = std::move(items);
    return r;
}

const std::vector<generator_publication> two_providers{
    {.providers = {"SIM_A"}, .outputs = {gbp_quote, gbp_fixing}},
    {.providers = {"SIM_B"}, .outputs = {usd_quote, usd_fixing}}};

}

TEST_CASE("a_party_expects_every_output_of_a_generator_it_demands_an_item_of",
          expected_market_data_tags) {
    CHECK(expected_market_data({route("SIM_A", {gbp_quote})}, two_providers) ==
          std::set<std::string>{gbp_quote, gbp_fixing});
}

TEST_CASE("a_party_routed_to_one_provider_does_not_expect_another_providers_outputs",
          expected_market_data_tags) {
    const auto expected =
        expected_market_data({route("SIM_A", {gbp_quote, usd_quote})}, two_providers);

    CHECK_FALSE(expected.contains(usd_quote));
    CHECK_FALSE(expected.contains(usd_fixing));
}

TEST_CASE("a_route_that_is_not_entitled_expects_nothing", expected_market_data_tags) {
    CHECK(expected_market_data({route("SIM_A", {gbp_quote}, false)}, two_providers).empty());
}

TEST_CASE("a_generator_no_route_demands_an_item_of_is_not_expected", expected_market_data_tags) {
    CHECK(expected_market_data({route("SIM_B", {})}, two_providers).empty());
}

TEST_CASE("a_route_whose_items_are_not_sorted_still_matches", expected_market_data_tags) {
    const auto expected = expected_market_data(
        {route("SIM_A", {usd_quote, gbp_quote, "oresmd://ir/AAA"})}, two_providers);

    CHECK(expected == std::set<std::string>{gbp_quote, gbp_fixing});
}

TEST_CASE("a_surface_with_no_contributor_publishes_for_no_provider", expected_market_data_tags) {
    namespace domain = ores::synthetic::domain;
    generator_catalogue catalogue;
    domain::market_data_generation_config collection;
    collection.id = boost::uuids::random_generator()();
    collection.enabled = true;
    collection.provider_code = "SIM_A";
    catalogue.collections = {collection};
    domain::fx_vol_surface_feed surface;
    surface.id = boost::uuids::random_generator()();
    surface.config_id = collection.id;
    surface.enabled = true;
    surface.base_currency_code = "EUR";
    surface.quote_currency_code = "USD";
    catalogue.surfaces = {surface};
    domain::fx_vol_surface_feed_expiry expiry;
    expiry.feed_id = surface.id;
    expiry.tenor = "1M";
    catalogue.surface_expiries = {expiry};

    const auto generators = generator_publications(catalogue);

    REQUIRE(generators.size() == 1);
    CHECK(generators.front().providers.empty());
    CHECK(expected_market_data({route("SIM_A", generators.front().outputs)}, generators).empty());
}
