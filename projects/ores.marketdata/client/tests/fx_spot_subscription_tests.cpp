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
#include "ores.marketdata.client/detail/subject_helpers.hpp"
#include <catch2/catch_test_macros.hpp>

using ores::marketdata::client::detail::ore_key_to_subject;

namespace {

const std::string tenant = "ac7420d6-6adf-40d5-b640-4132f3d4c3b0";
const std::string workspace = "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa";
const std::string party = "64eddccd-3ebc-4e69-abcb-91e18a81e72c";

} // namespace

TEST_CASE("ore_key_to_subject builds the per-party subject", "[fx_spot_subscription]") {
    REQUIRE(ore_key_to_subject("FX/RATE/EUR/USD", tenant, workspace, party) ==
            "marketdata.v1.tick." + tenant + "." + workspace + "." + party + ".fx.rate.eur.usd");
}

TEST_CASE("ore_key_to_subject passes through already-lowercase key", "[fx_spot_subscription]") {
    REQUIRE(ore_key_to_subject("fx/rate/gbp/usd", tenant, workspace, party) ==
            "marketdata.v1.tick." + tenant + "." + workspace + "." + party + ".fx.rate.gbp.usd");
}

TEST_CASE("ore_key_to_subject separates parties of the same source", "[fx_spot_subscription]") {
    const std::string other_party = "4acd51dc-1068-4e3f-bb3e-5f5e908f4a01";
    const auto us = ore_key_to_subject("FX/RATE/EUR/USD", tenant, workspace, party);
    const auto them = ore_key_to_subject("FX/RATE/EUR/USD", tenant, workspace, other_party);
    REQUIRE(us != them);
}

TEST_CASE("ore_key_to_subject separates workspaces of the same party", "[fx_spot_subscription]") {
    const std::string scenario = "b3c0d4e5-f607-4819-a2b3-c4d5e6f70819";
    const auto live = ore_key_to_subject("FX/RATE/EUR/USD", tenant, workspace, party);
    const auto alt = ore_key_to_subject("FX/RATE/EUR/USD", tenant, scenario, party);
    REQUIRE(live != alt);
}

TEST_CASE("ore_key_to_subject handles single-component key with no slash",
          "[fx_spot_subscription]") {
    REQUIRE(ore_key_to_subject("FX", tenant, workspace, party) ==
            "marketdata.v1.tick." + tenant + "." + workspace + "." + party + ".fx");
}

TEST_CASE("ore_key_to_subject handles empty key", "[fx_spot_subscription]") {
    REQUIRE(ore_key_to_subject("", tenant, workspace, party) ==
            "marketdata.v1.tick." + tenant + "." + workspace + "." + party + ".");
}
