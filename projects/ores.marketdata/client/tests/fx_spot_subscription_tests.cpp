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

TEST_CASE("ore_key_to_subject converts FX/RATE/EUR/USD correctly",
          "[fx_spot_subscription]") {
    REQUIRE(ore_key_to_subject("FX/RATE/EUR/USD") == "marketdata.v1.tick.fx.rate.eur.usd");
}

TEST_CASE("ore_key_to_subject passes through already-lowercase key",
          "[fx_spot_subscription]") {
    REQUIRE(ore_key_to_subject("fx/rate/gbp/usd") == "marketdata.v1.tick.fx.rate.gbp.usd");
}

TEST_CASE("ore_key_to_subject handles single-component key with no slash",
          "[fx_spot_subscription]") {
    REQUIRE(ore_key_to_subject("FX") == "marketdata.v1.tick.fx");
}

TEST_CASE("ore_key_to_subject handles empty key",
          "[fx_spot_subscription]") {
    REQUIRE(ore_key_to_subject("") == "marketdata.v1.tick.");
}
