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
#include <algorithm>
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

/**
 * @brief Mirrors the subject derivation in fx_spot_subscription.cpp.
 *
 * Tested here as a free function so the conversion logic can be
 * verified without a live NATS connection.
 */
std::string ore_key_to_subject(std::string ore_key) {
    std::transform(ore_key.begin(), ore_key.end(), ore_key.begin(),
                   [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    std::replace(ore_key.begin(), ore_key.end(), '/', '.');
    return "marketdata.v1.tick." + ore_key;
}

} // namespace

TEST_CASE("ore_key_to_subject converts FX/RATE/EUR/USD correctly",
          "[fx_spot_subscription]") {
    REQUIRE(ore_key_to_subject("FX/RATE/EUR/USD") == "marketdata.v1.tick.fx.rate.eur.usd");
}

TEST_CASE("ore_key_to_subject converts already-lowercase key",
          "[fx_spot_subscription]") {
    REQUIRE(ore_key_to_subject("fx/rate/gbp/usd") == "marketdata.v1.tick.fx.rate.gbp.usd");
}
