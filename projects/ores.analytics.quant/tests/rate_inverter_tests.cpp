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
#include "ores.analytics.quant/domain/derived_rate.hpp"
#include "ores.analytics.quant/domain/rate_status.hpp"
#include "ores.analytics.quant/service/rate_inverter.hpp"
#include <catch2/catch_approx.hpp>
#include <catch2/catch_test_macros.hpp>
#include <vector>

using ores::analytics::quant::domain::derived_rate;
using ores::analytics::quant::domain::rate_status;
using ores::analytics::quant::service::rate_inverter;

namespace {

auto epoch(int seconds) {
    return std::chrono::system_clock::time_point{} + std::chrono::seconds(seconds);
}

} // namespace

TEST_CASE("a direct entry is returned unchanged, not inverted", "[rate_inverter]") {
    const std::vector<derived_rate> rates = {
        {"EUR", "USD", 1.10, rate_status::fresh, epoch(100)},
    };
    const auto lookup = rate_inverter::make_lookup(rates);

    const auto view = rate_inverter::resolve("EUR", "USD", lookup, true);
    CHECK_FALSE(view.inverted);
    CHECK(view.rate == Catch::Approx(1.10));
    CHECK(view.status == rate_status::fresh);
    CHECK(view.as_of == epoch(100));
}

TEST_CASE("a direct entry with unavailable status is still returned as-is", "[rate_inverter]") {
    const std::vector<derived_rate> rates = {
        {"EUR", "USD", 0.0, rate_status::unavailable, std::chrono::system_clock::time_point{}},
    };
    const auto lookup = rate_inverter::make_lookup(rates);

    const auto view = rate_inverter::resolve("EUR", "USD", lookup, true);
    CHECK_FALSE(view.inverted);
    CHECK(view.status == rate_status::unavailable);
}

TEST_CASE("a missing pair falls back to the reverse pair's inverse when allowed",
         "[rate_inverter]") {
    const std::vector<derived_rate> rates = {
        {"EUR", "CAD", 1.50, rate_status::fresh, epoch(200)},
    };
    const auto lookup = rate_inverter::make_lookup(rates);

    const auto view = rate_inverter::resolve("CAD", "EUR", lookup, true);
    CHECK(view.inverted);
    CHECK(view.status == rate_status::fresh);
    CHECK(view.rate == Catch::Approx(1.0 / 1.50));
    CHECK(view.as_of == epoch(200));
    CHECK(view.base_code == "CAD");
    CHECK(view.quote_code == "EUR");
}

TEST_CASE("a missing pair is unavailable when inversion is not allowed", "[rate_inverter]") {
    const std::vector<derived_rate> rates = {
        {"EUR", "CAD", 1.50, rate_status::fresh, epoch(200)},
    };
    const auto lookup = rate_inverter::make_lookup(rates);

    const auto view = rate_inverter::resolve("CAD", "EUR", lookup, false);
    CHECK_FALSE(view.inverted);
    CHECK(view.status == rate_status::unavailable);
}

TEST_CASE("a stale reverse pair inverts but keeps the stale status", "[rate_inverter]") {
    const std::vector<derived_rate> rates = {
        {"EUR", "CAD", 1.50, rate_status::stale, epoch(50)},
    };
    const auto lookup = rate_inverter::make_lookup(rates);

    const auto view = rate_inverter::resolve("CAD", "EUR", lookup, true);
    CHECK(view.inverted);
    CHECK(view.status == rate_status::stale);
    CHECK(view.rate == Catch::Approx(1.0 / 1.50));
}

TEST_CASE("an unavailable reverse pair does not divide by its zero rate", "[rate_inverter]") {
    const std::vector<derived_rate> rates = {
        {"EUR", "CAD", 0.0, rate_status::unavailable, std::chrono::system_clock::time_point{}},
    };
    const auto lookup = rate_inverter::make_lookup(rates);

    const auto view = rate_inverter::resolve("CAD", "EUR", lookup, true);
    CHECK(view.inverted);
    CHECK(view.status == rate_status::unavailable);
    CHECK(view.rate == Catch::Approx(0.0));
}

TEST_CASE("neither direction present is unavailable regardless of allow_invert",
         "[rate_inverter]") {
    const std::vector<derived_rate> rates = {
        {"EUR", "USD", 1.10, rate_status::fresh, epoch(100)},
    };
    const auto lookup = rate_inverter::make_lookup(rates);

    const auto view = rate_inverter::resolve("GBP", "JPY", lookup, true);
    CHECK_FALSE(view.inverted);
    CHECK(view.status == rate_status::unavailable);
    CHECK(view.rate == Catch::Approx(0.0));
}

TEST_CASE("make_lookup keeps the last entry for a duplicated pair", "[rate_inverter]") {
    const std::vector<derived_rate> rates = {
        {"EUR", "USD", 1.10, rate_status::fresh, epoch(100)},
        {"EUR", "USD", 1.11, rate_status::fresh, epoch(200)},
    };
    const auto lookup = rate_inverter::make_lookup(rates);

    const auto view = rate_inverter::resolve("EUR", "USD", lookup, true);
    CHECK(view.rate == Catch::Approx(1.11));
    CHECK(view.as_of == epoch(200));
}
