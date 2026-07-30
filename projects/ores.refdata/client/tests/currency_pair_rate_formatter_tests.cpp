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
#include "ores.refdata.client/presentation/currency_pair_rate_formatter.hpp"
#include <catch2/catch_test_macros.hpp>

using ores::refdata::client::presentation::currency_pair_rate_formatter;
using ores::refdata::domain::currency_pair_convention;

namespace {

currency_pair_convention make_convention(double pip_factor, double tick_size, int decimal_places) {
    currency_pair_convention convention;
    convention.pair_code = "EUR/USD";
    convention.pip_factor = pip_factor;
    convention.tick_size = tick_size;
    convention.decimal_places = decimal_places;
    return convention;
}

}

TEST_CASE("currency_pair_rate_formatter rounds rate to the nearest tick and decimal_places",
          "[currency_pair_rate_formatter]") {
    // pip_factor 0.0001, tick_size 1 pip -> absolute tick 0.0001;
    // 1.10285123 snaps to 1.1029 at the nearest 0.0001, shown to 3dp.
    const auto convention = make_convention(0.0001, 1.0, 3);
    REQUIRE(currency_pair_rate_formatter::format_rate(1.10285123, convention, false) == "1.103");
}

TEST_CASE("currency_pair_rate_formatter falls back to a fixed default precision "
          "when no convention is available",
          "[currency_pair_rate_formatter]") {
    REQUIRE(currency_pair_rate_formatter::format_rate(1.10285123, std::nullopt, false) ==
            "1.10285");
}

TEST_CASE("currency_pair_rate_formatter snaps to a coarser half-pip tick",
          "[currency_pair_rate_formatter]") {
    // absolute tick = 0.5 * 0.0001 = 0.00005; nearest multiple of 0.00005
    // to 1.10287 is 1.10285.
    const auto convention = make_convention(0.0001, 0.5, 5);
    REQUIRE(currency_pair_rate_formatter::format_rate(1.10287, convention, false) == "1.10285");
}

TEST_CASE("currency_pair_rate_formatter derives reciprocal-preserving precision for a "
          "reversed convention",
          "[currency_pair_rate_formatter]") {
    // Convention is AUD/JPY's own (decimal_places 2, ~83 magnitude); this
    // request renders the reciprocal JPY/AUD rate.
    auto convention = make_convention(0.01, 1.0, 2);
    convention.pair_code = "AUD/JPY";
    REQUIRE(currency_pair_rate_formatter::format_rate(0.0120481928, convention, true) ==
            "0.01205");
}

TEST_CASE("currency_pair_rate_formatter derives reciprocal precision correctly when "
          "the reciprocal magnitude lands exactly on a power-of-ten boundary",
          "[currency_pair_rate_formatter]") {
    // direct magnitude 1/0.01 = 100 (order 2); decimal_places 2 ->
    // 5 significant figures; reciprocal order -2 -> 5-1-(-2) = 6 dp.
    const auto convention = make_convention(0.01, 1.0, 2);
    REQUIRE(currency_pair_rate_formatter::format_rate(0.01, convention, true) == "0.010000");
}

TEST_CASE("currency_pair_rate_formatter does not tick-snap a reversed-convention rate",
          "[currency_pair_rate_formatter]") {
    const auto convention = make_convention(0.0001, 1.0, 3);
    REQUIRE(currency_pair_rate_formatter::format_rate(0.90675, convention, true) == "0.9067");
}
