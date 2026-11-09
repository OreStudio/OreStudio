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
#include "ores.trading.api/domain/fx_variance_swap_instrument.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

using namespace ores::trading::domain;

}

TEST_CASE("an_fx_variance_swap_keeps_its_dates_across_the_wire", "[fx][wire]") {
    fx_variance_swap_instrument row;
    row.identity.trade_type_code = "FxVarianceSwap";
    row.start_date = std::chrono::year{2027} / std::chrono::month{3} / std::chrono::day{15};
    row.end_date = std::chrono::year{2029} / std::chrono::month{3} / std::chrono::day{15};
    row.currency = "EUR";
    row.underlying_code = "EUR-USD";
    row.long_short = "Long";
    row.strike = 0.05;
    row.notional = *ores::utility::decimal::decimal::from_double(1000000.0);
    row.moment_type = "Variance";

    using codec = ores::utility::codec::wire_traits<fx_variance_swap_instrument>;
    const auto written = codec::write_json(row);
    INFO(written);
    const auto back = codec::read_json(written);
    CHECK(back.start_date == row.start_date);
    CHECK(back.end_date == row.end_date);
}

TEST_CASE("an_fx_variance_swap_keeps_its_dates_across_msgpack", "[fx][wire]") {
    fx_variance_swap_instrument row;
    row.identity.trade_type_code = "FxVarianceSwap";
    row.start_date = std::chrono::year{2027} / std::chrono::month{3} / std::chrono::day{15};
    row.end_date = std::chrono::year{2029} / std::chrono::month{3} / std::chrono::day{15};
    row.currency = "EUR";
    row.underlying_code = "EUR-USD";
    row.long_short = "Long";
    row.strike = 0.05;
    row.notional = *ores::utility::decimal::decimal::from_double(1000000.0);
    row.moment_type = "Variance";

    using codec = ores::utility::codec::wire_traits<fx_variance_swap_instrument>;
    const auto bytes = codec::write_msgpack(row);
    const auto back = codec::read_msgpack(
        std::span(reinterpret_cast<const std::byte*>(bytes.data()), bytes.size()));
    CHECK(back.start_date == row.start_date);
    CHECK(back.end_date == row.end_date);
}
