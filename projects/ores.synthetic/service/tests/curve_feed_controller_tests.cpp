/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 3 of the License, or (at your option)
 * any later version.
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
#include "../src/curve_feed_controller.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[curve_feed_controller]");

using ores::synthetic::service::ir_curve_feeds_conflict;

}

TEST_CASE("ir_curve_feeds_conflict: same qualifier, same role, is a conflict", tags) {
    CHECK(ir_curve_feeds_conflict("USD/SOFR", "discount", "USD/SOFR", "discount"));
}

TEST_CASE(
    "ir_curve_feeds_conflict: same qualifier, different role, is NOT a conflict "
    "-- a discount curve and a projection curve for the same currency+index+tenor coexist",
    tags) {
    CHECK_FALSE(ir_curve_feeds_conflict("USD/SOFR", "discount", "USD/SOFR", "projection"));
    CHECK_FALSE(ir_curve_feeds_conflict("USD/LIBOR-3M", "self_discounting", "USD/LIBOR-3M",
                                       "projection"));
}

TEST_CASE("ir_curve_feeds_conflict: different qualifier, same role, is NOT a conflict", tags) {
    CHECK_FALSE(ir_curve_feeds_conflict("USD/SOFR", "discount", "EUR/ESTR", "discount"));
}

TEST_CASE("ir_curve_feeds_conflict: different qualifier, different role, is NOT a conflict", tags) {
    CHECK_FALSE(ir_curve_feeds_conflict("USD/SOFR", "discount", "EUR/ESTR", "projection"));
}
