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
#include "ores.synthetic.api/feeds/ir_curve_feed.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[ir_curve_feed][vintage]");

using ores::synthetic::feed::ir_curve_resolved_entry;
using ores::synthetic::feed::select_vintage_anchor_entry;

ir_curve_resolved_entry make_entry(const std::string& point_id,
                                   const std::string& curve_role,
                                   std::size_t ticks_ahead_end) {
    ir_curve_resolved_entry e;
    e.point_id = point_id;
    e.curve_role = curve_role;
    e.ticks_ahead_end = ticks_ahead_end;
    return e;
}

}

TEST_CASE("select_vintage_anchor_entry picks the shortest-tenor DEPOSIT entry", tags) {
    const std::vector<ir_curve_resolved_entry> resolved{
        make_entry("3M", "DEPOSIT", 90),
        make_entry("1M", "DEPOSIT", 30),
        make_entry("6M", "FRA", 180),
        make_entry("1Y", "SWAP", 365),
    };

    const auto* anchor = select_vintage_anchor_entry(resolved);

    REQUIRE(anchor != nullptr);
    CHECK(anchor->point_id == "1M");
    CHECK(anchor->curve_role == "DEPOSIT");
}

TEST_CASE("select_vintage_anchor_entry ignores non-DEPOSIT entries entirely", tags) {
    const std::vector<ir_curve_resolved_entry> resolved{
        make_entry("1M", "FRA", 30),
        make_entry("2M", "SWAP", 60),
    };

    CHECK(select_vintage_anchor_entry(resolved) == nullptr);
}

TEST_CASE("select_vintage_anchor_entry returns nullptr for an empty template", tags) {
    const std::vector<ir_curve_resolved_entry> resolved{};
    CHECK(select_vintage_anchor_entry(resolved) == nullptr);
}

TEST_CASE("select_vintage_anchor_entry is stable when several DEPOSIT entries tie on tenor", tags) {
    // Ties should not throw or crash; either candidate is an acceptable anchor. This just
    // documents that behaviour is deterministic (first-seen wins, since strictly-less-than never
    // replaces an equal ticks_ahead_end).
    const std::vector<ir_curve_resolved_entry> resolved{
        make_entry("ON-A", "DEPOSIT", 1),
        make_entry("ON-B", "DEPOSIT", 1),
    };

    const auto* anchor = select_vintage_anchor_entry(resolved);

    REQUIRE(anchor != nullptr);
    CHECK(anchor->point_id == "ON-A");
}
