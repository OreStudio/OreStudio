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
#include "ores.database/repository/stated_order.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <utility>
#include <vector>

namespace {

const std::string tags("[stated_order]");

using column_t = std::pair<std::string, bool>;

std::vector<column_t> columns(const sqlgen::dynamic::OrderBy& order) {
    std::vector<column_t> r;
    for (const auto& w : order.columns)
        r.emplace_back(w.column.name, w.desc);
    return r;
}

}

using ores::database::repository::make_order;

TEST_CASE("an order by a column ends in the key, ascending", tags) {
    const auto order = make_order({"name"}, false, {"id"});
    CHECK(columns(order) == std::vector<column_t>{{"name", false}, {"id", false}});
}

TEST_CASE("a descending order keeps the key ascending", tags) {
    const auto order = make_order({"name"}, true, {"id"});
    CHECK(columns(order) == std::vector<column_t>{{"name", true}, {"id", false}});
}

TEST_CASE("an order by the key takes the stated direction and no tie-break", tags) {
    const auto order = make_order({"id"}, true, {"id"});
    CHECK(columns(order) == std::vector<column_t>{{"id", true}});
}

TEST_CASE("an order by part of a compound key ends in the rest of it", tags) {
    const auto order = make_order({"b"}, true, {"a", "b"});
    CHECK(columns(order) == std::vector<column_t>{{"b", true}, {"a", false}});
}
