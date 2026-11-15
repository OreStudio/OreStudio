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
#include "ores.utility/domain/protocol.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <numeric>
#include <span>
#include <vector>

using ores::utility::domain::batches_of;
using ores::utility::domain::max_batch_size;

namespace {

std::vector<int> numbered(std::size_t n) {
    std::vector<int> items(n);
    std::iota(items.begin(), items.end(), 0);
    return items;
}

std::vector<int> joined(const std::vector<std::span<const int>>& parts) {
    std::vector<int> all;
    for (const auto part : parts)
        all.insert(all.end(), part.begin(), part.end());
    return all;
}

}

TEST_CASE("a_set_longer_than_the_bound_is_split_into_bounded_parts_in_order", "[protocol]") {
    const auto items = numbered(2 * max_batch_size + 1);

    const auto parts = batches_of(items);

    REQUIRE(parts.size() == 3);
    CHECK(parts[0].size() == max_batch_size);
    CHECK(parts[1].size() == max_batch_size);
    CHECK(parts[2].size() == 1);
    CHECK(joined(parts) == items);
}

TEST_CASE("a_set_of_exactly_the_bound_is_one_part", "[protocol]") {
    const auto items = numbered(max_batch_size);

    const auto parts = batches_of(items);

    REQUIRE(parts.size() == 1);
    CHECK(joined(parts) == items);
}

TEST_CASE("an_empty_set_has_no_part", "[protocol]") {
    CHECK(batches_of(std::vector<int>{}).empty());
}

TEST_CASE("the_bound_is_the_one_the_protocol_states", "[protocol]") {
    CHECK(max_batch_size == 1000);
}
