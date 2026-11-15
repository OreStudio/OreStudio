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
#include "ores.platform/process/memory.hpp"
#include <catch2/catch_test_macros.hpp>
#include <cstdint>
#include <numeric>
#include <vector>

using namespace ores::platform::process;

TEST_CASE("peak_resident_bytes_counts_memory_the_process_touched", "[process]") {
    constexpr std::int64_t touched = 64 * 1024 * 1024;

    std::vector<unsigned char> block(static_cast<std::size_t>(touched), 1);
    const auto sum = std::accumulate(block.begin(), block.end(), std::int64_t{0});

    REQUIRE(sum == touched);
    CHECK(peak_resident_bytes() >= touched);
}

TEST_CASE("peak_resident_bytes_never_falls", "[process]") {
    const auto first = peak_resident_bytes();
    const auto second = peak_resident_bytes();

    CHECK(first > 0);
    CHECK(second >= first);
}
