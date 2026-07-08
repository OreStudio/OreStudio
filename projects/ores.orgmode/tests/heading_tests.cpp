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
#include "ores.orgmode/domain/heading.hpp"
#include <catch2/catch_test_macros.hpp>

using ores::orgmode::domain::join_paragraph_lines;

TEST_CASE("join_paragraph_lines joins wrapped lines with a single space",
          "[ores.orgmode][heading]") {
    const std::vector<std::string> lines = {
        "This is a paragraph that",
        "wraps across several",
        "source lines.",
    };
    REQUIRE(join_paragraph_lines(lines) == "This is a paragraph that wraps across several source lines.");
}

TEST_CASE("join_paragraph_lines trims each line before joining", "[ores.orgmode][heading]") {
    const std::vector<std::string> lines = {"  leading space", "trailing space  "};
    REQUIRE(join_paragraph_lines(lines) == "leading space trailing space");
}

TEST_CASE("join_paragraph_lines skips blank lines", "[ores.orgmode][heading]") {
    const std::vector<std::string> lines = {"first", "", "second"};
    REQUIRE(join_paragraph_lines(lines) == "first second");
}

TEST_CASE("join_paragraph_lines returns empty string for no lines", "[ores.orgmode][heading]") {
    REQUIRE(join_paragraph_lines({}).empty());
}
