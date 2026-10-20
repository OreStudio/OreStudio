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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/xml/stable_arena.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <utility>

    namespace {

    const std::string_view test_suite("ores.ore.tests");
    const std::string tags("[ore][xml][arena]");

    using ores::ore::xml::stable_arena;
    using namespace ores::logging;

    struct row {
        std::string name;
    };
}

TEST_CASE("an_arena_keeps_every_element_where_it_was_put", tags) {
    auto lg(make_logger(test_suite));

    stable_arena<row> arena;
    auto* const first = &arena.emplace_back();
    first->name = "first";
    for (int i = 0; i < 1000; ++i)
        arena.emplace_back();

    CHECK(arena.size() == 1001);
    CHECK(&arena[0] == first);
    CHECK(arena[0].name == "first");
}

TEST_CASE("a_copied_arena_owns_its_own_elements", tags) {
    auto lg(make_logger(test_suite));

    stable_arena<row> arena;
    arena.emplace_back().name = "a";
    arena.emplace_back().name = "b";

    stable_arena<row> copy(arena);
    copy[0].name = "changed";

    CHECK(copy.size() == 2);
    CHECK(arena[0].name == "a");
    CHECK(&copy[1] != &arena[1]);
    CHECK(copy.back().name == "b");
}

TEST_CASE("a_moved_arena_keeps_the_addresses_of_its_elements", tags) {
    auto lg(make_logger(test_suite));

    stable_arena<row> arena;
    const auto* const element = &arena.emplace_back();

    const stable_arena<row> moved(std::move(arena));

    CHECK(moved.size() == 1);
    CHECK(&moved[0] == element);
}
