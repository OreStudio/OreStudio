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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */
#include "ores.marketdata.service/app/named_by.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>
#include <vector>

constexpr const char* named_by_tags = "[named_by]";

namespace {

using ores::marketdata::service::app::named_by;

struct curve_config {
    std::string name;
};

struct surface_blueprint {
    std::string base;
    std::string quote;
};

}

TEST_CASE("a step that names nothing keeps the catalogue whole", named_by_tags) {
    const auto kept = named_by(std::vector<curve_config>{{"EUR-ESTR"}, {"GBP-SONIA"}},
                               std::vector<std::string>{},
                               [](const curve_config& c) { return c.name; });
    REQUIRE(kept.size() == 2);
    CHECK(kept[0].name == "EUR-ESTR");
    CHECK(kept[1].name == "GBP-SONIA");
}

TEST_CASE("a step keeps the items it names, in the catalogue's order", named_by_tags) {
    const auto kept =
        named_by(std::vector<curve_config>{{"EUR-ESTR"}, {"GBP-SONIA"}, {"GBP-LIBOR-3M"}},
                 std::vector<std::string>{"GBP-LIBOR-3M", "EUR-ESTR"},
                 [](const curve_config& c) { return c.name; });
    REQUIRE(kept.size() == 2);
    CHECK(kept[0].name == "EUR-ESTR");
    CHECK(kept[1].name == "GBP-LIBOR-3M");
}

TEST_CASE("a name the catalogue does not hold is left out rather than refused", named_by_tags) {
    const auto kept = named_by(std::vector<curve_config>{{"EUR-ESTR"}},
                               std::vector<std::string>{"EUR-ESTR", "JPY-TONA"},
                               [](const curve_config& c) { return c.name; });
    REQUIRE(kept.size() == 1);
    CHECK(kept[0].name == "EUR-ESTR");
}

TEST_CASE("an item named by two fields is kept by the pair the step states", named_by_tags) {
    const auto kept = named_by(std::vector<surface_blueprint>{{"EUR", "GBP"}, {"GBP", "USD"}},
                               std::vector<std::string>{"GBPUSD"},
                               [](const surface_blueprint& b) { return b.base + b.quote; });
    REQUIRE(kept.size() == 1);
    CHECK(kept[0].base == "GBP");
    CHECK(kept[0].quote == "USD");
}
