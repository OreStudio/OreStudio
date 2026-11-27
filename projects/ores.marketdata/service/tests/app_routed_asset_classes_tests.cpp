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
#include "ores.marketdata.service/app/routed_asset_classes.hpp"
#include <catch2/catch_test_macros.hpp>
#include <set>
#include <string>
#include <vector>

constexpr const char* routed_classes_tags = "[routed_asset_classes]";

namespace {

using ores::marketdata::service::app::routed_asset_classes;

}

TEST_CASE("a feed that states no scope is routed to every class its publisher maps",
          routed_classes_tags) {
    const auto routed = routed_asset_classes({"fx", "ir"}, {});
    CHECK(routed == std::vector<std::string>{"fx", "ir"});
}

TEST_CASE("a feed whose scope holds one class routes that class alone", routed_classes_tags) {
    const auto routed = routed_asset_classes({"fx", "ir"}, {"fx"});
    CHECK(routed == std::vector<std::string>{"fx"});
}

TEST_CASE("a feed whose scope matches none of the mapped classes is routed by its scope",
          routed_classes_tags) {
    const auto routed = routed_asset_classes({"ir"}, {"fx"});
    CHECK(routed == std::vector<std::string>{"fx"});
}

TEST_CASE("a publisher that maps nothing is routed as fx", routed_classes_tags) {
    const auto routed = routed_asset_classes({}, {});
    CHECK(routed == std::vector<std::string>{"fx"});
}
