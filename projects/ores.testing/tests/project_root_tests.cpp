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
#include "ores.logging/make_logger.hpp"
#include "ores.testing/project_root.hpp"
#include <catch2/catch_test_macros.hpp>
#include <filesystem>
#include <string>

namespace {

const std::string test_suite("ores.testing.tests");
const std::string tags("[testing]");

}

TEST_CASE("resolves_a_path_that_names_the_component_model", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto resolved = ores::testing::project_root::resolve(
        "projects/ores.testing/modeling/component_overview.org");
    BOOST_LOG_SEV(lg, ores::logging::info) << "Resolved: " << resolved.string();

    CHECK(resolved.filename() == "component_overview.org");
    CHECK(resolved.parent_path().filename() == "modeling");
    CHECK(resolved.is_absolute());
    CHECK(std::filesystem::exists(resolved));
}

TEST_CASE("discovers_a_root_that_holds_the_repository", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto root = ores::testing::project_root::get();
    BOOST_LOG_SEV(lg, ores::logging::info) << "Root: " << root.string();

    CHECK(std::filesystem::exists(root / ".git"));
    CHECK(std::filesystem::is_directory(root / "projects"));
}

TEST_CASE("resolves_a_relative_path_under_the_root_it_found", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const auto resolved = ores::testing::project_root::resolve("projects/ores.testing").string();
    BOOST_LOG_SEV(lg, ores::logging::info) << "Resolved: " << resolved;

    CHECK(resolved.ends_with("projects/ores.testing"));
}
