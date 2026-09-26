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
#include "ores.testing/logging_listener.hpp"
#include <catch2/catch_test_case_info.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <string>

namespace {

const std::string tags("[ores.testing.logging_listener]");

using ores::testing::logging_listener;

}

TEST_CASE("logging_listener reports the module name it was given", tags) {
    logging_listener::set_test_module_name("ores.testing.tests");

    CHECK(logging_listener::extract_module_name() == "ores.testing.tests");

    logging_listener::set_test_module_name("ores.testing.tests");
}

TEST_CASE("logging_listener takes the suite name from the first tag", tags) {
    const Catch::TestCaseInfo info(
        "",
        Catch::NameAndTags("a case", "[my_suite][other]"),
        Catch::SourceLineInfo(__FILE__, static_cast<std::size_t>(__LINE__)));

    CHECK(logging_listener::extract_suite_name(info) == "my_suite");
}

TEST_CASE("logging_listener names the default suite when a case has no tags", tags) {
    const Catch::TestCaseInfo info(
        "",
        Catch::NameAndTags("a case", ""),
        Catch::SourceLineInfo(__FILE__, static_cast<std::size_t>(__LINE__)));

    CHECK(logging_listener::extract_suite_name(info) == "default_suite");
}
