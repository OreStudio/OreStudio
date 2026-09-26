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
#include "ores.testing/logging_listener.hpp"
#include <catch2/catch_test_case_info.hpp>
#include <catch2/catch_test_macros.hpp>
#include <cstddef>
#include <string>

namespace {

const std::string test_suite("ores.testing.tests");
const std::string tags("[testing]");

using ores::testing::logging_listener;

}

TEST_CASE("reports_the_module_name_it_was_given", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    logging_listener::set_test_module_name("ores.other.tests");
    const auto module = logging_listener::extract_module_name();
    BOOST_LOG_SEV(lg, ores::logging::info) << "Module: " << module;
    CHECK(module == "ores.other.tests");

    logging_listener::set_test_module_name("ores.testing.tests");
    CHECK(logging_listener::extract_module_name() == "ores.testing.tests");
}

TEST_CASE("takes_the_suite_name_from_the_first_tag", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const Catch::TestCaseInfo info(
        "",
        Catch::NameAndTags("a case", "[my_suite][other]"),
        Catch::SourceLineInfo(__FILE__, static_cast<std::size_t>(__LINE__)));
    const auto suite = logging_listener::extract_suite_name(info);
    BOOST_LOG_SEV(lg, ores::logging::info) << "Suite: " << suite;

    CHECK(suite == "my_suite");
}

TEST_CASE("names_the_default_suite_when_a_case_has_no_tags", tags) {
    auto lg(ores::logging::make_logger(test_suite));

    const Catch::TestCaseInfo info(
        "",
        Catch::NameAndTags("a case", ""),
        Catch::SourceLineInfo(__FILE__, static_cast<std::size_t>(__LINE__)));
    const auto suite = logging_listener::extract_suite_name(info);
    BOOST_LOG_SEV(lg, ores::logging::info) << "Suite: " << suite;

    CHECK(suite == "default_suite");
}
