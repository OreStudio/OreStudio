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
#include "ores.testing/scoped_environment_override.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

const std::string test_suite("ores.testing.tests");
const std::string tags("[testing]");
const std::string probe("ORES_TESTING_SCOPED_ENVIRONMENT_PROBE");

using ores::platform::environment::environment;

}

TEST_CASE("overrides_a_value_for_its_lifetime_only", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    environment::set_value(probe, "outer");

    {
        const ores::testing::scoped_environment_override guard({{probe, "inner"}});
        const auto inside = environment::get_value_or_default(probe, "");
        BOOST_LOG_SEV(lg, ores::logging::info) << "Inside the guard: " << inside;
        CHECK(inside == "inner");
    }

    CHECK(environment::get_value_or_default(probe, "") == "outer");

    environment::unset_value(probe);
}

TEST_CASE("hides_a_stripped_key_for_its_lifetime_only", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    environment::set_value(probe, "outer");

    {
        const ores::testing::scoped_environment_override guard({}, {probe});
        const auto inside = environment::get_value_or_default(probe, "hidden");
        BOOST_LOG_SEV(lg, ores::logging::info) << "Inside the guard: " << inside;
        CHECK(inside == "hidden");
    }

    CHECK(environment::get_value_or_default(probe, "") == "outer");

    environment::unset_value(probe);
}

TEST_CASE("hides_every_key_it_does_not_carry", tags) {
    auto lg(ores::logging::make_logger(test_suite));
    environment::set_value(probe, "outer");

    {
        const ores::testing::scoped_environment_override guard(
            {{"ORES_TESTING_OTHER_PROBE", "other"}});
        const auto inside = environment::get_value_or_default(probe, "absent");
        BOOST_LOG_SEV(lg, ores::logging::info) << "Inside the guard: " << inside;
        CHECK(inside == "absent");
        CHECK(environment::get_value_or_default("ORES_TESTING_OTHER_PROBE", "") == "other");
    }

    CHECK(environment::get_value_or_default("ORES_TESTING_OTHER_PROBE", "gone") == "gone");
    CHECK(environment::get_value_or_default(probe, "") == "outer");

    environment::unset_value(probe);
}
