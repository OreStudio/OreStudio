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
#include "ores.testing/scoped_environment_override.hpp"
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

const std::string tags("[ores.testing.scoped_environment_override]");

const std::string probe("ORES_TESTING_SCOPED_ENVIRONMENT_PROBE");

using ores::platform::environment::environment;

}

TEST_CASE("scoped_environment_override overrides a value for its lifetime only", tags) {
    environment::set_value(probe, "outer");

    {
        const ores::testing::scoped_environment_override guard({{probe, "inner"}});

        CHECK(environment::get_value_or_default(probe, "") == "inner");
    }

    CHECK(environment::get_value_or_default(probe, "") == "outer");

    environment::unset_value(probe);
}

TEST_CASE("scoped_environment_override hides a stripped key for its lifetime only", tags) {
    environment::set_value(probe, "outer");

    {
        const ores::testing::scoped_environment_override guard({}, {probe});

        CHECK(environment::get_value_or_default(probe, "hidden") == "hidden");
    }

    CHECK(environment::get_value_or_default(probe, "") == "outer");

    environment::unset_value(probe);
}

TEST_CASE("scoped_environment_override hides every key it does not carry", tags) {
    environment::set_value(probe, "outer");

    {
        const ores::testing::scoped_environment_override guard(
            {{"ORES_TESTING_OTHER_PROBE", "other"}});

        CHECK(environment::get_value_or_default(probe, "absent") == "absent");
        CHECK(environment::get_value_or_default("ORES_TESTING_OTHER_PROBE", "") == "other");
    }

    CHECK(environment::get_value_or_default("ORES_TESTING_OTHER_PROBE", "gone") == "gone");
    CHECK(environment::get_value_or_default(probe, "") == "outer");

    environment::unset_value(probe);
}
