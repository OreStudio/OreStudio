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
#include "ores.vault.client/client/service_password.hpp"
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <stdexcept>
#include <string>

using namespace ores::vault::client;
using Catch::Matchers::ContainsSubstring;

namespace {

// The two variables as names the fixture can set. The suites of this component
// share one translation unit, so these are spelled apart from the constants the
// other suite holds.
std::string named_consumer() {
    return std::string(consumer_variable);
}

std::string named_secret() {
    return std::string(secret_variable);
}

}

TEST_CASE("a_named_process_learns_both_halves_of_its_credential", "[client]") {
    const ores::testing::scoped_environment_override environment(
        {{named_consumer(), "iam_service@system.ores"},
         {named_secret(), "db.iam_service.password"}});

    const auto placed = placed_credential_from_environment();

    REQUIRE(placed.has_value());
    CHECK(placed->consumer == "iam_service@system.ores");
    CHECK(placed->secret == "db.iam_service.password");
}

TEST_CASE("a_process_named_nothing_resolves_nothing", "[client]") {
    const ores::testing::scoped_environment_override environment(
        {}, {named_consumer(), named_secret()});

    CHECK_FALSE(placed_credential_from_environment().has_value());
}

TEST_CASE("half_a_named_unit_is_refused_naming_the_missing_half", "[client]") {
    {
        const ores::testing::scoped_environment_override environment(
            {{named_consumer(), "iam_service@system.ores"}}, {named_secret()});

        REQUIRE_THROWS_WITH(placed_credential_from_environment(),
                            ContainsSubstring("ORES_VAULT_PASSWORD_SECRET"));
    }

    {
        const ores::testing::scoped_environment_override environment(
            {{named_secret(), "db.iam_service.password"}}, {named_consumer()});

        REQUIRE_THROWS_WITH(placed_credential_from_environment(),
                            ContainsSubstring("ORES_VAULT_CONSUMER"));
    }
}
