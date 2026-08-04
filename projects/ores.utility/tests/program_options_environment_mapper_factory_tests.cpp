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
#include "ores.utility/program_options/environment_mapper_factory.hpp"
#include "ores.utility/program_options/shared_domain_registry.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[environment_mapper_factory]");

using ores::utility::program_options::environment_mapper_factory;
using ores::utility::program_options::shared_domain_registry;

}

TEST_CASE("app_prefixed_variable_maps_to_kebab_case_option", tags) {
    const auto mapper(environment_mapper_factory::make_mapper("IAM_SERVICE"));
    REQUIRE(mapper("ORES_IAM_SERVICE_DB_PASSWORD") == "db-password");
}

TEST_CASE("unrelated_variable_returns_empty", tags) {
    const auto mapper(environment_mapper_factory::make_mapper("IAM_SERVICE"));
    REQUIRE(mapper("SOME_OTHER_VAR").empty());
}

TEST_CASE("unregistered_domain_variable_returns_empty", tags) {
    const auto mapper(environment_mapper_factory::make_mapper("IAM_SERVICE"));
    REQUIRE(mapper("ORES_TOTALLY_UNREGISTERED_DOMAIN_OPTION").empty());
}

TEST_CASE("registered_shared_domain_variable_falls_back_generically", tags) {
    shared_domain_registry::register_domain("WIDGET");
    const auto mapper(environment_mapper_factory::make_mapper("IAM_SERVICE"));
    REQUIRE(mapper("ORES_WIDGET_COLOUR") == "colour");
}

TEST_CASE("app_prefix_wins_over_shared_domain_when_both_match", tags) {
    shared_domain_registry::register_domain("IAM_SERVICE");
    const auto mapper(environment_mapper_factory::make_mapper("IAM_SERVICE"));

    // ORES_IAM_SERVICE_FOO matches the app prefix directly; the shared-domain
    // fallback is never consulted for it either way, but this pins the
    // documented precedence in case that ever changes.
    REQUIRE(mapper("ORES_IAM_SERVICE_FOO") == "foo");
}

TEST_CASE("a_second_shared_domain_resolves_without_mapper_changes", tags) {
    shared_domain_registry::register_domain("GADGET");
    const auto mapper(environment_mapper_factory::make_mapper("SOME_APP"));
    REQUIRE(mapper("ORES_GADGET_SIZE") == "size");
}
