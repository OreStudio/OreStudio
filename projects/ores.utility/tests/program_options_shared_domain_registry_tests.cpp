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
#include "ores.utility/program_options/shared_domain_registry.hpp"
#include <catch2/catch_test_macros.hpp>

namespace {

const std::string tags("[shared_domain_registry]");

using ores::utility::program_options::shared_domain_registry;

}

TEST_CASE("register_domain_makes_it_appear_in_domains", tags) {
    shared_domain_registry::register_domain("SPROCKET", {"COLOUR"});
    REQUIRE(shared_domain_registry::domains().contains("SPROCKET"));
}

TEST_CASE("registered_domain_carries_its_allowed_suffixes", tags) {
    shared_domain_registry::register_domain("COG", {"SIZE", "MATERIAL"});

    const auto& suffixes = shared_domain_registry::domains().at("COG");
    REQUIRE(suffixes.contains("SIZE"));
    REQUIRE(suffixes.contains("MATERIAL"));
    REQUIRE_FALSE(suffixes.contains("COLOUR"));
}

TEST_CASE("registering_the_same_domain_twice_merges_suffixes", tags) {
    shared_domain_registry::register_domain("RATCHET", {"SIZE"});
    shared_domain_registry::register_domain("RATCHET", {"TORQUE"});

    const auto& suffixes = shared_domain_registry::domains().at("RATCHET");
    REQUIRE(suffixes.contains("SIZE"));
    REQUIRE(suffixes.contains("TORQUE"));
}

TEST_CASE("unregistered_domain_is_absent", tags) {
    REQUIRE_FALSE(shared_domain_registry::domains().contains("NEVER_REGISTERED_DOMAIN"));
}
