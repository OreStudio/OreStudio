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
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/generation/generation_engine.hpp"
#include "ores.utility/generation/generation_environment.hpp"
#include "ores.utility/generation/generation_keys.hpp"
#include <algorithm>
#include <array>
#include <catch2/catch_test_macros.hpp>
#include <cctype>
#include <cstdint>
#include <memory>
#include <optional>
#include <stdexcept>
#include <string>
#include <vector>

namespace {

const std::string_view test_suite("ores.utility.tests");
const std::string tags("[generation]");

}

using namespace ores::utility::generation;
using namespace ores::logging;

TEST_CASE("engine_repeats_itself_for_the_same_seed", tags) {
    auto lg(make_logger(test_suite));

    generation_engine first(7);
    generation_engine second(7);

    BOOST_LOG_SEV(lg, info) << "Seed: " << first.seed();

    CHECK(first.seed() == 7);
    for (int i = 0; i < 20; ++i)
        CHECK(first.random_int(1, 1000) == second.random_int(1, 1000));
}

TEST_CASE("engine_differs_for_a_different_seed", tags) {
    auto lg(make_logger(test_suite));

    generation_engine first(7);
    generation_engine second(8);

    bool differs = false;
    for (int i = 0; i < 20; ++i) {
        if (first.random_int(1, 1000) != second.random_int(1, 1000))
            differs = true;
    }

    // Twenty identical draws would mean the engine ignores its seed.
    CHECK(differs);
}

TEST_CASE("random_int_stays_inside_the_requested_bounds", tags) {
    auto lg(make_logger(test_suite));

    generation_engine engine(3);

    for (int i = 0; i < 20; ++i) {
        const auto value = engine.random_int(-5, 5);
        CHECK(value >= -5);
        CHECK(value <= 5);
    }
}

TEST_CASE("random_bool_honours_the_certainty_boundaries", tags) {
    auto lg(make_logger(test_suite));

    generation_engine engine(3);

    for (int i = 0; i < 20; ++i) {
        CHECK(!engine.random_bool(0.0));
        CHECK(engine.random_bool(1.0));
    }
}

TEST_CASE("alphanumeric_returns_the_requested_alphanumeric_length", tags) {
    auto lg(make_logger(test_suite));

    generation_engine engine(3);
    const auto value = engine.alphanumeric(64);

    BOOST_LOG_SEV(lg, info) << "Value: " << value;

    CHECK(value.size() == 64);
    CHECK(std::all_of(
        value.begin(), value.end(), [](unsigned char c) { return std::isalnum(c) != 0; }));
}

TEST_CASE("alphanumeric_of_zero_length_is_empty", tags) {
    auto lg(make_logger(test_suite));

    generation_engine engine(3);

    CHECK(engine.alphanumeric(0).empty());
}

TEST_CASE("pick_returns_an_element_and_rejects_an_empty_vector", tags) {
    auto lg(make_logger(test_suite));

    generation_engine engine(3);
    const std::vector<std::string> items{"alpha", "beta", "gamma"};
    const std::array<int, 2> numbers{10, 20};

    const auto& chosen = engine.pick(items);
    const auto& number = engine.pick(numbers);

    CHECK(std::find(items.begin(), items.end(), chosen) != items.end());
    CHECK((number == 10 || number == 20));
    CHECK_THROWS_AS(engine.pick(std::vector<int>{}), std::out_of_range);
}

TEST_CASE("generate_uuid_carries_version_7_and_the_rfc_variant", tags) {
    auto lg(make_logger(test_suite));

    generation_engine engine(3);
    const auto uuid = engine.generate_uuid();

    CHECK(((uuid.data[6] >> 4) & 0x0F) == 7);
    CHECK(((uuid.data[8] >> 6) & 0x03) == 0x02);
    CHECK(engine.generate_uuid() != uuid);
}

TEST_CASE("environment_reads_its_initial_entries", tags) {
    auto lg(make_logger(test_suite));

    const generation_environment env({{"tenant_id", "abc"}, {"party_id", "42"}});

    CHECK(env.has("tenant_id"));
    CHECK(env.get("tenant_id") == std::optional<std::string>("abc"));
    CHECK(env.get("absent") == std::nullopt);
    CHECK(env.get_or("absent", "fallback") == "fallback");
    CHECK(!env.parent());
}

TEST_CASE("child_environment_inherits_and_overrides", tags) {
    auto lg(make_logger(test_suite));

    const auto parent = std::make_shared<generation_environment>(
        generation_environment::entries{{"tenant_id", "abc"}, {"party_id", "42"}});
    const generation_environment child(parent, {{"party_id", "7"}});

    CHECK(child.get("tenant_id") == std::optional<std::string>("abc"));
    CHECK(child.get("party_id") == std::optional<std::string>("7"));
    CHECK(child.parent() == std::shared_ptr<const generation_environment>(parent));
}

TEST_CASE("child_context_continues_the_parent_random_sequence", tags) {
    auto lg(make_logger(test_suite));

    generation_context parent(11);
    generation_context fork(11);

    (void)parent.random_int(1, 1000000);
    (void)parent.random_int(1, 1000000);
    const auto from_child = parent.child({}).random_int(1, 1000000);

    (void)fork.random_int(1, 1000000);
    (void)fork.random_int(1, 1000000);

    // A child that copied the engine instead of sharing it would restart the
    // sequence here.
    CHECK(fork.random_int(1, 1000000) == from_child);
}

TEST_CASE("context_exposes_its_seed_and_environment", tags) {
    auto lg(make_logger(test_suite));

    const generation_context ctx(99, {{std::string(generation_keys::modified_by), "tester"}});

    CHECK(ctx.seed() == 99);
    CHECK(ctx.env().get(generation_keys::modified_by) == std::optional<std::string>("tester"));
}

TEST_CASE("generation_keys_name_the_shared_entries", tags) {
    auto lg(make_logger(test_suite));

    CHECK(generation_keys::modified_by == "modified_by");
    CHECK(generation_keys::tenant_id == "tenant_id");
    CHECK(generation_keys::party_id == "party_id");
    CHECK(generation_keys::counterparty_id == "counterparty_id");
    CHECK(generation_keys::account_id == "account_id");
    CHECK(generation_keys::catalog_id == "catalog_id");
}
