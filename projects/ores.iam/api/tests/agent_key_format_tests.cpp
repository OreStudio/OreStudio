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
#include "ores.iam.api/domain/agent_key_format.hpp"
#include <boost/uuid/random_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <string>

namespace {

const std::string tags("[agent_key_format]");

using ores::iam::domain::agent_key_parts;
using ores::iam::domain::format_agent_key;
using ores::iam::domain::is_agent_key;
using ores::iam::domain::parse_agent_key;

agent_key_parts sample() {
    boost::uuids::random_generator gen;
    return {.tenant_id = gen(), .key_id = gen(), .secret = std::string(64, 'a')};
}

}

TEST_CASE("a_formatted_key_parses_back_to_its_parts", tags) {
    const auto parts = sample();
    const auto key = format_agent_key(parts);
    const auto parsed = parse_agent_key(key);
    REQUIRE(parsed.has_value());
    CHECK(parsed->tenant_id == parts.tenant_id);
    CHECK(parsed->key_id == parts.key_id);
    CHECK(parsed->secret == parts.secret);
    CHECK(is_agent_key(key));
}

TEST_CASE("a_token_is_not_an_agent_key", tags) {
    CHECK_FALSE(is_agent_key("eyJhbGciOiJSUzI1NiJ9.e30.sig"));
    CHECK_FALSE(parse_agent_key("eyJhbGciOiJSUzI1NiJ9.e30.sig").has_value());
}

TEST_CASE("a_key_of_the_wrong_length_does_not_parse", tags) {
    const auto key = format_agent_key(sample());
    CHECK_FALSE(parse_agent_key(key.substr(0, key.size() - 1)).has_value());
    CHECK_FALSE(parse_agent_key(key + "0").has_value());
}

TEST_CASE("a_key_with_a_non_hex_digit_does_not_parse", tags) {
    auto key = format_agent_key(sample());
    key.back() = 'g';
    CHECK_FALSE(parse_agent_key(key).has_value());
    key = format_agent_key(sample());
    key[10] = 'Z';
    CHECK_FALSE(parse_agent_key(key).has_value());
}

TEST_CASE("a_key_with_a_misplaced_separator_does_not_parse", tags) {
    auto key = format_agent_key(sample());
    const auto first = std::string("ores_ak_").size() + 32;
    key[first] = 'a';
    CHECK_FALSE(parse_agent_key(key).has_value());
}
