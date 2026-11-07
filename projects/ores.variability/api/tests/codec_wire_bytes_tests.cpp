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
#include "ores.utility/codec/wire_traits.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.variability.api/domain/system_setting_codec.hpp"
#include "ores.variability.api/generators/system_setting_generator.hpp"
#include "ores.variability.api/messaging/operations_protocol.hpp"
#include "ores.variability.api/messaging/system_setting_protocol.hpp"
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <optional>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <span>
#include <string>
#include <vector>

namespace {

const std::string tags("[codec]");

template <typename T>
std::string their_msgpack(const T& v) {
    const auto bytes = rfl::msgpack::write(v);
    return std::string(bytes.begin(), bytes.end());
}

std::span<const std::byte> bytes_of(const std::string& s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

/**
 * @brief The generated codec writes the bytes reflect-cpp wrote, in both
 * formats, and reads them back to the same bytes.
 */
template <typename T>
void same_bytes_as_reflect_cpp(const T& v) {
    using codec = ores::utility::codec::wire_traits<T>;
    CHECK(codec::is_generated);

    const auto json = codec::write_json(v);
    CHECK(json == rfl::json::write(v));
    CHECK(codec::write_json(codec::read_json(json)) == json);

    const auto msgpack = codec::write_msgpack(v);
    CHECK(msgpack == their_msgpack(v));
    CHECK(codec::write_msgpack(codec::read_msgpack(bytes_of(msgpack))) == msgpack);
}

ores::variability::domain::system_setting a_setting() {
    ores::utility::generation::generation_context ctx(20261101);
    return ores::variability::generators::generate_synthetic_system_setting(ctx);
}

boost::uuids::uuid an_id() {
    return boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301");
}

}

using namespace ores::variability::messaging;
using ores::utility::domain::outcome;
using ores::utility::domain::precondition;
using ores::utility::domain::precondition_kind;
using ores::utility::domain::result;

TEST_CASE("a system setting writes the bytes reflect-cpp wrote", tags) {
    same_bytes_as_reflect_cpp(a_setting());
}

TEST_CASE("the system setting messages write the bytes reflect-cpp wrote", tags) {
    same_bytes_as_reflect_cpp(system_setting_key{"iam.token.access_lifetime_seconds"});

    same_bytes_as_reflect_cpp(
        system_setting_change{system_setting_write{an_id(),
                                                   "iam.token.access_lifetime_seconds",
                                                   an_id(),
                                                   "900",
                                                   "integer",
                                                   "Access token \"lifetime\""},
                              precondition{precondition_kind::must_match_version, 3}});

    same_bytes_as_reflect_cpp(system_setting_removal{system_setting_key{"a"},
                                                     ores::utility::domain::removal_precondition});

    same_bytes_as_reflect_cpp(system_setting_lookup{system_setting_key{"a"}, a_setting()});
    same_bytes_as_reflect_cpp(system_setting_lookup{system_setting_key{"a"}, std::nullopt});

    same_bytes_as_reflect_cpp(system_settings_filter{std::vector{an_id()}});

    same_bytes_as_reflect_cpp(system_setting_event{
        an_id(),
        system_setting_key{"a"},
        "written",
        7,
        std::chrono::system_clock::time_point(std::chrono::microseconds(1'761'652'800'123'456)),
        std::string("c1")});

    same_bytes_as_reflect_cpp(system_setting_versions_filter{std::nullopt, 1, 4});

    same_bytes_as_reflect_cpp(list_system_settings_request{
        0, 100, ores::utility::domain::order{"name", true}, std::nullopt, std::string("now")});

    same_bytes_as_reflect_cpp(
        list_system_settings_response{result{outcome::ok, "", "", {}}, {a_setting()}, 1});

    same_bytes_as_reflect_cpp(get_system_setting_response{
        result{outcome::denied, "denied", "not allowed", {}}, std::nullopt});
}

TEST_CASE("the variability operations write the bytes reflect-cpp wrote", tags) {
    same_bytes_as_reflect_cpp(clear_bootstrap_mode_request{});
    same_bytes_as_reflect_cpp(complete_party_onboarding_request{an_id()});
    same_bytes_as_reflect_cpp(get_setting_request{"iam.token.access_lifetime_seconds"});
    same_bytes_as_reflect_cpp(
        get_setting_response{result{outcome::ok, "", "", {}}, "900", "integer"});
}
