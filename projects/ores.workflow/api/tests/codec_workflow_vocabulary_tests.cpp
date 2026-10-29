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
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/msgpack_codec.hpp"
#include "ores.workflow.api/messaging/workflow_vocabulary.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>

namespace wire_json = ores::utility::codec::json;
namespace wire_msgpack = ores::utility::codec::msgpack;

namespace {

template <typename T>
std::string rfl_msgpack_vocabulary(const T& v) {
    const auto bytes = rfl::msgpack::write(v);
    return std::string(bytes.begin(), bytes.end());
}

}

TEST_CASE("the workflow vocabulary is the tokens reflect-cpp writes", "[codec]") {
    using namespace ores::workflow::messaging;

    for (const auto v : {step_log_level::info, step_log_level::warn, step_log_level::error}) {
        CHECK(wire_json::write(v) == rfl::json::write(v));
        CHECK(wire_msgpack::write(v) == rfl_msgpack_vocabulary(v));
        CHECK(wire_json::read<step_log_level>(wire_json::write(v)) == v);
        CHECK(wire_msgpack::read<step_log_level>(wire_msgpack::write(v)) == v);
        CHECK(wire_json::read<step_log_level>(rfl::json::write(v)) == v);
        CHECK(wire_msgpack::read<step_log_level>(rfl_msgpack_vocabulary(v)) == v);
    }
    for (const auto v :
         {step_outcome::completed, step_outcome::completed_with_warnings, step_outcome::failed}) {
        CHECK(wire_json::write(v) == rfl::json::write(v));
        CHECK(wire_msgpack::write(v) == rfl_msgpack_vocabulary(v));
        CHECK(wire_json::read<step_outcome>(wire_json::write(v)) == v);
        CHECK(wire_msgpack::read<step_outcome>(wire_msgpack::write(v)) == v);
        CHECK(wire_json::read<step_outcome>(rfl::json::write(v)) == v);
        CHECK(wire_msgpack::read<step_outcome>(rfl_msgpack_vocabulary(v)) == v);
    }
}

TEST_CASE("a token neither vocabulary states is refused", "[codec]") {
    CHECK_THROWS_AS(wire_json::read<ores::workflow::messaging::step_outcome>("\"no such outcome\""),
                    std::invalid_argument);
}
