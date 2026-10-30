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
#include "ores.scheduler.api/domain/cron_expression.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/msgpack_codec.hpp"
#include "test_suite.hpp"
#include <catch2/catch_test_macros.hpp>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>

namespace wire_json = ores::utility::codec::json;
namespace wire_msgpack = ores::utility::codec::msgpack;

namespace {

template <typename T>
std::string rfl_msgpack_cron(const T& v) {
    const auto bytes = rfl::msgpack::write(v);
    return std::string(bytes.begin(), bytes.end());
}

}

TEST_CASE("a cron expression is the text reflect-cpp writes", "[codec]") {
    const auto expression = *ores::scheduler::domain::cron_expression::from_string("0 0 * * *");

    CHECK(wire_json::write(expression) == rfl::json::write(expression));
    CHECK(wire_msgpack::write(expression) == rfl_msgpack_cron(expression));
    CHECK(wire_json::read<ores::scheduler::domain::cron_expression>(wire_json::write(expression)) ==
          expression);
    CHECK(wire_msgpack::read<ores::scheduler::domain::cron_expression>(
              wire_msgpack::write(expression)) == expression);
    CHECK(wire_json::read<ores::scheduler::domain::cron_expression>(rfl::json::write(expression)) ==
          expression);
    CHECK(wire_msgpack::read<ores::scheduler::domain::cron_expression>(
              rfl_msgpack_cron(expression)) == expression);
}

TEST_CASE("an expression the type refuses is refused on the wire too", "[codec]") {
    CHECK_THROWS_AS(
        wire_json::read<ores::scheduler::domain::cron_expression>("\"not a cron expression\""),
        std::invalid_argument);
}
