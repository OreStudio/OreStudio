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
#include "ores.nats/domain/wire_codec.hpp"
#include "ores.scheduler.core/service/action_payloads.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/codec/wire_traits.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <optional>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <span>
#include <string>

namespace {

const std::string tags("[service][codec]");

template <typename T>
std::string their_msgpack(const T& v) {
    const auto bytes = rfl::msgpack::write(v);
    return std::string(bytes.begin(), bytes.end());
}

std::span<const std::byte> bytes_of(const std::string& s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

/**
 * @brief The codec writes the bytes reflect-cpp wrote, in both formats, and
 * reads them back to the same bytes.
 */
template <typename T>
void same_bytes_as_reflect_cpp(const T& v) {
    namespace object = ores::utility::codec::object;
    const auto json = object::write_json(v);
    CHECK(json == rfl::json::write(v));
    CHECK(object::write_json(object::read_json<T>(json)) == json);

    const auto msgpack = object::write_msgpack(v);
    CHECK(msgpack == their_msgpack(v));
    CHECK(object::write_msgpack(object::read_msgpack<T>(bytes_of(msgpack))) == msgpack);
}

}

using namespace ores::scheduler::service;
using ores::utility::codec::object::read_json;

TEST_CASE("the action payloads write the bytes reflect-cpp wrote", tags) {
    same_bytes_as_reflect_cpp(mq_payload_fields{"q1", "report.run", R"({"a": "b"})"});
    same_bytes_as_reflect_cpp(nats_publish_payload{"reporting.v1.ops.trigger", "r1", std::nullopt});
    same_bytes_as_reflect_cpp(
        nats_trigger_body{boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301"),
                          boost::uuids::string_generator{}("00000000-0000-0000-0000-000000000000"),
                          -42,
                          "production",
                          "",
                          "",
                          ""});
    same_bytes_as_reflect_cpp(reply_body{
        reply_result{ores::utility::domain::outcome::denied, "denied", "not \"allowed\""}});
}

TEST_CASE("a job's action payload reads as reflect-cpp read it", tags) {
    const auto mq = read_json<mq_payload_fields>(
        R"({"queue_id": "q1", "message_type": "t", "payload": "{}", "extra": 1})");
    CHECK(mq.queue_id == "q1");
    CHECK(mq.payload == "{}");

    const auto nats = read_json<nats_publish_payload>(R"({"subject": "a.b"})");
    CHECK(nats.subject == "a.b");
    CHECK_FALSE(nats.report_definition_id.has_value());
    CHECK_FALSE(nats.tenant_id.has_value());

    CHECK_THROWS_WITH(read_json<mq_payload_fields>(R"({"queue_id": "q1"})"),
                      Catch::Matchers::ContainsSubstring("Missing member"));
    CHECK_FALSE(rfl::json::read<mq_payload_fields>(std::string(R"({"queue_id": "q1"})")));
}

TEST_CASE("an answer's result reads among the members it does not name", tags) {
    const auto reply = read_json<reply_body>(
        R"({"result": {"outcome": "failed", "code": "c", "message": "m", "fields": []},
            "run_id": "x"})");
    CHECK(reply.result.outcome == ores::utility::domain::outcome::failed);
    CHECK(reply.result.code == "c");
    CHECK(reply.result.message == "m");
}

TEST_CASE("the NATS carrier sends the trigger body through the codec", tags) {
    using codec = ores::utility::codec::wire_traits<nats_trigger_body>;
    CHECK(codec::is_generated);

    const nats_trigger_body body{
        boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301"),
        boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3302"),
        7,
        "production",
        "",
        "",
        ""};
    for (const auto format : {ores::nats::wire_format::json, ores::nats::wire_format::msgpack}) {
        const ores::nats::wire_codec nats_codec(format);
        const auto bytes = nats_codec.encode(body);
        const std::string sent(reinterpret_cast<const char*>(bytes.data()), bytes.size());
        CHECK(sent == (format == ores::nats::wire_format::json ? codec::write_json(body) :
                                                                 codec::write_msgpack(body)));
    }
}
