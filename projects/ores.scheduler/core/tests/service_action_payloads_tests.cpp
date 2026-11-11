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
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <catch2/matchers/catch_matchers_string.hpp>
#include <optional>
#include <span>
#include <string>
#include <vector>

namespace {

const std::string tags("[service][codec]");

std::span<const std::byte> bytes_of(const std::string& s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

struct wider_result {
    ores::utility::domain::outcome outcome;
    std::string code;
    std::string message;
    std::vector<std::string> fields;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<wider_result>) {
    return ::ores::utility::codec::field_list{
        ores::utility::codec::field{"outcome", &wider_result::outcome},
        ores::utility::codec::field{"code", &wider_result::code},
        ores::utility::codec::field{"message", &wider_result::message},
        ores::utility::codec::field{"fields", &wider_result::fields}};
}

struct wider_reply {
    std::string run_id;
    wider_result result;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<wider_reply>) {
    return ::ores::utility::codec::field_list{
        ores::utility::codec::field{"run_id", &wider_reply::run_id},
        ores::utility::codec::field{"result", &wider_reply::result}};
}

/**
 * @brief The codec reads back what it wrote, in both formats.
 */
template <typename T>
void survives_the_wire(const T& v) {
    namespace object = ores::utility::codec::object;
    const auto json = object::write_json(v);
    CHECK(json == ores::utility::codec::json::write(v));
    CHECK(object::write_json(object::read_json<T>(json)) == json);

    const auto msgpack = object::write_msgpack(v);
    CHECK(object::write_msgpack(object::read_msgpack<T>(bytes_of(msgpack))) == msgpack);
}

}

using namespace ores::scheduler::service;
using ores::utility::codec::object::read_json;

TEST_CASE("the action payloads survive the wire", tags) {
    survives_the_wire(mq_payload_fields{"q1", "report.run", R"({"a": "b"})"});
    survives_the_wire(nats_publish_payload{"reporting.v1.ops.trigger", "r1", std::nullopt});
    survives_the_wire(
        nats_trigger_body{boost::uuids::string_generator{}("3f2504e0-4f89-11d3-9a0c-0305e82c3301"),
                          boost::uuids::string_generator{}("00000000-0000-0000-0000-000000000000"),
                          -42,
                          "production",
                          "",
                          "",
                          ""});
    survives_the_wire(reply_body{
        reply_result{ores::utility::domain::outcome::denied, "denied", "not \"allowed\""}});
}

TEST_CASE("a job's action payload reads from the wire it", tags) {
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
    CHECK_FALSE(ores::utility::codec::json::read_expected<mq_payload_fields>(
        std::string(R"({"queue_id": "q1"})")));
}

TEST_CASE("an answer's result reads among the members it does not name", tags) {
    const auto reply = read_json<reply_body>(
        R"({"result": {"outcome": "failed", "code": "c", "message": "m", "fields": []},
            "run_id": "x"})");
    CHECK(reply.result.outcome == ores::utility::domain::outcome::failed);
    CHECK(reply.result.code == "c");
    CHECK(reply.result.message == "m");
}

TEST_CASE("an answer's result reads among unnamed members in MessagePack", tags) {
    const wider_reply wider{"x", {ores::utility::domain::outcome::failed, "c", "m", {"f"}}};
    const auto bytes = ores::utility::codec::msgpack::write(wider);
    const ores::nats::wire_codec nats_codec(ores::nats::wire_format::msgpack);
    const auto reply = nats_codec.decode<reply_body>(bytes_of(bytes));
    REQUIRE(reply);
    CHECK(reply->result.outcome == ores::utility::domain::outcome::failed);
    CHECK(reply->result.code == "c");
    CHECK(reply->result.message == "m");
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
