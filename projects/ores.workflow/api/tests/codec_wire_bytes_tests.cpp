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
#include "ores.utility/codec/struct_codec.hpp"
#include "ores.utility/codec/wire_traits.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.workflow.api/domain/workflow_instance_codec.hpp"
#include "ores.workflow.api/domain/workflow_plan_dependency_codec.hpp"
#include "ores.workflow.api/domain/workflow_plan_step_codec.hpp"
#include "ores.workflow.api/domain/workflow_step_codec.hpp"
#include "ores.workflow.api/generators/workflow_instance_generator.hpp"
#include "ores.workflow.api/generators/workflow_plan_dependency_generator.hpp"
#include "ores.workflow.api/generators/workflow_plan_step_generator.hpp"
#include "ores.workflow.api/generators/workflow_step_generator.hpp"
#include "ores.workflow.api/messaging/workflow_claim_messages.hpp"
#include "ores.workflow.api/messaging/workflow_protocol.hpp"
#include "ores.workflow.api/workflow/identity_workflow.hpp"
#include <catch2/catch_test_macros.hpp>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <span>
#include <string>

namespace {

const std::string codec_tags("[codec]");

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

template <typename... T>
void every_default_has_the_same_bytes() {
    (same_bytes_as_reflect_cpp(T{}), ...);
}

}

TEST_CASE("every workflow domain type has reflect-cpp's bytes", codec_tags) {
    ores::utility::generation::generation_context ctx(20261102);
    same_bytes_as_reflect_cpp(
        ores::workflow::generators::generate_synthetic_workflow_instance(ctx));
    same_bytes_as_reflect_cpp(
        ores::workflow::generators::generate_synthetic_workflow_plan_dependency(ctx));
    same_bytes_as_reflect_cpp(
        ores::workflow::generators::generate_synthetic_workflow_plan_step(ctx));
    same_bytes_as_reflect_cpp(ores::workflow::generators::generate_synthetic_workflow_step(ctx));
}

TEST_CASE("every workflow message's default has reflect-cpp's bytes", codec_tags) {
    using namespace ores::workflow::messaging;
    every_default_has_the_same_bytes<step_log_entry,
                                     step_completed_event,
                                     start_workflow_message,
                                     get_step_result_request,
                                     get_step_result_response,
                                     workflow_instance_summary,
                                     list_workflow_instance_summaries_request,
                                     list_workflow_instance_summaries_response,
                                     workflow_step_summary,
                                     get_workflow_steps_request,
                                     get_workflow_steps_response,
                                     workflow_step_definition_summary,
                                     workflow_definition_summary,
                                     list_workflow_definitions_request,
                                     list_workflow_definitions_response,
                                     retry_workflow_instance_request,
                                     retry_workflow_instance_response>();
}

TEST_CASE("a completed step with its log has reflect-cpp's bytes", codec_tags) {
    using namespace ores::workflow::messaging;
    step_completed_event event;
    event.workflow_instance_id = "i1";
    event.step_id = "s1";
    event.outcome = step_outcome::completed_with_warnings;
    event.result_json = R"({"a":1})";
    event.error_message = "";
    event.log = {{step_log_level::warn, "skipped", "row 3"}, {step_log_level::error, "bad", ""}};
    same_bytes_as_reflect_cpp(event);

    namespace json = ores::utility::codec::json;
    CHECK(json::write(event.log) == rfl::json::write(event.log));
    const auto log = json::try_read<std::vector<step_log_entry>>(rfl::json::write(event.log));
    REQUIRE(log);
    CHECK(json::write(*log) == json::write(event.log));
}

TEST_CASE("the step claim messages go on the wire through their fields", codec_tags) {
    using namespace ores::workflow::messaging;
    same_bytes_as_reflect_cpp(claim_step_request{"s1", "t1", "svc/42"});
    same_bytes_as_reflect_cpp(claim_step_response{true, false, "running", "held", 2});
}

TEST_CASE("the identity workflow's messages have reflect-cpp's bytes", codec_tags) {
    namespace object = ores::utility::codec::object;
    using ores::workflow::workflow::identity_step_request;
    using ores::workflow::workflow::identity_workflow_request;
    const identity_workflow_request request{
        {identity_step_request{"a", std::string("warn"), true, 2},
         identity_step_request{"b", std::nullopt, std::nullopt, std::nullopt}}};
    const auto json = object::write_json(request);
    CHECK(json == rfl::json::write(request));
    CHECK(object::write_json(object::read_json<identity_workflow_request>(json)) == json);
    CHECK(object::write_msgpack(request) == their_msgpack(request));
}
