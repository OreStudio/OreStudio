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
#include "ores.compute.api/domain/app_codec.hpp"
#include "ores.compute.api/domain/app_version_codec.hpp"
#include "ores.compute.api/domain/app_version_platform_codec.hpp"
#include "ores.compute.api/domain/batch_codec.hpp"
#include "ores.compute.api/domain/grid_sample_codec.hpp"
#include "ores.compute.api/domain/host_codec.hpp"
#include "ores.compute.api/domain/node_sample_codec.hpp"
#include "ores.compute.api/domain/platform_codec.hpp"
#include "ores.compute.api/domain/result_codec.hpp"
#include "ores.compute.api/domain/workflow_batch_link_codec.hpp"
#include "ores.compute.api/domain/workunit_codec.hpp"
#include "ores.compute.api/generators/app_generator.hpp"
#include "ores.compute.api/generators/app_version_generator.hpp"
#include "ores.compute.api/generators/app_version_platform_generator.hpp"
#include "ores.compute.api/generators/batch_generator.hpp"
#include "ores.compute.api/generators/grid_sample_generator.hpp"
#include "ores.compute.api/generators/host_generator.hpp"
#include "ores.compute.api/generators/node_sample_generator.hpp"
#include "ores.compute.api/generators/platform_generator.hpp"
#include "ores.compute.api/generators/result_generator.hpp"
#include "ores.compute.api/generators/workflow_batch_link_generator.hpp"
#include "ores.compute.api/generators/workunit_generator.hpp"
#include "ores.compute.api/messaging/app_protocol.hpp"
#include "ores.compute.api/messaging/batch_protocol.hpp"
#include "ores.compute.api/messaging/telemetry_protocol.hpp"
#include "ores.compute.api/messaging/work_protocol.hpp"
#include "ores.utility/codec/wire_traits.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
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

TEST_CASE("every compute domain type has reflect-cpp's bytes", codec_tags) {
    ores::utility::generation::generation_context ctx(20261102);
    same_bytes_as_reflect_cpp(ores::compute::generators::generate_synthetic_app(ctx));
    same_bytes_as_reflect_cpp(ores::compute::generators::generate_synthetic_app_version(ctx));
    same_bytes_as_reflect_cpp(
        ores::compute::generators::generate_synthetic_app_version_platform(ctx));
    same_bytes_as_reflect_cpp(ores::compute::generators::generate_synthetic_batch(ctx));
    same_bytes_as_reflect_cpp(ores::compute::generators::generate_synthetic_grid_sample(ctx));
    same_bytes_as_reflect_cpp(ores::compute::generators::generate_synthetic_host(ctx));
    same_bytes_as_reflect_cpp(ores::compute::generators::generate_synthetic_node_sample(ctx));
    same_bytes_as_reflect_cpp(ores::compute::generators::generate_synthetic_platform(ctx));
    same_bytes_as_reflect_cpp(ores::compute::generators::generate_synthetic_result(ctx));
    same_bytes_as_reflect_cpp(
        ores::compute::generators::generate_synthetic_workflow_batch_link(ctx));
    same_bytes_as_reflect_cpp(ores::compute::generators::generate_synthetic_workunit(ctx));
}

TEST_CASE("every telemetry and work message's default has reflect-cpp's bytes", codec_tags) {
    using namespace ores::compute::messaging;
    every_default_has_the_same_bytes<get_grid_stats_request,
                                     node_stats_summary,
                                     get_grid_stats_response,
                                     get_grid_series_request,
                                     get_grid_series_response,
                                     get_node_series_request,
                                     node_series_node,
                                     get_node_series_response,
                                     node_sample_message,
                                     get_grid_usage_request,
                                     get_grid_usage_response,
                                     work_assignment_event,
                                     heartbeat_message,
                                     reap_work_message,
                                     claim_work_request,
                                     claim_work_response,
                                     work_available_event,
                                     submit_result_request,
                                     submit_result_response>();
}

TEST_CASE("the entity list responses have reflect-cpp's bytes", codec_tags) {
    ores::utility::generation::generation_context ctx(20261103);
    same_bytes_as_reflect_cpp(ores::compute::messaging::list_apps_response{
        ores::utility::domain::result{ores::utility::domain::outcome::ok, "", "", {}},
        {ores::compute::generators::generate_synthetic_app(ctx),
         ores::compute::generators::generate_synthetic_app(ctx)},
        2});
    same_bytes_as_reflect_cpp(ores::compute::messaging::list_batches_response{
        ores::utility::domain::result{
            ores::utility::domain::outcome::denied, "denied", "not allowed", {}},
        {ores::compute::generators::generate_synthetic_batch(ctx)},
        1});
}
