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
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <span>
#include <string>

namespace {

const std::string codec_tags("[codec]");

std::span<const std::byte> bytes_of(const std::string& s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

/**
 * @brief The codec reads back what it wrote, in both formats.
 */
template <typename T>
void survives_the_wire(const T& v) {
    using codec = ores::utility::codec::wire_traits<T>;
    CHECK(codec::is_generated);

    const auto json = codec::write_json(v);
    CHECK(codec::write_json(codec::read_json(json)) == json);

    const auto msgpack = codec::write_msgpack(v);
    CHECK(codec::write_msgpack(codec::read_msgpack(bytes_of(msgpack))) == msgpack);
}

template <typename... T>
void every_default_has_the_same_bytes() {
    (survives_the_wire(T{}), ...);
}

}

TEST_CASE("every compute domain type has its wire form", codec_tags) {
    ores::utility::generation::generation_context ctx(20261102);
    survives_the_wire(ores::compute::generators::generate_synthetic_app(ctx));
    survives_the_wire(ores::compute::generators::generate_synthetic_app_version(ctx));
    survives_the_wire(ores::compute::generators::generate_synthetic_app_version_platform(ctx));
    survives_the_wire(ores::compute::generators::generate_synthetic_batch(ctx));
    survives_the_wire(ores::compute::generators::generate_synthetic_grid_sample(ctx));
    survives_the_wire(ores::compute::generators::generate_synthetic_host(ctx));
    survives_the_wire(ores::compute::generators::generate_synthetic_node_sample(ctx));
    survives_the_wire(ores::compute::generators::generate_synthetic_platform(ctx));
    survives_the_wire(ores::compute::generators::generate_synthetic_result(ctx));
    survives_the_wire(ores::compute::generators::generate_synthetic_workflow_batch_link(ctx));
    survives_the_wire(ores::compute::generators::generate_synthetic_workunit(ctx));
}

TEST_CASE("every telemetry and work message's default has its wire form", codec_tags) {
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

TEST_CASE("filled work and telemetry messages survive the wire", codec_tags) {
    ores::compute::messaging::claim_work_response claimed;
    claimed.success = true;
    claimed.has_work = true;
    claimed.message = "claimed";
    claimed.tenant_id = "t1";
    claimed.result_id = "r1";
    claimed.workunit_id = "w1";
    claimed.app_version_id = "a1";
    claimed.package_uri = "s3://p";
    claimed.package_sha256 = "abc";
    claimed.input_uri = "s3://i";
    claimed.config_uri = "s3://c";
    claimed.output_uri = "s3://o";
    claimed.output_files_uri = "s3://f";
    claimed.logs_uri = "s3://l";
    claimed.storage_token = "tok";
    claimed.attempt = 3;
    survives_the_wire(claimed);

    ores::compute::messaging::get_grid_series_response series;
    series.success = true;
    series.message = "ok";
    series.start_time =
        std::chrono::system_clock::time_point(std::chrono::microseconds(1'761'652'800'123'456));
    series.bucket_seconds = 60;
    series.bucket_count = 2;
    series.samples = {1, 2};
    series.total_hosts = {3, 4};
    series.online_hosts = {2, 3};
    series.idle_hosts = {1, 0};
    series.results_inactive = {0, 1};
    series.results_unsent = {5, 6};
    series.results_in_progress = {7, 8};
    series.results_done = {9, 10};
    series.total_workunits = {11, 12};
    series.total_batches = {1, 1};
    series.active_batches = {0, 1};
    series.outcomes_success = {4, 5};
    series.outcomes_client_error = {0, 1};
    series.outcomes_no_reply = {1, 0};
    survives_the_wire(series);
}

TEST_CASE("the entity list responses survive the wire", codec_tags) {
    ores::utility::generation::generation_context ctx(20261103);
    survives_the_wire(ores::compute::messaging::list_apps_response{
        ores::utility::domain::result{ores::utility::domain::outcome::ok, "", "", {}},
        {ores::compute::generators::generate_synthetic_app(ctx),
         ores::compute::generators::generate_synthetic_app(ctx)},
        2});
    survives_the_wire(ores::compute::messaging::list_batches_response{
        ores::utility::domain::result{
            ores::utility::domain::outcome::denied, "denied", "not allowed", {}},
        {ores::compute::generators::generate_synthetic_batch(ctx)},
        1});
}
