/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.telemetry.core/messaging/logs_protocol.hpp"
#include "ores.telemetry.core/messaging/nats_samples_protocol.hpp"
#include "ores.telemetry.core/messaging/service_samples_protocol.hpp"
#include "ores.utility/codec/wire_traits.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "test_suite.hpp"
#include <boost/uuid/string_generator.hpp>
#include <catch2/catch_test_macros.hpp>
#include <chrono>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <span>
#include <string>

namespace {

const std::string tags("[messaging][codec]");

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

std::chrono::system_clock::time_point instant() {
    using namespace std::chrono;
    return sys_days{2026y / November / 1} + 16h + 51min + 7s + 123456us;
}

}

using namespace ores::telemetry::messaging;

TEST_CASE("a service roster writes the bytes reflect-cpp wrote", tags) {
    get_service_roster_response v;
    service_roster_slot running;
    running.service_name = "ores.iam.service";
    running.display_name = "IAM";
    running.description = "Accounts and sessions";
    running.service_account = "ores_iam_service";
    running.slot = 1;
    running.state = ores::telemetry::domain::service_state::running;
    running.instance_id = "a1";
    running.host_id = "h1";
    running.version = "0.0.29";
    running.sampled_at = instant();
    service_roster_slot missing;
    missing.service_name = "ores.trading.service";
    missing.display_name = "Trading";
    v.slots = {running, missing};
    same_bytes_as_reflect_cpp(v);
}

TEST_CASE("a NATS server sample writes the bytes reflect-cpp wrote", tags) {
    nats_server_sample v;
    v.sampled_at = instant();
    v.in_msgs = 18446744073709551615ULL;
    v.out_msgs = 42;
    v.in_bytes = 1u << 31;
    v.out_bytes = 7;
    v.connections = 12;
    v.mem_bytes = 104857600;
    v.slow_consumers = 1;
    same_bytes_as_reflect_cpp(v);
}

TEST_CASE("a page of logs writes the bytes reflect-cpp wrote", tags) {
    boost::uuids::string_generator uuid;
    telemetry_log_entry with_session;
    with_session.id = uuid("6f1b2c3d-4e5f-4a6b-8c7d-9e0f1a2b3c4d");
    with_session.timestamp = instant();
    with_session.source_name = "ores.iam.service";
    with_session.session_id = uuid("0a1b2c3d-4e5f-4a6b-8c7d-9e0f1a2b3c4e");
    with_session.level = "warn";
    with_session.component = "ores.iam.core";
    with_session.message = "A sign-in was refused: \"bad\" password";
    telemetry_log_entry bare;
    bare.id = uuid("1a1b2c3d-4e5f-4a6b-8c7d-9e0f1a2b3c4f");
    bare.timestamp = instant();

    get_telemetry_logs_response v;
    v.success = true;
    v.entries = {with_session, bare};
    v.total_count = 2;
    same_bytes_as_reflect_cpp(v);
}

TEST_CASE("a log publish writes the bytes reflect-cpp wrote", tags) {
    publish_log_entries_request v;
    v.source_name = "ores.web";
    v.tag = "bff";
    v.entries = {publish_log_entry_item{"info", 1793551867123, "router", "started"},
                 publish_log_entry_item{"error", -1, "", ""}};
    same_bytes_as_reflect_cpp(v);
}
