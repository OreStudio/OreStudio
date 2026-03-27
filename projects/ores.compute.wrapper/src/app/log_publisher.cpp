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
#include "ores.compute.wrapper/app/log_publisher.hpp"

#include <fstream>
#include <span>
#include <string>
#include <vector>
#include <rfl/json.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.ore/log/ore_log_parser.hpp"
#include "ores.telemetry/messaging/telemetry_protocol.hpp"

namespace ores::compute::wrapper::app {

namespace {

using namespace ores::logging;

inline auto& log_publisher_lg() {
    static auto instance = ores::logging::make_logger(
        "ores.compute.wrapper.app.log_publisher");
    return instance;
}

constexpr std::size_t batch_size = 200;

void publish_batch(ores::nats::service::client& nats,
    ores::telemetry::messaging::publish_log_entries_request& req)
{
    if (req.entries.empty())
        return;

    const auto json = rfl::json::write(req);
    const auto* p = reinterpret_cast<const std::byte*>(json.data());
    nats.publish(ores::telemetry::messaging::publish_log_entries_request::nats_subject,
        std::span<const std::byte>(p, json.size()));
    req.entries.clear();
}

} // namespace

void publish_ore_logs(ores::nats::service::client& nats,
    const std::string& result_id,
    const std::filesystem::path& job_dir)
{
    const auto log_path = job_dir / "Output" / "log.txt";
    std::ifstream f(log_path);
    if (!f.is_open()) {
        BOOST_LOG_SEV(log_publisher_lg(), debug)
            << "ORE log not found: " << log_path;
        return;
    }

    ores::telemetry::messaging::publish_log_entries_request req;
    req.source_name = "ores.compute.ore_log";
    req.tag         = result_id;

    std::size_t parsed = 0;
    std::size_t skipped = 0;
    std::string line;
    while (std::getline(f, line)) {
        const auto parsed_line = ores::ore::log::parse_ore_log_line(line);
        if (!parsed_line) {
            ++skipped;
            continue;
        }
        ores::telemetry::messaging::publish_log_entry_item item;
        item.level        = parsed_line->level;
        item.timestamp_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
            parsed_line->timestamp.time_since_epoch()).count();
        item.component    = parsed_line->source;
        item.message      = parsed_line->message;
        req.entries.push_back(std::move(item));
        ++parsed;

        if (req.entries.size() >= batch_size)
            publish_batch(nats, req);
    }
    publish_batch(nats, req);

    BOOST_LOG_SEV(log_publisher_lg(), debug)
        << "Published " << parsed << " ORE log entries ("
        << skipped << " skipped) for result " << result_id;
}

void publish_engine_logs(ores::nats::service::client& nats,
    const std::string& result_id,
    const std::filesystem::path& job_dir)
{
    const auto log_path = job_dir / "engine.log";
    std::ifstream f(log_path);
    if (!f.is_open()) {
        BOOST_LOG_SEV(log_publisher_lg(), debug)
            << "Engine log not found: " << log_path;
        return;
    }

    ores::telemetry::messaging::publish_log_entries_request req;
    req.source_name = "ores.compute.wrapper";
    req.tag         = result_id;

    const auto now_ms = std::chrono::duration_cast<std::chrono::milliseconds>(
        std::chrono::system_clock::now().time_since_epoch()).count();

    std::size_t count = 0;
    std::string line;
    while (std::getline(f, line)) {
        if (line.empty())
            continue;
        ores::telemetry::messaging::publish_log_entry_item item;
        item.level        = "info";
        item.timestamp_ms = now_ms;
        item.component    = "ores.compute.wrapper";
        item.message      = line;
        req.entries.push_back(std::move(item));
        ++count;

        if (req.entries.size() >= batch_size)
            publish_batch(nats, req);
    }
    publish_batch(nats, req);

    BOOST_LOG_SEV(log_publisher_lg(), debug)
        << "Published " << count << " engine log lines for result " << result_id;
}

} // namespace ores::compute::wrapper::app
