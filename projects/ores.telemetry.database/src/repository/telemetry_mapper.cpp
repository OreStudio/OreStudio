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
#include "ores.telemetry.database/repository/telemetry_mapper.hpp"

#include <format>
#include <iomanip>
#include <sstream>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "sqlgen/Timestamp.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::telemetry::database::repository {

using namespace ores::logging;
using namespace ores::database::repository;

namespace {

inline static std::string_view logger_name =
    "ores.telemetry.repository.telemetry_mapper";

[[nodiscard]] auto& lg() {
    static auto instance = make_logger(logger_name);
    return instance;
}

/**
 * @brief Parses a timestamp string into a time_point.
 */
std::optional<std::chrono::system_clock::time_point>
parse_timestamp(const std::string& str) {
    if (str.empty()) {
        return std::nullopt;
    }
    std::tm tm = {};
    std::istringstream ss(str);
    ss >> std::get_time(&tm, "%Y-%m-%d %H:%M:%S");
    if (ss.fail()) {
        return std::nullopt;
    }
    return std::chrono::system_clock::from_time_t(std::mktime(&tm));
}

}

telemetry_entity telemetry_mapper::to_entity(
    const domain::telemetry_log_entry& entry) {
    telemetry_entity r;

    r.id = boost::lexical_cast<std::string>(entry.id);
    r.timestamp = timepoint_to_timestamp(entry.timestamp, lg());
    r.source = std::string(domain::to_string(entry.source));
    r.source_name = entry.source_name;
    r.session_id = entry.session_id
        ? boost::lexical_cast<std::string>(*entry.session_id)
        : "";
    r.account_id = entry.account_id
        ? boost::lexical_cast<std::string>(*entry.account_id)
        : "";
    r.level = entry.level;
    r.component = entry.component;
    r.message = entry.message;
    r.tag = entry.tag;
    r.recorded_at = timepoint_to_timestamp(entry.recorded_at, lg());

    return r;
}

domain::telemetry_log_entry telemetry_mapper::to_domain(
    const telemetry_entity& entity) {
    domain::telemetry_log_entry r;
    using boost::uuids::uuid;

    r.id = boost::lexical_cast<uuid>(entity.id.value());
    r.timestamp = timestamp_to_timepoint(entity.timestamp.value());
    r.source = domain::telemetry_source_from_string(entity.source);
    r.source_name = entity.source_name;
    r.session_id = entity.session_id.empty()
        ? std::nullopt
        : std::optional<uuid>(boost::lexical_cast<uuid>(entity.session_id));
    r.account_id = entity.account_id.empty()
        ? std::nullopt
        : std::optional<uuid>(boost::lexical_cast<uuid>(entity.account_id));
    r.level = entity.level;
    r.component = entity.component;
    r.message = entity.message;
    r.tag = entity.tag;
    r.recorded_at = timestamp_to_timepoint(entity.recorded_at);

    return r;
}

domain::telemetry_stats telemetry_mapper::to_domain(
    const telemetry_stats_hourly_entity& entity) {
    domain::telemetry_stats r;

    auto period_start = parse_timestamp(entity.hour);
    if (period_start) {
        r.period_start = *period_start;
    }

    r.source = domain::telemetry_source_from_string(entity.source);
    r.source_name = entity.source_name;
    r.level = entity.level;
    r.log_count = static_cast<std::uint64_t>(entity.log_count);
    r.unique_sessions = static_cast<std::uint32_t>(entity.unique_sessions);
    r.unique_accounts = static_cast<std::uint32_t>(entity.unique_accounts);

    return r;
}

domain::telemetry_stats telemetry_mapper::to_domain(
    const telemetry_stats_daily_entity& entity) {
    domain::telemetry_stats r;

    auto period_start = parse_timestamp(entity.day);
    if (period_start) {
        r.period_start = *period_start;
    }

    r.source = domain::telemetry_source_from_string(entity.source);
    r.source_name = entity.source_name;
    r.component = entity.component;
    r.level = entity.level;
    r.log_count = static_cast<std::uint64_t>(entity.log_count);
    r.unique_sessions = static_cast<std::uint32_t>(entity.unique_sessions);
    r.unique_accounts = static_cast<std::uint32_t>(entity.unique_accounts);

    return r;
}

}
