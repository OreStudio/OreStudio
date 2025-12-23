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
#include "ores.iam/repository/session_mapper.hpp"

#include <iomanip>
#include <sstream>
#include <boost/uuid/uuid_io.hpp>
#include <boost/lexical_cast.hpp>
#include "sqlgen/Timestamp.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.utility/datetime/datetime.hpp"
#include "ores.iam/domain/session_json_io.hpp" // IWYU pragma: keep.

namespace ores::iam::repository {

using namespace ores::utility::log;
using namespace ores::database::repository;

namespace {

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

/**
 * @brief Formats a time_point as a timestamp string (thread-safe).
 */
std::string format_timestamp(const std::chrono::system_clock::time_point& tp) {
    return ores::utility::datetime::datetime::format_time_point(tp);
}

/**
 * @brief Parses an optional double from a string.
 */
std::optional<double> parse_optional_double(const std::string& str) {
    if (str.empty()) {
        return std::nullopt;
    }
    try {
        return std::stod(str);
    } catch (...) {
        return std::nullopt;
    }
}

}

domain::session session_mapper::map(const session_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::session r;
    using boost::uuids::uuid;
    using namespace boost::asio;

    r.id = boost::lexical_cast<uuid>(v.id.value());
    r.account_id = boost::lexical_cast<uuid>(v.account_id);
    r.is_admin = v.is_admin != 0;
    r.start_time = timestamp_to_timepoint(v.start_time.value());
    r.end_time = v.end_time.empty() ? std::nullopt : parse_timestamp(v.end_time);
    r.client_ip = ip::make_address(v.client_ip);
    r.client_identifier = v.client_identifier;
    r.client_version_major = static_cast<std::uint16_t>(v.client_version_major);
    r.client_version_minor = static_cast<std::uint16_t>(v.client_version_minor);
    r.bytes_sent = static_cast<std::uint64_t>(v.bytes_sent);
    r.bytes_received = static_cast<std::uint64_t>(v.bytes_received);
    r.country_code = v.country_code;
    r.city = v.city;
    r.latitude = parse_optional_double(v.latitude);
    r.longitude = parse_optional_double(v.longitude);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

session_entity session_mapper::map(const domain::session& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    session_entity r;
    r.id = boost::lexical_cast<std::string>(v.id);
    r.account_id = boost::lexical_cast<std::string>(v.account_id);
    r.is_admin = v.is_admin ? 1 : 0;
    r.start_time = timepoint_to_timestamp(v.start_time, lg());
    r.end_time = v.end_time ? format_timestamp(*v.end_time) : "";
    r.client_ip = v.client_ip.to_string();
    r.client_identifier = v.client_identifier;
    r.client_version_major = static_cast<int>(v.client_version_major);
    r.client_version_minor = static_cast<int>(v.client_version_minor);
    r.bytes_sent = static_cast<std::int64_t>(v.bytes_sent);
    r.bytes_received = static_cast<std::int64_t>(v.bytes_received);
    r.country_code = v.country_code;
    r.city = v.city;
    r.latitude = v.latitude ? std::to_string(*v.latitude) : "";
    r.longitude = v.longitude ? std::to_string(*v.longitude) : "";

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::session>
session_mapper::map(const std::vector<session_entity>& v) {
    return map_vector<session_entity, domain::session>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "db entities");
}

std::vector<session_entity>
session_mapper::map(const std::vector<domain::session>& v) {
    return map_vector<domain::session, session_entity>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "domain entities");
}

domain::session_statistics
session_mapper::map(const session_statistics_entity& v) {
    domain::session_statistics r;

    // Parse day as period_start
    auto period_start = parse_timestamp(v.day);
    if (period_start) {
        r.period_start = *period_start;
        // period_end is start + 1 day
        r.period_end = r.period_start + std::chrono::hours(24);
    }

    if (!v.account_id.empty()) {
        r.account_id = boost::lexical_cast<boost::uuids::uuid>(v.account_id);
    }

    r.session_count = static_cast<std::uint64_t>(v.session_count);
    r.avg_duration_seconds = v.avg_duration_seconds;
    r.total_bytes_sent = static_cast<std::uint64_t>(v.total_bytes_sent);
    r.total_bytes_received = static_cast<std::uint64_t>(v.total_bytes_received);

    if (r.session_count > 0) {
        r.avg_bytes_sent = static_cast<double>(r.total_bytes_sent) /
            static_cast<double>(r.session_count);
        r.avg_bytes_received = static_cast<double>(r.total_bytes_received) /
            static_cast<double>(r.session_count);
    }

    return r;
}

std::vector<domain::session_statistics>
session_mapper::map(const std::vector<session_statistics_entity>& v) {
    return map_vector<session_statistics_entity, domain::session_statistics>(
        v,
        [](const auto& ve) { return map(ve); },
        lg(),
        "statistics entities");
}

}
