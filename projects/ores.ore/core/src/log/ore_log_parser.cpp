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
#include "ores.ore.core/log/ore_log_parser.hpp"
#include "ores.platform/time/datetime.hpp"
#include <array>
#include <chrono>
#include <cstdlib>
#include <cstring>
#include <optional>
#include <stdexcept>
#include <string>

namespace ores::ore::log {

namespace {

std::string_view map_level(std::string_view ore_level) {
    if (ore_level == "DATA")
        return "debug";
    if (ore_level == "NOTICE")
        return "info";
    if (ore_level == "WARNING")
        return "warn";
    if (ore_level == "ERROR")
        return "error";
    if (ore_level == "ALERT")
        return "error";
    return {};
}

/**
 * ORE reports its memory use through the WARNING channel so that the line
 * appears at any log mask. Each line ends with this fixed suffix.
 */
constexpr std::string_view memory_report_suffix = "(peakMemoryUsage (bytes) | memoryUsage (bytes))";

std::string_view ltrim(std::string_view sv) {
    const auto pos = sv.find_first_not_of(" \t");
    return pos == std::string_view::npos ? std::string_view{} : sv.substr(pos);
}

} // namespace

std::optional<ore_log_line> parse_ore_log_line(std::string_view line) {
    line = ltrim(line);
    if (line.empty())
        return std::nullopt;

    // 1. Level token
    const auto level_end = line.find_first_of(" \t");
    if (level_end == std::string_view::npos)
        return std::nullopt;

    const auto ore_level = line.substr(0, level_end);
    const auto mapped = map_level(ore_level);
    if (mapped.empty())
        return std::nullopt;

    line = ltrim(line.substr(level_end));

    // 2. Timestamp in [...]
    if (line.empty() || line.front() != '[')
        return std::nullopt;

    const auto ts_end = line.find(']');
    if (ts_end == std::string_view::npos)
        return std::nullopt;

    const auto ts_str = line.substr(1, ts_end - 1);
    line = ltrim(line.substr(ts_end + 1));

    // ts_str: "YYYY-Mon-DD HH:MM:SS.microseconds"
    std::chrono::system_clock::time_point timestamp;
    {
        const auto dot_pos = ts_str.rfind('.');
        std::string_view dt_part = ts_str;
        long microseconds = 0;
        if (dot_pos != std::string_view::npos) {
            dt_part = ts_str.substr(0, dot_pos);
            const auto us_str = ts_str.substr(dot_pos + 1);
            // Pad to 6 digits so "123" → 123000 µs
            std::array<char, 7> us_buf = {'0', '0', '0', '0', '0', '0', '\0'};
            const auto copy_len = std::min(us_str.size(), std::size_t{6});
            std::memcpy(us_buf.data(), us_str.data(), copy_len);
            microseconds = std::strtol(us_buf.data(), nullptr, 10);
        }

        // Log timestamps use Boost.Log format "YYYY-Mon-DD HH:MM:SS" with no
        // timezone designator. The log system writes UTC timestamps.
        try {
            timestamp = ores::platform::time::datetime::from_utc_format(
                            std::string(dt_part), ore_log_timestamp_format.data()) +
                        std::chrono::microseconds(microseconds);
        } catch (const std::invalid_argument&) {
            return std::nullopt;
        }
    }

    // 3. Source in (...)
    if (line.empty() || line.front() != '(')
        return std::nullopt;

    const auto src_end = line.find(')');
    if (src_end == std::string_view::npos)
        return std::nullopt;

    const auto source = std::string(line.substr(1, src_end - 1));
    line = ltrim(line.substr(src_end + 1));

    // 4. Message after ": "
    if (!line.starts_with(": "))
        return std::nullopt;

    ore_log_line result;
    result.message = std::string(line.substr(2));
    const bool memory_report = mapped == "warn" && result.message.ends_with(memory_report_suffix);
    result.level = std::string(memory_report ? "debug" : mapped);
    result.timestamp = timestamp;
    result.source = source;
    return result;
}

} // namespace ores::ore::log
