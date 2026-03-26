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
#ifndef ORES_ORE_LOG_ORE_LOG_PARSER_HPP
#define ORES_ORE_LOG_ORE_LOG_PARSER_HPP

#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::ore::log {

/**
 * @brief A single parsed ORE engine log line.
 *
 * ORE log format:
 * @code
 * LEVEL    [YYYY-Mon-DD HH:MM:SS.microseconds]    (source/file.cpp:line) : message
 * @endcode
 */
struct ore_log_line {
    /**
     * @brief Normalized severity: "debug", "info", "warn", or "error".
     *
     * ORE level mapping:
     * - DATA    → "debug"
     * - NOTICE  → "info"
     * - WARNING → "warn"
     * - ERROR   → "error"
     * - ALERT   → "error"
     */
    std::string level;

    /**
     * @brief Timestamp from the log line, interpreted as UTC.
     */
    std::chrono::system_clock::time_point timestamp;

    /**
     * @brief Source location, e.g. "ore/oreapp.cpp:42".
     */
    std::string source;

    /**
     * @brief The log message body.
     */
    std::string message;
};

/**
 * @brief Parses a single ORE engine log line.
 *
 * @return Parsed result, or std::nullopt for empty, malformed, or
 *         unrecognised lines.
 */
std::optional<ore_log_line> parse_ore_log_line(std::string_view line);

} // namespace ores::ore::log

#endif
