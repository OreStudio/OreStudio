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
#ifndef ORES_ORE_CORE_LOG_ENGINE_LOG_HPP
#define ORES_ORE_CORE_LOG_ENGINE_LOG_HPP

#include "ores.ore.core/export.hpp"
#include <chrono>
#include <cstddef>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::log {

/**
 * @brief The format the engine writes its own timestamps in.
 *
 * The engine writes them as =YYYY-Mon-DD HH:MM:SS= with an optional fractional
 * second and no timezone designator, and it logs in UTC.
 */
inline constexpr std::string_view engine_timestamp_format = "%Y-%b-%d %H:%M:%S";

/**
 * @brief The suffix the engine puts on the memory reports it sends as warnings.
 *
 * The engine writes its memory use through the warning channel so that the line
 * appears at any log mask. It is a measurement and not a warning, so a reader
 * that ranks severities has to recognise it by this suffix.
 */
inline constexpr std::string_view engine_memory_report_suffix =
    "(peakMemoryUsage (bytes) | memoryUsage (bytes))";

/**
 * @brief One thing the engine said, as it said it.
 *
 * The engine writes two logs and they carry the same information in two shapes:
 * =log_structured.json=, one JSON object per line, and =log.txt=, the same JSON
 * after a marker on a plain line. This struct is what both come to, so a reader
 * of either sees one shape and the engine's format is known in one place.
 */
struct engine_log_event {
    /**
     * @brief The line's place in the log, from one.
     */
    std::size_t ordinal = 0;
    /**
     * @brief The severity, in the words the store keeps: =debug=, =info=,
     * =notice=, =warning=, =error=.
     */
    std::string severity;
    /**
     * @brief Whether the line stated the fields the engine writes on its lines.
     *
     * The engine writes a level, a time in brackets, the location that logged
     * the line in parentheses, then the separator. Only a line that states all
     * four, in that order, and whose time can be read, sets this true; a line
     * that states less is kept as text so a reader sees it, but it is not one of
     * the engine's lines and a client that needs a time and a location skips it.
     *
     * This is always false for a structured log: its records state no line
     * fields, because there is no line around them.
     */
    bool states_line_fields = false;
    /**
     * @brief The group the engine put the message in: =Curve=, =Trade=,
     * =Analytics=, and the report's own =Report= for a line the wrapper adds.
     */
    std::string scope;
    /**
     * @brief What the engine said.
     *
     * A structured message that states no message of its own leaves the reader
     * with the text it was given, so this is never empty while @c message_stated
     * says whether the engine stated it or the reader had to keep the line.
     */
    std::string message;
    /**
     * @brief Whether the engine stated @c message, rather than the reader
     * keeping the line because the message stated none.
     */
    bool message_stated = false;
    /**
     * @brief The kind of record the message is about, when it names one: =trade=.
     */
    std::string subject;
    /**
     * @brief The record the message is about, when it names one.
     */
    std::string subject_id;
    /**
     * @brief What caused it, when the message names one: the curve.
     */
    std::string cause;
    /**
     * @brief Every sub-field the engine stated, as the JSON object it wrote.
     *
     * A consumer reads the field it needs by name from here, so what an error is
     * about is decided where the meaning is known rather than by the parser.
     */
    std::string fields;
    /**
     * @brief The sub-fields that are not the subject, as the engine wrote them.
     *
     * The engine states the exception it raised and the analytic it was in, and a
     * reader needs both to act on the line. They are kept as one JSON object so a
     * field the engine adds later is stored as the engine named it.
     */
    std::string detail;
    /**
     * @brief The location that logged the line, as the engine wrote it in
     * parentheses, such as =ore/oreapp.cpp:42=. Empty when the line stated none.
     */
    std::string location;
    /**
     * @brief The time the engine stated, as it wrote it.
     */
    std::string event_time;
    /**
     * @brief The time the engine stated, as a value, in UTC.
     *
     * Empty when the line stated no time or one that cannot be read, which is
     * why it is an optional rather than a default. @c event_time keeps what the
     * engine wrote; this is what it means.
     */
    std::optional<std::chrono::system_clock::time_point> occurred_at;
    /**
     * @brief The file the event came from.
     */
    std::string source;
};

/**
 * @brief The events in an engine text log, one per line.
 *
 * Every non-blank line becomes an event, whatever its severity, so a reader can
 * see the whole account and not only the failures. A line's severity is the word
 * the engine put first; a structured line takes the category the engine stated
 * instead, and its group, message and subject from the JSON it carries.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT std::vector<engine_log_event>
parse_engine_log_text(std::string_view text, std::string_view source_file);

/**
 * @brief The events in an engine structured log, one per line.
 *
 * The format is the JSON the text log embeds, with nothing around it. A line that
 * is not a JSON object is kept as an event carrying what it says, because the
 * engine wrote it and a reader should see it.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT std::vector<engine_log_event>
parse_engine_log_json(std::string_view text, std::string_view source_file);

/**
 * @brief The value of a sub-field the engine stated, by the name it gave it.
 *
 * The sub-fields are kept as the JSON object the engine wrote, so a client that
 * needs one by name reads it here rather than parsing the engine's JSON itself.
 *
 * @return The value, or an empty string when the message stated no such field.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT std::string field_of(const engine_log_event& event,
                                                        std::string_view name);

/**
 * @brief Whether the line is one of the engine's memory reports.
 *
 * @see engine_memory_report_suffix
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT bool is_memory_report(const engine_log_event& event);

/**
 * @brief The level a log store keeps for a line, or empty when it keeps none.
 *
 * The store names its levels for a person to read -- =debug=, =info=, =warn=,
 * =error= -- and the engine names its own; this is the one table between them.
 * A line that is not one of the engine's is not forwarded, so this is empty for
 * it, and the engine's memory reports read as =debug= rather than as warnings.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT std::string store_level_of(const engine_log_event& event);

/**
 * @brief How many lines a log holds, as the engine wrote them.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT std::size_t line_count(std::string_view text);

/**
 * @brief The events as the newline-delimited CSV the loader reads.
 *
 * The header is
 * =ordinal,severity,scope,message,subject,subject_id,cause,detail,event_time,source=,
 * which is the shape the reporting service stores and the screen draws.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT std::string
canonical_log_csv(const std::vector<engine_log_event>& events);

}

#endif
