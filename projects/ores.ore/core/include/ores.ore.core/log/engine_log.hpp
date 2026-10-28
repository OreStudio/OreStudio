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
#include <cstddef>
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::log {

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
     * @brief The group the engine put the message in: =Curve=, =Trade=,
     * =Analytics=, and the report's own =Report= for a line the wrapper adds.
     */
    std::string scope;
    /**
     * @brief What the engine said.
     */
    std::string message;
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
     * @brief The sub-fields that are not the subject, as the engine wrote them.
     *
     * The engine states the exception it raised and the analytic it was in, and a
     * reader needs both to act on the line. They are kept as one JSON object so a
     * field the engine adds later is stored as the engine named it.
     */
    std::string detail;
    /**
     * @brief The time the engine stated, as it wrote it.
     */
    std::string event_time;
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
 * @brief The events as the newline-delimited CSV the loader reads.
 *
 * The header is
 * =ordinal,severity,scope,message,subject,subject_id,cause,event_time,source=,
 * which is the shape the reporting service stores and the screen draws.
 */
[[nodiscard]] ORES_ORE_CORE_EXPORT std::string
canonical_log_csv(const std::vector<engine_log_event>& events);

}

#endif
