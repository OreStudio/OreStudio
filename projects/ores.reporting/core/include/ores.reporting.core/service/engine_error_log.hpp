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
#ifndef ORES_REPORTING_CORE_SERVICE_ENGINE_ERROR_LOG_HPP
#define ORES_REPORTING_CORE_SERVICE_ENGINE_ERROR_LOG_HPP

#include "ores.reporting.core/export.hpp"
#include <cstddef>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace ores::reporting::service {

/**
 * @brief One error the engine reported about a run, as the engine stated it.
 *
 * The fields are the engine's own: @c severity is the message's category,
 * @c scope its group, and @c subject and @c subject_id are read from the
 * identifying sub-fields of whichever group the message belongs to. Nothing
 * here is inferred, and a sub-field the engine did not emit leaves its field
 * empty.
 */
struct engine_log_error {
    std::string severity;
    std::string scope;
    std::string message;
    std::string subject;
    std::string subject_id;

    /// The thing the error is about in a person's words: the curve that would
    /// not build, or the curve the failed trade needed. Empty when the engine
    /// stated neither.
    std::string cause;

    /// The sub-fields other than the identifying one, as the engine wrote them.
    std::string detail;

    /// The time the engine stated, as the node passed it on.
    std::string event_time;

    /// The file the row came from.
    std::string source;
};

/**
 * @brief What reading a run's output found.
 *
 * A run whose archive holds no structured log, or holds one the engine did not
 * write, is a run whose errors cannot be read. That is a fact about the run and
 * not an exception, so it is a message here.
 */
struct engine_log_read {
    std::vector<engine_log_error> errors;
    std::string error;

    [[nodiscard]] bool ok() const {
        return error.empty();
    }
};

/**
 * @brief Reads one line of the engine's structured log.
 *
 * Returns an empty optional for a line that is not one of the engine's
 * messages. The log is written by a library that may add lines of its own, and
 * a line this reader does not recognise is skipped rather than failing the read.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT std::optional<engine_log_error>
parse_engine_log_line(std::string_view line);

/**
 * @brief Reads the errors out of a run's output archive.
 *
 * The archive is the run's whole output, which is what the collect step
 * retrieves and keeps. Its @c log_structured.json is the engine's account of
 * what it could not do.
 */
[[nodiscard]] ORES_REPORTING_CORE_EXPORT engine_log_read
read_engine_errors_from_archive(std::span<const std::byte> archive_bytes);

}

#endif
