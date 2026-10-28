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
#ifndef ORES_DATABASE_REPOSITORY_VALID_AT_HPP
#define ORES_DATABASE_REPOSITORY_VALID_AT_HPP

#include "ores.database/repository/helpers.hpp"
#include "ores.platform/time/datetime.hpp"
#include <optional>
#include <regex>
#include <stdexcept>
#include <string>

namespace ores::database::repository {

/**
 * @brief The instant a list request states, if it is one the database reads.
 *
 * The form is a UTC timestamp: a date, 'T' or a space, a time with an optional
 * fraction of up to six digits, and Z, +00 or +00:00. The text is returned as
 * the caller wrote it, so a fraction of a second is kept; the parse only proves
 * the date and time exist.
 */
inline std::optional<std::string> parse_as_of(const std::string& text) {
    static const std::regex form(
        R"(\d{4}-\d{2}-\d{2}[T ]\d{2}:\d{2}:\d{2}(\.\d{1,6})?(Z|\+00(:00)?))");
    if (!std::regex_match(text, form))
        return std::nullopt;
    try {
        static_cast<void>(ores::platform::time::datetime::from_iso8601_utc(text));
    } catch (const std::invalid_argument&) {
        return std::nullopt;
    }
    return text;
}


}

#endif
