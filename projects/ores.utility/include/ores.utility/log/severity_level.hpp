/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2024 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_UTILITY_LOG_SEVERITY_LEVEL_HPP
#define ORES_UTILITY_LOG_SEVERITY_LEVEL_HPP

#include <string>
#include <ostream>
#include <boost/throw_exception.hpp>
#include "ores.utility/exception/invalid_enum_value.hpp"

namespace ores::utility::log {

/**
 * @brief Logging severity.
 *
 * @warn This must be a C++-03 enum due to the boost::log internals. It also
 * makes logging easier otherwise we'd have to always prefix "severity_level::".
 */
enum severity_level {
    trace,
    debug,
    info,
    warn,
    error
};

/**
 * @brief Converts the supplied string into the severity level, if valid.
 *
 * Cannot use magic enums as we can't use enum classes.
 */
severity_level to_severity_level(const std::string& s);

/**
 * @brief Inserter for severity level enum.
 *
 * Required for interoperability with boost log.
 */
template<typename CharT, typename TraitsT>
inline std::basic_ostream<CharT, TraitsT>&
operator<<(std::basic_ostream<CharT, TraitsT>& stream, severity_level level) {
    switch(level) {
    case severity_level::trace: stream << "TRACE"; break;
    case severity_level::debug: stream << "DEBUG"; break;
    case severity_level::info: stream << "INFO"; break;
    case severity_level::warn: stream << "WARN"; break;
    case severity_level::error: stream << "ERROR"; break;
    default:
        using ores::utility::exception::invalid_enum_value;
        BOOST_THROW_EXCEPTION(
            invalid_enum_value("Invalid or unexpected severity level"));
        break;
    }
    return stream;
}

}

#endif
