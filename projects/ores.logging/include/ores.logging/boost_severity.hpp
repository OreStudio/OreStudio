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
#ifndef ORES_LOGGING_BOOST_SEVERITY_HPP
#define ORES_LOGGING_BOOST_SEVERITY_HPP

#include <string>
#include <ostream>
#include <stdexcept>
#include "ores.logging/severity_level.hpp"

namespace ores::logging {

/**
 * @brief Internal Boost.Log severity level enum.
 *
 * This must be a C++03 enum due to Boost.Log internals. It is used only
 * internally for Boost.Log integration. External code should use
 * ores::logging::severity_level instead.
 *
 * @note This is an implementation detail and should not be used directly
 * by client code.
 */
enum boost_severity {
    trace,
    debug,
    info,
    warn,
    error
};

/**
 * @brief Converts a string to boost_severity.
 *
 * @param s String representation (trace, debug, info, warn, error).
 * @return Corresponding boost_severity value.
 * @throws std::invalid_argument if the string is not a valid severity.
 */
boost_severity to_boost_severity(const std::string& s);

/**
 * @brief Converts domain severity_level to internal boost_severity.
 *
 * @param level The domain severity level.
 * @return Corresponding boost_severity value.
 */
boost_severity to_boost_severity(severity_level level);

/**
 * @brief Converts internal boost_severity to domain severity_level.
 *
 * @param sev The boost severity.
 * @return Corresponding domain severity_level value.
 */
severity_level to_domain_severity(boost_severity sev);

/**
 * @brief Inserter for boost_severity enum.
 *
 * Required for interoperability with Boost.Log.
 */
template<typename CharT, typename TraitsT>
inline std::basic_ostream<CharT, TraitsT>&
operator<<(std::basic_ostream<CharT, TraitsT>& stream, boost_severity level) {
    switch(level) {
    case boost_severity::trace: stream << "TRACE"; break;
    case boost_severity::debug: stream << "DEBUG"; break;
    case boost_severity::info:  stream << "INFO"; break;
    case boost_severity::warn:  stream << "WARN"; break;
    case boost_severity::error: stream << "ERROR"; break;
    default:
        throw std::invalid_argument("Invalid or unexpected severity level");
        break;
    }
    return stream;
}

}

#endif
