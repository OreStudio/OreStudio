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
#include "ores.logging/boost_severity.hpp"
#include <stdexcept>

namespace ores::logging {

backend::boost_severity to_boost_severity(const std::string& s) {
    static const std::string trace_level("trace");
    static const std::string debug_level("debug");
    static const std::string info_level("info");
    static const std::string warn_level("warn");
    static const std::string error_level("error");

    if (s == trace_level)
        return backend::boost_severity::trace;
    if (s == debug_level)
        return backend::boost_severity::debug;
    if (s == info_level)
        return backend::boost_severity::info;
    if (s == warn_level)
        return backend::boost_severity::warn;
    if (s == error_level)
        return backend::boost_severity::error;

    throw std::invalid_argument("Invalid or unexpected severity level: " + s);
}

backend::boost_severity to_boost_severity(severity_level level) {
    switch (level) {
        case severity_level::trace:
            return backend::boost_severity::trace;
        case severity_level::debug:
            return backend::boost_severity::debug;
        case severity_level::info:
            return backend::boost_severity::info;
        case severity_level::warn:
            return backend::boost_severity::warn;
        case severity_level::error:
            return backend::boost_severity::error;
        case severity_level::fatal:
            return backend::boost_severity::error;
        default:
            throw std::invalid_argument("Invalid or unexpected severity level");
    }
}

severity_level to_domain_severity(backend::boost_severity sev) {
    switch (sev) {
        case backend::boost_severity::trace:
            return severity_level::trace;
        case backend::boost_severity::debug:
            return severity_level::debug;
        case backend::boost_severity::info:
            return severity_level::info;
        case backend::boost_severity::warn:
            return severity_level::warn;
        case backend::boost_severity::error:
            return severity_level::error;
        default:
            throw std::invalid_argument("Invalid or unexpected severity level");
    }
}

}
