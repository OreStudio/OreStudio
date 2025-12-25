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
#include "ores.telemetry/log/boost_severity.hpp"

#include <stdexcept>

namespace ores::telemetry::log {

boost_severity to_boost_severity(const std::string& s) {
    static const std::string trace_level("trace");
    static const std::string debug_level("debug");
    static const std::string info_level("info");
    static const std::string warn_level("warn");
    static const std::string error_level("error");

    if (s == trace_level)
        return boost_severity::trace;
    if (s == debug_level)
        return boost_severity::debug;
    if (s == info_level)
        return boost_severity::info;
    if (s == warn_level)
        return boost_severity::warn;
    if (s == error_level)
        return boost_severity::error;

    throw std::invalid_argument("Invalid or unexpected severity level: " + s);
}

boost_severity to_boost_severity(domain::severity_level level) {
    switch (level) {
    case domain::severity_level::trace: return boost_severity::trace;
    case domain::severity_level::debug: return boost_severity::debug;
    case domain::severity_level::info:  return boost_severity::info;
    case domain::severity_level::warn:  return boost_severity::warn;
    case domain::severity_level::error: return boost_severity::error;
    case domain::severity_level::fatal: return boost_severity::error;
    default:
        throw std::invalid_argument("Invalid or unexpected severity level");
    }
}

domain::severity_level to_domain_severity(boost_severity sev) {
    switch (sev) {
    case boost_severity::trace: return domain::severity_level::trace;
    case boost_severity::debug: return domain::severity_level::debug;
    case boost_severity::info:  return domain::severity_level::info;
    case boost_severity::warn:  return domain::severity_level::warn;
    case boost_severity::error: return domain::severity_level::error;
    default:
        throw std::invalid_argument("Invalid or unexpected severity level");
    }
}

}
