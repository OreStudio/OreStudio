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
#ifndef ORES_DATABASE_LOG_HELPER_HPP
#define ORES_DATABASE_LOG_HELPER_HPP

#include <boost/log/trivial.hpp>
#include <boost/log/sources/severity_channel_logger.hpp>

namespace ores::database::log {

/**
 * @brief Severity level type alias for boost log trivial severity.
 */
using severity_level = boost::log::trivial::severity_level;

/**
 * @brief Logger type using severity channel logger.
 *
 * This is compatible with the logger type used in ores.utility::log.
 */
using logger_t = boost::log::sources::severity_channel_logger_mt<
    severity_level, std::string>;

/**
 * @brief Severity level constants for convenience.
 */
constexpr auto trace = severity_level::trace;
constexpr auto debug = severity_level::debug;
constexpr auto info = severity_level::info;
constexpr auto warn = severity_level::warning;
constexpr auto error = severity_level::error;
constexpr auto fatal = severity_level::fatal;

}

#endif
