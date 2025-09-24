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
#ifndef ORES_UTILITY_LOG_LOGGING_CONFIGURATION_HPP
#define ORES_UTILITY_LOG_LOGGING_CONFIGURATION_HPP

#if defined(_MSC_VER) && (_MSC_VER >= 1200)
#pragma once
#endif

#include <iosfwd>
#include <string>
#include <filesystem>

namespace ores::utility::log {

/**
 * @brief Options related to logging.
 */
struct logging_configuration final {
    /**
     * @brief Level at which to log.
     */
    std::string severity;
    /**
     * @brief Name of the file to log into.
     *
     * If empty, file logging is disabled.
     */
    std::string filename;
    /**
     * @brief If true, dumps the log into the console.
     */
    bool output_to_console;
    /**
     * @brief Directory in which to place the output.
     */
    std::filesystem::path output_directory;
};

std::ostream& operator<<(std::ostream& s, const logging_configuration& v);

}

#endif
