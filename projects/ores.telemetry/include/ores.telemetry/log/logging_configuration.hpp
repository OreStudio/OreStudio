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
#ifndef ORES_TELEMETRY_LOG_LOGGING_CONFIGURATION_HPP
#define ORES_TELEMETRY_LOG_LOGGING_CONFIGURATION_HPP

#include <string>
#include <optional>
#include <boost/program_options.hpp>
#include "ores.telemetry/log/logging_options.hpp"

namespace ores::telemetry::log {

/**
 * @brief Centralized manager for logging configuration parsing.
 *
 * Provides utilities for creating command-line option descriptions,
 * reading configuration from parsed options.
 */
class logging_configuration final {
public:
    logging_configuration() = delete;

    /**
     * @brief Creates the boost::program_options description for logging CLI
     * arguments.
     *
     * @param log_file name of the file to log to by default.
     * @return options_description for logging configuration.
     */
    static boost::program_options::options_description
    make_options_description(const std::string& log_file);

    /**
     * @brief Reads logging options from parsed variables map.
     *
     * If logging is not enabled (no --log-enabled flag), returns std::nullopt.
     *
     * @param vm Parsed command-line options.
     * @param default_filename Default log filename to use if not specified
     * @param default_directory Default log directory (defaults to "log")
     * @return logging_options if logging was enabled, std::nullopt otherwise.
     */
    static std::optional<logging_options>
    read_options(const boost::program_options::variables_map& vm);
};

}

#endif
