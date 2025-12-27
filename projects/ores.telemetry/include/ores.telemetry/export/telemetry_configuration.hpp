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
#ifndef ORES_TELEMETRY_EXPORT_TELEMETRY_CONFIGURATION_HPP
#define ORES_TELEMETRY_EXPORT_TELEMETRY_CONFIGURATION_HPP

#include <string>
#include <optional>
#include <boost/program_options.hpp>
#include "ores.telemetry/export/telemetry_options.hpp"

namespace ores::telemetry::exp {

/**
 * @brief Centralized manager for telemetry export configuration parsing.
 *
 * Provides utilities for creating command-line option descriptions and
 * reading configuration from parsed options. The telemetry export feature
 * captures all log records and exports them to a JSON Lines file for
 * log aggregation and analysis.
 */
class telemetry_configuration final {
public:
    telemetry_configuration() = delete;

    /**
     * @brief Creates the boost::program_options description for telemetry CLI
     * arguments.
     *
     * @param default_service_name Default name of the service.
     * @param default_service_version Default version of the service.
     * @return options_description for telemetry configuration.
     */
    static boost::program_options::options_description
    make_options_description(const std::string& default_service_name,
                             const std::string& default_service_version);

    /**
     * @brief Reads telemetry options from parsed variables map.
     *
     * If telemetry is not enabled (no --telemetry-enabled flag), returns
     * std::nullopt.
     *
     * @param vm Parsed command-line options.
     * @return telemetry_options if telemetry was enabled, std::nullopt otherwise.
     */
    static std::optional<telemetry_options>
    read_options(const boost::program_options::variables_map& vm);
};

}

#endif
