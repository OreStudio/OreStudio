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
#ifndef ORES_NATS_CONFIG_NATS_CONFIGURATION_HPP
#define ORES_NATS_CONFIG_NATS_CONFIGURATION_HPP

#include <boost/program_options.hpp>
#include "ores.nats/config/nats_options.hpp"

namespace ores::nats::config {

/**
 * @brief Centralized manager for NATS configuration parsing.
 *
 * Provides utilities for creating command-line option descriptions and
 * reading configuration from parsed options. Follows the same pattern as
 * logging_configuration and database_configuration.
 *
 * Standard options:
 *   --nats-url      NATS server URL (default: nats://localhost:4222)
 */
class nats_configuration final {
public:
    nats_configuration() = delete;

    /**
     * @brief Creates the boost::program_options description for NATS CLI
     * arguments.
     *
     * @return options_description for NATS configuration.
     */
    static boost::program_options::options_description
    make_options_description();

    /**
     * @brief Reads NATS options from parsed variables map.
     *
     * @param vm Parsed command-line options.
     * @return nats_options populated from the variables map.
     */
    static nats_options
    read_options(const boost::program_options::variables_map& vm);
};

}

#endif
