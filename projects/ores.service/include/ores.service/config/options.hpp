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
#ifndef ORES_SERVICE_CONFIG_OPTIONS_HPP
#define ORES_SERVICE_CONFIG_OPTIONS_HPP

#include <iosfwd>
#include <optional>
#include "ores.utility/log/logging_options.hpp"
#include "ores.database/domain/database_options.hpp"
#include "ores.comms/net/server_options.hpp"

namespace ores::service::config {

/**
 * @brief All of the configuration options required by the service.
 */
struct options final {
    /**
     * @brief Configuration options related to logging, if any.
     */
    std::optional<ores::utility::log::logging_options> logging;
    /**
     * @brief Configuration related to server operations.
     */
    comms::net::server_options server;
    /**
     * @brief Configuration related to database operations.
     */
    ores::database::database_options database;
};

std::ostream& operator<<(std::ostream& s, const options& v);

}

#endif
