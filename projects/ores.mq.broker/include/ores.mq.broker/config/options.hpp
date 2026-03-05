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
#ifndef ORES_MQ_BROKER_CONFIG_OPTIONS_HPP
#define ORES_MQ_BROKER_CONFIG_OPTIONS_HPP

#include <iosfwd>
#include <optional>
#include "ores.logging/logging_options.hpp"

namespace ores::mq::broker::config {

/**
 * @brief All configuration options required by the broker process.
 */
struct options final {
    /**
     * @brief Logging configuration, if any.
     */
    std::optional<ores::logging::logging_options> logging;

    /**
     * @brief NNG endpoint for client connections (frontend).
     *
     * Default: tcp://0.0.0.0:7001
     */
    std::string frontend_endpoint = "tcp://0.0.0.0:7001";

    /**
     * @brief NNG endpoint for service connections (backend).
     *
     * Default: tcp://0.0.0.0:7002
     */
    std::string backend_endpoint = "tcp://0.0.0.0:7002";
};

std::ostream& operator<<(std::ostream& s, const options& v);

}

#endif
