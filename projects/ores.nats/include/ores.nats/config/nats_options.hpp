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
#ifndef ORES_NATS_CONFIG_NATS_OPTIONS_HPP
#define ORES_NATS_CONFIG_NATS_OPTIONS_HPP

#include <string>

namespace ores::nats::config {

/**
 * @brief Configuration for a NATS connection.
 */
struct nats_options final {
    /**
     * @brief NATS server URL (e.g. "nats://localhost:4222" or
     *        "tls+tcp://localhost:4222" for TLS).
     */
    std::string url = "nats://localhost:4222";

    /**
     * @brief Subject on which the service listens for requests.
     *
     * All client requests are sent to this subject and the service
     * publishes responses to the per-request NATS reply subject.
     */
    std::string subject = "ores.comms.service";
};

}

#endif
