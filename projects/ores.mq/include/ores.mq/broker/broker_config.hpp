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
#ifndef ORES_MQ_BROKER_BROKER_CONFIG_HPP
#define ORES_MQ_BROKER_BROKER_CONFIG_HPP

#include <string>
#include <vector>
#include <cstdint>
#include <utility>

namespace ores::mq::broker {

/**
 * @brief Configuration for the NNG broker.
 */
struct broker_config final {
    /**
     * @brief NNG endpoint for client connections (e.g. "tcp://0.0.0.0:7001").
     */
    std::string frontend_endpoint = "tcp://0.0.0.0:7001";

    /**
     * @brief NNG endpoint for service connections (e.g. "tcp://0.0.0.0:7002").
     */
    std::string backend_endpoint = "tcp://0.0.0.0:7002";

    /**
     * @brief Message-type ranges that require durable delivery via pgmq.
     *
     * For ranges listed here, the broker writes the message to pgmq before
     * forwarding to the service. Pairs of [min, max] message type values.
     */
    std::vector<std::pair<std::uint16_t, std::uint16_t>> durable_ranges;
};

}

#endif
