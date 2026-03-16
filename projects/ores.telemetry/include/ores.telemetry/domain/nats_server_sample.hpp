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
#ifndef ORES_TELEMETRY_DOMAIN_NATS_SERVER_SAMPLE_HPP
#define ORES_TELEMETRY_DOMAIN_NATS_SERVER_SAMPLE_HPP

#include <chrono>
#include <cstdint>

namespace ores::telemetry::domain {

/**
 * @brief A single point-in-time sample of NATS server-level metrics.
 *
 * Populated by polling the NATS server's HTTP monitoring endpoint at
 * /varz.  One row is written to ores_nats_server_samples_tbl per poll.
 */
struct nats_server_sample final {
    /**
     * @brief When this sample was taken.
     *
     * Partitioning key for the TimescaleDB hypertable.
     */
    std::chrono::system_clock::time_point sampled_at;

    /** @brief Total inbound messages since server start. */
    std::uint64_t in_msgs{0};

    /** @brief Total outbound messages since server start. */
    std::uint64_t out_msgs{0};

    /** @brief Total inbound bytes since server start. */
    std::uint64_t in_bytes{0};

    /** @brief Total outbound bytes since server start. */
    std::uint64_t out_bytes{0};

    /** @brief Current number of client connections. */
    int connections{0};

    /** @brief Server process resident memory in bytes. */
    std::uint64_t mem_bytes{0};

    /**
     * @brief Number of slow consumers detected since server start.
     *
     * A non-zero value indicates that one or more subscribers are not
     * keeping up with message delivery and messages may be dropped.
     */
    int slow_consumers{0};
};

}

#endif
