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
#ifndef ORES_TELEMETRY_DOMAIN_NATS_STREAM_SAMPLE_HPP
#define ORES_TELEMETRY_DOMAIN_NATS_STREAM_SAMPLE_HPP

#include <chrono>
#include <cstdint>
#include <string>

namespace ores::telemetry::domain {

/**
 * @brief A single point-in-time sample of a JetStream stream's metrics.
 *
 * Populated by polling the NATS server's HTTP monitoring endpoint at
 * /jsz?streams=true.  One row per stream per poll is written to
 * ores_nats_stream_samples_tbl.
 */
struct nats_stream_sample final {
    /**
     * @brief When this sample was taken.
     *
     * Part of the composite partitioning key for the TimescaleDB hypertable.
     */
    std::chrono::system_clock::time_point sampled_at;

    /**
     * @brief JetStream stream name (e.g. "ORES_TRADES").
     *
     * Part of the composite primary key.
     */
    std::string stream_name;

    /** @brief Number of messages currently stored in the stream. */
    std::uint64_t messages{0};

    /** @brief Total bytes currently stored in the stream. */
    std::uint64_t bytes{0};

    /**
     * @brief Number of active consumers on this stream.
     *
     * A value of zero means no subscriber is reading from the stream.
     */
    int consumer_count{0};
};

}

#endif
