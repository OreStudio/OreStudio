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
#ifndef ORES_TELEMETRY_DOMAIN_NATS_SAMPLES_QUERY_HPP
#define ORES_TELEMETRY_DOMAIN_NATS_SAMPLES_QUERY_HPP

#include <chrono>
#include <cstdint>
#include <string>

namespace ores::telemetry::domain {

/**
 * @brief Query parameters for retrieving NATS server samples.
 */
struct nats_server_samples_query final {
    std::chrono::system_clock::time_point start_time;
    std::chrono::system_clock::time_point end_time;
    std::uint32_t limit{1000};
};

/**
 * @brief Query parameters for retrieving NATS stream samples.
 */
struct nats_stream_samples_query final {
    /** @brief JetStream stream name to query. */
    std::string stream_name;
    std::chrono::system_clock::time_point start_time;
    std::chrono::system_clock::time_point end_time;
    std::uint32_t limit{1000};
};

}

#endif
