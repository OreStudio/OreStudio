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
#ifndef ORES_NATS_DOMAIN_CONSUMER_INFO_HPP
#define ORES_NATS_DOMAIN_CONSUMER_INFO_HPP

#include <chrono>
#include <cstdint>
#include <string>

namespace ores::nats::domain {

/**
 * @brief Metadata and delivery stats for a JetStream consumer.
 *
 * Populated by jetstream_admin::list_consumers() and get_consumer().
 */
struct consumer_info {
    std::string stream_name;
    std::string name;
    std::uint64_t num_pending = 0;       // messages not yet delivered
    std::uint64_t num_ack_pending = 0;   // delivered but not yet acknowledged
    std::uint64_t num_redelivered = 0;
    std::uint64_t delivered_count = 0;
    std::chrono::system_clock::time_point created_at;
};

}

#endif
