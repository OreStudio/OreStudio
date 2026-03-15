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
#ifndef ORES_NATS_DOMAIN_STREAM_INFO_HPP
#define ORES_NATS_DOMAIN_STREAM_INFO_HPP

#include <chrono>
#include <cstdint>
#include <string>
#include <vector>

namespace ores::nats::domain {

/**
 * @brief Metadata for a JetStream stream.
 *
 * Populated by jetstream_admin::list_streams() and get_stream().
 * All cnats types are hidden behind the admin implementation.
 */
struct stream_info {
    std::string name;
    std::vector<std::string> subjects;
    std::uint64_t message_count = 0;
    std::uint64_t byte_count = 0;
    std::uint64_t consumer_count = 0;
    std::uint64_t first_seq = 0;  // sequence of first stored message (0 if empty)
    std::uint64_t last_seq = 0;   // sequence of last stored message (0 if empty)
    std::chrono::system_clock::time_point created_at;
    std::chrono::system_clock::time_point first_message_at;
    std::chrono::system_clock::time_point last_message_at;
};

}

#endif
