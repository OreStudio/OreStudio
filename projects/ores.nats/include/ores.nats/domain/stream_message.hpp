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
#ifndef ORES_NATS_DOMAIN_STREAM_MESSAGE_HPP
#define ORES_NATS_DOMAIN_STREAM_MESSAGE_HPP

#include <chrono>
#include <cstddef>
#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>

namespace ores::nats::domain {

/**
 * @brief A message stored in a JetStream stream.
 *
 * Returned by jetstream_admin::peek_message() and peek_last_message().
 * The payload is raw bytes; callers interpret as UTF-8 JSON or binary.
 */
struct stream_message {
    std::string subject;
    std::uint64_t sequence = 0;
    std::chrono::system_clock::time_point timestamp;
    std::vector<std::byte> data;
    std::unordered_map<std::string, std::string> headers;
};

}

#endif
