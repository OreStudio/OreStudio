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
#ifndef ORES_MQ_DOMAIN_MQ_MESSAGE_HPP
#define ORES_MQ_DOMAIN_MQ_MESSAGE_HPP

#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.mq/domain/mq_message_status.hpp"

namespace ores::mq::domain {

/**
 * @brief A message stored in a queue.
 *
 * Mirrors a row in ores_mq_messages_tbl.
 */
struct mq_message final {
    std::int64_t id = 0;
    boost::uuids::uuid queue_id;
    std::optional<boost::uuids::uuid> tenant_id;
    std::optional<boost::uuids::uuid> party_id;
    std::string message_type;
    std::string payload_type;  // "json" or "binary"
    std::optional<std::string> payload;    // JSON text when payload_type == "json"
    std::vector<std::byte> raw_payload;    // when payload_type == "binary"
    mq_message_status status = mq_message_status::pending;
    std::chrono::system_clock::time_point visible_after;
    std::chrono::system_clock::time_point created_at;
    int read_count = 0;
    std::string error_message;
};

}

#endif
