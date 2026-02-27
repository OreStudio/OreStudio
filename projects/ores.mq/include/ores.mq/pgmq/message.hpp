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
#ifndef ORES_MQ_PGMQ_MESSAGE_HPP
#define ORES_MQ_PGMQ_MESSAGE_HPP

#include <chrono>
#include <cstdint>

namespace ores::mq::pgmq {

/**
 * @brief A message retrieved from a pgmq queue.
 *
 * Mirrors the pgmq.message_record composite type returned by pgmq.read(),
 * pgmq.read_with_poll(), pgmq.pop() and pgmq.set_vt().
 *
 * @tparam T The type of the message body. Use std::string to receive the
 *           raw JSONB text without deserialization.
 */
template<typename T>
struct message final {
    /// Unique monotonically increasing message identifier within the queue.
    int64_t msg_id{0};

    /// Number of times this message has been read (visibility timeout popped).
    int32_t read_ct{0};

    /// Timestamp when the message was enqueued (UTC).
    std::chrono::system_clock::time_point enqueued_at;

    /// Visibility timeout: the message is invisible to other readers until this time.
    std::chrono::system_clock::time_point vt;

    /// The message body, deserialized from JSONB.
    T body{};
};

}

#endif
