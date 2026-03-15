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
#ifndef ORES_NATS_DOMAIN_MESSAGE_HPP
#define ORES_NATS_DOMAIN_MESSAGE_HPP

#include <functional>
#include <string>
#include <unordered_map>
#include <vector>
#include <cstddef>

namespace ores::nats {

/**
 * @brief A received NATS message.
 *
 * No cnats types appear here. All data is copied out of the cnats message
 * before the callback returns so the cnats message can be destroyed
 * immediately on the cnats internal thread.
 */
struct message {
    /**
     * @brief The subject the message was published to.
     */
    std::string subject;

    /**
     * @brief The reply-to subject, empty for one-way publishes.
     *
     * Services write their response to this subject.
     */
    std::string reply_subject;

    /**
     * @brief The message payload bytes.
     *
     * Typically a MessagePack-encoded domain struct.
     */
    std::vector<std::byte> data;

    /**
     * @brief NATS message headers (NATS 2.2+).
     *
     * The @c Authorization header carries the JWT bearer token for
     * per-request authentication: @c "Authorization" -> @c "Bearer <token>".
     */
    std::unordered_map<std::string, std::string> headers;
};

/**
 * @brief Callback type for incoming NATS messages.
 */
using message_handler = std::function<void(message)>;

}

#endif
