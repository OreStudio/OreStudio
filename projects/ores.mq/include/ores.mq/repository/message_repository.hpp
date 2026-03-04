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
#ifndef ORES_MQ_REPOSITORY_MESSAGE_REPOSITORY_HPP
#define ORES_MQ_REPOSITORY_MESSAGE_REPOSITORY_HPP

#include <cstdint>
#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.database/domain/context.hpp"
#include "ores.mq/domain/mq_message.hpp"

namespace ores::mq::repository {

/**
 * @brief Data access layer for queue messages.
 *
 * Wraps ores_mq_messages_send_fn, ores_mq_messages_read_fn,
 * ores_mq_messages_ack_fn and ores_mq_messages_nack_fn via parameterized SQL.
 * All methods are stateless.
 */
class message_repository final {
public:
    using context = ores::database::context;

    /**
     * @brief Sends a JSON message to a queue.
     *
     * @param queue_id      Target queue UUID.
     * @param message_type  Application-level message type tag.
     * @param payload_json  JSON text payload.
     * @param delay_seconds Visibility delay in seconds (0 = immediate).
     * @return The assigned message ID.
     */
    std::int64_t send(context ctx, const boost::uuids::uuid& queue_id,
        const std::string& message_type, const std::string& payload_json,
        int delay_seconds = 0);

    /**
     * @brief Reads and locks up to @p batch messages from a queue.
     *
     * Messages are hidden from other readers for @p vt_seconds seconds.
     * Call ack() to permanently delete them or nack() to make them
     * visible again immediately.
     *
     * @param batch      Maximum number of messages to return.
     * @param vt_seconds Visibility timeout in seconds.
     */
    std::vector<domain::mq_message> read(context ctx,
        const boost::uuids::uuid& queue_id, int batch = 1,
        int vt_seconds = 30);

    /**
     * @brief Acknowledges (deletes) a batch of messages.
     */
    void ack(context ctx, const std::vector<std::int64_t>& message_ids);

    /**
     * @brief Negative-acknowledges a message, recording an error.
     *
     * The message is made immediately visible again for redelivery.
     */
    void nack(context ctx, std::int64_t message_id,
        const std::string& error = "");

    /**
     * @brief Deletes all messages from a queue.
     *
     * @return The number of messages deleted.
     */
    std::int64_t purge(context ctx, const boost::uuids::uuid& queue_id);
};

}

#endif
