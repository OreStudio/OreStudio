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
#ifndef ORES_MQ_SERVICE_MQ_SERVICE_HPP
#define ORES_MQ_SERVICE_MQ_SERVICE_HPP

#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.database/domain/context.hpp"
#include "ores.mq/domain/queue_definition.hpp"
#include "ores.mq/domain/mq_message.hpp"
#include "ores.mq/domain/queue_stats.hpp"
#include "ores.mq/repository/queue_repository.hpp"
#include "ores.mq/repository/message_repository.hpp"
#include "ores.mq/repository/queue_stats_repository.hpp"

namespace ores::mq::service {

/**
 * @brief High-level service facade for the custom MQ subsystem.
 *
 * Composes the three repository classes (queue_repository,
 * message_repository, queue_stats_repository) behind a single interface.
 * All operations are executed within the database::context supplied at
 * construction time. The object is not thread-safe; create one per request.
 */
class mq_service final {
public:
    using context = ores::database::context;

    explicit mq_service(context ctx);

    // -----------------------------------------------------------------------
    // Queue management
    // -----------------------------------------------------------------------

    /**
     * @brief Creates a new queue definition.
     *
     * @return The UUID assigned to the new queue.
     */
    boost::uuids::uuid create_queue(const domain::queue_definition& def,
        const std::string& modified_by);

    /**
     * @brief Finds a queue by name and optional scope filters.
     *
     * @return The queue definition, or nullopt if not found.
     */
    std::optional<domain::queue_definition> find_queue(const std::string& name,
        const std::optional<boost::uuids::uuid>& tenant_id,
        const std::optional<boost::uuids::uuid>& party_id);

    /**
     * @brief Returns all active queues visible in the current context.
     */
    std::vector<domain::queue_definition> list_queues();

    /**
     * @brief Marks a queue as inactive (soft delete).
     */
    void deactivate(const boost::uuids::uuid& queue_id);

    // -----------------------------------------------------------------------
    // Message operations
    // -----------------------------------------------------------------------

    /**
     * @brief Sends a JSON message to a queue.
     *
     * @return The assigned message ID.
     */
    std::int64_t send(const boost::uuids::uuid& queue_id,
        const std::string& message_type, const std::string& payload,
        int delay_seconds = 0);

    /**
     * @brief Reads and locks up to @p batch messages from a queue.
     */
    std::vector<domain::mq_message> read(const boost::uuids::uuid& queue_id,
        int batch = 1, int vt_seconds = 30);

    /**
     * @brief Acknowledges (permanently deletes) a batch of messages.
     */
    void ack(const std::vector<std::int64_t>& message_ids);

    /**
     * @brief Negative-acknowledges a message, recording an error.
     */
    void nack(std::int64_t message_id, const std::string& error = "");

    /**
     * @brief Deletes all messages from a queue.
     *
     * @return The number of messages deleted.
     */
    std::int64_t purge(const boost::uuids::uuid& queue_id);

    // -----------------------------------------------------------------------
    // Statistics
    // -----------------------------------------------------------------------

    /**
     * @brief Returns the latest statistics for all queues.
     */
    std::vector<domain::queue_stats> get_stats();

    /**
     * @brief Returns time-series statistics samples for a specific queue.
     */
    std::vector<domain::queue_stats> get_stats_samples(
        const boost::uuids::uuid& queue_id,
        std::optional<std::chrono::system_clock::time_point> from = {},
        std::optional<std::chrono::system_clock::time_point> to = {});

private:
    context ctx_;
    repository::queue_repository queue_repo_;
    repository::message_repository msg_repo_;
    repository::queue_stats_repository stats_repo_;
};

}

#endif
