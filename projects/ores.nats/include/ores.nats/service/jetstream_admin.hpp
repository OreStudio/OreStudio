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
#ifndef ORES_NATS_SERVICE_JETSTREAM_ADMIN_HPP
#define ORES_NATS_SERVICE_JETSTREAM_ADMIN_HPP

#include "ores.nats/export.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::nats::service {

/**
 * @brief What a stream does with a message once its consumers have it.
 */
enum class stream_retention {
    /// Keep every message until the age limit. The server's default, and the
    /// right choice for a stream of data that outlives its readers.
    limits,
    /// Remove a message as soon as every consumer that filters it has
    /// acknowledged it. A message on a subject no consumer filters is dropped
    /// the moment it is published, so a stream that uses this is only safe
    /// while its consumer exists. Reach for it when the messages are commands:
    /// once a command is served there is nothing left to keep.
    interest,
};

/**
 * @brief What the server holds for one consumer.
 *
 * The server's own record, read back after a subscription has drained, rather
 * than the client's view of it. @c created_ns identifies the consumer: a
 * consumer that was deleted and recreated carries a different creation time
 * even when its name, filter and position look the same.
 */
struct consumer_status {
    /// UTC, in nanoseconds since the epoch.
    int64_t created_ns = 0;
    uint64_t delivered = 0;
    uint64_t ack_floor_stream = 0;
    int64_t num_ack_pending = 0;
    int64_t num_redelivered = 0;
    uint64_t num_pending = 0;
    bool push_bound = false;
};

/**
 * @brief JetStream stream provisioning.
 *
 * Wraps the cnats stream-creation function. No cnats types appear in this
 * header: all cnats state is held as an opaque void* internally.
 *
 * Obtain an instance via client::make_admin() after connecting. The admin is
 * lightweight (a single pointer) and can be copied freely. The underlying
 * client must outlive all admin instances derived from it.
 *
 * Every method throws std::runtime_error on failure.
 */
class ORES_NATS_EXPORT jetstream_admin {
public:
    /**
     * @brief Construct from an opaque JetStream context pointer.
     *
     * Only callable from client::make_admin(). The void* is a jsCtx*.
     */
    explicit jetstream_admin(void* js_ctx) noexcept;

    /**
     * @brief Create a durable stream covering @p subjects, or update it if a
     *        stream with @p name already exists.
     *
     * Idempotent: safe to call on every service startup. The stream uses
     * file-backed storage and retains messages for up to @p max_age_days days.
     * The filter, the retention policy and the age limit are all reconciled, so
     * a stream created by an older build converges on the current contract.
     *
     * @param name          Stream name (NATS naming rules: A-Z a-z 0-9 - _).
     * @param subjects      Subjects the stream captures (fully-qualified,
     *                      including any subject prefix).
     * @param max_age_days  How long messages are retained (default: 7 days).
     * @param retention     What removes a message. The default keeps every
     *                      message until the age limit.
     */
    void ensure_stream(std::string_view name,
                       std::vector<std::string> subjects,
                       int max_age_days = 7,
                       stream_retention retention = stream_retention::limits);

    /**
     * @brief Delete every consumer on @p name except @p keep.
     *
     * A consumer is identified by its name, so a renamed subject, or a service
     * that moves from one consumer per subject to one consumer per stream,
     * leaves consumers behind that nothing declares and nothing reads. They hold
     * no work worth keeping, because the consumer that carries the whole family
     * filters every subject, and they leave the stream with bookkeeping that
     * describes a shape the code no longer has. Delete them.
     *
     * Idempotent, so call it on every startup. A failure to prune is not fatal
     * and is logged rather than thrown: the consumer that serves the stream is
     * not the one being deleted, so the service still works.
     */
    void prune_consumers(std::string_view name, std::string_view keep);

    /**
     * @brief Read the server's record of @p consumer on @p stream.
     *
     * @return nothing when the server holds no consumer of that name.
     */
    std::optional<consumer_status> consumer_state(std::string_view stream,
                                                  std::string_view consumer);

    /**
     * @brief Delete @p name, and every consumer it holds.
     *
     * Idempotent: a stream that is already gone is not an error. For a test
     * that owns a stream of its own, so a run leaves nothing behind.
     */
    void delete_stream(std::string_view name);

private:
    // An opaque jsCtx*, not owned.
    void* js_ctx_;
};

}

#endif
