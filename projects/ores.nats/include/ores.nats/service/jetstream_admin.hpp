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

#include <cstdint>
#include <string>
#include <string_view>
#include <vector>
#include "ores.nats/domain/consumer_info.hpp"
#include "ores.nats/domain/stream_info.hpp"
#include "ores.nats/domain/stream_message.hpp"

namespace ores::nats::service {

/**
 * @brief JetStream management API.
 *
 * Wraps the cnats JetStream management functions. No cnats types appear in
 * this header — all cnats state is held as an opaque void* internally.
 *
 * Obtain an instance via client::make_admin() after connecting.
 * The admin is lightweight (a single pointer) and can be copied freely.
 * The underlying client must outlive all admin instances derived from it.
 *
 * All methods throw std::runtime_error on failure.
 *
 * @par Usage
 * @code
 * auto admin = nats_client.make_admin();
 * auto streams = admin.list_streams();
 * for (const auto& s : streams)
 *     auto consumers = admin.list_consumers(s.name);
 * @endcode
 */
class jetstream_admin {
public:
    /**
     * @brief Construct from an opaque JetStream context pointer.
     *
     * Only callable from client::make_admin(). The void* is a jsCtx*.
     */
    explicit jetstream_admin(void* js_ctx) noexcept;

    // -------------------------------------------------------------------------
    // Streams
    // -------------------------------------------------------------------------

    /**
     * @brief Create a durable stream covering @p subjects, or update it if a
     *        stream with @p name already exists.
     *
     * Idempotent: safe to call on every service startup. The stream uses
     * file-backed storage and retains messages for up to @p max_age_days days.
     *
     * @param name          Stream name (NATS naming rules: A-Z a-z 0-9 - _).
     * @param subjects      Subjects the stream captures (fully-qualified,
     *                      including any subject prefix).
     * @param max_age_days  How long messages are retained (default: 7 days).
     */
    void ensure_stream(std::string_view name,
        std::vector<std::string> subjects,
        int max_age_days = 7);

    /**
     * @brief List all streams on the connected NATS server.
     */
    [[nodiscard]] std::vector<domain::stream_info> list_streams();

    /**
     * @brief Get metadata for a single stream by name.
     */
    [[nodiscard]] domain::stream_info get_stream(std::string_view name);

    /**
     * @brief Purge all messages from a stream (non-destructive to the stream
     *        itself — consumers and configuration are preserved).
     */
    void purge_stream(std::string_view name);

    // -------------------------------------------------------------------------
    // Consumers
    // -------------------------------------------------------------------------

    /**
     * @brief List all consumers for a stream.
     */
    [[nodiscard]] std::vector<domain::consumer_info>
    list_consumers(std::string_view stream_name);

    // -------------------------------------------------------------------------
    // Messages
    // -------------------------------------------------------------------------

    /**
     * @brief Peek at a message by sequence number (non-destructive).
     */
    [[nodiscard]] domain::stream_message
    peek_message(std::string_view stream_name, std::uint64_t sequence);

    /**
     * @brief Peek at the last message published to a subject (non-destructive).
     */
    [[nodiscard]] domain::stream_message
    peek_last_message(std::string_view stream_name,
                      std::string_view subject);

    /**
     * @brief Delete a message by sequence number.
     *
     * The message is removed from the stream. This cannot be undone.
     */
    void delete_message(std::string_view stream_name, std::uint64_t sequence);

    /**
     * @brief Publish a message to a JetStream subject.
     *
     * The subject must be covered by an existing stream. Blocks until the
     * server acknowledges. Payload is treated as raw bytes.
     */
    void publish(std::string_view subject, std::string_view payload);

private:
    void* js_ctx_; // opaque jsCtx*; not owned
};

}

#endif
