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
#ifndef ORES_NATS_SERVICE_CLIENT_HPP
#define ORES_NATS_SERVICE_CLIENT_HPP

#include <chrono>
#include <memory>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <cstddef>
#include <boost/asio/awaitable.hpp>
#include "ores.nats/config/nats_options.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/jetstream_admin.hpp"
#include "ores.nats/service/subscription.hpp"

namespace ores::nats::service {

/**
 * @brief NATS client: connection, pub/sub, request/reply, and JetStream.
 *
 * No cnats types appear in this header. All cnats state is hidden behind
 * a Pimpl. The same client instance is used by both service-side handlers
 * (via @c queue_subscribe) and client-side request/reply callers.
 *
 * Thread safety:
 * - @c connect() and @c disconnect() must be called from one thread.
 * - @c publish(), @c request_sync(), @c subscribe(), @c queue_subscribe()
 *   are thread-safe once the client is connected.
 * - @c request() (async) must be called from an ASIO coroutine.
 *
 * Lifecycle: all @c subscription handles must be destroyed before the
 * @c client that created them.
 *
 * @par Service-side usage
 * @code
 * nats::service::client nats(cfg.nats);
 * nats.connect();
 * auto sub = nats.queue_subscribe(
 *     "ores.iam.v1.>", "ores.iam.service",
 *     [](nats::message msg) { handle(msg); });
 * nats.drain();   // blocks until SIGINT/SIGTERM
 * @endcode
 *
 * @par Client-side usage
 * @code
 * nats::service::client nats(cfg.nats);
 * nats.connect();
 * auto reply = nats.request_sync(
 *     "ores.iam.v1.accounts.list", msgpack_bytes,
 *     {{"Authorization", "Bearer " + jwt}});
 * @endcode
 */
class client {
public:
    explicit client(config::nats_options opts);
    ~client();

    client(const client&) = delete;
    client& operator=(const client&) = delete;

    // -------------------------------------------------------------------------
    // Lifecycle
    // -------------------------------------------------------------------------

    /**
     * @brief Connect to the NATS server and initialise JetStream.
     *
     * Synchronous. Throws @c std::runtime_error on failure.
     */
    void connect();

    /**
     * @brief Disconnect from NATS, freeing all cnats resources.
     *
     * Safe to call more than once. All subscriptions must be destroyed
     * before calling this.
     */
    void disconnect();

    [[nodiscard]] bool is_connected() const noexcept;

    // -------------------------------------------------------------------------
    // Core pub/sub (ephemeral — no persistence)
    // -------------------------------------------------------------------------

    /**
     * @brief Publish a message to a subject.
     *
     * Fire-and-forget. @c headers are optional; pass the @c Authorization
     * header for authenticated publishes.
     */
    void publish(std::string_view subject,
        std::span<const std::byte> data,
        std::unordered_map<std::string, std::string> headers = {});

    /**
     * @brief Synchronous request/reply.
     *
     * Blocks until a response arrives or @c timeout elapses. Throws on
     * timeout or transport error. Suitable for Qt/shell client code that
     * is not running on an ASIO executor.
     */
    [[nodiscard]] message request_sync(
        std::string_view subject,
        std::span<const std::byte> data,
        std::unordered_map<std::string, std::string> headers = {},
        std::chrono::milliseconds timeout = std::chrono::seconds(30));

    /**
     * @brief Asynchronous request/reply (ASIO coroutine).
     *
     * Must be called from within a @c boost::asio::awaitable coroutine.
     * The caller's executor is used for callback delivery.
     */
    [[nodiscard]] boost::asio::awaitable<message> request(
        std::string_view subject,
        std::span<const std::byte> data,
        std::unordered_map<std::string, std::string> headers = {},
        std::chrono::milliseconds timeout = std::chrono::seconds(30));

    /**
     * @brief Subscribe to a subject.
     *
     * All instances receive every message (fan-out). Use @c queue_subscribe
     * for competing-consumer load balancing.
     */
    [[nodiscard]] subscription subscribe(std::string_view subject,
        message_handler handler);

    /**
     * @brief Queue-group subscribe (competing consumers).
     *
     * Only one subscriber in the @c queue_group receives each message.
     * Use this for all service-side subscriptions so that multiple
     * instances of the same service share the load.
     *
     * Convention: @c queue_group == service name, e.g. @c "ores.iam.service".
     */
    [[nodiscard]] subscription queue_subscribe(std::string_view subject,
        std::string_view queue_group,
        message_handler handler);

    // -------------------------------------------------------------------------
    // JetStream (durable, persistent)
    // -------------------------------------------------------------------------

    /**
     * @brief Publish a message to a JetStream stream.
     *
     * The target subject must be covered by a stream configured on the
     * NATS server. Blocks until the server acknowledges receipt.
     * Throws on failure.
     */
    void js_publish(std::string_view subject,
        std::span<const std::byte> data,
        std::unordered_map<std::string, std::string> headers = {});

    /**
     * @brief Push-subscribe to a JetStream subject with a durable consumer.
     *
     * Messages are delivered automatically to @c handler. The consumer
     * state survives client restarts (durable).
     *
     * Each message is auto-acknowledged after @c handler returns without
     * throwing.
     */
    [[nodiscard]] subscription js_subscribe(std::string_view subject,
        std::string_view durable_name,
        message_handler handler);

    /**
     * @brief Queue-group push-subscribe to a JetStream subject.
     *
     * Combines durable consumer semantics with competing-consumer load
     * balancing across a queue group.
     */
    [[nodiscard]] subscription js_queue_subscribe(std::string_view subject,
        std::string_view durable_name,
        std::string_view queue_group,
        message_handler handler);

    /**
     * @brief Create a JetStream admin handle for managing streams and
     *        consumers.
     *
     * The returned @c jetstream_admin is lightweight and borrows this
     * client's JetStream context. The client must outlive the admin handle.
     */
    [[nodiscard]] jetstream_admin make_admin();

    // -------------------------------------------------------------------------
    // Shutdown
    // -------------------------------------------------------------------------

    /**
     * @brief Graceful shutdown.
     *
     * Stops accepting new messages, waits for all in-flight deliveries to
     * complete, then returns. Call this instead of @c disconnect() when you
     * want a clean shutdown at the end of a service lifetime.
     */
    void drain();

    // -------------------------------------------------------------------------
    // Subject helpers
    // -------------------------------------------------------------------------

    /**
     * @brief Prepend the configured subject prefix to a relative subject.
     *
     * If @c nats_options::subject_prefix is non-empty, returns
     * @c "{prefix}.{relative}"; otherwise returns @c relative unchanged.
     *
     * All pub/sub/request methods call this internally, so callers always
     * pass relative subjects (e.g. @c "iam.v1.accounts.list").
     */
    [[nodiscard]] std::string make_subject(std::string_view relative) const;

private:
    struct impl;
    std::unique_ptr<impl> impl_;
};

}

#endif
