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

#include "ores.nats/config/nats_options.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/export.hpp"
#include "ores.nats/service/buffered_subscription.hpp"
#include "ores.nats/service/jetstream_admin.hpp"
#include "ores.nats/service/subscription.hpp"
#include "ores.nats/service/timeouts.hpp"
#include <boost/asio/awaitable.hpp>
#include <chrono>
#include <cstddef>
#include <initializer_list>
#include <memory>
#include <span>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>

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
class ORES_NATS_EXPORT client {
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
    [[nodiscard]] message request_sync(std::string_view subject,
                                       std::span<const std::byte> data,
                                       std::unordered_map<std::string, std::string> headers = {},
                                       std::chrono::milliseconds timeout = default_request_timeout);

    /**
     * @brief Asynchronous request/reply (ASIO coroutine).
     *
     * Must be called from within a @c boost::asio::awaitable coroutine.
     * The caller's executor is used for callback delivery.
     */
    [[nodiscard]] boost::asio::awaitable<message>
    request(std::string_view subject,
            std::span<const std::byte> data,
            std::unordered_map<std::string, std::string> headers = {},
            std::chrono::milliseconds timeout = default_request_timeout);

    /**
     * @brief Subscribe to a subject.
     *
     * All instances receive every message (fan-out). Use @c queue_subscribe
     * for competing-consumer load balancing.
     */
    [[nodiscard]] subscription subscribe(std::string_view subject, message_handler handler);

    /**
     * @brief Subscribe and retain the last @p capacity messages in a buffer.
     *
     * Each arriving message is stored in a fixed-size circular buffer before
     * @p handler is called. @p handler is optional — pass @c {} to buffer
     * only. Call @c buffered_subscription::snapshot() to read history.
     */
    [[nodiscard]] buffered_subscription subscribe_buffered(std::string_view subject,
                                                           std::size_t capacity,
                                                           message_handler handler = {});

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
     * @brief Queue-group push-subscribe to a JetStream subject.
     *
     * Combines durable consumer semantics with competing-consumer load
     * balancing across a queue group. The consumer is created explicitly, with
     * a delivery subject derived from @p stream_name and @p durable_name and a
     * delivery group equal to @p queue_group, and only then is the subscription
     * bound to it. A
     * consumer created by the subscribe itself would be removed by the server
     * when the subscription drains, even when marked durable, and every
     * service drains on shutdown; creating it explicitly keeps it, and the
     * position it holds, across a restart.
     *
     * Idempotent: an existing consumer with the same configuration is reused.
     * One whose filter, ack policy, delivery bound, delivery subject or
     * delivery group differs is updated in place, so it keeps its position.
     *
     * @param stream_name The stream holding @p subject, as
     *        @c make_stream_name returns it. Required: the consumer is created
     *        on this stream. The call also reconciles a stored consumer whose
     *        filter, ack policy, delivery bound, delivery subject or delivery
     *        group differs from the ones asked for.
     * @param max_deliver How many times a message may be delivered before the
     *        server stops. Every delivery is explicit, so the handler must
     *        answer it -- with @c ack_guard, or by answering the message's own
     *        handle -- and the transport asks for a retry when it does not.
     *        The bound is what keeps a message that always fails from being
     *        redelivered forever; it is required, and there is no unlimited
     *        setting.
     */
    [[nodiscard]] subscription js_queue_subscribe(std::string_view subject,
                                                  std::string_view durable_name,
                                                  std::string_view queue_group,
                                                  message_handler handler,
                                                  std::string_view stream_name,
                                                  int max_deliver);

    /**
     * @brief Queue-group push-subscribe to several subjects on one consumer.
     *
     * The same durable as @c js_queue_subscribe, with a filter set rather than
     * a single subject. A service whose step commands are a family of subjects
     * wants this: one consumer holding the whole family keeps one cursor, so a
     * subject added or renamed does not orphan a consumer, and the service has
     * one server-side object to own rather than one per subject.
     *
     * The server supports multiple filters from 2.10. A stored configuration
     * that differs is updated in place, so the consumer keeps its position:
     * widening the filter set does not replay a backlog it had already worked
     * through. The filter set is compared as a set, so reordering @p subjects
     * does not look like a change. A stored singular filter is read as a set of
     * one, which is how the server stores a single filter.
     *
     * @param subjects The relative subjects, prefixed internally. A wildcard is
     *        allowed, but a list of exact subjects is preferred: a wildcard
     *        that is wider than the service's work makes the consumer receive
     *        messages it has no handler for.
     * @param stream_name The stream holding @p subjects, as
     *        @c make_stream_name returns it. Required, for the same reason as
     *        @c js_queue_subscribe.
     */
    [[nodiscard]] subscription js_queue_subscribe_multi(std::span<const std::string_view> subjects,
                                                        std::string_view durable_name,
                                                        std::string_view queue_group,
                                                        message_handler handler,
                                                        std::string_view stream_name,
                                                        int max_deliver);

    /**
     * @brief Create a JetStream admin handle for provisioning streams.
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

    /**
     * @brief Derive a JetStream stream name for this environment.
     *
     * Combines the configured subject prefix with @p logical_suffix to
     * produce a stream name that is unique per environment and valid for
     * NATS (no dots — replaced with underscores, all lowercase).
     *
     * Example: prefix @c "ores.dev.local2", suffix @c "workflow"
     *          → @c "ores_dev_local2_workflow"
     *
     * Pass the result directly to @c jetstream_admin::ensure_stream().
     */
    [[nodiscard]] std::string make_stream_name(std::string_view logical_suffix) const;

    /**
     * @brief The subjects below @p subject, prefixed for this environment.
     *
     * A @c nats_subject constant names one subject. A stream filter that must
     * also cover the family below it needs the trailing @c ">" wildcard, which
     * is a different string: a filter on the bare subject matches none of its
     * children. The result is prefixed, so it is for stream filters and other
     * absolute uses. Do not pass it to @c subscribe(), which prefixes its
     * subject itself and would match nothing.
     */
    [[nodiscard]] std::string subjects_below(std::string_view subject) const;

    /**
     * @brief The filters that cover each of @p subjects and everything below
     *        it, prefixed for this environment.
     *
     * What a JetStream stream's subject filter needs. A stream provisioned on
     * the bare constant alone holds nothing a producer publishes below it,
     * and the bare constant sitting beside the wildcard is what made that
     * mistake easy to make.
     *
     * Returns the prefixed subjects and their wildcards, in the order given.
     */
    [[nodiscard]] std::vector<std::string>
    covering_subjects(std::initializer_list<std::string_view> subjects) const;

    /**
     * @brief The same filters, for a caller that holds its subjects in a
     *        container.
     *
     * A service registering a family of subjects keeps them in an array, and
     * naming the bare subject and its wildcard by hand at every call site is
     * how the pair drifts apart. This overload is the one a service uses: it
     * accepts any range whose elements convert to a @c std::string_view, which
     * covers both the generated @c nats_subject constants and a literal.
     *
     * @code
     * static constexpr std::array subjects{"a.v1.ops.one", "a.v1.ops.two"};
     * admin.ensure_stream(name, nats.covering_subjects(subjects));
     * @endcode
     */
    template <typename Range>
    [[nodiscard]] std::vector<std::string> covering_subjects(const Range& subjects) const {
        std::vector<std::string> filters;
        for (const auto& subject : subjects) {
            const std::string_view sv(subject);
            filters.push_back(make_subject(sv));
            filters.push_back(subjects_below(sv));
        }
        return filters;
    }

private:
    struct impl;
    std::unique_ptr<impl> impl_;

    /**
     * @brief Log a publish with throttling: first subject at info, later at trace.
     */
    static void log_publish(impl& impl, std::string_view op, const std::string& full_subject);
};

}

#endif
