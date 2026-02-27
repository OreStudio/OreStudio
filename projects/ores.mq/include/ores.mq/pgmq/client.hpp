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
#ifndef ORES_MQ_PGMQ_CLIENT_HPP
#define ORES_MQ_PGMQ_CLIENT_HPP

#include <chrono>
#include <optional>
#include <string>
#include <vector>
#include <type_traits>
#include <vector>
#include <rfl/json.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include "ores.mq/pgmq/mq_exception.hpp"
#include "ores.mq/pgmq/message.hpp"
#include "ores.mq/pgmq/queue_info.hpp"
#include "ores.mq/pgmq/queue_metrics.hpp"

namespace ores::mq::pgmq {

/**
 * @brief Stateless low-level client for the pgmq PostgreSQL message queue extension.
 *
 * Mirrors the Npgmq (.NET) API. Every method takes an ores::database::context
 * and is safe to call concurrently from multiple threads (no shared state).
 *
 * Template send/read/pop/set_visibility_timeout methods automatically
 * serialize/deserialize the body via rfl::json. Instantiate with std::string
 * to bypass JSON (the raw JSONB text is returned/sent as-is).
 *
 * Usage:
 * @code
 *     pgmq::client c;
 *     c.create(ctx, "my_queue");
 *
 *     // Typed send/receive
 *     struct my_payload { std::string task; int priority; };
 *     c.send(ctx, "my_queue", my_payload{"process", 1});
 *     auto msgs = c.read<my_payload>(ctx, "my_queue", std::chrono::seconds(30));
 *     c.erase(ctx, "my_queue", msgs[0].msg_id);
 * @endcode
 */
class client final {
private:
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger("ores.mq.pgmq.client");
        return instance;
    }

    /// Internal row returned by pgmq message functions.
    struct raw_msg {
        int64_t msg_id{};
        int32_t read_ct{};
        std::chrono::system_clock::time_point enqueued_at;
        std::chrono::system_clock::time_point vt;
        std::string body;
    };

    // Non-template implementations (defined in client.cpp).
    int64_t do_send(ores::database::context ctx,
        const std::string& queue_name, const std::string& json,
        std::chrono::seconds delay);

    std::vector<int64_t> do_send_batch(ores::database::context ctx,
        const std::string& queue_name,
        const std::vector<std::string>& jsons, std::chrono::seconds delay);

    std::vector<raw_msg> do_read(ores::database::context ctx,
        const std::string& queue_name, std::chrono::seconds vt, int qty);

    std::vector<raw_msg> do_read_with_poll(ores::database::context ctx,
        const std::string& queue_name, std::chrono::seconds vt, int qty,
        std::chrono::seconds max_poll, std::chrono::milliseconds poll_interval);

    std::optional<raw_msg> do_pop(ores::database::context ctx,
        const std::string& queue_name);

    std::optional<raw_msg> do_set_vt(ores::database::context ctx,
        const std::string& queue_name, int64_t msg_id, std::chrono::seconds vt);

    // Timestamp and row parsing helpers (defined in client.cpp).
    static std::chrono::system_clock::time_point parse_pg_timestamp(
        const std::string& s);

    static raw_msg parse_raw_msg(
        const std::vector<std::optional<std::string>>& row);

    static queue_metrics parse_metrics_row(
        const std::vector<std::optional<std::string>>& row);

    // Conversion helpers.
    template<typename T>
    static message<T> to_message(const raw_msg& row) {
        message<T> m;
        m.msg_id     = row.msg_id;
        m.read_ct    = row.read_ct;
        m.enqueued_at = row.enqueued_at;
        m.vt         = row.vt;
        if constexpr (std::is_same_v<T, std::string>) {
            m.body = row.body;
        } else {
            auto result = rfl::json::read<T>(row.body);
            if (!result) {
                throw mq_exception("Failed to deserialize message body: " + row.body);
            }
            m.body = std::move(*result);
        }
        return m;
    }

    template<typename T>
    static std::vector<message<T>> to_messages(const std::vector<raw_msg>& rows) {
        std::vector<message<T>> msgs;
        msgs.reserve(rows.size());
        for (const auto& row : rows) {
            msgs.push_back(to_message<T>(row));
        }
        return msgs;
    }

public:
    // -----------------------------------------------------------------------
    // Queue management
    // -----------------------------------------------------------------------

    /**
     * @brief Creates a durable, logged queue.
     *
     * No-op if the queue already exists.
     */
    void create(ores::database::context ctx, const std::string& queue_name);

    /**
     * @brief Creates an unlogged queue (faster, but not crash-safe).
     *
     * No-op if the queue already exists.
     */
    void create_unlogged(ores::database::context ctx, const std::string& queue_name);

    /**
     * @brief Drops a queue and all its messages.
     *
     * @return true if the queue was dropped, false if it did not exist.
     */
    bool drop(ores::database::context ctx, const std::string& queue_name);

    /**
     * @brief Deletes all messages from a queue without dropping it.
     *
     * @return The number of messages purged.
     */
    int64_t purge(ores::database::context ctx, const std::string& queue_name);

    /**
     * @brief Returns metadata for all queues.
     */
    std::vector<queue_info> list_queues(ores::database::context ctx);

    // -----------------------------------------------------------------------
    // Sending messages
    // -----------------------------------------------------------------------

    /**
     * @brief Sends a single message.
     *
     * If T is std::string, the value is sent as raw JSONB (bypasses rfl::json).
     * Otherwise the body is serialized with rfl::json::write.
     *
     * @param delay Optional delivery delay (message is invisible until elapsed).
     * @return The message ID assigned by pgmq.
     */
    template<typename T>
    int64_t send(ores::database::context ctx, const std::string& queue_name,
                 const T& body, std::chrono::seconds delay = {}) {
        if constexpr (std::is_same_v<T, std::string>) {
            return do_send(std::move(ctx), queue_name, body, delay);
        } else {
            return do_send(std::move(ctx), queue_name, rfl::json::write(body), delay);
        }
    }

    /**
     * @brief Sends multiple messages atomically.
     *
     * @param delay Optional delivery delay applied to all messages.
     * @return The message IDs assigned, in the same order as the inputs.
     */
    template<typename T>
    std::vector<int64_t> send_batch(ores::database::context ctx,
                                    const std::string& queue_name,
                                    const std::vector<T>& bodies,
                                    std::chrono::seconds delay = {}) {
        std::vector<std::string> jsons;
        jsons.reserve(bodies.size());
        if constexpr (std::is_same_v<T, std::string>) {
            jsons = bodies;
        } else {
            for (const auto& b : bodies) {
                jsons.push_back(rfl::json::write(b));
            }
        }
        return do_send_batch(std::move(ctx), queue_name, jsons, delay);
    }

    // -----------------------------------------------------------------------
    // Reading messages
    // -----------------------------------------------------------------------

    /**
     * @brief Reads up to @p qty messages and sets their visibility timeout.
     *
     * Messages are hidden from other readers for @p vt seconds. The caller
     * must erase or archive them before the VT expires to prevent redelivery.
     *
     * @param vt      Visibility timeout duration.
     * @param qty     Maximum number of messages to return (default 1).
     */
    template<typename T>
    std::vector<message<T>> read(ores::database::context ctx,
                                  const std::string& queue_name,
                                  std::chrono::seconds vt, int qty = 1) {
        return to_messages<T>(do_read(std::move(ctx), queue_name, vt, qty));
    }

    /**
     * @brief Long-polls for messages, blocking up to @p max_poll.
     *
     * Polls the queue at @p poll_interval until at least one message arrives
     * or @p max_poll elapses. Implemented via pgmq.read_with_poll() which
     * runs the poll loop server-side.
     *
     * @param max_poll      Maximum time to wait for messages.
     * @param poll_interval Server-side polling interval.
     */
    template<typename T>
    std::vector<message<T>> read_with_poll(ores::database::context ctx,
                                            const std::string& queue_name,
                                            std::chrono::seconds vt, int qty = 1,
                                            std::chrono::seconds max_poll = std::chrono::seconds(5),
                                            std::chrono::milliseconds poll_interval = std::chrono::milliseconds(250)) {
        return to_messages<T>(do_read_with_poll(
            std::move(ctx), queue_name, vt, qty, max_poll, poll_interval));
    }

    /**
     * @brief Reads and atomically deletes a single message (pop semantics).
     *
     * @return The message, or nullopt if the queue is empty.
     */
    template<typename T>
    std::optional<message<T>> pop(ores::database::context ctx,
                                   const std::string& queue_name) {
        auto row = do_pop(std::move(ctx), queue_name);
        if (!row) return std::nullopt;
        return to_message<T>(*row);
    }

    // -----------------------------------------------------------------------
    // Deleting / archiving
    // -----------------------------------------------------------------------

    /**
     * @brief Permanently deletes a single message.
     *
     * @return true if the message was deleted, false if it was not found.
     */
    bool erase(ores::database::context ctx, const std::string& queue_name,
               int64_t msg_id);

    /**
     * @brief Permanently deletes multiple messages.
     *
     * @return The IDs that were actually deleted (may be a subset if some
     *         messages had already been deleted by other consumers).
     */
    std::vector<int64_t> erase(ores::database::context ctx,
                                const std::string& queue_name,
                                const std::vector<int64_t>& msg_ids);

    /**
     * @brief Moves a single message to the archive table.
     *
     * @return true if the message was archived, false if it was not found.
     */
    bool archive(ores::database::context ctx, const std::string& queue_name,
                 int64_t msg_id);

    /**
     * @brief Moves multiple messages to the archive table.
     *
     * @return The IDs that were actually archived.
     */
    std::vector<int64_t> archive(ores::database::context ctx,
                                  const std::string& queue_name,
                                  const std::vector<int64_t>& msg_ids);

    // -----------------------------------------------------------------------
    // Visibility timeout
    // -----------------------------------------------------------------------

    /**
     * @brief Changes the visibility timeout of a message already in-flight.
     *
     * Use this to extend or shorten the window before a message becomes
     * visible again to other readers.
     *
     * @param vt New visibility timeout offset from now (in seconds).
     * @return The updated message, or nullopt if the message was not found.
     */
    template<typename T>
    std::optional<message<T>> set_visibility_timeout(ores::database::context ctx,
                                                       const std::string& queue_name,
                                                       int64_t msg_id,
                                                       std::chrono::seconds vt) {
        auto row = do_set_vt(std::move(ctx), queue_name, msg_id, vt);
        if (!row) return std::nullopt;
        return to_message<T>(*row);
    }

    // -----------------------------------------------------------------------
    // Metrics
    // -----------------------------------------------------------------------

    /**
     * @brief Returns statistics for a single queue.
     */
    queue_metrics metrics(ores::database::context ctx, const std::string& queue_name);

    /**
     * @brief Returns statistics for all queues.
     */
    std::vector<queue_metrics> metrics_all(ores::database::context ctx);

    // -----------------------------------------------------------------------
    // NOTIFY / LISTEN integration
    // -----------------------------------------------------------------------

    /**
     * @brief Enables pg_notify on INSERT for a queue.
     *
     * After this call, every pgmq.send() to the queue triggers a PostgreSQL
     * NOTIFY on channel @c pgmq.q_<queue_name>.INSERT. Use with
     * ores::database::service::postgres_listener_service to receive events.
     *
     * @param throttle Minimum interval between notifications (0 = every insert).
     */
    void enable_notify(ores::database::context ctx, const std::string& queue_name,
                       std::chrono::milliseconds throttle = {});

    /**
     * @brief Disables pg_notify on INSERT for a queue.
     */
    void disable_notify(ores::database::context ctx, const std::string& queue_name);
};

}

#endif
