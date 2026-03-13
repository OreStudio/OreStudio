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
#ifndef ORES_NATS_SERVICE_NATS_SERVER_HPP
#define ORES_NATS_SERVICE_NATS_SERVER_HPP

#include <atomic>
#include <memory>
#include <span>
#include <string>
#include <vector>
#include <cstddef>
#include <cstdint>
#include <string_view>
#include <boost/asio/awaitable.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/asio/cancellation_signal.hpp>
#include <nats/nats.h>
#include "ores.logging/make_logger.hpp"
#include "ores.comms/messaging/message_server.hpp"
#include "ores.comms/messaging/message_dispatcher.hpp"
#include "ores.nats/config/nats_options.hpp"

namespace ores::nats::service {

/**
 * @brief NATS-backed message server.
 *
 * Implements the message_server interface using NATS as the transport layer.
 * Subscribes to a single NATS subject, deserialises incoming ORES binary
 * frames, dispatches them to the registered message handlers, and publishes
 * response frames to the NATS reply subject supplied by the caller.
 *
 * The existing ORES binary frame format (magic, type, JWT, payload) is used
 * as the NATS message body without modification. Authentication via JWT
 * embedded in the frame header is fully supported.
 *
 * Thread-safety: register_handler() must be called before run() on the same
 * thread. All NATS callbacks are marshalled onto the ASIO io_context before
 * coroutine dispatch, so handlers run single-threaded on the ASIO executor.
 */
class nats_server final : public comms::messaging::message_server {
private:
    inline static std::string_view logger_name = "ores.nats.service.nats_server";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Construct a NATS server.
     *
     * @param options NATS connection and subject configuration.
     */
    explicit nats_server(config::nats_options options);

    ~nats_server() override;

    // message_server interface
    void register_handler(comms::messaging::message_type_range range,
        std::shared_ptr<comms::messaging::message_handler> handler) override;

    [[nodiscard]] std::shared_ptr<comms::service::auth_session_service>
    sessions() const override { return sessions_; }

    void broadcast_database_status(bool available,
        const std::string& error_message) override;

    /**
     * @brief Publish raw bytes to a NATS subject.
     *
     * Used by the NATS session factory injected into subscription_handler to
     * deliver serialized notification_message payloads to client inboxes.
     * Safe to call from any thread.
     *
     * @param subject NATS subject (typically a client inbox)
     * @param data Serialized notification_message bytes
     */
    void publish_to_subject(const std::string& subject,
        std::span<const std::byte> data);

    /**
     * @brief Run the server.
     *
     * Connects to NATS, subscribes to the configured subject, and waits
     * until a stop signal is received (SIGINT, SIGTERM, or stop()).
     *
     * @param io_ctx ASIO io_context on which handler coroutines are dispatched.
     */
    boost::asio::awaitable<void> run(boost::asio::io_context& io_ctx);

    /**
     * @brief Stop the server.
     *
     * Cancels the run() coroutine. Safe to call from any thread.
     */
    void stop();

private:
    /**
     * @brief NATS message callback (called on cnats internal thread).
     *
     * Copies the message body and reply subject, destroys the cnats msg
     * immediately, then posts handle_message() to the ASIO executor.
     */
    static void on_message(natsConnection* nc, natsSubscription* sub,
        natsMsg* msg, void* closure);

    /**
     * @brief Dispatch a single received message (runs on ASIO executor).
     */
    boost::asio::awaitable<void> handle_message(
        std::vector<std::byte> body, std::string reply_subject);

    /**
     * @brief Watch for SIGINT/SIGTERM and call stop().
     */
    boost::asio::awaitable<void> watch_for_stop_signals(
        boost::asio::io_context& io_ctx);

    config::nats_options options_;
    boost::asio::io_context* io_ctx_ = nullptr;
    natsConnection* conn_ = nullptr;
    natsSubscription* sub_ = nullptr;
    std::shared_ptr<comms::service::auth_session_service> sessions_;
    std::shared_ptr<comms::messaging::message_dispatcher> dispatcher_;
    std::atomic<std::uint32_t> sequence_{0};
    boost::asio::cancellation_signal stop_signal_;
};

}

#endif
