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
#ifndef ORES_MQ_SERVICE_NNG_SERVICE_RUNNER_HPP
#define ORES_MQ_SERVICE_NNG_SERVICE_RUNNER_HPP

#include <span>
#include <string>
#include <vector>
#include <atomic>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <nng/nng.h>
#include "ores.comms/messaging/message_handler.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::mq::service {

/**
 * @brief Reusable NNG service runner for microservice integration.
 *
 * Handles broker registration and the persistent recv/dispatch loop.
 * Any microservice can instantiate its own runner with its own message-type
 * ranges and dispatcher function.
 *
 * Threading model: run() blocks the calling thread. Call stop() from another
 * thread (or on shutdown) to unblock run() and exit the loop cleanly.
 */
class nng_service_runner {
private:
    inline static std::string_view logger_name =
        "ores.mq.service.nng_service_runner";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    /**
     * @brief Synchronous dispatcher: raw bytes in, raw bytes out.
     *
     * The nng_pipe does not leak into callers — only the uint32_t pipe ID
     * is passed through to allow per-client identity lookup.
     */
    using dispatcher_fn = std::function<
        std::vector<std::byte>(std::span<const std::byte>, std::uint32_t)>;

    struct config {
        /// NNG URL of the broker backend socket (e.g. "ipc:///tmp/broker-backend")
        std::string broker_backend;
        /// Human-readable service name sent in register_service_request
        std::string service_name;
        /// Message-type ranges handled by this service
        std::vector<comms::messaging::message_type_range> ranges;
    };

    nng_service_runner(config cfg, dispatcher_fn dispatcher);
    ~nng_service_runner();

    nng_service_runner(const nng_service_runner&) = delete;
    nng_service_runner& operator=(const nng_service_runner&) = delete;

    /**
     * @brief Register with the broker then enter the recv/dispatch loop.
     *
     * Blocks the calling thread. Returns when stop() is called or the
     * socket is closed.
     */
    void run();

    /**
     * @brief Stop the runner.
     *
     * Thread-safe. Closes the NNG socket, which unblocks nng_recvmsg in
     * the run() loop so the thread can exit cleanly.
     */
    void stop();

private:
    /**
     * @brief Send register_service_request and await register_service_response.
     *
     * @return true on success, false on any failure (already logged).
     */
    bool register_with_broker();

    config cfg_;
    dispatcher_fn dispatcher_;
    std::atomic<bool> running_{false};
    nng_socket sock_ = NNG_SOCKET_INITIALIZER;
};

}

#endif
