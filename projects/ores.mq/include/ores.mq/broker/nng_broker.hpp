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
#ifndef ORES_MQ_BROKER_NNG_BROKER_HPP
#define ORES_MQ_BROKER_NNG_BROKER_HPP

#include <atomic>
#include <memory>
#include <nng/nng.h>
#include "ores.logging/make_logger.hpp"
#include "ores.mq/broker/broker_config.hpp"
#include "ores.mq/broker/routing_table.hpp"
#include "ores.mq/broker/service_registry.hpp"

namespace ores::mq::broker {

/**
 * @brief NNG-based message broker for routing binary protocol frames.
 *
 * Architecture:
 * - Frontend: raw-mode REP socket that accepts client connections
 * - Backend: raw-mode REQ socket that services connect to
 *
 * Dispatch loop:
 * 1. Receive a frame from a client on the frontend socket
 * 2. Read the 2-byte message type from the frame header (bytes 2–3)
 * 3. If the type is register_service_request (0xC000): handle internally
 * 4. Look up the destination service pipe in the routing table
 * 5. Forward the raw frame to the service via the backend socket
 * 6. Receive the response from the service and send it back to the client
 *
 * Thread safety: run() blocks the calling thread. Call stop() from another
 * thread or signal handler to shut down cleanly.
 */
class nng_broker final {
public:
    explicit nng_broker(broker_config cfg);
    ~nng_broker();

    nng_broker(const nng_broker&) = delete;
    nng_broker& operator=(const nng_broker&) = delete;

    /**
     * @brief Start listening on configured endpoints and run dispatch loop.
     *
     * Blocks until stop() is called or a fatal error occurs.
     */
    void run();

    /**
     * @brief Signal the dispatch loop to stop.
     *
     * Safe to call from any thread or signal handler.
     */
    void stop();

private:
    /**
     * @brief Handle an internally-processed register_service_request.
     *
     * Registers the service's ranges in the routing table and returns
     * a serialised register_service_response payload.
     */
    std::vector<std::byte> handle_register_service(
        std::span<const std::byte> payload, nng_pipe pipe_id);

    /**
     * @brief Static NNG pipe-close callback — deregisters the pipe.
     */
    static void on_pipe_close(nng_pipe pipe, nng_pipe_ev ev, void* arg);

    inline static std::string_view logger_name = "ores.mq.broker.nng_broker";
    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    broker_config cfg_;
    routing_table routing_table_;
    service_registry service_registry_;

    nng_socket frontend_sock_ = NNG_SOCKET_INITIALIZER;
    nng_socket backend_sock_  = NNG_SOCKET_INITIALIZER;
    std::atomic<bool> running_{false};
};

}

#endif
