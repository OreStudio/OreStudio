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
#include "ores.mq/broker/nng_broker.hpp"

#include <thread>
#include <cstring>
#include <stdexcept>
#include <nng/protocol/reqrep0/rep.h>
#include <boost/uuid/uuid_io.hpp>
#include "ores.mq/messaging/broker_protocol.hpp"

namespace ores::mq::broker {

using namespace ores::logging;

// Offset of the 2-byte message type field in an ores.comms binary frame.
// Frame layout: [magic u32][version_major u16][version_minor u16][type u16]...
// magic=4, version_major=2, version_minor=2 → type starts at byte 8.
static constexpr std::size_t MSG_TYPE_OFFSET = 8;

// ============================================================================
// Helpers
// ============================================================================

namespace {

/// Extract the 2-byte message type from raw frame bytes (big-endian).
std::optional<std::uint16_t> parse_msg_type(const std::byte* data, std::size_t size) {
    if (size < MSG_TYPE_OFFSET + 2)
        return std::nullopt;
    std::uint16_t t = 0;
    std::memcpy(&t, data + MSG_TYPE_OFFSET, 2);
    // Protocol uses big-endian (network byte order)
    return static_cast<std::uint16_t>((t >> 8) | (t << 8)); // bswap16
}

void nng_check(int rv, const char* op) {
    if (rv != 0) {
        throw std::runtime_error(
            std::string(op) + " failed: " + nng_strerror(rv));
    }
}

} // namespace

// ============================================================================
// nng_broker
// ============================================================================

nng_broker::nng_broker(broker_config cfg)
    : cfg_(std::move(cfg)) {}

nng_broker::~nng_broker() {
    stop();
    nng_close(frontend_sock_);
    nng_close(backend_sock_);
}

void nng_broker::on_pipe_close(nng_pipe pipe, nng_pipe_ev /*ev*/, void* arg) {
    auto* self = static_cast<nng_broker*>(arg);
    self->routing_table_.deregister_pipe(pipe);
    self->service_registry_.deregister_pipe(pipe);
}

std::vector<std::byte> nng_broker::handle_register_service(
    std::span<const std::byte> payload, nng_pipe pipe_id) {

    auto req = messaging::register_service_request::deserialize(payload);
    if (!req) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to deserialize register_service_request";
        messaging::register_service_response resp{
            .success = false,
            .assigned_id = "",
            .error_message = "Deserialisation failed"
        };
        return resp.serialize();
    }

    const auto& r = *req;
    const auto assigned_id = service_registry_.register_service(
        r.service_name, r.handled_ranges, pipe_id);

    for (const auto& [min, max] : r.handled_ranges)
        routing_table_.register_route(min, max, pipe_id);

    messaging::register_service_response resp{
        .success = true,
        .assigned_id = boost::uuids::to_string(assigned_id),
        .error_message = ""
    };
    return resp.serialize();
}

void nng_broker::run() {
    // Open raw REP sockets — raw mode preserves pipe IDs for routing.
    nng_check(nng_rep0_open_raw(&frontend_sock_), "frontend nng_rep0_open_raw");
    nng_check(nng_rep0_open_raw(&backend_sock_),  "backend nng_rep0_open_raw");

    // Register pipe-close callback so we clean up routes when services disconnect.
    nng_check(nng_pipe_notify(backend_sock_, NNG_PIPE_EV_REM_POST,
        &nng_broker::on_pipe_close, this), "backend nng_pipe_notify");

    nng_check(nng_listen(frontend_sock_, cfg_.frontend_endpoint.c_str(), nullptr, 0),
        "nng_listen frontend");
    nng_check(nng_listen(backend_sock_, cfg_.backend_endpoint.c_str(), nullptr, 0),
        "nng_listen backend");

    BOOST_LOG_SEV(lg(), info) << "Broker listening: frontend="
                              << cfg_.frontend_endpoint
                              << " backend=" << cfg_.backend_endpoint;

    running_ = true;

    // Dispatch threads:
    //   backend_thread  — handles service registrations on the backend socket.
    //   Main loop       — handles client requests on the frontend socket.
    std::thread backend_thread([this] {
        while (running_) {
            nng_msg* msg = nullptr;
            int rv = nng_recvmsg(backend_sock_, &msg, 0);
            if (rv == NNG_ECLOSED || rv == NNG_ECANCELED) break;
            if (rv != 0) {
                BOOST_LOG_SEV(lg(), warn) << "Backend recv error: " << nng_strerror(rv);
                continue;
            }

            const nng_pipe pipe_id = nng_msg_get_pipe(msg);
            const auto* data = static_cast<const std::byte*>(nng_msg_body(msg));
            const auto size  = nng_msg_len(msg);

            // The backend socket carries broker control messages (register_service_request,
            // etc.) serialised directly by broker_protocol — no ores.comms frame wrapper.
            // The entire NNG message body IS the serialised payload.
            std::span<const std::byte> payload_span(data, size);
            auto resp_bytes = handle_register_service(payload_span, pipe_id);

            nng_msg_clear(msg);
            nng_msg_append(msg, resp_bytes.data(), resp_bytes.size());
            nng_msg_set_pipe(msg, pipe_id);
            int send_rv = nng_sendmsg(backend_sock_, msg, 0);
            if (send_rv != 0) {
                BOOST_LOG_SEV(lg(), warn) << "Backend send error: "
                                         << nng_strerror(send_rv);
                nng_msg_free(msg);
            }
        }
    });

    // Frontend dispatch loop — forward client requests to registered services.
    while (running_) {
        nng_msg* msg = nullptr;
        int rv = nng_recvmsg(frontend_sock_, &msg, 0);
        if (rv == NNG_ECLOSED || rv == NNG_ECANCELED) break;
        if (rv != 0) {
            BOOST_LOG_SEV(lg(), warn) << "Frontend recv error: " << nng_strerror(rv);
            continue;
        }

        const nng_pipe client_pipe = nng_msg_get_pipe(msg);
        const auto* data = static_cast<const std::byte*>(nng_msg_body(msg));
        const auto size  = nng_msg_len(msg);
        const auto mt    = parse_msg_type(data, size);

        if (!mt) {
            BOOST_LOG_SEV(lg(), warn) << "Frame too short to parse message type";
            nng_msg_free(msg);
            continue;
        }

        const auto msg_type_val = *mt;

        // Look up the destination service pipe
        const auto service_pipe = routing_table_.find_pipe(msg_type_val);
        if (!service_pipe) {
            BOOST_LOG_SEV(lg(), warn) << "No service registered for message type 0x"
                                     << std::hex << msg_type_val;
            nng_msg_free(msg);
            continue;
        }

        // Forward the raw frame to the service via the backend socket
        nng_msg_set_pipe(msg, *service_pipe);
        rv = nng_sendmsg(backend_sock_, msg, 0);
        if (rv != 0) {
            BOOST_LOG_SEV(lg(), warn) << "Forward to service failed: " << nng_strerror(rv);
            continue; // msg ownership transferred even on failure in some NNG versions
        }

        // Receive service response
        nng_msg* response = nullptr;
        rv = nng_recvmsg(backend_sock_, &response, 0);
        if (rv != 0) {
            BOOST_LOG_SEV(lg(), warn) << "Service response recv failed: " << nng_strerror(rv);
            continue;
        }

        // Forward response back to the original client
        nng_msg_set_pipe(response, client_pipe);
        rv = nng_sendmsg(frontend_sock_, response, 0);
        if (rv != 0) {
            BOOST_LOG_SEV(lg(), warn) << "Client response send failed: " << nng_strerror(rv);
            nng_msg_free(response);
        }
    }

    // Signal backend thread to stop and wait
    nng_close(backend_sock_);
    if (backend_thread.joinable())
        backend_thread.join();
}

void nng_broker::stop() {
    if (running_.exchange(false)) {
        BOOST_LOG_SEV(lg(), info) << "Broker stopping";
        nng_close(frontend_sock_);
    }
}

}
