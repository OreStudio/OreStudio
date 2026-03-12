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
#include "ores.mq/service/nng_service_runner.hpp"

#include <nng/protocol/reqrep0/req.h>
#include "ores.mq/messaging/broker_protocol.hpp"

namespace ores::mq::service {

using namespace ores::logging;

nng_service_runner::nng_service_runner(config cfg, dispatcher_fn dispatcher)
    : cfg_(std::move(cfg)), dispatcher_(std::move(dispatcher)) {}

nng_service_runner::~nng_service_runner() {
    stop();
}

bool nng_service_runner::register_with_broker() {
    messaging::register_service_request req;
    req.service_name = cfg_.service_name;
    for (const auto& r : cfg_.ranges)
        req.handled_ranges.emplace_back(r.min, r.max);

    const auto payload = req.serialize();
    nng_msg* msg = nullptr;
    nng_msg_alloc(&msg, 0);
    nng_msg_append(msg, payload.data(), payload.size());

    int rv = nng_sendmsg(sock_, msg, 0);
    if (rv != 0) {
        BOOST_LOG_SEV(lg(), error) << "Failed to send register_service_request: "
                                   << nng_strerror(rv);
        return false;
    }

    nng_msg* resp_msg = nullptr;
    rv = nng_recvmsg(sock_, &resp_msg, 0);
    if (rv != 0) {
        BOOST_LOG_SEV(lg(), error) << "Failed to receive register_service_response: "
                                   << nng_strerror(rv);
        return false;
    }

    const auto* data = static_cast<const std::byte*>(nng_msg_body(resp_msg));
    const auto size = nng_msg_len(resp_msg);
    const auto resp = messaging::register_service_response::deserialize({data, size});
    nng_msg_free(resp_msg);

    if (!resp || !resp->success) {
        const auto err = resp ? resp->error_message : "deserialisation failed";
        BOOST_LOG_SEV(lg(), error) << "Broker registration failed: " << err;
        return false;
    }

    BOOST_LOG_SEV(lg(), info) << "NNG service runner registered, entering dispatch loop"
                              << " assigned_id=" << resp->assigned_id;
    return true;
}

void nng_service_runner::run() {
    int rv = nng_req0_open_raw(&sock_);
    if (rv != 0) {
        BOOST_LOG_SEV(lg(), error) << "nng_req0_open_raw failed: " << nng_strerror(rv);
        return;
    }

    rv = nng_dial(sock_, cfg_.broker_backend.c_str(), nullptr, 0);
    if (rv != 0) {
        BOOST_LOG_SEV(lg(), error) << "nng_dial to broker backend failed: "
                                   << nng_strerror(rv);
        nng_close(sock_);
        return;
    }

    if (!register_with_broker())
        return;

    running_ = true;

    while (running_) {
        nng_msg* msg = nullptr;
        rv = nng_recvmsg(sock_, &msg, 0);
        if (rv == NNG_ECLOSED || rv == NNG_ECANCELED) break;
        if (rv != 0) {
            BOOST_LOG_SEV(lg(), warn) << "recvmsg error: " << nng_strerror(rv);
            continue;
        }

        const auto client_pipe_id = nng_msg_get_pipe(msg).id;
        const auto* body = static_cast<const std::byte*>(nng_msg_body(msg));
        const auto size = nng_msg_len(msg);

        auto resp_bytes = dispatcher_({body, size}, client_pipe_id);

        nng_msg_clear(msg);
        nng_msg_append(msg, resp_bytes.data(), resp_bytes.size());
        int send_rv = nng_sendmsg(sock_, msg, 0);
        if (send_rv != 0) {
            BOOST_LOG_SEV(lg(), warn) << "sendmsg error: " << nng_strerror(send_rv);
            nng_msg_free(msg);
        }
    }

    BOOST_LOG_SEV(lg(), info) << "NNG service runner dispatch loop exited";
}

void nng_service_runner::stop() {
    running_ = false;
    nng_close(sock_);
}

}
