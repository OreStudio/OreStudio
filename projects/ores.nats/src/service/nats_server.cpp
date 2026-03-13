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
#include "ores.nats/service/nats_server.hpp"

#include <boost/asio/co_spawn.hpp>
#include <boost/asio/detached.hpp>
#include <boost/asio/post.hpp>
#include <boost/asio/signal_set.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/throw_exception.hpp>
#include "ores.comms/messaging/frame.hpp"

namespace ores::nats::service {

using namespace ores::logging;

nats_server::nats_server(config::nats_options options)
    : options_(std::move(options))
    , sessions_(std::make_shared<comms::service::auth_session_service>())
    , dispatcher_(std::make_shared<comms::messaging::message_dispatcher>(sessions_)) {
}

nats_server::~nats_server() {
    if (sub_) {
        natsSubscription_Unsubscribe(sub_);
        natsSubscription_Destroy(sub_);
        sub_ = nullptr;
    }
    if (conn_) {
        natsConnection_Close(conn_);
        natsConnection_Destroy(conn_);
        conn_ = nullptr;
    }
}

void nats_server::register_handler(comms::messaging::message_type_range range,
    std::shared_ptr<comms::messaging::message_handler> handler) {
    dispatcher_->register_handler(range, std::move(handler));
}

void nats_server::broadcast_database_status(bool available,
    const std::string& error_message) {
    // No persistent client connections in NATS mode; DB status changes are
    // published as NATS pub/sub events in a future phase.
    if (!available) {
        BOOST_LOG_SEV(lg(), warn) << "Database unavailable: " << error_message
                                  << " (NATS clients will receive errors on next request)";
    }
}

void nats_server::on_message(natsConnection*, natsSubscription*,
    natsMsg* msg, void* closure) {
    auto* self = static_cast<nats_server*>(closure);

    // Copy body and reply subject before destroying the cnats message
    const int data_len = natsMsg_GetDataLength(msg);
    std::vector<std::byte> body;
    if (data_len > 0) {
        const auto* data_ptr =
            reinterpret_cast<const std::byte*>(natsMsg_GetData(msg));
        body.assign(data_ptr, data_ptr + data_len);
    }
    const char* reply_ptr = natsMsg_GetReply(msg);
    std::string reply_subject(reply_ptr ? reply_ptr : "");
    natsMsg_Destroy(msg);

    if (body.empty() || reply_subject.empty()) {
        BOOST_LOG_SEV(lg(), debug) << "Received message without body or reply subject, ignoring";
        return;
    }

    // Marshal onto the ASIO executor so handlers run single-threaded
    boost::asio::post(*self->io_ctx_,
        [self, body = std::move(body), reply = std::move(reply_subject)]() mutable {
            boost::asio::co_spawn(*self->io_ctx_,
                self->handle_message(std::move(body), std::move(reply)),
                boost::asio::detached);
        });
}

boost::asio::awaitable<void> nats_server::handle_message(
    std::vector<std::byte> body, std::string reply_subject) {

    using namespace comms::messaging;

    // Deserialize the ORES frame header
    auto header_result = frame::deserialize_header(body);
    if (!header_result) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to deserialize frame header from NATS message";
        co_return;
    }

    // Deserialize the complete frame
    auto frame_result = frame::deserialize(*header_result, body);
    if (!frame_result) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to deserialize ORES frame from NATS message";
        co_return;
    }

    // Dispatch to the registered handler
    const auto seq = sequence_.fetch_add(1, std::memory_order_relaxed);
    auto response = co_await dispatcher_->dispatch(
        *frame_result, seq, reply_subject);

    if (!response) {
        BOOST_LOG_SEV(lg(), warn) << "Dispatch failed for message type "
            << static_cast<int>(frame_result->header().type);
        co_return;
    }

    // Serialize and publish the response to the NATS reply subject
    const auto response_bytes = response->serialize();
    const natsStatus s = natsConnection_Publish(conn_,
        reply_subject.c_str(),
        response_bytes.data(),
        static_cast<int>(response_bytes.size()));

    if (s != NATS_OK) {
        BOOST_LOG_SEV(lg(), error) << "Failed to publish NATS response: "
                                   << natsStatus_GetText(s);
    }
}

boost::asio::awaitable<void> nats_server::watch_for_stop_signals(
    boost::asio::io_context& io_ctx) {
    boost::asio::signal_set signals(io_ctx, SIGINT, SIGTERM);
    co_await signals.async_wait(boost::asio::use_awaitable);
    BOOST_LOG_SEV(lg(), info) << "Stop signal received";
    stop();
}

boost::asio::awaitable<void> nats_server::run(boost::asio::io_context& io_ctx) {
    io_ctx_ = &io_ctx;

    BOOST_LOG_SEV(lg(), info) << "Connecting to NATS at " << options_.url;

    natsStatus s = natsConnection_ConnectTo(&conn_, options_.url.c_str());
    if (s != NATS_OK) {
        BOOST_LOG_SEV(lg(), error) << "NATS connection failed: " << natsStatus_GetText(s);
        BOOST_THROW_EXCEPTION(std::runtime_error(
            std::string("NATS connection failed: ") + natsStatus_GetText(s)));
    }
    BOOST_LOG_SEV(lg(), info) << "Connected to NATS";

    s = natsConnection_Subscribe(&sub_, conn_,
        options_.subject.c_str(), on_message, this);
    if (s != NATS_OK) {
        BOOST_LOG_SEV(lg(), error) << "NATS subscribe failed: " << natsStatus_GetText(s);
        natsConnection_Destroy(conn_);
        conn_ = nullptr;
        BOOST_THROW_EXCEPTION(std::runtime_error(
            std::string("NATS subscribe failed: ") + natsStatus_GetText(s)));
    }
    BOOST_LOG_SEV(lg(), info) << "Listening on NATS subject: " << options_.subject;

    boost::asio::co_spawn(io_ctx,
        watch_for_stop_signals(io_ctx),
        boost::asio::detached);

    // Wait until stop() emits the cancellation signal
    boost::asio::steady_timer forever(co_await boost::asio::this_coro::executor);
    forever.expires_at(std::chrono::steady_clock::time_point::max());
    stop_signal_.slot().assign([&forever](boost::asio::cancellation_type) {
        forever.cancel();
    });
    try {
        co_await forever.async_wait(boost::asio::use_awaitable);
    } catch (const boost::system::system_error&) {
        // Cancelled — stop requested
    }

    BOOST_LOG_SEV(lg(), info) << "Stopping NATS server";

    if (sub_) {
        natsSubscription_Unsubscribe(sub_);
        natsSubscription_Destroy(sub_);
        sub_ = nullptr;
    }
    if (conn_) {
        natsConnection_Close(conn_);
        natsConnection_Destroy(conn_);
        conn_ = nullptr;
    }
}

void nats_server::stop() {
    stop_signal_.emit(boost::asio::cancellation_type::all);
}

}
