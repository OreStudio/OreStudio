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
#include "ores.telemetry.service/app/nats_poller.hpp"

#include <chrono>
#include <optional>
#include <stdexcept>
#include <boost/asio/io_context.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/asio/this_coro.hpp>
#include <boost/asio/use_awaitable.hpp>
#include <boost/beast/core/flat_buffer.hpp>
#include <boost/beast/core/tcp_stream.hpp>
#include <boost/beast/http/empty_body.hpp>
#include <boost/beast/http/field.hpp>
#include <boost/beast/http/read.hpp>
#include <boost/beast/http/string_body.hpp>
#include <boost/beast/http/verb.hpp>
#include <boost/beast/http/write.hpp>
#include <boost/system/system_error.hpp>
#include <rfl/json.hpp>
#include "ores.utility/rfl/reflectors.hpp"
#include "ores.utility/uuid/tenant_id.hpp"

namespace ores::telemetry::service::app {

using namespace ores::logging;

// ============================================================================
// Internal types for parsing NATS monitoring JSON responses
// ============================================================================

namespace {

/**
 * @brief Maps the relevant fields of the NATS /varz response.
 *
 * Field names match the NATS monitoring HTTP API.
 * Note: NATS reports memory as "mem" (bytes).
 */
struct varz_response {
    std::uint64_t in_msgs{0};
    std::uint64_t out_msgs{0};
    std::uint64_t in_bytes{0};
    std::uint64_t out_bytes{0};
    int connections{0};
    std::uint64_t mem{0};
    int slow_consumers{0};
};

struct jsz_stream_state {
    std::uint64_t messages{0};
    std::uint64_t bytes{0};
    int consumer_count{0};
};

struct jsz_stream_config {
    std::string name;
};

struct jsz_stream_info {
    jsz_stream_config config;
    jsz_stream_state state;
};

struct jsz_response {
    std::optional<std::vector<jsz_stream_info>> streams;
};

} // namespace

// ============================================================================
// Construction and URL parsing
// ============================================================================

nats_poller::nats_poller(const std::string& monitor_url,
                         std::uint32_t interval_seconds,
                         ores::database::context ctx)
    : interval_seconds_(interval_seconds)
    , ctx_(ctx.with_tenant(ores::utility::uuid::tenant_id::system(), "nats_poller")) {

    // Parse "http://host:port" or "http://host" → host + port
    auto pos = monitor_url.find("://");
    const auto authority = (pos != std::string::npos)
        ? monitor_url.substr(pos + 3)
        : monitor_url;

    const auto colon = authority.rfind(':');
    if (colon != std::string::npos) {
        monitor_host_ = authority.substr(0, colon);
        const auto port_str = authority.substr(colon + 1);
        monitor_port_ = static_cast<unsigned short>(std::stoi(port_str));
    } else {
        monitor_host_ = authority;
        monitor_port_ = 8222; // NATS default monitoring port
    }
}

// ============================================================================
// HTTP helper
// ============================================================================

std::string nats_poller::http_get(const std::string& target) const {
    namespace asio = boost::asio;
    namespace beast = boost::beast;
    namespace http = beast::http;

    asio::io_context io;
    asio::ip::tcp::resolver resolver(io);
    beast::tcp_stream stream(io);

    stream.expires_after(std::chrono::seconds(5));

    const auto results = resolver.resolve(
        monitor_host_, std::to_string(monitor_port_));
    stream.connect(results);

    http::request<http::empty_body> req{http::verb::get, target, 11};
    req.set(http::field::host,
        monitor_host_ + ":" + std::to_string(monitor_port_));
    req.set(http::field::connection, "close");
    http::write(stream, req);

    beast::flat_buffer buf;
    http::response<http::string_body> res;
    http::read(stream, buf, res);

    boost::beast::error_code ec;
    stream.socket().shutdown(asio::ip::tcp::socket::shutdown_both, ec);

    return res.body();
}

// ============================================================================
// Polling logic
// ============================================================================

void nats_poller::poll_server() {
    const auto body = http_get("/varz");
    const auto parsed = rfl::json::read<varz_response>(body);
    if (!parsed) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to parse /varz response";
        return;
    }

    domain::nats_server_sample sample;
    sample.sampled_at = std::chrono::system_clock::now();
    sample.in_msgs = parsed->in_msgs;
    sample.out_msgs = parsed->out_msgs;
    sample.in_bytes = parsed->in_bytes;
    sample.out_bytes = parsed->out_bytes;
    sample.connections = parsed->connections;
    sample.mem_bytes = parsed->mem;
    sample.slow_consumers = parsed->slow_consumers;

    repo_.insert_server_sample(ctx_, sample);
    BOOST_LOG_SEV(lg(), trace) << "Stored NATS server sample: "
                               << sample.connections << " connection(s), "
                               << sample.in_msgs << " in_msgs";
}

void nats_poller::poll_streams() {
    const auto body = http_get("/jsz?streams=true");
    const auto parsed = rfl::json::read<jsz_response>(body);
    if (!parsed) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to parse /jsz response";
        return;
    }

    if (!parsed->streams || parsed->streams->empty()) {
        BOOST_LOG_SEV(lg(), trace) << "No JetStream streams found";
        return;
    }

    const auto now = std::chrono::system_clock::now();
    std::vector<domain::nats_stream_sample> samples;
    samples.reserve(parsed->streams->size());

    for (const auto& stream : *parsed->streams) {
        domain::nats_stream_sample s;
        s.sampled_at = now;
        s.stream_name = stream.config.name;
        s.messages = stream.state.messages;
        s.bytes = stream.state.bytes;
        s.consumer_count = stream.state.consumer_count;
        samples.push_back(std::move(s));
    }

    repo_.insert_stream_samples(ctx_, samples);
    BOOST_LOG_SEV(lg(), trace) << "Stored " << samples.size()
                               << " NATS stream sample(s)";
}

void nats_poller::poll_once() {
    try {
        poll_server();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "NATS server poll failed: " << e.what();
    }

    try {
        poll_streams();
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "NATS stream poll failed: " << e.what();
    }
}

// ============================================================================
// Coroutine entry point
// ============================================================================

boost::asio::awaitable<void> nats_poller::run() {
    BOOST_LOG_SEV(lg(), info) << "NATS poller started. Monitoring "
                              << monitor_host_ << ":" << monitor_port_
                              << " every " << interval_seconds_ << "s";

    auto executor = co_await boost::asio::this_coro::executor;
    boost::asio::steady_timer timer(executor);

    try {
        for (;;) {
            poll_once();

            timer.expires_after(std::chrono::seconds(interval_seconds_));
            co_await timer.async_wait(boost::asio::use_awaitable);
        }
    } catch (const boost::system::system_error& e) {
        if (e.code() != boost::asio::error::operation_aborted) {
            BOOST_LOG_SEV(lg(), warn) << "NATS poller timer error: " << e.what();
        }
    }

    BOOST_LOG_SEV(lg(), info) << "NATS poller stopped.";
}

}
