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
#include "ores.http.client/net/transport.hpp"
#include <boost/asio/connect.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/ssl/context.hpp>
#include <boost/asio/ssl/error.hpp>
#include <boost/asio/ssl/host_name_verification.hpp>
#include <boost/asio/ssl/stream.hpp>
#include <boost/asio/steady_timer.hpp>
#include <boost/beast/core/flat_buffer.hpp>
#include <boost/beast/core/tcp_stream.hpp>
#include <boost/beast/http/empty_body.hpp>
#include <boost/beast/http/error.hpp>
#include <boost/beast/http/parser.hpp>
#include <boost/beast/http/read.hpp>
#include <boost/beast/http/string_body.hpp>
#include <boost/beast/http/write.hpp>
#include <boost/url/parse.hpp>
#include <boost/url/url.hpp>
#include <boost/url/url_view.hpp>
#include <algorithm>
#include <array>
#include <cctype>
#include <openssl/ssl.h>
#include <utility>

namespace ores::http::client::net {

namespace {

namespace asio = boost::asio;
namespace beast = boost::beast;
namespace bhttp = boost::beast::http;
using tcp = asio::ip::tcp;

bool names_a_secret(std::string_view name) {
    std::string lower(name);
    std::ranges::transform(
        lower, lower.begin(), [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    return std::ranges::any_of(
        std::array<std::string_view, 4>{"key", "token", "secret", "password"},
        [&](std::string_view word) { return lower.contains(word); });
}

failure failed(failure_kind kind, std::string_view url, const std::string& what) {
    return {kind, redacted(url) + ": " + what};
}

failure_kind kind_of(const beast::error_code& ec) {
    if (ec == beast::error::timeout)
        return failure_kind::timeout;
    if (ec == bhttp::error::body_limit)
        return failure_kind::too_large;
    if (ec.category() == asio::error::get_ssl_category() ||
        ec.category() == asio::ssl::error::get_stream_category())
        return failure_kind::tls;
    return failure_kind::network;
}

/**
 * @brief Runs one asynchronous step to its end and returns its error.
 *
 * Beast holds a stream's deadline only for an asynchronous operation, so each step is
 * started asynchronously and the context runs until it completes or its deadline passes.
 */
template <typename Start>
beast::error_code run(asio::io_context& io, Start start) {
    beast::error_code result;
    start([&result](beast::error_code ec, auto&&...) { result = ec; });
    io.restart();
    io.run();
    return result;
}

/// Sends the request on @p stream and reads the answer, every step within @p deadline.
template <typename Stream>
std::expected<response, failure> exchange(asio::io_context& io,
                                          Stream& stream,
                                          beast::tcp_stream& layer,
                                          const bhttp::request<bhttp::empty_body>& request,
                                          const request_limits& limits,
                                          std::chrono::steady_clock::time_point deadline,
                                          std::string_view url) {
    layer.expires_at(deadline);
    if (const auto ec =
            run(io, [&](auto handler) { bhttp::async_write(stream, request, std::move(handler)); }))
        return std::unexpected(failed(kind_of(ec), url, ec.message()));

    beast::flat_buffer buffer;
    bhttp::response_parser<bhttp::string_body> parser;
    parser.body_limit(limits.max_body_bytes);
    layer.expires_at(deadline);
    if (const auto ec = run(io, [&](auto handler) {
            bhttp::async_read(stream, buffer, parser, std::move(handler));
        }))
        return std::unexpected(failed(kind_of(ec), url, ec.message()));

    auto message = parser.release();
    response out;
    out.status = message.result_int();
    if (const auto type = message.find(bhttp::field::content_type); type != message.end())
        out.content_type = std::string(type->value());
    out.body = std::move(message.body());
    return out;
}

}

beast_transport::beast_transport()
    : beast_transport(options{}) {}

beast_transport::beast_transport(options o)
    : options_(std::move(o)) {}

std::expected<response, failure> beast_transport::get(std::string_view url,
                                                      const request_limits& limits) {
    const auto parsed = boost::urls::parse_absolute_uri(url);
    if (!parsed)
        return std::unexpected(failed(failure_kind::bad_url, url, parsed.error().message()));
    const auto& u = *parsed;
    const bool secure = u.scheme() == "https";
    if (!secure && u.scheme() != "http")
        return std::unexpected(
            failed(failure_kind::bad_url, url, "only http and https addresses are read"));
    // The address without the brackets of an IPv6 literal, which the resolver and the name
    // check take; the Host header keeps the brackets.
    const std::string host(u.host_address());
    if (host.empty())
        return std::unexpected(failed(failure_kind::bad_url, url, "the address names no host"));
    const bool named = u.host_type() == boost::urls::host_type::name;
    const std::string port = u.has_port() ? std::string(u.port()) : (secure ? "443" : "80");
    std::string target(u.encoded_target());
    if (target.empty())
        target = "/";

    bhttp::request<bhttp::empty_body> request{bhttp::verb::get, target, 11};
    request.set(bhttp::field::host, std::string(u.encoded_host_and_port()));
    request.set(bhttp::field::user_agent, options_.user_agent);
    request.set(bhttp::field::accept, "*/*");

    const auto deadline = std::chrono::steady_clock::now() + limits.timeout;
    asio::io_context io;
    beast::error_code ec;

    // A stream's deadline does not cover the resolver, so a timer on the same deadline
    // cancels a lookup that takes too long.
    tcp::resolver resolver(io);
    asio::steady_timer timer(io, deadline);
    tcp::resolver::results_type endpoints;
    beast::error_code resolved;
    bool timed_out = false;
    resolver.async_resolve(
        host, port, [&](beast::error_code e, tcp::resolver::results_type results) {
            resolved = e;
            endpoints = std::move(results);
            timer.cancel();
        });
    timer.async_wait([&](beast::error_code e) {
        if (!e) {
            timed_out = true;
            resolver.cancel();
        }
    });
    io.run();
    if (timed_out)
        return std::unexpected(failed(failure_kind::timeout, url, "the host name took too long"));
    if (resolved)
        return std::unexpected(failed(failure_kind::network, url, resolved.message()));

    if (!secure) {
        beast::tcp_stream stream(io);
        stream.expires_at(deadline);
        if (const auto connected =
                run(io, [&](auto handler) { stream.async_connect(endpoints, std::move(handler)); }))
            return std::unexpected(failed(kind_of(connected), url, connected.message()));
        auto out = exchange(io, stream, stream, request, limits, deadline, url);
        // The answer is read; a failure to close the connection changes nothing.
        stream.socket().shutdown(tcp::socket::shutdown_both, ec);
        return out;
    }

    asio::ssl::context ctx(asio::ssl::context::tls_client);
    ctx.set_verify_mode(asio::ssl::verify_peer);
    if (options_.trusted_authorities)
        ctx.load_verify_file(options_.trusted_authorities->string(), ec);
    else
        ctx.set_default_verify_paths(ec);
    if (ec)
        return std::unexpected(failed(failure_kind::tls, url, ec.message()));

    asio::ssl::stream<beast::tcp_stream> stream(io, ctx);
    // A server name goes in the handshake only for a name: RFC 6066 forbids an address.
    if (named && !SSL_set_tlsext_host_name(stream.native_handle(), host.c_str()))
        return std::unexpected(failed(failure_kind::tls, url, "the host name cannot be sent"));
    stream.set_verify_callback(asio::ssl::host_name_verification(host));

    auto& layer = beast::get_lowest_layer(stream);
    layer.expires_at(deadline);
    if (const auto connected =
            run(io, [&](auto handler) { layer.async_connect(endpoints, std::move(handler)); }))
        return std::unexpected(failed(kind_of(connected), url, connected.message()));
    layer.expires_at(deadline);
    if (const auto shaken = run(io, [&](auto handler) {
            stream.async_handshake(asio::ssl::stream_base::client, std::move(handler));
        }))
        return std::unexpected(failed(kind_of(shaken), url, shaken.message()));
    auto out = exchange(io, stream, layer, request, limits, deadline, url);
    // The answer is read; a server that closes without a TLS goodbye, as many do, changes
    // nothing.
    layer.expires_at(deadline);
    run(io, [&](auto handler) { stream.async_shutdown(std::move(handler)); });
    return out;
}

std::string redacted(std::string_view url) {
    auto parsed = boost::urls::parse_absolute_uri(url);
    if (!parsed)
        return std::string(url);
    boost::urls::url u(*parsed);
    if (u.has_userinfo()) {
        u.set_user("***");
        if (u.has_password())
            u.set_password("***");
    }
    auto params = u.params();
    for (auto it = params.begin(); it != params.end(); ++it) {
        const auto param = *it;
        if (param.has_value && names_a_secret(param.key))
            it = params.replace(it, {param.key, "***"});
    }
    return std::string(u.buffer());
}

}
