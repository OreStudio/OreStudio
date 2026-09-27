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
#include "ores.storage.core/net/http_client.hpp"
#include <boost/asio/connect.hpp>
#include <boost/asio/io_context.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <limits>
#include <stdexcept>

namespace ores::storage::net {

namespace beast = boost::beast;
namespace http = boost::beast::http;
namespace asio = boost::asio;
using tcp = asio::ip::tcp;

http_client::url_parts http_client::parse_url(const std::string& url) {
    constexpr std::string_view prefix = "http://";
    if (url.substr(0, prefix.size()) != prefix)
        throw std::runtime_error("http_client: only http:// URLs supported: " + url);

    const auto rest = url.substr(prefix.size());
    const auto slash = rest.find('/');
    const auto authority = (slash == std::string::npos) ? rest : rest.substr(0, slash);
    const auto path = (slash == std::string::npos) ? std::string("/") : rest.substr(slash);

    const auto colon = authority.rfind(':');
    if (colon == std::string::npos)
        return {authority, "80", path};

    return {authority.substr(0, colon), authority.substr(colon + 1), path};
}

namespace {

/// The bearer header every storage request carries, stated once.
template <typename Request>
void set_authorization(Request& req, const std::string& bearer_token) {
    req.set(http::field::authorization, "Bearer " + bearer_token);
}

/// A stream already connected to the host and port a storage URL names.
struct connected_stream {
    explicit connected_stream(const std::string& host, const std::string& port)
        : stream(ioc) {
        tcp::resolver resolver(ioc);
        stream.connect(resolver.resolve(host, port));
    }

    asio::io_context ioc;
    beast::tcp_stream stream;
};

/// Closes the socket, ignoring the error a peer's own close produces.
void shutdown(beast::tcp_stream& stream) {
    beast::error_code ec;
    stream.socket().shutdown(tcp::socket::shutdown_both, ec);
}

}

void http_client::get(const std::string& url,
                      const std::filesystem::path& dest,
                      const std::string& bearer_token) {
    const auto parts = parse_url(url);

    connected_stream conn(parts.host, parts.port);

    http::request<http::empty_body> req{http::verb::get, parts.path, 11};
    req.set(http::field::host, parts.host);
    req.set(http::field::user_agent, "ores.storage/1.0");
    set_authorization(req, bearer_token);

    http::write(conn.stream, req);

    // The status is read before the destination is opened, so a failed
    // download neither creates a file nor truncates one that is already
    // there.
    http::response_parser<http::file_body> parser;
    parser.body_limit(std::numeric_limits<std::uint64_t>::max());
    beast::flat_buffer buf;
    http::read_header(conn.stream, buf, parser);

    if (parser.get().result_int() < 200 || parser.get().result_int() >= 300)
        throw std::runtime_error("http_client: GET " + url + " returned HTTP " +
                                 std::to_string(parser.get().result_int()));

    const auto parent = dest.parent_path();
    if (!parent.empty())
        std::filesystem::create_directories(parent);
    beast::error_code open_ec;
    parser.get().body().open(dest.string().c_str(), beast::file_mode::write, open_ec);
    if (open_ec)
        throw std::runtime_error("http_client: cannot open for writing: " + dest.string());

    http::read(conn.stream, buf, parser);

    shutdown(conn.stream);
}

void http_client::put(const std::string& url,
                      const std::filesystem::path& src,
                      const std::string& bearer_token) {
    put_returning_body(url, src, bearer_token);
}

std::string http_client::put_returning_body(const std::string& url,
                                            const std::filesystem::path& src,
                                            const std::string& bearer_token) {
    const auto parts = parse_url(url);

    connected_stream conn(parts.host, parts.port);

    http::request<http::file_body> req{http::verb::put, parts.path, 11};
    req.set(http::field::host, parts.host);
    req.set(http::field::user_agent, "ores.storage/1.0");
    req.set(http::field::content_type, "application/octet-stream");
    set_authorization(req, bearer_token);
    beast::error_code open_ec;
    req.body().open(src.string().c_str(), beast::file_mode::read, open_ec);
    if (open_ec)
        throw std::runtime_error("http_client: cannot open for reading: " + src.string());
    req.prepare_payload();

    http::write(conn.stream, req);

    beast::flat_buffer buf;
    http::response<http::string_body> res;
    http::read(conn.stream, buf, res);

    if (res.result_int() < 200 || res.result_int() >= 300)
        throw std::runtime_error("http_client: PUT " + url + " returned HTTP " +
                                 std::to_string(res.result_int()));

    shutdown(conn.stream);

    return res.body();
}

std::string http_client::get_returning_body(const std::string& url,
                                           const std::string& bearer_token) {
    const auto parts = parse_url(url);

    connected_stream conn(parts.host, parts.port);

    http::request<http::empty_body> req{http::verb::get, parts.path, 11};
    req.set(http::field::host, parts.host);
    req.set(http::field::user_agent, "ores.storage/1.0");
    set_authorization(req, bearer_token);

    http::write(conn.stream, req);

    beast::flat_buffer buf;
    http::response<http::string_body> res;
    http::read(conn.stream, buf, res);

    if (res.result_int() < 200 || res.result_int() >= 300)
        throw std::runtime_error("http_client: GET " + url + " returned HTTP " +
                                 std::to_string(res.result_int()));

    shutdown(conn.stream);

    return res.body();
}

std::string http_client::del(const std::string& url, const std::string& bearer_token) {
    const auto parts = parse_url(url);

    connected_stream conn(parts.host, parts.port);

    http::request<http::empty_body> req{http::verb::delete_, parts.path, 11};
    req.set(http::field::host, parts.host);
    req.set(http::field::user_agent, "ores.storage/1.0");
    set_authorization(req, bearer_token);

    http::write(conn.stream, req);

    beast::flat_buffer buf;
    http::response<http::string_body> res;
    http::read(conn.stream, buf, res);

    if (res.result_int() < 200 || res.result_int() >= 300)
        throw std::runtime_error("http_client: DELETE " + url + " returned HTTP " +
                                 std::to_string(res.result_int()));

    shutdown(conn.stream);

    return res.body();
}

}
