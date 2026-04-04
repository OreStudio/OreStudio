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
#include "ores.storage/net/http_client.hpp"

#include <limits>
#include <stdexcept>
#include <boost/asio/io_context.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/connect.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>

namespace ores::storage::net {

namespace beast = boost::beast;
namespace http  = boost::beast::http;
namespace asio  = boost::asio;
using tcp       = asio::ip::tcp;

http_client::url_parts http_client::parse_url(const std::string& url) {
    constexpr std::string_view prefix = "http://";
    if (url.substr(0, prefix.size()) != prefix)
        throw std::runtime_error("http_client: only http:// URLs supported: " + url);

    const auto rest      = url.substr(prefix.size());
    const auto slash     = rest.find('/');
    const auto authority = (slash == std::string::npos) ? rest : rest.substr(0, slash);
    const auto path      = (slash == std::string::npos) ? std::string("/") :
                           rest.substr(slash);

    const auto colon = authority.rfind(':');
    if (colon == std::string::npos)
        return {authority, "80", path};

    return {authority.substr(0, colon), authority.substr(colon + 1), path};
}

void http_client::get(const std::string& url, const std::filesystem::path& dest) {
    const auto parts = parse_url(url);

    asio::io_context ioc;
    tcp::resolver resolver(ioc);
    beast::tcp_stream stream(ioc);

    const auto results = resolver.resolve(parts.host, parts.port);
    stream.connect(results);

    http::request<http::empty_body> req{http::verb::get, parts.path, 11};
    req.set(http::field::host, parts.host);
    req.set(http::field::user_agent, "ores.storage/1.0");

    http::write(stream, req);

    std::filesystem::create_directories(dest.parent_path());
    http::response_parser<http::file_body> parser;
    parser.body_limit(std::numeric_limits<std::uint64_t>::max());
    beast::error_code open_ec;
    parser.get().body().open(dest.c_str(), beast::file_mode::write, open_ec);
    if (open_ec)
        throw std::runtime_error("http_client: cannot open for writing: " + dest.string());

    beast::flat_buffer buf;
    http::read(stream, buf, parser);

    if (parser.get().result_int() < 200 || parser.get().result_int() >= 300)
        throw std::runtime_error("http_client: GET " + url + " returned HTTP "
            + std::to_string(parser.get().result_int()));

    beast::error_code ec;
    stream.socket().shutdown(tcp::socket::shutdown_both, ec);
}

void http_client::put(const std::string& url, const std::filesystem::path& src) {
    const auto parts = parse_url(url);

    asio::io_context ioc;
    tcp::resolver resolver(ioc);
    beast::tcp_stream stream(ioc);

    const auto results = resolver.resolve(parts.host, parts.port);
    stream.connect(results);

    http::request<http::file_body> req{http::verb::put, parts.path, 11};
    req.set(http::field::host, parts.host);
    req.set(http::field::user_agent, "ores.storage/1.0");
    req.set(http::field::content_type, "application/octet-stream");
    beast::error_code open_ec;
    req.body().open(src.c_str(), beast::file_mode::read, open_ec);
    if (open_ec)
        throw std::runtime_error("http_client: cannot open for reading: " + src.string());
    req.prepare_payload();

    http::write(stream, req);

    beast::flat_buffer buf;
    http::response<http::string_body> res;
    http::read(stream, buf, res);

    if (res.result_int() < 200 || res.result_int() >= 300)
        throw std::runtime_error("http_client: PUT " + url + " returned HTTP "
            + std::to_string(res.result_int()));

    beast::error_code ec;
    stream.socket().shutdown(tcp::socket::shutdown_both, ec);
}

}
