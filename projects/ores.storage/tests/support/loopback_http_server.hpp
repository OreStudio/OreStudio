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
#ifndef ORES_STORAGE_TESTS_SUPPORT_LOOPBACK_HTTP_SERVER_HPP
#define ORES_STORAGE_TESTS_SUPPORT_LOOPBACK_HTTP_SERVER_HPP

#include <boost/asio/io_context.hpp>
#include <boost/asio/ip/address.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/beast/core.hpp>
#include <boost/beast/http.hpp>
#include <boost/system/error_code.hpp>
#include <atomic>
#include <cstdint>
#include <limits>
#include <mutex>
#include <string>
#include <thread>
#include <utility>

namespace ores::storage::tests {

namespace beast = boost::beast;
namespace http = boost::beast::http;

/**
 * @brief Loopback HTTP/1.1 server for the storage client tests.
 *
 * Binds an ephemeral port on 127.0.0.1 and serves one connection at a time
 * from a background thread. A GET replies with the configured body and
 * status; a PUT records the request target and body, then replies with the
 * configured status and body. The server never leaves the host and is
 * intentionally minimal: it exists so the tests can assert on the exact
 * bytes the client sends and receives.
 */
class loopback_http_server final {
public:
    loopback_http_server()
        : acceptor_(io_context_,
                    boost::asio::ip::tcp::endpoint(boost::asio::ip::make_address("127.0.0.1"), 0)) {
        port_ = acceptor_.local_endpoint().port();
        base_url_ = "http://127.0.0.1:" + std::to_string(port_);
        thread_ = std::thread([this] { accept_loop(); });
    }

    loopback_http_server(const loopback_http_server&) = delete;
    loopback_http_server& operator=(const loopback_http_server&) = delete;

    ~loopback_http_server() {
        stopped_.store(true);

        // close() alone does not interrupt a synchronous accept() blocked in
        // another thread -- the syscall holds its own reference to the
        // listening socket -- so wake the loop through a real connection
        // while the acceptor is still open. The loop leaves on stopped_
        // instead of serving this connection.
        boost::system::error_code ec;
        boost::asio::io_context wake;
        boost::asio::ip::tcp::socket socket(wake);
        const boost::asio::ip::tcp::endpoint endpoint(boost::asio::ip::make_address("127.0.0.1"),
                                                      port_);
        socket.connect(endpoint, ec);

        if (thread_.joinable())
            thread_.join();

        acceptor_.close(ec);
    }

    [[nodiscard]] std::string base_url() const {
        return base_url_;
    }

    [[nodiscard]] std::string last_method() const {
        std::lock_guard<std::mutex> lock(mutex_);
        return last_method_;
    }

    [[nodiscard]] std::string last_target() const {
        std::lock_guard<std::mutex> lock(mutex_);
        return last_target_;
    }

    [[nodiscard]] std::string last_body() const {
        std::lock_guard<std::mutex> lock(mutex_);
        return last_body_;
    }

    [[nodiscard]] std::string last_put_body() const {
        std::lock_guard<std::mutex> lock(mutex_);
        return last_put_body_;
    }

    void set_get_body(std::string body) {
        std::lock_guard<std::mutex> lock(mutex_);
        get_body_ = std::move(body);
    }

    void set_put_response_body(std::string body) {
        std::lock_guard<std::mutex> lock(mutex_);
        put_response_body_ = std::move(body);
    }

    void set_status(int status) {
        std::lock_guard<std::mutex> lock(mutex_);
        status_ = status;
    }

private:
    void accept_loop() {
        while (!stopped_.load()) {
            boost::asio::ip::tcp::socket socket(io_context_);
            boost::system::error_code ec;
            acceptor_.accept(socket, ec);
            if (ec || stopped_.load())
                break;
            serve(std::move(socket));
        }
    }

    void serve(boost::asio::ip::tcp::socket socket) {
        beast::flat_buffer buffer;
        http::request_parser<http::string_body> parser;
        // The client streams file bodies with no limit, so the server must
        // not impose Beast's one-megabyte default and truncate a large PUT.
        parser.body_limit(std::numeric_limits<std::uint64_t>::max());

        boost::system::error_code ec;
        http::read(socket, buffer, parser, ec);
        if (ec)
            return;

        const auto& request = parser.get();
        http::response<http::string_body> response;
        {
            std::lock_guard<std::mutex> lock(mutex_);
            last_method_ = std::string(request.method_string());
            last_target_ = std::string(request.target());
            last_body_ = request.body();
            if (request.method() == http::verb::put) {
                last_put_body_ = request.body();
                response.body() = put_response_body_;
            } else {
                response.body() = get_body_;
            }
            response.result(status_);
        }
        response.version(11);
        response.set(http::field::content_type, "application/octet-stream");
        response.prepare_payload();

        http::write(socket, response, ec);
        socket.shutdown(boost::asio::ip::tcp::socket::shutdown_send, ec);
    }

    boost::asio::io_context io_context_;
    boost::asio::ip::tcp::acceptor acceptor_;
    std::string base_url_;
    std::uint16_t port_ = 0;
    std::thread thread_;
    std::atomic<bool> stopped_{false};
    mutable std::mutex mutex_;
    int status_ = 200;
    std::string get_body_;
    std::string put_response_body_;
    std::string last_method_;
    std::string last_target_;
    std::string last_body_;
    std::string last_put_body_;
};

}

#endif
