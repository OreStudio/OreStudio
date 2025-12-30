/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.http/net/http_session.hpp"

#include <algorithm>
#include <boost/asio/use_awaitable.hpp>
#include <boost/beast/http/read.hpp>
#include <boost/beast/http/write.hpp>

namespace ores::http::net {

using namespace ores::telemetry::log;
namespace beast = boost::beast;
namespace http = beast::http;
namespace asio = boost::asio;

http_session::http_session(asio::ip::tcp::socket socket,
    std::shared_ptr<router> router,
    std::shared_ptr<middleware::jwt_authenticator> authenticator,
    const http_server_options& options)
    : stream_(std::move(socket))
    , router_(std::move(router))
    , authenticator_(std::move(authenticator))
    , options_(options) {

    auto endpoint = stream_.socket().remote_endpoint();
    remote_address_ = endpoint.address().to_string() + ":" +
        std::to_string(endpoint.port());
    BOOST_LOG_SEV(lg(), debug) << "HTTP session created for: " << remote_address_;
}

asio::awaitable<void> http_session::run() {
    BOOST_LOG_SEV(lg(), info) << "Starting HTTP session for: " << remote_address_;

    beast::error_code ec;
    stream_.expires_after(options_.request_timeout);

    try {
        for (;;) {
            http::request<http::string_body> req;
            co_await http::async_read(stream_, buffer_, req, asio::use_awaitable);

            BOOST_LOG_SEV(lg(), debug) << "Received request: "
                << req.method_string() << " " << req.target()
                << " from " << remote_address_;

            co_await handle_request(std::move(req));

            // Check for connection close
            if (req[http::field::connection] == "close") {
                BOOST_LOG_SEV(lg(), debug) << "Client requested connection close";
                break;
            }
        }
    } catch (const boost::system::system_error& e) {
        if (e.code() != beast::errc::not_connected &&
            e.code() != asio::error::eof &&
            e.code() != asio::error::operation_aborted) {
            BOOST_LOG_SEV(lg(), warn) << "Session error for " << remote_address_
                << ": " << e.what();
        } else {
            BOOST_LOG_SEV(lg(), debug) << "Session ended for " << remote_address_
                << ": " << e.what();
        }
    }

    BOOST_LOG_SEV(lg(), info) << "HTTP session ended for: " << remote_address_;

    // Graceful shutdown
    stream_.socket().shutdown(asio::ip::tcp::socket::shutdown_send, ec);
}

asio::awaitable<void> http_session::handle_request(
    http::request<http::string_body> req) {

    auto domain_req = convert_request(req);

    // Extract path without query string
    std::string path = domain_req.target;
    auto query_pos = path.find('?');
    if (query_pos != std::string::npos) {
        path = path.substr(0, query_pos);
    }

    BOOST_LOG_SEV(lg(), trace) << "Processing request path: " << path
        << " method: " << static_cast<int>(domain_req.method);

    // Handle CORS preflight
    if (options_.enable_cors && domain_req.method == domain::http_method::options) {
        domain::http_response cors_resp;
        cors_resp.status = domain::http_status::no_content;
        cors_resp.set_header("Access-Control-Allow-Origin", options_.cors_allowed_origins);
        cors_resp.set_header("Access-Control-Allow-Methods", "GET, POST, PUT, PATCH, DELETE, OPTIONS");
        cors_resp.set_header("Access-Control-Allow-Headers", "Content-Type, Authorization");
        cors_resp.set_header("Access-Control-Max-Age", "86400");

        auto beast_resp = convert_response(cors_resp, req.version(), req.keep_alive());
        co_await http::async_write(stream_, beast_resp, asio::use_awaitable);
        co_return;
    }

    // Match route
    std::unordered_map<std::string, std::string> path_params;
    auto matched = router_->match(domain_req.method, path, path_params);

    if (!matched) {
        BOOST_LOG_SEV(lg(), debug) << "No route matched for: "
            << static_cast<int>(domain_req.method) << " " << path;
        auto not_found = domain::http_response::not_found("Endpoint not found");
        auto beast_resp = convert_response(not_found, req.version(), req.keep_alive());
        co_await http::async_write(stream_, beast_resp, asio::use_awaitable);
        co_return;
    }

    domain_req.path_params = std::move(path_params);

    BOOST_LOG_SEV(lg(), debug) << "Route matched: " << matched->pattern;

    // Check authentication if required
    if (matched->requires_auth) {
        auto token = domain_req.get_bearer_token();
        if (!token) {
            BOOST_LOG_SEV(lg(), warn) << "Missing authorization token for "
                << matched->pattern << " from " << remote_address_;
            auto unauthorized = domain::http_response::unauthorized(
                "Authorization header with Bearer token required");
            auto beast_resp = convert_response(unauthorized, req.version(), req.keep_alive());
            co_await http::async_write(stream_, beast_resp, asio::use_awaitable);
            co_return;
        }

        if (authenticator_ && authenticator_->is_configured()) {
            auto claims_result = authenticator_->validate(*token);
            if (!claims_result) {
                BOOST_LOG_SEV(lg(), warn) << "Invalid token for " << matched->pattern
                    << " from " << remote_address_
                    << ": " << middleware::to_string(claims_result.error());
                auto unauthorized = domain::http_response::unauthorized(
                    middleware::to_string(claims_result.error()));
                auto beast_resp = convert_response(unauthorized, req.version(), req.keep_alive());
                co_await http::async_write(stream_, beast_resp, asio::use_awaitable);
                co_return;
            }

            domain_req.authenticated_user = std::move(claims_result.value());
            BOOST_LOG_SEV(lg(), debug) << "Authenticated user: "
                << domain_req.authenticated_user->subject;

            // Check roles if required
            if (!matched->required_roles.empty()) {
                bool has_required_role = false;
                for (const auto& required : matched->required_roles) {
                    for (const auto& user_role : domain_req.authenticated_user->roles) {
                        if (user_role == required) {
                            has_required_role = true;
                            break;
                        }
                    }
                    if (has_required_role) break;
                }

                if (!has_required_role) {
                    BOOST_LOG_SEV(lg(), warn) << "User " << domain_req.authenticated_user->subject
                        << " lacks required role for " << matched->pattern;
                    auto forbidden = domain::http_response::forbidden(
                        "Insufficient permissions");
                    auto beast_resp = convert_response(forbidden, req.version(), req.keep_alive());
                    co_await http::async_write(stream_, beast_resp, asio::use_awaitable);
                    co_return;
                }
            }
        }
    }

    // Execute handler
    domain::http_response response;
    try {
        BOOST_LOG_SEV(lg(), trace) << "Executing handler for: " << matched->pattern;
        response = co_await matched->handler(domain_req);
        BOOST_LOG_SEV(lg(), debug) << "Handler completed with status: "
            << static_cast<int>(response.status);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "Handler exception for " << matched->pattern
            << ": " << e.what();
        response = domain::http_response::internal_error(e.what());
    }

    // Add CORS headers if enabled
    if (options_.enable_cors) {
        response.set_header("Access-Control-Allow-Origin", options_.cors_allowed_origins);
    }

    // Add server header
    response.set_header("Server", options_.server_identifier);

    auto beast_resp = convert_response(response, req.version(), req.keep_alive());
    co_await http::async_write(stream_, beast_resp, asio::use_awaitable);
}

domain::http_request http_session::convert_request(
    const http::request<http::string_body>& req) {

    domain::http_request result;
    result.method = convert_method(req.method());
    result.target = std::string(req.target());
    result.body = req.body();
    result.remote_address = remote_address_;

    // Copy headers (normalize keys to lowercase for case-insensitive lookup per RFC 7230)
    for (const auto& field : req) {
        std::string key = std::string(field.name_string());
        std::transform(key.begin(), key.end(), key.begin(),
            [](unsigned char c) { return std::tolower(c); });
        result.headers[key] = std::string(field.value());
    }

    // Parse query parameters
    parse_query_params(result.target, result);

    return result;
}

http::response<http::string_body> http_session::convert_response(
    const domain::http_response& resp, unsigned version, bool keep_alive) {

    http::response<http::string_body> result;
    result.version(version);
    result.result(static_cast<http::status>(static_cast<unsigned>(resp.status)));
    result.body() = resp.body;
    result.set(http::field::content_type, resp.content_type);
    result.keep_alive(keep_alive);
    result.prepare_payload();

    // Copy custom headers
    for (const auto& [name, value] : resp.headers) {
        result.set(name, value);
    }

    return result;
}

domain::http_method http_session::convert_method(http::verb verb) {
    switch (verb) {
        case http::verb::get: return domain::http_method::get;
        case http::verb::post: return domain::http_method::post;
        case http::verb::put: return domain::http_method::put;
        case http::verb::patch: return domain::http_method::patch;
        case http::verb::delete_: return domain::http_method::delete_;
        case http::verb::head: return domain::http_method::head;
        case http::verb::options: return domain::http_method::options;
        default: return domain::http_method::get;
    }
}

void http_session::parse_query_params(const std::string& target,
    domain::http_request& req) {

    auto query_pos = target.find('?');
    if (query_pos == std::string::npos) {
        return;
    }

    std::string query = target.substr(query_pos + 1);
    std::size_t pos = 0;

    while (pos < query.size()) {
        auto eq_pos = query.find('=', pos);
        auto amp_pos = query.find('&', pos);

        if (eq_pos == std::string::npos || (amp_pos != std::string::npos && eq_pos > amp_pos)) {
            pos = (amp_pos == std::string::npos) ? query.size() : amp_pos + 1;
            continue;
        }

        std::string key = query.substr(pos, eq_pos - pos);
        std::string value;

        if (amp_pos != std::string::npos) {
            value = query.substr(eq_pos + 1, amp_pos - eq_pos - 1);
            pos = amp_pos + 1;
        } else {
            value = query.substr(eq_pos + 1);
            pos = query.size();
        }

        req.query_params[key] = value;
    }
}

}
