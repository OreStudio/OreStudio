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
#include "ores.http.api/net/http_session.hpp"
#include <boost/asio/use_awaitable.hpp>
#include <boost/beast/http/read.hpp>
#include <boost/beast/http/write.hpp>
#include <boost/url/parse.hpp>
#include <boost/url/url_view.hpp>
#include <algorithm>
#include <limits>

namespace ores::http::net {

using namespace ores::logging;
namespace beast = boost::beast;
namespace http = beast::http;
namespace asio = boost::asio;

http_session::http_session(asio::ip::tcp::socket socket,
                           std::shared_ptr<router> router,
                           std::shared_ptr<ores::security::jwt::jwt_authenticator> verifier,
                           std::shared_ptr<worker_pool> workers,
                           const http_server_options& options,
                           session_bytes_callback bytes_callback)
    : stream_(std::move(socket))
    , router_(std::move(router))
    , verifier_(std::move(verifier))
    , workers_(std::move(workers))
    , options_(options)
    , bytes_callback_(std::move(bytes_callback)) {

    auto endpoint = stream_.socket().remote_endpoint();
    remote_address_ = endpoint.address().to_string();
    BOOST_LOG_SEV(lg(), debug) << "HTTP session created for: " << remote_address_ << ":"
                               << endpoint.port();
}

asio::awaitable<void> http_session::run() {
    BOOST_LOG_SEV(lg(), info) << "Starting HTTP session for: " << remote_address_;

    beast::error_code ec;
    stream_.expires_after(options_.request_timeout);

    try {
        for (;;) {
            /*
             * The headers are read first, on their own, so the body limit can be
             * decided by the route the request addresses before any of the body
             * is held. Reading the message whole would force one limit on every
             * route: small enough for a JSON call is too small for an engine
             * package, and large enough for a package is a body bomb everywhere
             * else.
             */
            http::request_parser<http::string_body> parser;
            parser.header_limit(options_.max_header_size);
            /*
             * The parser reaches no body limit of its own while the headers are
             * read. Beast compares the declared length against that limit as the
             * headers finish, so any limit here ends the session over a body it
             * has not read, and the caller learns nothing. The route's limit is
             * set below, once the route is known, and it is the one that governs
             * the body.
             */
            parser.body_limit((std::numeric_limits<std::uint64_t>::max)());
            co_await http::async_read_header(stream_, buffer_, parser, asio::use_awaitable);

            const auto version = parser.get().version();
            const auto keep_alive = parser.get().keep_alive();
            const auto subject = std::string(parser.get().target());

            BOOST_LOG_SEV(lg(), debug) << "Received request: " << parser.get().method_string()
                                       << " " << subject << " from " << remote_address_;

            const auto limit = body_limit_for(parser.get());

            /*
             * The refusal is written here, not by the failed read, so the caller
             * is told why instead of watching the socket close. It answers both
             * the declared size below and the chunked body that declares none.
             */
            const auto refuse_too_large =
                [this, version, &subject](std::uint64_t size) -> asio::awaitable<void> {
                BOOST_LOG_SEV(lg(), warn) << "Refusing " << subject << ": a body of " << size
                                          << " bytes is past the route's limit";
                auto too_large = domain::http_response::error(
                    domain::http_status::payload_too_large,
                    "The request body is larger than this route accepts.");
                auto beast_resp = convert_response(too_large, version, false);
                co_await http::async_write(stream_, beast_resp, asio::use_awaitable);
            };

            const auto declared = parser.content_length().value_or(0);
            if (declared > limit) {
                co_await refuse_too_large(declared);
                break;
            }

            // A client that asks before sending is told to send, and only once
            // the body is known to be acceptable.
            if (beast::iequals(parser.get()[http::field::expect], "100-continue")) {
                static constexpr std::string_view cont = "HTTP/1.1 100 Continue\r\n\r\n";
                co_await asio::async_write(stream_, asio::buffer(cont), asio::use_awaitable);
            }

            parser.body_limit(limit);
            auto body_too_large = false;
            try {
                co_await http::async_read(stream_, buffer_, parser, asio::use_awaitable);
            } catch (const boost::system::system_error& e) {
                // A chunked body declares no length, so it is caught here rather
                // than above, and it is still a refusal with a reason.
                if (e.code() != http::error::body_limit)
                    throw;
                body_too_large = true;
            }

            if (body_too_large) {
                co_await refuse_too_large(limit);
                break;
            }

            auto req = parser.release();
            co_await handle_request(std::move(req));

            if (!keep_alive) {
                BOOST_LOG_SEV(lg(), debug) << "Client requested connection close";
                break;
            }
        }
    } catch (const boost::system::system_error& e) {
        if (e.code() != beast::errc::not_connected && e.code() != asio::error::eof &&
            e.code() != asio::error::operation_aborted) {
            BOOST_LOG_SEV(lg(), warn)
                << "Session error for " << remote_address_ << ": " << e.what();
        } else {
            BOOST_LOG_SEV(lg(), debug)
                << "Session ended for " << remote_address_ << ": " << e.what();
        }
    }

    BOOST_LOG_SEV(lg(), info) << "HTTP session ended for: " << remote_address_;

    // Graceful shutdown
    stream_.socket().shutdown(asio::ip::tcp::socket::shutdown_send, ec);
}

std::uint64_t http_session::body_limit_for(const http::request<http::string_body>& req) {
    const auto ceiling = options_.max_body_size;
    const auto fallback = std::min<std::uint64_t>(options_.default_max_body_size, ceiling);

    auto path = std::string(req.target());
    const auto query_pos = path.find('?');
    if (query_pos != std::string::npos)
        path = path.substr(0, query_pos);

    std::unordered_map<std::string, std::string> path_params;
    const auto matched = router_->match(convert_method(req.method()), path, path_params);
    if (!matched)
        return fallback;

    const auto stated =
        matched->max_body_size == 0 ? options_.default_max_body_size : matched->max_body_size;
    return std::min<std::uint64_t>(stated, ceiling);
}

asio::awaitable<void> http_session::handle_request(http::request<http::string_body> req) {

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
        cors_resp.set_header("Access-Control-Allow-Methods",
                             "GET, POST, PUT, PATCH, DELETE, OPTIONS");
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
        BOOST_LOG_SEV(lg(), debug)
            << "No route matched for: " << static_cast<int>(domain_req.method) << " " << path;
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
            BOOST_LOG_SEV(lg(), warn) << "Missing authorization token for " << matched->pattern
                                      << " from " << remote_address_;
            auto unauthorized = domain::http_response::unauthorized(
                "Authorization header with Bearer token required");
            auto beast_resp = convert_response(unauthorized, req.version(), req.keep_alive());
            co_await http::async_write(stream_, beast_resp, asio::use_awaitable);
            co_return;
        }

        // A route that requires authentication fails closed. If the server
        // cannot verify anybody -- no verifier configured, or the public key
        // failed to load -- it must not serve anybody. There is deliberately no
        // second verifier to fall back to: one way of being believed.
        auto* effective = verifier_.get();
        if (effective == nullptr || !effective->is_configured()) {
            BOOST_LOG_SEV(lg(), error)
                << "Route requires authentication but no JWT verifier is "
                << "configured; refusing " << matched->pattern << " from " << remote_address_;
            auto unauthorized = domain::http_response::unauthorized(
                "Authentication is not configured on this server");
            auto beast_resp = convert_response(unauthorized, req.version(), req.keep_alive());
            co_await http::async_write(stream_, beast_resp, asio::use_awaitable);
            co_return;
        }

        if (effective != nullptr && effective->is_configured()) {
            auto claims_result = effective->validate(*token);
            if (!claims_result) {
                BOOST_LOG_SEV(lg(), warn)
                    << "Invalid token for " << matched->pattern << " from " << remote_address_
                    << ": " << ores::security::jwt::to_string(claims_result.error());
                auto unauthorized = domain::http_response::unauthorized(
                    ores::security::jwt::to_string(claims_result.error()));
                auto beast_resp = convert_response(unauthorized, req.version(), req.keep_alive());
                co_await http::async_write(stream_, beast_resp, asio::use_awaitable);
                co_return;
            }

            domain_req.authenticated_user = std::move(claims_result.value());
            BOOST_LOG_SEV(lg(), debug)
                << "Authenticated user: " << domain_req.authenticated_user->subject;

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
                    if (has_required_role)
                        break;
                }

                if (!has_required_role) {
                    BOOST_LOG_SEV(lg(), warn) << "User " << domain_req.authenticated_user->subject
                                              << " lacks required role for " << matched->pattern;
                    auto forbidden = domain::http_response::forbidden("Insufficient permissions");
                    auto beast_resp = convert_response(forbidden, req.version(), req.keep_alive());
                    co_await http::async_write(stream_, beast_resp, asio::use_awaitable);
                    co_return;
                }
            }
        }
    }

    /*
     * The handler blocks on the service that owns the data, so it runs on the
     * worker pool and not here. This thread stays free to serve every other
     * connection while one request waits on a slow service. A null pool keeps
     * the old behaviour, which only a test wants.
     */
    domain::http_response response;
    if (workers_ != nullptr && workers_->saturated()) {
        BOOST_LOG_SEV(lg(), warn) << "Refusing " << matched->pattern << ": the handler pool holds "
                                  << workers_->inflight() << " requests already";
        response = domain::http_response::error(domain::http_status::service_unavailable,
                                                "The server is saturated. Try again shortly.");
    } else {
        try {
            BOOST_LOG_SEV(lg(), trace) << "Executing handler for: " << matched->pattern;
            if (workers_ != nullptr) {
                const auto handler = matched->handler;
                response = co_await workers_->run(
                    [&handler, &domain_req]() { return handler(domain_req); });
            } else {
                response = co_await matched->handler(domain_req);
            }
            BOOST_LOG_SEV(lg(), debug)
                << "Handler completed with status: " << static_cast<int>(response.status);
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), error)
                << "Handler exception for " << matched->pattern << ": " << e.what();
            response = domain::http_response::internal_error(e.what());
        }
    }

    // Add CORS headers if enabled
    if (options_.enable_cors) {
        response.set_header("Access-Control-Allow-Origin", options_.cors_allowed_origins);
    }

    // Add server header
    response.set_header("Server", options_.server_identifier);

    auto beast_resp = convert_response(response, req.version(), req.keep_alive());
    co_await http::async_write(stream_, beast_resp, asio::use_awaitable);

    // Update session bytes if we have an authenticated user with session info
    if (bytes_callback_ && domain_req.authenticated_user &&
        domain_req.authenticated_user->session_id &&
        domain_req.authenticated_user->session_start_time) {
        try {
            bytes_callback_(*domain_req.authenticated_user->session_id,
                            *domain_req.authenticated_user->session_start_time,
                            response.body.size(),
                            domain_req.body.size());
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "Failed to update session bytes: " << e.what();
        }
    }
}

domain::http_request http_session::convert_request(const http::request<http::string_body>& req) {

    domain::http_request result;
    result.method = convert_method(req.method());
    result.target = std::string(req.target());
    result.body = req.body();
    result.remote_address = remote_address_;

    // Extract HTTP version (e.g., 11 = HTTP/1.1, 20 = HTTP/2)
    auto version = req.version();
    result.http_version_major = static_cast<std::uint16_t>(version / 10);
    result.http_version_minor = static_cast<std::uint16_t>(version % 10);

    // Copy headers (normalize keys to lowercase for case-insensitive lookup per RFC 7230)
    for (const auto& field : req) {
        std::string key = std::string(field.name_string());
        std::transform(
            key.begin(), key.end(), key.begin(), [](unsigned char c) { return std::tolower(c); });
        result.headers[key] = std::string(field.value());
    }

    // Parse query parameters
    parse_query_params(result.target, result);

    return result;
}

http::response<http::string_body> http_session::convert_response(const domain::http_response& resp,
                                                                 unsigned version,
                                                                 bool keep_alive) {

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
        case http::verb::get:
            return domain::http_method::get;
        case http::verb::post:
            return domain::http_method::post;
        case http::verb::put:
            return domain::http_method::put;
        case http::verb::patch:
            return domain::http_method::patch;
        case http::verb::delete_:
            return domain::http_method::delete_;
        case http::verb::head:
            return domain::http_method::head;
        case http::verb::options:
            return domain::http_method::options;
        default:
            return domain::http_method::get;
    }
}

void http_session::parse_query_params(const std::string& target, domain::http_request& req) {

    auto result = boost::urls::parse_uri_reference(target);
    if (!result) {
        BOOST_LOG_SEV(lg(), warn) << "Failed to parse URI: " << target;
        return;
    }

    for (const auto& param : result->params()) {
        req.query_params.emplace(std::string(param.key), std::string(param.value));
    }
}

}
