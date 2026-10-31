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
#include <boost/none.hpp>
#include <boost/url/parse.hpp>
#include <boost/url/url_view.hpp>
#include <algorithm>
#include <stdexcept>
#include <string_view>
#include <vector>

namespace ores::http::net {

using namespace ores::logging;

namespace {

/// The request target without its query string, which is what a route matches.
std::string route_path(std::string_view target) {
    const auto query = target.find('?');
    return std::string(query == std::string_view::npos ? target : target.substr(0, query));
}

}

namespace beast = boost::beast;
namespace http = beast::http;
namespace asio = boost::asio;

http_session::http_session(asio::ip::tcp::socket socket,
                           std::shared_ptr<router> router,
                           std::shared_ptr<ores::security::jwt::jwt_authenticator> verifier,
                           std::shared_ptr<worker_pool> workers,
                           const http_server_options& options,
                           session_bytes_callback bytes_callback,
                           server_counters* counters,
                           std::shared_ptr<domain::session_store> sessions,
                           domain::cookie_settings cookie)
    : stream_(std::move(socket))
    , router_(std::move(router))
    , verifier_(std::move(verifier))
    , workers_(std::move(workers))
    , options_(options)
    , bytes_callback_(std::move(bytes_callback))
    , counters_(counters)
    , sessions_(std::move(sessions))
    , cookie_(std::move(cookie)) {

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
             * The stream applies this timeout to every operation that follows,
             * so it bounds each header read, each body chunk and each write on
             * this connection. A handler that ran for longer than the timeout
             * does not leave the next request with an expired clock.
             */
            stream_.expires_after(options_.request_timeout);
            /*
             * The headers are read first, on their own, so the limit and the sink
             * can be decided by the route the request addresses before any of the
             * body is read. Reading the message whole would force one limit on
             * every route: small enough for a JSON call is too small for an
             * engine package, and large enough for a package is a body bomb
             * everywhere else. So the parser carries no body limit while the
             * headers are read: Beast compares the declared length against that
             * limit as the headers finish, and a limit here would end the session
             * over a body it has not read. The route's limit is set below, once
             * the route is known, and it is the one that governs the body.
             */
            http::request_parser<http::buffer_body> parser;
            parser.header_limit(options_.max_header_size);
            parser.body_limit(boost::none);
            co_await http::async_read_header(stream_, buffer_, parser, asio::use_awaitable);

            const auto version = parser.get().version();
            const auto keep_alive = parser.get().keep_alive();
            const auto subject = std::string(parser.get().target());

            BOOST_LOG_SEV(lg(), debug) << "Received request: " << parser.get().method_string()
                                       << " " << subject << " from " << remote_address_;

            /*
             * The route is resolved before the body, because the limit and the
             * sink both come from it, and matching once keeps the pattern off the
             * path the body takes.
             */
            std::unordered_map<std::string, std::string> path_params;
            auto route = router_->match(
                convert_method(parser.get().method()), route_path(subject), path_params);
            const auto limit = body_limit_for(route);

            /*
             * The refusal is written here, not by the failed read, so the caller
             * is told why instead of watching the socket close. One place answers
             * the declared size, the chunked body that declares none, the stream
             * that runs past the limit, the route that will not take the request,
             * and the destination that cannot take the bytes.
             */
            const auto refuse =
                [this, version, &subject](domain::http_status status,
                                          std::string_view reason,
                                          std::string_view note) -> asio::awaitable<void> {
                BOOST_LOG_SEV(lg(), warn) << "Refusing " << subject << ": " << note;
                auto response = domain::http_response::error(status, std::string(reason));
                co_await write_response(response, version, false, true);
            };

            const auto refuse_too_large = [this,
                                           &refuse](std::uint64_t size) -> asio::awaitable<void> {
                if (counters_ != nullptr)
                    counters_->refused_too_large.fetch_add(1, std::memory_order_relaxed);
                co_await refuse(domain::http_status::payload_too_large,
                                "The request body is larger than this route accepts.",
                                "a body of " + std::to_string(size) + " bytes is past the limit");
            };

            const auto refuse_destination =
                [&refuse](std::string_view note) -> asio::awaitable<void> {
                co_await refuse(domain::http_status::internal_server_error,
                                "The request body could not be stored.",
                                note);
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

            auto domain_req = convert_request(parser.get().base());
            domain_req.path_params = std::move(path_params);

            /*
             * The sink belongs to this request, so a session that keeps its
             * connection holds nothing once the request is answered. A factory
             * that refuses the request says so with invalid_argument, which is a
             * bad request and not a fault; the body is not read, so nothing has
             * been written.
             */
            std::unique_ptr<domain::body_sink> sink;
            std::string refused_request;
            std::string sink_failed;
            if (route && route->sink) {
                try {
                    sink = route->sink(domain_req);
                } catch (const std::invalid_argument& e) {
                    refused_request = e.what();
                } catch (const std::exception& e) {
                    sink_failed = e.what();
                }
                if (sink == nullptr && refused_request.empty() && sink_failed.empty())
                    sink_failed = "the route produced no sink";
            }

            if (!refused_request.empty()) {
                co_await refuse(domain::http_status::bad_request,
                                refused_request,
                                "the route will not take this request");
                break;
            }

            if (!sink_failed.empty()) {
                co_await refuse_destination(sink_failed);
                break;
            }

            domain_req.sink = sink.get();

            /*
             * Every path that does not publish leaves the bytes discarded, and the
             * guard is what makes that true of the refused, the failed and the
             * abandoned request alike. abort() does nothing once published.
             */
            struct discard_unless_published final {
                domain::body_sink* sink;
                ~discard_unless_published() {
                    if (sink != nullptr)
                        sink->abort();
                }
            } discard_guard{sink.get()};

            std::string content;
            std::uint64_t byte_count = 0;
            const auto outcome = co_await read_body(parser, sink.get(), content, byte_count);
            if (outcome != body_outcome::complete) {
                if (outcome == body_outcome::too_large)
                    co_await refuse_too_large(limit);
                else
                    co_await refuse_destination("the body could not be written where it goes");
                break;
            }

            // The body has been consumed, so the message is released for its
            // framing and not for its bytes.
            parser.release();

            domain_req.body = std::move(content);
            domain_req.body_bytes = byte_count;
            if (counters_ != nullptr) {
                counters_->requests.fetch_add(1, std::memory_order_relaxed);
                counters_->request_bytes.fetch_add(byte_count, std::memory_order_relaxed);
            }
            co_await handle_request(std::move(domain_req), std::move(route), keep_alive);
            if (sink != nullptr && !sink->published())
                BOOST_LOG_SEV(lg(), warn)
                    << "The handler for " << subject << " answered without publishing its body";

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

asio::awaitable<http_session::body_outcome>
http_session::read_body(http::request_parser<http::buffer_body>& parser,
                        domain::body_sink* sink,
                        std::string& content,
                        std::uint64_t& byte_count) {
    /*
     * One chunk at a time, so the body costs the chunk and not itself. The buffer
     * belongs to this call, so a session that is idle between requests holds no
     * body memory at all.
     */
    auto chunk = std::vector<char>(options_.body_chunk_size);

    while (!parser.is_done()) {
        auto& view = parser.get().body();
        view.data = chunk.data();
        view.size = chunk.size();

        /*
         * No deadline is set here: the stream arms its own timeout for every
         * operation, so each read gets the full request timeout. That bounds the
         * gap between chunks and not the length of the transfer, which is what a
         * large body needs, and a client that stops sending is closed rather than
         * held.
         */
        try {
            co_await http::async_read_some(stream_, buffer_, parser, asio::use_awaitable);
        } catch (const boost::system::system_error& e) {
            if (e.code() == http::error::need_buffer) {
                // The chunk filled, which is the loop going round again.
            } else if (e.code() == http::error::body_limit) {
                co_return body_outcome::too_large;
            } else {
                throw;
            }
        }

        const auto taken = chunk.size() - view.size;
        byte_count += taken;
        if (taken == 0)
            continue;

        if (sink != nullptr) {
            if (!sink->consume(std::string_view(chunk.data(), taken)))
                co_return body_outcome::destination_failed;
        } else {
            content.append(chunk.data(), taken);
        }
    }

    if (sink != nullptr && !sink->finish())
        co_return body_outcome::destination_failed;

    co_return body_outcome::complete;
}

std::uint64_t http_session::body_limit_for(const std::optional<domain::route>& route) const {
    const auto ceiling = options_.max_body_size;
    const auto fallback = std::min<std::uint64_t>(options_.default_max_body_size, ceiling);
    if (!route)
        return fallback;

    const auto stated =
        route->max_body_size == 0 ? options_.default_max_body_size : route->max_body_size;
    return std::min<std::uint64_t>(stated, ceiling);
}

asio::awaitable<void> http_session::handle_request(domain::http_request domain_req,
                                                   std::optional<domain::route> route,
                                                   bool keep_alive) {

    const auto path = route_path(domain_req.target);
    const auto version =
        static_cast<unsigned>(domain_req.http_version_major * 10 + domain_req.http_version_minor);

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

        co_await write_response(cors_resp, version, keep_alive, true);
        co_return;
    }

    // The route was resolved before the body was read.
    auto matched = std::move(route);

    if (!matched) {
        BOOST_LOG_SEV(lg(), debug)
            << "No route matched for: " << static_cast<int>(domain_req.method) << " " << path;
        auto not_found = domain::http_response::not_found("Endpoint not found");
        co_await write_response(not_found, version, keep_alive, true);
        co_return;
    }

    BOOST_LOG_SEV(lg(), debug) << "Route matched: " << matched->pattern;

    /*
     * One identity, two proofs. A machine client presents a bearer token. A browser
     * presents the cookie a sign-in gave it, and the store says what that session may
     * do: its claims were taken when it signed in, so the store is the authority for
     * it and the token inside is not verified again. The session path is a different
     * proof and not a cache of the first.
     */
    if (matched->requires_auth) {
        std::optional<ores::security::jwt::jwt_claims> claims;
        auto clear_the_cookie = false;

        if (const auto token = domain_req.get_bearer_token(); token.has_value()) {
            /*
             * A route that requires authentication fails closed. If the server
             * cannot verify anybody -- no verifier configured, or the public key
             * failed to load -- it must not serve anybody. There is deliberately no
             * second verifier to fall back to: one way of being believed.
             */
            auto* effective = verifier_.get();
            if (effective == nullptr || !effective->is_configured()) {
                BOOST_LOG_SEV(lg(), error)
                    << "Route requires authentication but no JWT verifier is "
                    << "configured; refusing " << matched->pattern << " from " << remote_address_;
                auto unauthorized = domain::http_response::unauthorized(
                    "Authentication is not configured on this server");
                co_await write_response(unauthorized, version, keep_alive, true);
                co_return;
            }

            auto claims_result = effective->validate(*token);
            if (!claims_result) {
                BOOST_LOG_SEV(lg(), warn)
                    << "Invalid token for " << matched->pattern << " from " << remote_address_
                    << ": " << ores::security::jwt::to_string(claims_result.error());
                auto unauthorized = domain::http_response::unauthorized(
                    ores::security::jwt::to_string(claims_result.error()));
                co_await write_response(unauthorized, version, keep_alive, true);
                co_return;
            }
            claims = std::move(claims_result.value());
        } else if (sessions_ != nullptr) {
            const auto identifier =
                domain::session_cookie::read(domain_req.get_header("cookie"), cookie_);
            if (identifier.has_value()) {
                if (auto record = sessions_->touch(*identifier); record.has_value()) {
                    claims = std::move(record->claims);
                } else {
                    /*
                     * The browser holds a session this server no longer has, so the
                     * cookie is worthless and telling the browser to drop it is what
                     * turns a refusal into a sign-in page rather than a loop.
                     */
                    clear_the_cookie = true;
                }
            }
        }

        if (!claims.has_value()) {
            BOOST_LOG_SEV(lg(), warn)
                << "No usable credential for " << matched->pattern << " from " << remote_address_;
            auto unauthorized = domain::http_response::unauthorized(
                "A bearer token or a session cookie is required");
            if (clear_the_cookie)
                unauthorized.set_header("Set-Cookie", domain::session_cookie::clear(cookie_));
            co_await write_response(unauthorized, version, keep_alive, true);
            co_return;
        }

        domain_req.authenticated_user = std::move(*claims);
        BOOST_LOG_SEV(lg(), debug)
            << "Authenticated user: " << domain_req.authenticated_user->subject;

        if (!matched->required_roles.empty()) {
            auto has_required_role = false;
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
                co_await write_response(forbidden, version, keep_alive, true);
                co_return;
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

    co_await write_response(
        response, version, keep_alive, domain_req.method != domain::http_method::head);

    // Update session bytes if we have an authenticated user with session info
    if (bytes_callback_ && domain_req.authenticated_user &&
        domain_req.authenticated_user->session_id &&
        domain_req.authenticated_user->session_start_time) {
        try {
            bytes_callback_(*domain_req.authenticated_user->session_id,
                            *domain_req.authenticated_user->session_start_time,
                            response.body.size(),
                            domain_req.sink != nullptr ? domain_req.body_bytes :
                                                         domain_req.body.size());
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "Failed to update session bytes: " << e.what();
        }
    }
}

domain::http_request http_session::convert_request(const http::header<true>& head) {

    domain::http_request result;
    result.method = convert_method(head.method());
    result.target = std::string(head.target());
    result.remote_address = remote_address_;

    // Extract HTTP version (e.g., 11 = HTTP/1.1, 20 = HTTP/2)
    auto version = head.version();
    result.http_version_major = static_cast<std::uint16_t>(version / 10);
    result.http_version_minor = static_cast<std::uint16_t>(version % 10);

    // Copy headers (normalize keys to lowercase for case-insensitive lookup per RFC 7230)
    for (const auto& field : head) {
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

asio::awaitable<void> http_session::write_response(const domain::http_response& resp,
                                                   unsigned version,
                                                   bool keep_alive,
                                                   bool send_body) {
    if (counters_ != nullptr) {
        counters_->record_response(static_cast<unsigned>(resp.status));
        const auto bytes =
            resp.body_stream != nullptr ? resp.body_stream->size().value_or(0) : resp.body.size();
        counters_->response_bytes.fetch_add(bytes, std::memory_order_relaxed);
    }

    if (resp.body_stream == nullptr) {
        auto beast_resp = convert_response(resp, version, keep_alive);
        co_await http::async_write(stream_, beast_resp, asio::use_awaitable);
        co_return;
    }

    /*
     * The headers go out first and the body follows from the source, so a file is
     * never held and a stream can be written while it is still being produced. A
     * source that states its length gets Content-Length; one that does not gets
     * chunked framing, which is how the client knows where the body ends.
     */
    http::response<http::buffer_body> head;
    head.version(version);
    head.result(static_cast<http::status>(static_cast<unsigned>(resp.status)));
    head.set(http::field::content_type, resp.content_type);
    head.keep_alive(keep_alive);
    for (const auto& [name, value] : resp.headers)
        head.set(name, value);

    const auto declared = resp.body_stream->size();
    if (declared.has_value())
        head.content_length(*declared);
    else
        head.chunked(true);

    http::response_serializer<http::buffer_body> serializer{head};
    co_await http::async_write_header(stream_, serializer, asio::use_awaitable);

    if (!send_body)
        co_return;

    auto buffer = std::vector<char>(options_.response_chunk_size);
    for (;;) {
        const auto taken = resp.body_stream->read(std::span(buffer.data(), buffer.size()));
        // The serializer reads the message it was given, so the body is set there.
        auto& body = head.body();
        body.data = buffer.data();
        body.size = taken;
        body.more = taken != 0;

        /*
         * A source that fails here cannot become a status: the headers are already
         * on the wire. The exception leaves the session, which closes the
         * connection, so the client sees a body shorter than its length rather than
         * waiting for one that will never arrive.
         */
        try {
            co_await http::async_write(stream_, serializer, asio::use_awaitable);
        } catch (const boost::system::system_error& e) {
            /*
             * A buffer body has no more to give when it has given what it holds,
             * which Beast reports as need_buffer and not as a failure.
             */
            if (e.code() != http::error::need_buffer)
                throw;
        }
        if (taken == 0)
            break;
    }
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
