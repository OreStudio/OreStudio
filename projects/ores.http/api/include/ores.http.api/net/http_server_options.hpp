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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_options_header.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_HTTP_API_NET_HTTP_SERVER_OPTIONS_HPP
#define ORES_HTTP_API_NET_HTTP_SERVER_OPTIONS_HPP

#include "ores.http.api/export.hpp"
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <chrono>
#include <cstdint>
#include <iosfwd>
#include <string>
#include <tuple>

namespace ores::http::net {

/**
 * @brief Configuration options for the HTTP server.
 */
struct http_server_options final {
    /**
     * @brief Address to bind to (default: 0.0.0.0).
     */
    std::string address{"0.0.0.0"};
    /**
     * @brief Port to listen on.
     */
    std::uint16_t port{8080};
    /**
     * @brief Maximum number of concurrent connections.
     */
    std::uint32_t max_connections{100};
    /**
     * @brief Request timeout duration.
     */
    std::chrono::seconds request_timeout{30};
    /**
     * @brief The body limit a route gets when it states none.
     *
     * A JSON request is small. A route that accepts an upload states its own
     * limit with max_body(); everything else refuses a larger body with 413
     * rather than reading it.
     */
    std::uint64_t default_max_body_size{1024 * 1024};
    /**
     * @brief Maximum accepted request body size, in bytes.
     *
     * Beast's default body_limit for a plain (non-parser) http::read is
     * 1MB, far below the size of a compute engine package upload
     * (tens of MB) -- storage_routes' PUT handler needs this raised.
     */
    std::uint64_t max_body_size{256 * 1024 * 1024};
    /**
     * @brief How much of a streamed body is read at a time, in bytes.
     *
     * The chunk is the whole memory bound of a streamed body: the session holds
     * one of these, hands it on, and reads the next.
     */
    std::size_t body_chunk_size{64 * 1024};
    /**
     * @brief How much of a streamed response is written at a time, in bytes.
     *
     * The write buffer is the response's memory bound: a body read from a source is
     * written one of these at a time, never held whole.
     */
    std::size_t response_chunk_size{64 * 1024};
    /**
     * @brief Maximum accepted size of the request headers, in bytes.
     *
     * Beast's default header_limit is 8KB. The access token a session sends in the
     * Authorization header carries the session's parties and permissions, so a person
     * who acts for many parties holds a token past 8KB, and the server closed the
     * connection before it read the request. The limit is raised so such a token is
     * read, and it stays bounded so a header cannot grow without limit.
     */
    std::uint32_t max_header_size{64 * 1024};
    /**
     * @brief Threads that run a route handler.
     *
     * A handler forwards to a service with a synchronous request, so running it
     * on the connection's thread stalls every other connection for the length of
     * one downstream answer. The handler runs here instead, and the connection
     * stays on its own executor. Zero selects the default of eight.
     */
    std::size_t worker_threads{0};
    /**
     * @brief Handlers the pool accepts at once.
     *
     * A slow service must cost a refusal once this is reached, not unbounded
     * queueing: the request is answered 503 and each worker keeps a bounded
     * share. Zero removes the cap, which is only appropriate for a test.
     */
    std::size_t max_inflight_requests{1024};
    /**
     * @brief How often a session's byte totals are written.
     *
     * The totals are kept in memory and written once per interval, from a
     * worker, so a request never waits on the database for a counter.
     */
    std::chrono::seconds bytes_flush_interval{5};
    /**
     * @brief How long a quiet session keeps its totals in memory.
     *
     * Bounds what a long-lived process holds. A session that comes back after
     * this window starts its total again from that point.
     */
    std::chrono::seconds bytes_retain{3600};
    /**
     * @brief Whether to enable SSL/TLS.
     */
    bool enable_ssl{false};
    /**
     * @brief Path to SSL certificate file.
     */
    std::string certificate_file;
    /**
     * @brief Path to SSL private key file.
     */
    std::string private_key_file;
    /**
     * @brief Enable CORS support.
     */
    bool enable_cors{true};
    /**
     * @brief Allowed origins for CORS (empty = all).
     */
    std::string cors_allowed_origins{"*"};
    /**
     * @brief Server identifier for responses.
     */
    std::string server_identifier{"ores-http-server"};
    /**
     * @brief Whether every route runs under a rate policy. Only a test turns it off.
     */
    bool rate_limit_enabled{true};
    /**
     * @brief The requests one client address may make in a minute to a route that needs no
     * session, and the burst it may spend at once.
     */
    std::uint32_t open_requests_per_minute{300};
    /**
     * @brief The requests one account may make in a minute to a route that needs a session.
     * A screen asks for many reads as it opens, so the burst is generous.
     */
    std::uint32_t authenticated_requests_per_minute{1200};
    /**
     * @brief The sign-in and sign-up attempts one client address may make in a minute, one
     * budget shared by both, as the BFF counted them.
     */
    std::uint32_t credential_attempts_per_minute{10};
    /**
     * @brief The registration policy reads one client address may make in a minute.
     */
    std::uint32_t registration_policy_reads_per_minute{120};
    /**
     * @brief The most clients and accounts the limiter tracks at once. Past it, the bucket
     * closest to full is dropped, which forgets the least.
     */
    std::size_t rate_limit_max_keys{65536};
    /**
     * @brief The proxies whose forwarded address names the client, as a comma separated
     * list of networks such as 127.0.0.1/32,10.0.0.0/8. Empty trusts none, so the
     * client is always the socket's peer.
     */
    std::string trusted_proxies{""};
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<http_server_options>) {
    return ::ores::utility::codec::field_list{
        ::ores::utility::codec::field{"address", &http_server_options::address},
        ::ores::utility::codec::field{"port", &http_server_options::port},
        ::ores::utility::codec::field{"max_connections", &http_server_options::max_connections},
        ::ores::utility::codec::field{"request_timeout", &http_server_options::request_timeout},
        ::ores::utility::codec::field{"default_max_body_size",
                                      &http_server_options::default_max_body_size},
        ::ores::utility::codec::field{"max_body_size", &http_server_options::max_body_size},
        ::ores::utility::codec::field{"body_chunk_size", &http_server_options::body_chunk_size},
        ::ores::utility::codec::field{"response_chunk_size",
                                      &http_server_options::response_chunk_size},
        ::ores::utility::codec::field{"max_header_size", &http_server_options::max_header_size},
        ::ores::utility::codec::field{"worker_threads", &http_server_options::worker_threads},
        ::ores::utility::codec::field{"max_inflight_requests",
                                      &http_server_options::max_inflight_requests},
        ::ores::utility::codec::field{"bytes_flush_interval",
                                      &http_server_options::bytes_flush_interval},
        ::ores::utility::codec::field{"bytes_retain", &http_server_options::bytes_retain},
        ::ores::utility::codec::field{"enable_ssl", &http_server_options::enable_ssl},
        ::ores::utility::codec::field{"certificate_file", &http_server_options::certificate_file},
        ::ores::utility::codec::field{"private_key_file", &http_server_options::private_key_file},
        ::ores::utility::codec::field{"enable_cors", &http_server_options::enable_cors},
        ::ores::utility::codec::field{"cors_allowed_origins",
                                      &http_server_options::cors_allowed_origins},
        ::ores::utility::codec::field{"server_identifier", &http_server_options::server_identifier},
        ::ores::utility::codec::field{"rate_limit_enabled",
                                      &http_server_options::rate_limit_enabled},
        ::ores::utility::codec::field{"open_requests_per_minute",
                                      &http_server_options::open_requests_per_minute},
        ::ores::utility::codec::field{"authenticated_requests_per_minute",
                                      &http_server_options::authenticated_requests_per_minute},
        ::ores::utility::codec::field{"credential_attempts_per_minute",
                                      &http_server_options::credential_attempts_per_minute},
        ::ores::utility::codec::field{"registration_policy_reads_per_minute",
                                      &http_server_options::registration_policy_reads_per_minute},
        ::ores::utility::codec::field{"rate_limit_max_keys",
                                      &http_server_options::rate_limit_max_keys},
        ::ores::utility::codec::field{"trusted_proxies", &http_server_options::trusted_proxies},
    };
}

static_assert(::ores::utility::codec::names_every_member<http_server_options>(),
              "http_server_options has a member its wire shape does not name");

ORES_HTTP_API_EXPORT std::ostream& operator<<(std::ostream& s, const http_server_options& v);


}

#endif
