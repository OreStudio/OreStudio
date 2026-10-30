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
#ifndef ORES_HTTP_NET_HTTP_SERVER_OPTIONS_HPP
#define ORES_HTTP_NET_HTTP_SERVER_OPTIONS_HPP

#include "ores.http.api/export.hpp"
#include <chrono>
#include <cstdint>
#include <iosfwd>
#include <string>

namespace ores::http::net {

/**
 * @brief Configuration options for the HTTP server.
 */
struct http_server_options final {
    /**
     * @brief Address to bind to (default: 0.0.0.0).
     */
    std::string address = "0.0.0.0";

    /**
     * @brief Port to listen on.
     */
    std::uint16_t port = 8080;

    /**
     * @brief Maximum number of concurrent connections.
     */
    std::uint32_t max_connections = 100;

    /**
     * @brief Request timeout duration.
     */
    std::chrono::seconds request_timeout{30};

    /**
     * @brief Maximum accepted request body size, in bytes.
     *
     * Beast's default body_limit for a plain (non-parser) http::read is
     * 1MB, far below the size of a compute engine package upload
     * (tens of MB) -- storage_routes' PUT handler needs this raised.
     */
    std::uint64_t max_body_size = 256 * 1024 * 1024;

    /**
     * @brief Maximum accepted size of the request headers, in bytes.
     *
     * Beast's default header_limit is 8KB. The access token a session sends in the
     * Authorization header carries the session's parties and permissions, so a person
     * who acts for many parties holds a token past 8KB, and the server closed the
     * connection before it read the request. The limit is raised so such a token is
     * read, and it stays bounded so a header cannot grow without limit.
     */
    std::uint32_t max_header_size = 64 * 1024;

    /**
     * @brief Threads that run a route handler.
     *
     * A handler forwards to a service with a synchronous request, so running it
     * on the connection's thread stalls every other connection for the length of
     * one downstream answer. The handler runs here instead, and the connection
     * stays on its own executor. Zero selects the default of eight.
     */
    std::size_t worker_threads = 0;

    /**
     * @brief Handlers the pool accepts at once.
     *
     * A slow service must cost a refusal once this is reached, not unbounded
     * queueing: the request is answered 503 and each worker keeps a bounded
     * share. Zero removes the cap, which is only appropriate for a test.
     */
    std::size_t max_inflight_requests = 1024;

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
    bool enable_ssl = false;

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
    bool enable_cors = true;

    /**
     * @brief Allowed origins for CORS (empty = all).
     */
    std::string cors_allowed_origins = "*";

    /**
     * @brief Server identifier for responses.
     */
    std::string server_identifier = "ores-http-server";
};

ORES_HTTP_API_EXPORT std::ostream& operator<<(std::ostream& s, const http_server_options& v);

}

#endif
