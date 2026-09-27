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
#ifndef ORES_STORAGE_NET_HTTP_CLIENT_HPP
#define ORES_STORAGE_NET_HTTP_CLIENT_HPP

#include "ores.storage.core/export.hpp"
#include <filesystem>
#include <string>

namespace ores::storage::net {

/**
 * @brief Synchronous HTTP client for storage file transfers.
 *
 * Uses Boost Beast for HTTP/1.1 GET and PUT over plain TCP.
 * All operations are synchronous and intended for use on background threads.
 *
 * URL format: http://host:port/path
 *
 * Every call states the caller's bearer token and sends it as
 * =Authorization: Bearer <token>=. The storage routes authenticate, so the
 * token is the calling session's own credential rather than the client's: the
 * service validates that caller and its own permission check governs. The
 * parameter is required, but an empty token is accepted and sent as-is. The
 * service then refuses the request with 401. The token is a parameter and not
 * a member because one client instance serves whichever caller invokes it.
 */
class ORES_STORAGE_CORE_EXPORT http_client {
public:
    /**
     * @brief Downloads a remote resource to a local file via HTTP GET.
     *
     * @param url           Full URL of the resource (http://host:port/path)
     * @param dest          Local path where the downloaded content will be written
     * @param bearer_token  The caller's bearer token, without the "Bearer " prefix
     * @throws std::runtime_error on connection, HTTP, or I/O failure
     */
    static void
    get(const std::string& url, const std::filesystem::path& dest, const std::string& bearer_token);

    /**
     * @brief Uploads a local file to a remote URL via HTTP PUT.
     *
     * @param url           Full URL of the destination (http://host:port/path)
     * @param src           Local path of the file to upload
     * @param bearer_token  The caller's bearer token, without the "Bearer " prefix
     * @throws std::runtime_error on connection, HTTP, or I/O failure
     */
    static void
    put(const std::string& url, const std::filesystem::path& src, const std::string& bearer_token);

    /**
     * @brief Uploads a local file via HTTP PUT, returning the response body.
     *
     * Same wire behaviour as put(), but returns the server's response body
     * instead of discarding it -- e.g. so callers can read a server-computed
     * checksum out of a JSON response.
     *
     * @param url           Full URL of the destination (http://host:port/path)
     * @param src           Local path of the file to upload
     * @param bearer_token  The caller's bearer token, without the "Bearer " prefix
     * @return              The response body.
     * @throws std::runtime_error on connection, HTTP, or I/O failure
     */
    static std::string put_returning_body(const std::string& url,
                                          const std::filesystem::path& src,
                                          const std::string& bearer_token);

    /**
     * @brief Downloads a remote resource into memory via HTTP GET.
     *
     * Same wire behaviour as get(), but returns the response body instead of
     * writing it to a file -- e.g. so a caller can read a listing document.
     *
     * @param url           Full URL of the resource (http://host:port/path)
     * @param bearer_token  The caller's bearer token, without the "Bearer " prefix
     * @return              The response body.
     * @throws std::runtime_error on connection, HTTP, or I/O failure
     */
    static std::string get_returning_body(const std::string& url,
                                         const std::string& bearer_token);

    /**
     * @brief Deletes a remote resource via HTTP DELETE, returning the response
     * body.
     *
     * The body carries the server's answer, so a caller can tell a removal from
     * a key that held nothing.
     *
     * @param url           Full URL of the resource (http://host:port/path)
     * @param bearer_token  The caller's bearer token, without the "Bearer " prefix
     * @return              The response body.
     * @throws std::runtime_error on connection, HTTP, or I/O failure
     */
    static std::string del(const std::string& url, const std::string& bearer_token);

private:
    struct url_parts {
        std::string host;
        std::string port;
        std::string path;
    };

    static url_parts parse_url(const std::string& url);
};

}

#endif
