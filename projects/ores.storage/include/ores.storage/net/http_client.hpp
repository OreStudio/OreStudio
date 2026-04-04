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

#include <string>
#include <filesystem>

namespace ores::storage::net {

/**
 * @brief Synchronous HTTP client for storage file transfers.
 *
 * Uses Boost Beast for HTTP/1.1 GET and PUT over plain TCP.
 * All operations are synchronous and intended for use on background threads.
 *
 * URL format: http://host:port/path
 */
class http_client {
public:
    /**
     * @brief Downloads a remote resource to a local file via HTTP GET.
     *
     * @param url   Full URL of the resource (http://host:port/path)
     * @param dest  Local path where the downloaded content will be written
     * @throws std::runtime_error on connection, HTTP, or I/O failure
     */
    static void get(const std::string& url, const std::filesystem::path& dest);

    /**
     * @brief Uploads a local file to a remote URL via HTTP PUT.
     *
     * @param url  Full URL of the destination (http://host:port/path)
     * @param src  Local path of the file to upload
     * @throws std::runtime_error on connection, HTTP, or I/O failure
     */
    static void put(const std::string& url, const std::filesystem::path& src);

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
