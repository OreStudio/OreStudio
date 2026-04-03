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
#ifndef ORES_HTTP_SERVER_ROUTES_STORAGE_ROUTES_HPP
#define ORES_HTTP_SERVER_ROUTES_STORAGE_ROUTES_HPP

#include <memory>
#include <string>
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::http_server::routes {

/**
 * @brief Generic S3-like object storage HTTP endpoints.
 *
 * Registers four operations on every object in the storage hierarchy:
 *
 *   PUT    /api/v1/storage/{bucket}/{key}   upload (create or replace)
 *   GET    /api/v1/storage/{bucket}/{key}   download
 *   DELETE /api/v1/storage/{bucket}/{key}   delete object
 *   HEAD   /api/v1/storage/{bucket}/{key}   check existence / get size
 *
 * The {key} segment may contain slashes, enabling hierarchical keys such
 * as "workunit-id/input.tar.gz". Buckets map to subdirectories of the
 * configured storage root; unknown buckets receive a 404 response.
 *
 * Well-known bucket names are defined in ores.storage::net::buckets.
 */
class storage_routes final {
public:
    explicit storage_routes(std::string storage_dir);

    /**
     * @brief Registers all storage routes with the router.
     */
    void register_routes(std::shared_ptr<http::net::router> router,
        std::shared_ptr<http::openapi::endpoint_registry> registry);

private:
    inline static std::string_view logger_name =
        "ores.http.server.routes.storage_routes";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    boost::asio::awaitable<http::domain::http_response>
    handle_put(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_get(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_delete(const http::domain::http_request& req);

    /**
     * @brief Resolves bucket + key to an absolute filesystem path and
     *        validates that the bucket is known.
     *
     * Returns an empty string if the bucket is unknown (caller should
     * respond with 404).
     */
    std::string resolve_path(const std::string& bucket,
        const std::string& key) const;

    static std::string read_file(const std::string& path);
    static void write_file(const std::string& path, const std::string& data);
    static void delete_file(const std::string& path);

    std::string storage_dir_;
};

}

#endif
