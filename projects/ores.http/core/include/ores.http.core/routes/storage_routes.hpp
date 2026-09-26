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

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http.core/export.hpp"
#include "ores.iam.core/service/authorization_service.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.storage.core/filesystem/local_store.hpp"
#include <boost/uuid/uuid.hpp>
#include <expected>
#include <memory>
#include <string>
#include <string_view>

namespace ores::http_server::routes {

/**
 * @brief Generic S3-like object storage HTTP endpoints.
 *
 * Registers five operations over the storage hierarchy:
 *
 *   PUT    /api/v1/storage/{bucket}/{key}   upload (create or replace)
 *   GET    /api/v1/storage/{bucket}/{key}   download
 *   HEAD   /api/v1/storage/{bucket}/{key}   existence and length, no body
 *   DELETE /api/v1/storage/{bucket}/{key}   delete object
 *   GET    /api/v1/storage/{bucket}         list a page, filtered by prefix
 *
 * The {key} segment may contain slashes, enabling hierarchical keys such
 * as "workunit-id/input.tar.gz". Buckets map to subdirectories of the
 * configured storage root.
 *
 * Every route authenticates before it acts, and every operation checks its own
 * permission code, so a read is never authorised by the code a write uses.
 *
 * The bytes themselves are not handled here: the routes resolve, guard and
 * store through =ores.storage.core='s local store, which is the same code the
 * NATS handler calls. That is what keeps the two interfaces from drifting in
 * how they treat a key, a bucket or a missing object.
 */
class ORES_HTTP_CORE_EXPORT storage_routes final {
public:
    storage_routes(std::string storage_dir,
                   std::shared_ptr<iam::service::authorization_service> auth_service);

    /**
     * @brief Registers all storage routes with the router.
     */
    void register_routes(std::shared_ptr<http::net::router> router,
                         std::shared_ptr<http::openapi::endpoint_registry> registry);

private:
    inline static std::string_view logger_name = "ores.http.server.routes.storage_routes";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    /**
     * @brief The caller's identity, once a route has admitted it.
     */
    struct auth_result {
        boost::uuids::uuid account_id;
    };

    /**
     * @brief Authenticates the caller and checks one operation's permission.
     *
     * Returns the response to send when either step fails, so a handler can
     * co_return it unchanged.
     */
    std::expected<auth_result, http::domain::http_response>
    check_auth(const http::domain::http_request& req,
               std::string_view required_permission,
               std::string_view operation_name);

    boost::asio::awaitable<http::domain::http_response>
    handle_put(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_get(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_head(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_delete(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_list(const http::domain::http_request& req);

    storage::filesystem::local_store store_;
    std::shared_ptr<iam::service::authorization_service> auth_service_;
};

}

#endif
