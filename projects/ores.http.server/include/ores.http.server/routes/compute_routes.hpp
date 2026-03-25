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
#ifndef ORES_HTTP_SERVER_ROUTES_COMPUTE_ROUTES_HPP
#define ORES_HTTP_SERVER_ROUTES_COMPUTE_ROUTES_HPP

#include <memory>
#include <string>
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::http_server::routes {

/**
 * @brief Registers compute grid HTTP endpoints.
 *
 * Provides file storage for the distributed compute grid:
 *
 * Package management (admin uploads once; wrappers download per job):
 * - POST /api/v1/compute/packages/{app_version_id}   upload package bundle
 * - GET  /api/v1/compute/packages/{app_version_id}   download package bundle
 *
 * Workunit input data (uploaded when workunit is created):
 * - POST /api/v1/compute/workunits/{workunit_id}/input   upload input
 * - GET  /api/v1/compute/workunits/{workunit_id}/input   download input
 * - POST /api/v1/compute/workunits/{workunit_id}/config  upload config
 * - GET  /api/v1/compute/workunits/{workunit_id}/config  download config
 *
 * Result output (wrapper uploads when job finishes):
 * - PUT  /api/v1/compute/results/{result_id}/output   upload output
 * - GET  /api/v1/compute/results/{result_id}/output   download output
 */
class compute_routes final {
public:
    explicit compute_routes(std::string storage_dir);

    /**
     * @brief Registers all compute routes with the router.
     */
    void register_routes(std::shared_ptr<http::net::router> router,
        std::shared_ptr<http::openapi::endpoint_registry> registry);

private:
    inline static std::string_view logger_name =
        "ores.http.server.routes.compute_routes";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    boost::asio::awaitable<http::domain::http_response>
    handle_get_package(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_post_package(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_get_workunit_input(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_post_workunit_input(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_get_workunit_config(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_post_workunit_config(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_put_result_output(const http::domain::http_request& req);

    boost::asio::awaitable<http::domain::http_response>
    handle_get_result_output(const http::domain::http_request& req);

    /**
     * @brief Reads all bytes from a file into a string.
     */
    static std::string read_file(const std::string& path);

    /**
     * @brief Writes a string to a file, creating parent directories as needed.
     */
    static void write_file(const std::string& path, const std::string& data);

    std::string storage_dir_;
};

}

#endif
