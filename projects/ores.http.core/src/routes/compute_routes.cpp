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
#include "ores.http.core/routes/compute_routes.hpp"

#include <fstream>
#include <filesystem>
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"

namespace ores::http_server::routes {

using namespace ores::logging;
using namespace ores::http::domain;
namespace asio = boost::asio;
namespace fs = std::filesystem;

compute_routes::compute_routes(std::string storage_dir)
    : storage_dir_(std::move(storage_dir)) {
    BOOST_LOG_SEV(lg(), debug) << "Compute routes storage dir: " << storage_dir_;
}

void compute_routes::register_routes(std::shared_ptr<http::net::router> router,
    std::shared_ptr<http::openapi::endpoint_registry> /*registry*/) {

    BOOST_LOG_SEV(lg(), info) << "Registering compute routes";

    // Package bundle routes
    router->add_route(router->get("/api/v1/compute/packages/{app_version_id}")
        .summary("Download compute package")
        .description("Download the wrapper+engine bundle for a given app version")
        .tags({"compute"})
        .handler([this](const http_request& req) {
            return handle_get_package(req);
        }).build());

    router->add_route(router->post("/api/v1/compute/packages/{app_version_id}")
        .summary("Upload compute package")
        .description("Upload a wrapper+engine bundle for a given app version")
        .tags({"compute"})
        .handler([this](const http_request& req) {
            return handle_post_package(req);
        }).build());

    // Workunit input routes
    router->add_route(router->get("/api/v1/compute/workunits/{workunit_id}/input")
        .summary("Download workunit input")
        .description("Download the input data bundle for a workunit")
        .tags({"compute"})
        .handler([this](const http_request& req) {
            return handle_get_workunit_input(req);
        }).build());

    router->add_route(router->post("/api/v1/compute/workunits/{workunit_id}/input")
        .summary("Upload workunit input")
        .description("Upload the input data bundle for a workunit")
        .tags({"compute"})
        .handler([this](const http_request& req) {
            return handle_post_workunit_input(req);
        }).build());

    // Workunit config routes
    router->add_route(router->get("/api/v1/compute/workunits/{workunit_id}/config")
        .summary("Download workunit config")
        .description("Download the engine config file for a workunit")
        .tags({"compute"})
        .handler([this](const http_request& req) {
            return handle_get_workunit_config(req);
        }).build());

    router->add_route(router->post("/api/v1/compute/workunits/{workunit_id}/config")
        .summary("Upload workunit config")
        .description("Upload the engine config file for a workunit")
        .tags({"compute"})
        .handler([this](const http_request& req) {
            return handle_post_workunit_config(req);
        }).build());

    // Result output routes
    router->add_route(router->put("/api/v1/compute/results/{result_id}/output")
        .summary("Upload result output")
        .description("Upload the job output file from a compute wrapper")
        .tags({"compute"})
        .handler([this](const http_request& req) {
            return handle_put_result_output(req);
        }).build());

    router->add_route(router->get("/api/v1/compute/results/{result_id}/output")
        .summary("Download result output")
        .description("Download the completed job output file")
        .tags({"compute"})
        .handler([this](const http_request& req) {
            return handle_get_result_output(req);
        }).build());

    BOOST_LOG_SEV(lg(), info) << "Compute routes registered: 8 endpoints";
}

std::string compute_routes::read_file(const std::string& path) {
    std::ifstream f(path, std::ios::binary);
    if (!f)
        throw std::runtime_error("File not found: " + path);
    return std::string(std::istreambuf_iterator<char>(f),
                       std::istreambuf_iterator<char>());
}

void compute_routes::write_file(const std::string& path, const std::string& data) {
    fs::create_directories(fs::path(path).parent_path());
    std::ofstream f(path, std::ios::binary | std::ios::trunc);
    if (!f)
        throw std::runtime_error("Cannot open file for writing: " + path);
    f.write(data.data(), static_cast<std::streamsize>(data.size()));
}

asio::awaitable<http_response>
compute_routes::handle_get_package(const http_request& req) {
    const auto id = req.get_path_param("app_version_id");
    BOOST_LOG_SEV(lg(), debug) << "GET package: " << id;

    try {
        const auto path = storage_dir_ + "/packages/" + id;
        auto data = read_file(path);
        http_response resp;
        resp.status = http_status::ok;
        resp.content_type = "application/octet-stream";
        resp.body = std::move(data);
        co_return resp;
    } catch (const std::runtime_error&) {
        co_return http_response::not_found("Package not found: " + id);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "GET package error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response>
compute_routes::handle_post_package(const http_request& req) {
    const auto id = req.get_path_param("app_version_id");
    BOOST_LOG_SEV(lg(), debug) << "POST package: " << id;

    try {
        const auto path = storage_dir_ + "/packages/" + id;
        write_file(path, req.body);
        co_return http_response::json(R"({"success":true})");
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "POST package error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response>
compute_routes::handle_get_workunit_input(const http_request& req) {
    const auto id = req.get_path_param("workunit_id");
    BOOST_LOG_SEV(lg(), debug) << "GET workunit input: " << id;

    try {
        const auto path = storage_dir_ + "/workunits/" + id + "/input";
        auto data = read_file(path);
        http_response resp;
        resp.status = http_status::ok;
        resp.content_type = "application/octet-stream";
        resp.body = std::move(data);
        co_return resp;
    } catch (const std::runtime_error&) {
        co_return http_response::not_found("Input not found for workunit: " + id);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "GET workunit input error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response>
compute_routes::handle_post_workunit_input(const http_request& req) {
    const auto id = req.get_path_param("workunit_id");
    BOOST_LOG_SEV(lg(), debug) << "POST workunit input: " << id;

    try {
        const auto path = storage_dir_ + "/workunits/" + id + "/input";
        write_file(path, req.body);
        co_return http_response::json(R"({"success":true})");
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "POST workunit input error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response>
compute_routes::handle_get_workunit_config(const http_request& req) {
    const auto id = req.get_path_param("workunit_id");
    BOOST_LOG_SEV(lg(), debug) << "GET workunit config: " << id;

    try {
        const auto path = storage_dir_ + "/workunits/" + id + "/config";
        auto data = read_file(path);
        http_response resp;
        resp.status = http_status::ok;
        resp.content_type = "application/octet-stream";
        resp.body = std::move(data);
        co_return resp;
    } catch (const std::runtime_error&) {
        co_return http_response::not_found("Config not found for workunit: " + id);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "GET workunit config error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response>
compute_routes::handle_post_workunit_config(const http_request& req) {
    const auto id = req.get_path_param("workunit_id");
    BOOST_LOG_SEV(lg(), debug) << "POST workunit config: " << id;

    try {
        const auto path = storage_dir_ + "/workunits/" + id + "/config";
        write_file(path, req.body);
        co_return http_response::json(R"({"success":true})");
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "POST workunit config error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response>
compute_routes::handle_put_result_output(const http_request& req) {
    const auto id = req.get_path_param("result_id");
    BOOST_LOG_SEV(lg(), debug) << "PUT result output: " << id;

    try {
        const auto path = storage_dir_ + "/results/" + id + "/output";
        write_file(path, req.body);
        co_return http_response::json(R"({"success":true})");
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "PUT result output error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response>
compute_routes::handle_get_result_output(const http_request& req) {
    const auto id = req.get_path_param("result_id");
    BOOST_LOG_SEV(lg(), debug) << "GET result output: " << id;

    try {
        const auto path = storage_dir_ + "/results/" + id + "/output";
        auto data = read_file(path);
        http_response resp;
        resp.status = http_status::ok;
        resp.content_type = "application/octet-stream";
        resp.body = std::move(data);
        co_return resp;
    } catch (const std::runtime_error&) {
        co_return http_response::not_found("Output not found for result: " + id);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "GET result output error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

}
