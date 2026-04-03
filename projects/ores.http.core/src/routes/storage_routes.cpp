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
#include "ores.http.core/routes/storage_routes.hpp"

#include <set>
#include <fstream>
#include <filesystem>
#include "ores.storage/net/buckets.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"

namespace ores::http_server::routes {

using namespace ores::logging;
using namespace ores::http::domain;
namespace asio = boost::asio;
namespace fs   = std::filesystem;
using buckets  = ores::storage::net::buckets;

// All bucket names accepted by this server. Unknown buckets return 404.
static const std::set<std::string> k_known_buckets {
    std::string(buckets::compute_packages),
    std::string(buckets::compute_inputs),
    std::string(buckets::compute_outputs),
    std::string(buckets::ore_imports),
};

storage_routes::storage_routes(std::string storage_dir)
    : storage_dir_(std::move(storage_dir)) {
    BOOST_LOG_SEV(lg(), debug) << "Storage routes dir: " << storage_dir_;
    for (const auto& bucket : k_known_buckets) {
        const auto dir = fs::path(storage_dir_) / bucket;
        fs::create_directories(dir);
        BOOST_LOG_SEV(lg(), debug) << "Storage bucket ready: " << dir.string();
    }
}

void storage_routes::register_routes(std::shared_ptr<http::net::router> router,
    std::shared_ptr<http::openapi::endpoint_registry> /*registry*/) {

    BOOST_LOG_SEV(lg(), info) << "Registering storage routes";

    // The {key} path parameter may contain slashes (e.g. "workunit-id/input.tar.gz").
    // The router captures everything after {bucket}/ as the key.
    router->add_route(router->put("/api/v1/storage/{bucket}/{key}")
        .summary("Upload storage object")
        .description("Upload an object into the named bucket under the given key")
        .tags({"storage"})
        .handler([this](const http_request& req) { return handle_put(req); })
        .build());

    router->add_route(router->get("/api/v1/storage/{bucket}/{key}")
        .summary("Download storage object")
        .description("Download an object from the named bucket by key")
        .tags({"storage"})
        .handler([this](const http_request& req) { return handle_get(req); })
        .build());

    router->add_route(router->delete_("/api/v1/storage/{bucket}/{key}")
        .summary("Delete storage object")
        .description("Delete an object from the named bucket by key")
        .tags({"storage"})
        .handler([this](const http_request& req) { return handle_delete(req); })
        .build());

    BOOST_LOG_SEV(lg(), info) << "Storage routes registered: 3 endpoints";
}

std::string storage_routes::resolve_path(const std::string& bucket,
    const std::string& key) const {
    if (k_known_buckets.find(bucket) == k_known_buckets.end())
        return {};
    return (fs::path(storage_dir_) / bucket / key).string();
}

std::string storage_routes::read_file(const std::string& path) {
    std::ifstream f(path, std::ios::binary);
    if (!f)
        throw std::runtime_error("Object not found: " + path);
    return std::string(std::istreambuf_iterator<char>(f),
                       std::istreambuf_iterator<char>());
}

void storage_routes::write_file(const std::string& path,
    const std::string& data) {
    fs::create_directories(fs::path(path).parent_path());
    std::ofstream f(path, std::ios::binary | std::ios::trunc);
    if (!f)
        throw std::runtime_error("Cannot write object: " + path);
    f.write(data.data(), static_cast<std::streamsize>(data.size()));
}

void storage_routes::delete_file(const std::string& path) {
    std::error_code ec;
    fs::remove(path, ec);
    if (ec && ec != std::errc::no_such_file_or_directory)
        throw std::runtime_error("Cannot delete object: " + path + ": " + ec.message());
}

asio::awaitable<http_response>
storage_routes::handle_put(const http_request& req) {
    const auto bucket = req.get_path_param("bucket");
    const auto key    = req.get_path_param("key");
    BOOST_LOG_SEV(lg(), debug) << "PUT " << bucket << "/" << key;

    const auto path = resolve_path(bucket, key);
    if (path.empty())
        co_return http_response::not_found("Unknown bucket: " + bucket);

    try {
        write_file(path, req.body);
        co_return http_response::json(R"({"success":true})");
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "PUT error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response>
storage_routes::handle_get(const http_request& req) {
    const auto bucket = req.get_path_param("bucket");
    const auto key    = req.get_path_param("key");
    BOOST_LOG_SEV(lg(), debug) << "GET " << bucket << "/" << key;

    const auto path = resolve_path(bucket, key);
    if (path.empty())
        co_return http_response::not_found("Unknown bucket: " + bucket);

    try {
        auto data = read_file(path);
        http_response resp;
        resp.status       = http_status::ok;
        resp.content_type = "application/octet-stream";
        resp.body         = std::move(data);
        co_return resp;
    } catch (const std::runtime_error&) {
        co_return http_response::not_found(bucket + "/" + key);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "GET error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response>
storage_routes::handle_delete(const http_request& req) {
    const auto bucket = req.get_path_param("bucket");
    const auto key    = req.get_path_param("key");
    BOOST_LOG_SEV(lg(), debug) << "DELETE " << bucket << "/" << key;

    const auto path = resolve_path(bucket, key);
    if (path.empty())
        co_return http_response::not_found("Unknown bucket: " + bucket);

    try {
        delete_file(path);
        co_return http_response::json(R"({"success":true})");
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "DELETE error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

}
