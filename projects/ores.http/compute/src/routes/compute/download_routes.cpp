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
#include "ores.http/routes/compute/download_routes.hpp"
#include "ores.compute.api/messaging/result_protocol.hpp"
#include "ores.compute.api/messaging/workunit_protocol.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.core/routes/storage_file_source.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/compute/grid_views.hpp"
#include "ores.storage.api/net/object_keys.hpp"
#include "ores.storage.api/net/storage_paths.hpp"
#include "ores.storage.core/filesystem/local_store.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/string_generator.hpp>
#include <filesystem>
#include <memory>
#include <optional>
#include <string>
#include <string_view>

namespace ores::http::routes::compute {

namespace {

namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace compute_messaging = ores::compute::messaging;
using storage_paths = ores::storage::net::storage_paths;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

}

std::optional<std::string> stored_key(const std::string& address) {
    std::string bucket;
    std::string key;
    if (!storage_paths::split_object_path(address, bucket, key))
        return std::nullopt;
    if (bucket != ores::storage::api::object_keys::ores_bucket)
        return std::nullopt;
    return key;
}

std::string content_type_of(const std::string& key) {
    const auto gzip = key.ends_with(".gz") || key.ends_with(".tgz");
    return gzip ? "application/gzip" : "application/octet-stream";
}

std::string download_name(const std::string& key) {
    const auto slash = key.find_last_of('/');
    return key.substr(slash == std::string::npos ? 0 : slash + 1);
}

std::string disposition_for(const std::string& name) {
    std::string safe;
    safe.reserve(name.size());
    for (const char c : name) {
        const auto printable = static_cast<unsigned char>(c);
        safe += (printable < 0x20 || printable > 0x7e || c == '"' || c == '\\') ? '_' : c;
    }
    return "attachment; filename=\"" + safe + "\"";
}

namespace {

/// The bytes of a stored object, or a refusal saying it is not there.
http_response answer(const ores::storage::filesystem::local_store& store,
                     const std::string& address,
                     std::string_view what) {
    const auto key = stored_key(address);
    if (!key)
        rr::refuse(http_status::not_found,
                   "not-found",
                   "This " + std::string(what) + " names no stored file.");

    const auto bucket(ores::storage::api::object_keys::ores_bucket);
    const auto path = store.resolve(std::string(bucket), *key);
    if (!path || !std::filesystem::is_regular_file(*path))
        rr::refuse(http_status::not_found,
                   "not-found",
                   "The stored " + std::string(what) + " is no longer there.");

    http_response resp;
    resp.content_type = content_type_of(*key);
    resp.set_header("Content-Disposition", disposition_for(download_name(*key)));
    resp.set_header("Cache-Control", "private, no-store");
    resp.body_stream = std::make_shared<ores::http::server::routes::storage_file_source>(*path);
    return resp;
}

}

void download_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session,
    std::string storage_dir) {
    using forwarding::guarded;
    const auto store =
        std::make_shared<ores::storage::filesystem::local_store>(std::move(storage_dir));

    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"grid"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/operations/grid/jobs/workunits/{id}/input")
            .summary("The input bundle of one work unit")
            .description("The record is read first on the caller's own token, so a caller who "
                         "may not read the work unit is refused before any byte is read.")
            .handler([&session,
                      store](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<compute_messaging::get_workunit_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    compute_messaging::get_workunit_request msg;
                    msg.key.id = grid::id_of(req);
                    const auto reply = forwarding::forward(req, session, msg);
                    if (reply.result.outcome == ores::utility::domain::outcome::missing)
                        rr::refuse(
                            http_status::not_found, "not-found", "No work unit has this id.");
                    rr::require_ok(reply.result);
                    if (!reply.workunit)
                        rr::refuse(
                            http_status::not_found, "not-found", "No work unit has this id.");
                    return answer(*store, reply.workunit->input_uri, "input");
                });
            }));

    add(router->get("/api/operations/grid/jobs/results/{id}/output")
            .summary("The output archive of one run")
            .description("The record is read first on the caller's own token, so a caller who "
                         "may not read the run is refused before any byte is read.")
            .handler([&session,
                      store](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<compute_messaging::get_result_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    compute_messaging::get_result_request msg;
                    msg.key.id = grid::id_of(req);
                    const auto reply = forwarding::forward(req, session, msg);
                    if (reply.result.outcome == ores::utility::domain::outcome::missing)
                        rr::refuse(http_status::not_found, "not-found", "No result has this id.");
                    rr::require_ok(reply.result);
                    if (!reply.result_value)
                        rr::refuse(http_status::not_found, "not-found", "No result has this id.");
                    return answer(*store, reply.result_value->output_uri, "output");
                });
            }));
}

}
