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
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.iam.api/domain/permission_codes.hpp"
#include "ores.utility/crypto/sha256.hpp"
#include <algorithm>
#include <charconv>
#include <iomanip>
#include <sstream>

namespace ores::http_server::routes {

using namespace ores::logging;
using namespace ores::http::domain;
using iam::domain::permissions::objects_delete;
using iam::domain::permissions::objects_read;
using iam::domain::permissions::objects_write;
using storage::filesystem::object_not_found;
namespace asio = boost::asio;

namespace {

/**
 * @brief The largest page a listing will return, whatever the caller asks for.
 */
constexpr std::uint32_t k_max_list_limit = 1000;

std::uint32_t parse_u32(const std::string& text, std::uint32_t fallback) {
    if (text.empty())
        return fallback;
    std::uint32_t value = 0;
    const auto* first = text.data();
    const auto* last = text.data() + text.size();
    const auto [ptr, ec] = std::from_chars(first, last, value);
    if (ec != std::errc{} || ptr != last)
        return fallback;
    return value;
}

/**
 * @brief Escapes a string for a JSON document.
 *
 * The listing is the one response that carries keys, and a key is opaque: it
 * may hold a quote or a backslash, so it cannot be concatenated raw.
 */
std::string json_escape(std::string_view text) {
    std::string out;
    out.reserve(text.size() + 8);
    for (const char c : text) {
        switch (c) {
        case '"':
            out += "\\\"";
            break;
        case '\\':
            out += "\\\\";
            break;
        case '\n':
            out += "\\n";
            break;
        case '\r':
            out += "\\r";
            break;
        case '\t':
            out += "\\t";
            break;
        default:
            if (static_cast<unsigned char>(c) < 0x20) {
                std::ostringstream hex;
                hex << "\\u" << std::hex << std::setw(4) << std::setfill('0')
                    << static_cast<int>(static_cast<unsigned char>(c));
                out += hex.str();
            } else {
                out += c;
            }
        }
    }
    return out;
}

}

storage_routes::storage_routes(std::string storage_dir,
                               std::shared_ptr<iam::service::authorization_service> auth_service)
    : store_(std::move(storage_dir))
    , auth_service_(std::move(auth_service)) {
    BOOST_LOG_SEV(lg(), debug) << "Storage routes dir: " << store_.root().string();
}

void storage_routes::register_routes(
    std::shared_ptr<http::net::router> router,
    std::shared_ptr<http::openapi::endpoint_registry> /*registry*/) {

    BOOST_LOG_SEV(lg(), info) << "Registering storage routes";

    // The {key} path parameter may contain slashes (e.g. "workunit-id/input.tar.gz").
    // The router captures everything after {bucket}/ as the key. The object
    // routes are registered before the listing route, because a bare
    // /api/v1/storage/{bucket} also matches a path with a slash: registration
    // order is what keeps the longer pattern in front.
    router->add_route(router->put("/api/v1/storage/{bucket}/{key}")
                          .summary("Upload storage object")
                          .description("Upload an object into the named bucket under the given key")
                          .tags({"storage"})
                          .auth_required()
                          .handler([this](const http_request& req) { return handle_put(req); })
                          .build());

    router->add_route(router->get("/api/v1/storage/{bucket}/{key}")
                          .summary("Download storage object")
                          .description("Download an object from the named bucket by key")
                          .tags({"storage"})
                          .auth_required()
                          .handler([this](const http_request& req) { return handle_get(req); })
                          .build());

    router->add_route(router->head("/api/v1/storage/{bucket}/{key}")
                          .summary("Check storage object")
                          .description("Report whether an object exists and how large it is, "
                                       "without transferring it")
                          .tags({"storage"})
                          .auth_required()
                          .handler([this](const http_request& req) { return handle_head(req); })
                          .build());

    router->add_route(router->delete_("/api/v1/storage/{bucket}/{key}")
                          .summary("Delete storage object")
                          .description("Delete an object from the named bucket by key")
                          .tags({"storage"})
                          .auth_required()
                          .handler([this](const http_request& req) { return handle_delete(req); })
                          .build());

    router->add_route(router->get("/api/v1/storage/{bucket}")
                          .summary("List storage objects")
                          .description("List a page of the objects in a bucket, filtered by "
                                       "key prefix")
                          .tags({"storage"})
                          .auth_required()
                          .query_param("prefix", "string", "", false,
                                       "Only keys that start with this")
                          .query_param("offset", "integer", "int32", false,
                                       "How many matching keys to skip")
                          .query_param("limit", "integer", "int32", false,
                                       "How many keys to return")
                          .handler([this](const http_request& req) { return handle_list(req); })
                          .build());

    BOOST_LOG_SEV(lg(), info) << "Storage routes registered: 5 endpoints";
}

std::expected<storage_routes::auth_result, http_response>
storage_routes::check_auth(const http_request& req,
                           std::string_view required_permission,
                           std::string_view operation_name) {

    if (!req.authenticated_user) {
        BOOST_LOG_SEV(lg(), warn) << operation_name << " denied: not authenticated";
        return std::unexpected(http_response::unauthorized("Not authenticated"));
    }

    boost::uuids::uuid account_id;
    try {
        account_id = boost::uuids::string_generator()(req.authenticated_user->subject);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error)
            << operation_name << " failed: invalid account id in the token: " << e.what();
        return std::unexpected(http_response::unauthorized("Invalid token"));
    }

    if (auth_service_ &&
        !auth_service_->has_permission(account_id, std::string(required_permission))) {
        BOOST_LOG_SEV(lg(), warn) << operation_name << " denied: account "
                                  << boost::uuids::to_string(account_id) << " lacks "
                                  << required_permission;
        return std::unexpected(http_response::forbidden("Insufficient permissions"));
    }

    return auth_result{account_id};
}

asio::awaitable<http_response> storage_routes::handle_put(const http_request& req) {
    const auto bucket = req.get_path_param("bucket");
    const auto key = req.get_path_param("key");

    if (auto auth = check_auth(req, objects_write, "put"); !auth)
        co_return auth.error();

    if (!store_.is_valid_key(bucket, key))
        co_return http_response::bad_request("Invalid bucket or key: " + bucket);

    BOOST_LOG_SEV(lg(), debug) << "PUT " << bucket << "/" << key;

    try {
        store_.put(bucket, key, req.body);
        const auto sha256 = ores::utility::crypto::sha256::hex_digest(req.body);
        co_return http_response::json(R"({"success":true,"sha256":")" + sha256 + R"("})");
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "PUT error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> storage_routes::handle_get(const http_request& req) {
    const auto bucket = req.get_path_param("bucket");
    const auto key = req.get_path_param("key");

    if (auto auth = check_auth(req, objects_read, "get"); !auth)
        co_return auth.error();

    if (!store_.is_valid_key(bucket, key))
        co_return http_response::bad_request("Invalid bucket or key: " + bucket);

    BOOST_LOG_SEV(lg(), debug) << "GET " << bucket << "/" << key;

    try {
        http_response resp;
        resp.status = http_status::ok;
        resp.content_type = "application/octet-stream";
        resp.body = store_.get(bucket, key);
        co_return resp;
    } catch (const object_not_found&) {
        co_return http_response::not_found(bucket + "/" + key);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "GET error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> storage_routes::handle_head(const http_request& req) {
    const auto bucket = req.get_path_param("bucket");
    const auto key = req.get_path_param("key");

    if (auto auth = check_auth(req, objects_read, "head"); !auth)
        co_return auth.error();

    if (!store_.is_valid_key(bucket, key))
        co_return http_response::bad_request("Invalid bucket or key: " + bucket);

    BOOST_LOG_SEV(lg(), debug) << "HEAD " << bucket << "/" << key;

    const auto bytes = store_.size(bucket, key);
    if (!bytes)
        co_return http_response::not_found(bucket + "/" + key);

    http_response resp;
    resp.status = http_status::ok;
    resp.content_type = "application/octet-stream";
    resp.headers["Content-Length"] = std::to_string(*bytes);
    // HEAD answers with the headers a GET would carry and no body.
    co_return resp;
}

asio::awaitable<http_response> storage_routes::handle_delete(const http_request& req) {
    const auto bucket = req.get_path_param("bucket");
    const auto key = req.get_path_param("key");

    if (auto auth = check_auth(req, objects_delete, "delete"); !auth)
        co_return auth.error();

    if (!store_.is_valid_key(bucket, key))
        co_return http_response::bad_request("Invalid bucket or key: " + bucket);

    BOOST_LOG_SEV(lg(), debug) << "DELETE " << bucket << "/" << key;

    try {
        const bool removed = store_.remove(bucket, key);
        co_return http_response::json(std::string(R"({"success":true,"removed":)") +
                                      (removed ? "true" : "false") + "}");
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), error) << "DELETE error: " << e.what();
        co_return http_response::internal_error(e.what());
    }
}

asio::awaitable<http_response> storage_routes::handle_list(const http_request& req) {
    const auto bucket = req.get_path_param("bucket");

    if (auto auth = check_auth(req, objects_read, "list"); !auth)
        co_return auth.error();

    if (!storage::filesystem::local_store::is_valid_bucket(bucket))
        co_return http_response::bad_request("Invalid bucket: " + bucket);

    const auto prefix = req.get_query_param("prefix");
    const auto offset = parse_u32(req.get_query_param("offset"), 0);
    const auto requested = parse_u32(req.get_query_param("limit"), 100);
    const auto limit = std::min(requested, k_max_list_limit);

    BOOST_LOG_SEV(lg(), debug) << "LIST " << bucket << " prefix=" << prefix;

    const auto entries = store_.list(bucket, prefix);
    const auto total = entries.size();
    const auto first = std::min<std::size_t>(offset, total);
    const auto last = std::min<std::size_t>(first + limit, total);

    std::ostringstream body;
    body << R"({"success":true,"total_available_count":)" << total << R"(,"objects":[)";
    for (std::size_t i = first; i < last; ++i) {
        if (i != first)
            body << ',';
        body << R"({"key":")" << json_escape(entries[i].key) << R"(","size_bytes":)"
             << entries[i].size_bytes << '}';
    }
    body << "]}";

    co_return http_response::json(body.str());
}

}
