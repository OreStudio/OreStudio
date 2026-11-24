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
#include "ores.http.core/routes/web_bundle_routes.hpp"
#include "ores.http.core/routes/storage_file_source.hpp"
#include <string_view>
#include <unordered_map>

namespace ores::http::server::routes {

namespace {

/// The file the browser loads, and the file every screen path is answered with.
constexpr std::string_view index_name = "index.html";

/// The directory the build puts content-hashed files in.
constexpr std::string_view assets_prefix = "assets/";

/// A hashed name never changes content, so it is cached for as long as a year.
constexpr std::string_view immutable_cache = "public, max-age=31536000, immutable";

/// The application's own file changes on every deploy, so it is revalidated.
constexpr std::string_view revalidate_cache = "no-cache";

std::string_view content_type_of(const std::string& path) {
    static const auto types = std::unordered_map<std::string_view, std::string_view>{
        {"html", "text/html; charset=utf-8"},
        {"js", "text/javascript; charset=utf-8"},
        {"mjs", "text/javascript; charset=utf-8"},
        {"css", "text/css; charset=utf-8"},
        {"json", "application/json"},
        {"map", "application/json"},
        {"svg", "image/svg+xml"},
        {"png", "image/png"},
        {"jpg", "image/jpeg"},
        {"jpeg", "image/jpeg"},
        {"gif", "image/gif"},
        {"webp", "image/webp"},
        {"ico", "image/x-icon"},
        {"woff", "font/woff"},
        {"woff2", "font/woff2"},
        {"ttf", "font/ttf"},
        {"txt", "text/plain; charset=utf-8"},
        {"wasm", "application/wasm"},
    };

    const auto dot = path.rfind('.');
    if (dot == std::string::npos)
        return "application/octet-stream";
    const auto it = types.find(std::string_view(path).substr(dot + 1));
    return it == types.end() ? std::string_view("application/octet-stream") : it->second;
}

/// Whether the caller said it would take brotli.
bool accepts_brotli(const http::domain::http_request& request) {
    const auto encoding = request.get_header("accept-encoding");
    return encoding.find("br") != std::string::npos;
}

/**
 * @brief A miss the bundle answers, in the error contract.
 *
 * The path travels with the leading slash the caller wrote it with, so the
 * sentence names what was asked for rather than echoing a lookup key.
 */
http::domain::http_response no_route(const std::string& path) {
    return http::domain::http_response::api_error(
        http::domain::http_status::not_found, "not-found", "No route answers /" + path);
}

/// A bundle that holds no application is a broken build, not a missing screen.
http::domain::http_response no_index() {
    return http::domain::http_response::api_error(http::domain::http_status::not_found,
                                                  "not-found",
                                                  "The bundle holds no " + std::string(index_name));
}

}

web_bundle_routes::web_bundle_routes(std::filesystem::path root)
    : manifest_(std::move(root)) {
    ORES_LOG_SEV(lg(), info) << "Serving the browser bundle from " << manifest_.root().string()
                             << " (" << manifest_.size() << " files)";
}

void web_bundle_routes::register_routes(std::shared_ptr<web_bundle_routes> self,
                                        std::shared_ptr<http::net::router> router,
                                        std::shared_ptr<http::openapi::endpoint_registry>) {
    /*
     * Registered last, and in this order: the root, then one greedy pattern that
     * carries the whole path. A screen path with four segments has to reach the
     * application shell, so the pattern may not stop at the first slash.
     */
    router->add_route(router->get("/")
                          .catalogue("web_get", "operations", "core", false)
                          .summary("The application")
                          .description("The built browser bundle's index")
                          .tags({"web"})
                          .auth_optional()
                          .handler([self](const http::domain::http_request& req)
                                       -> asio::awaitable<http::domain::http_response> {
                              co_return self->index(req);
                          })
                          .build());

    router->add_route(
        router->get("/{path}")
            .catalogue("web_get_path", "operations", "core", false)
            .summary("The application, and what it was built from")
            .description("A file of the built bundle, or the application shell for a screen path")
            .tags({"web"})
            .auth_optional()
            .handler([self](const http::domain::http_request& req)
                         -> asio::awaitable<http::domain::http_response> {
                co_return self->answer(req.get_path_param("path"), req);
            })
            .build());
}

http::domain::http_response
web_bundle_routes::answer(const std::string& path,
                          const http::domain::http_request& request) const {
    // An API path is never a screen: a misspelled one must not be answered with the
    // application shell, because a caller reading HTML as JSON learns nothing.
    if (path == "api" || path.starts_with("api/"))
        return no_route(path);

    if (const auto* entry = manifest_.find(path); entry != nullptr)
        return file(*entry, path, request);

    /*
     * A build that publishes while this server runs writes a new index naming
     * assets the startup scan never saw. The index is read from disk on every
     * request, so the browser follows it here, and an asset the snapshot does
     * not hold is asked of the directory before it is refused: the index and
     * every file it names then come from the same build.
     */
    if (path.starts_with(assets_prefix)) {
        if (const auto fresh = manifest_.read(path); fresh.has_value())
            return file(*fresh, path, request);
        // A file the build should have written and did not is a breakage, not
        // a screen.
        return no_route(path);
    }

    return index(request);
}

http::domain::http_response
web_bundle_routes::index(const http::domain::http_request& request) const {
    const auto* entry = manifest_.find(std::string(index_name));
    if (entry == nullptr)
        return no_index();

    return file(*entry, std::string(index_name), request);
}

http::domain::http_response
web_bundle_routes::file(const bundle_entry& entry,
                        const std::string& path,
                        const http::domain::http_request& request) const {
    const auto is_asset = path.starts_with(assets_prefix);
    const auto compressed = entry.compressed && accepts_brotli(request);

    http::domain::http_response response;
    response.status = http::domain::http_status::ok;
    response.content_type = std::string(content_type_of(path));
    response.set_header("Cache-Control",
                        std::string(is_asset ? immutable_cache : revalidate_cache));

    if (compressed) {
        // The copy the build made, sent as the same resource.
        response.set_header("Content-Encoding", "br");
    }
    if (entry.compressed) {
        // Two answers for one path, so a cache must key on both.
        response.set_header("Vary", "Accept-Encoding");
    }

    auto served = manifest_.root() / path;
    if (compressed)
        served += ".br";
    response.body_stream = std::make_shared<storage_file_source>(served);

    return response;
}

}
