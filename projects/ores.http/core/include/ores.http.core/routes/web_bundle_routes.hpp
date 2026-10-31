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
#ifndef ORES_HTTP_CORE_ROUTES_WEB_BUNDLE_ROUTES_HPP
#define ORES_HTTP_CORE_ROUTES_WEB_BUNDLE_ROUTES_HPP

#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.http.core/routes/bundle_manifest.hpp"
#include "ores.logging/make_logger.hpp"
#include <filesystem>
#include <memory>
#include <string>

namespace ores::http::server::routes {

/**
 * @brief Serves the built browser bundle, and the application it routes.
 *
 * Register these routes last: the bundle answers what no other route claimed, so a
 * screen path deep in the application reaches =index.html= and the browser's own
 * router decides what to show.
 *
 * Nothing is joined to the bundle root at request time. A path is looked up in the
 * manifest, so =..= cannot leave the root, and an asset that the build did not
 * write is a 404 rather than an =index.html= that hides the breakage.
 */
class ORES_HTTP_CORE_EXPORT web_bundle_routes final {
public:
    /**
     * @brief Reads the bundle this root holds.
     *
     * @throws std::runtime_error when the root is not a directory.
     */
    explicit web_bundle_routes(std::filesystem::path root);

    /**
     * @brief Registers the bundle's routes with the router.
     *
     * Must be called after every other route module.
     */
    void register_routes(std::shared_ptr<http::net::router> router,
                         std::shared_ptr<http::openapi::endpoint_registry> registry);

    /// The files the bundle holds, so a caller can log what it is serving.
    [[nodiscard]] std::size_t size() const {
        return manifest_.size();
    }

private:
    inline static std::string_view logger_name = "ores.http.server.routes.web_bundle";

    static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    [[nodiscard]] http::domain::http_response
    answer(const std::string& path, const http::domain::http_request& request) const;

    [[nodiscard]] http::domain::http_response
    index(const http::domain::http_request& request) const;

    [[nodiscard]] http::domain::http_response file(const bundle_entry& entry,
                                                   const std::string& path,
                                                   const http::domain::http_request& request) const;

    bundle_manifest manifest_;
};

}

#endif
