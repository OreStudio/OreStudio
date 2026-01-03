/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#ifndef ORES_HTTP_NET_ROUTER_HPP
#define ORES_HTTP_NET_ROUTER_HPP

#include <vector>
#include <memory>
#include <optional>
#include "ores.http/domain/route.hpp"
#include "ores.telemetry/log/make_logger.hpp"

namespace ores::http::net {

/**
 * @brief Route builder for fluent route registration.
 */
class route_builder final {
public:
    route_builder(domain::http_method method, const std::string& pattern);

    /**
     * @brief Sets the handler for this route.
     */
    route_builder& handler(domain::request_handler h);

    /**
     * @brief Marks this route as requiring authentication.
     */
    route_builder& auth_required();

    /**
     * @brief Sets required roles for this route.
     */
    route_builder& roles(std::vector<std::string> r);

    /**
     * @brief Sets OpenAPI summary.
     */
    route_builder& summary(std::string s);

    /**
     * @brief Sets OpenAPI description.
     */
    route_builder& description(std::string d);

    /**
     * @brief Sets OpenAPI tags.
     */
    route_builder& tags(std::vector<std::string> t);

    /**
     * @brief Adds a query parameter for OpenAPI documentation.
     */
    route_builder& query_param(const std::string& name,
        const std::string& type = "string",
        const std::string& format = "",
        bool required = false,
        const std::string& desc = "",
        const std::optional<std::string>& default_value = std::nullopt);

    /**
     * @brief Sets the request body schema for OpenAPI documentation.
     */
    route_builder& body(std::vector<domain::schema_property> properties,
        bool required = true,
        const std::string& content_type = "application/json");

    /**
     * @brief Builds the route.
     */
    domain::route build() const;

private:
    domain::http_method method_;
    std::string pattern_;
    domain::request_handler handler_;
    bool requires_auth_ = false;
    std::vector<std::string> required_roles_;
    std::string summary_;
    std::string description_;
    std::vector<std::string> tags_;
    std::vector<domain::query_param> query_params_;
    std::optional<domain::request_body_schema> body_schema_;
};

/**
 * @brief HTTP request router that matches requests to handlers.
 */
class router final {
public:
    router();

    /**
     * @brief Registers a GET route.
     */
    route_builder get(const std::string& pattern);

    /**
     * @brief Registers a POST route.
     */
    route_builder post(const std::string& pattern);

    /**
     * @brief Registers a PUT route.
     */
    route_builder put(const std::string& pattern);

    /**
     * @brief Registers a PATCH route.
     */
    route_builder patch(const std::string& pattern);

    /**
     * @brief Registers a DELETE route.
     */
    route_builder delete_(const std::string& pattern);

    /**
     * @brief Adds a built route to the router.
     */
    void add_route(const domain::route& route);

    /**
     * @brief Attempts to match a request and returns the matching route.
     */
    std::optional<domain::route> match(domain::http_method method,
        const std::string& path,
        std::unordered_map<std::string, std::string>& path_params) const;

    /**
     * @brief Returns all registered routes (for OpenAPI generation).
     */
    const std::vector<domain::route>& routes() const { return routes_; }

private:
    inline static std::string_view logger_name = "ores.http.net.router";

    static auto& lg() {
        using namespace ores::telemetry::log;
        static auto instance = make_logger(logger_name);
        return instance;
    }

    std::vector<domain::route> routes_;
};

}

#endif
