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
#include "ores.http/openapi/endpoint_registry.hpp"

#include <sstream>
#include <algorithm>

namespace ores::http::openapi {

using namespace ores::telemetry::log;

endpoint_registry::endpoint_registry() {
    BOOST_LOG_SEV(lg(), debug) << "Endpoint registry created";
}

void endpoint_registry::set_info(const api_info& info) {
    info_ = info;
    BOOST_LOG_SEV(lg(), debug) << "API info set: " << info.title << " v" << info.version;
}

void endpoint_registry::add_server(const server_info& server) {
    servers_.push_back(server);
    BOOST_LOG_SEV(lg(), debug) << "Server added: " << server.url;
}

void endpoint_registry::register_route(const domain::route& route) {
    routes_.push_back(route);
    BOOST_LOG_SEV(lg(), debug) << "Route registered: "
        << method_to_string(route.method) << " " << route.pattern;
}

std::string endpoint_registry::method_to_string(domain::http_method method) const {
    switch (method) {
        case domain::http_method::get: return "get";
        case domain::http_method::post: return "post";
        case domain::http_method::put: return "put";
        case domain::http_method::patch: return "patch";
        case domain::http_method::delete_: return "delete";
        case domain::http_method::head: return "head";
        case domain::http_method::options: return "options";
        default: return "get";
    }
}

std::string endpoint_registry::generate_openapi_json() const {
    BOOST_LOG_SEV(lg(), debug) << "Generating OpenAPI JSON spec with "
        << routes_.size() << " routes";

    std::ostringstream oss;
    oss << "{\n";
    oss << R"(  "openapi": "3.0.3",)" << "\n";

    // Info section
    oss << R"(  "info": {)" << "\n";
    oss << R"(    "title": ")" << info_.title << R"(",)" << "\n";
    oss << R"(    "description": ")" << info_.description << R"(",)" << "\n";
    oss << R"(    "version": ")" << info_.version << R"(")" << "\n";
    if (!info_.contact_name.empty() || !info_.contact_email.empty()) {
        oss << R"(    ,"contact": {)" << "\n";
        if (!info_.contact_name.empty()) {
            oss << R"(      "name": ")" << info_.contact_name << R"(")" << "\n";
        }
        if (!info_.contact_email.empty()) {
            oss << R"(      ,"email": ")" << info_.contact_email << R"(")" << "\n";
        }
        oss << R"(    })" << "\n";
    }
    if (!info_.license_name.empty()) {
        oss << R"(    ,"license": {)" << "\n";
        oss << R"(      "name": ")" << info_.license_name << R"(")" << "\n";
        if (!info_.license_url.empty()) {
            oss << R"(      ,"url": ")" << info_.license_url << R"(")" << "\n";
        }
        oss << R"(    })" << "\n";
    }
    oss << R"(  },)" << "\n";

    // Servers section
    oss << R"(  "servers": [)" << "\n";
    if (servers_.empty()) {
        oss << R"(    {"url": "/", "description": "Default server"})" << "\n";
    } else {
        for (std::size_t i = 0; i < servers_.size(); ++i) {
            if (i > 0) oss << ",\n";
            oss << R"(    {"url": ")" << servers_[i].url << R"(", "description": ")"
                << servers_[i].description << R"("})";
        }
        oss << "\n";
    }
    oss << R"(  ],)" << "\n";

    // Security schemes
    oss << R"(  "components": {)" << "\n";
    oss << R"(    "securitySchemes": {)" << "\n";
    oss << R"(      "bearerAuth": {)" << "\n";
    oss << R"(        "type": "http",)" << "\n";
    oss << R"(        "scheme": "bearer",)" << "\n";
    oss << R"(        "bearerFormat": "JWT")" << "\n";
    oss << R"(      })" << "\n";
    oss << R"(    })" << "\n";
    oss << R"(  },)" << "\n";

    // Paths section
    oss << R"(  "paths": {)" << "\n";

    // Group routes by path
    std::map<std::string, std::vector<const domain::route*>> paths_map;
    for (const auto& route : routes_) {
        paths_map[route.pattern].push_back(&route);
    }

    bool first_path = true;
    for (const auto& [path, path_routes] : paths_map) {
        if (!first_path) oss << ",\n";
        first_path = false;

        // Convert path pattern to OpenAPI format: {id} stays as {id}
        oss << R"(    ")" << path << R"(": {)" << "\n";

        bool first_method = true;
        for (const auto* route : path_routes) {
            if (!first_method) oss << ",\n";
            first_method = false;

            std::string method = method_to_string(route->method);
            oss << R"(      ")" << method << R"(": {)" << "\n";

            // Tags
            if (!route->tags.empty()) {
                oss << R"(        "tags": [)";
                for (std::size_t i = 0; i < route->tags.size(); ++i) {
                    if (i > 0) oss << ", ";
                    oss << R"(")" << route->tags[i] << R"(")";
                }
                oss << R"(],)" << "\n";
            }

            // Summary and description
            if (!route->summary.empty()) {
                oss << R"(        "summary": ")" << route->summary << R"(",)" << "\n";
            }
            if (!route->description.empty()) {
                oss << R"(        "description": ")" << route->description << R"(",)" << "\n";
            }

            // Path parameters
            if (!route->param_names.empty()) {
                oss << R"(        "parameters": [)" << "\n";
                for (std::size_t i = 0; i < route->param_names.size(); ++i) {
                    if (i > 0) oss << ",\n";
                    oss << R"(          {)";
                    oss << R"("name": ")" << route->param_names[i] << R"(", )";
                    oss << R"("in": "path", )";
                    oss << R"("required": true, )";
                    oss << R"("schema": {"type": "string"}})";
                }
                oss << "\n" << R"(        ],)" << "\n";
            }

            // Security
            if (route->requires_auth) {
                oss << R"(        "security": [{"bearerAuth": []}],)" << "\n";
            }

            // Responses
            oss << R"(        "responses": {)" << "\n";
            oss << R"(          "200": {"description": "Successful response"},)" << "\n";
            if (route->requires_auth) {
                oss << R"(          "401": {"description": "Unauthorized"},)" << "\n";
                oss << R"(          "403": {"description": "Forbidden"},)" << "\n";
            }
            oss << R"(          "500": {"description": "Internal server error"})" << "\n";
            oss << R"(        })" << "\n";

            oss << R"(      })";
        }
        oss << "\n" << R"(    })";
    }

    oss << "\n" << R"(  })" << "\n";
    oss << "}\n";

    BOOST_LOG_SEV(lg(), debug) << "OpenAPI JSON spec generated";
    return oss.str();
}

std::string endpoint_registry::generate_swagger_ui_html(const std::string& spec_url) const {
    BOOST_LOG_SEV(lg(), debug) << "Generating Swagger UI HTML";

    std::ostringstream oss;
    oss << R"(<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>)" << info_.title << R"( - API Documentation</title>
    <link rel="stylesheet" href="https://unpkg.com/swagger-ui-dist@5/swagger-ui.css">
    <style>
        body { margin: 0; padding: 0; }
        .topbar { display: none; }
    </style>
</head>
<body>
    <div id="swagger-ui"></div>
    <script src="https://unpkg.com/swagger-ui-dist@5/swagger-ui-bundle.js"></script>
    <script>
        window.onload = function() {
            SwaggerUIBundle({
                url: ")" << spec_url << R"(",
                dom_id: '#swagger-ui',
                presets: [
                    SwaggerUIBundle.presets.apis,
                    SwaggerUIBundle.SwaggerUIStandalonePreset
                ],
                layout: "StandaloneLayout",
                deepLinking: true,
                showExtensions: true,
                showCommonExtensions: true
            });
        };
    </script>
</body>
</html>)";

    return oss.str();
}

}
