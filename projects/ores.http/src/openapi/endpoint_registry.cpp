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

#include <map>
#include <sstream>
#include <algorithm>
#include <rfl.hpp>
#include <rfl/json.hpp>

namespace ores::http::openapi {

using namespace ores::telemetry::log;

// OpenAPI spec structures for JSON serialization
// Note: These need external linkage for rfl reflection to work properly

namespace detail {

struct openapi_contact final {
    std::optional<std::string> name;
    std::optional<std::string> email;
};

struct openapi_license final {
    std::string name;
    std::optional<std::string> url;
};

struct openapi_info final {
    std::string title;
    std::string description;
    std::string version;
    std::optional<openapi_contact> contact;
    std::optional<openapi_license> license;
};

struct openapi_server final {
    std::string url;
    std::string description;
};

struct openapi_security_scheme final {
    std::string type;
    std::string scheme;
    rfl::Rename<"bearerFormat", std::string> bearer_format;
};

struct openapi_security_schemes final {
    rfl::Rename<"bearerAuth", openapi_security_scheme> bearer_auth;
};

struct openapi_components final {
    rfl::Rename<"securitySchemes", openapi_security_schemes> security_schemes;
};

struct openapi_parameter final {
    std::string name;
    std::string in;
    bool required;
    rfl::Object<std::string> schema;
};

struct openapi_response final {
    std::string description;
};

struct openapi_operation final {
    std::optional<std::vector<std::string>> tags;
    std::optional<std::string> summary;
    std::optional<std::string> description;
    std::optional<std::vector<openapi_parameter>> parameters;
    std::optional<std::vector<rfl::Object<std::vector<std::string>>>> security;
    std::map<std::string, openapi_response> responses;
};

struct openapi_spec final {
    std::string openapi;
    openapi_info info;
    std::vector<openapi_server> servers;
    openapi_components components;
    std::map<std::string, std::map<std::string, openapi_operation>> paths;
};

} // namespace detail

using namespace detail;

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

    openapi_spec spec;
    spec.openapi = "3.0.3";

    // Build info section
    spec.info.title = info_.title;
    spec.info.description = info_.description;
    spec.info.version = info_.version;

    if (!info_.contact_name.empty() || !info_.contact_email.empty()) {
        openapi_contact contact;
        if (!info_.contact_name.empty()) {
            contact.name = info_.contact_name;
        }
        if (!info_.contact_email.empty()) {
            contact.email = info_.contact_email;
        }
        spec.info.contact = contact;
    }

    if (!info_.license_name.empty()) {
        openapi_license license;
        license.name = info_.license_name;
        if (!info_.license_url.empty()) {
            license.url = info_.license_url;
        }
        spec.info.license = license;
    }

    // Build servers section
    if (servers_.empty()) {
        spec.servers.push_back(openapi_server{"/", "Default server"});
    } else {
        for (const auto& server : servers_) {
            spec.servers.push_back(openapi_server{server.url, server.description});
        }
    }

    // Build components section
    spec.components = openapi_components{
        openapi_security_schemes{
            openapi_security_scheme{"http", "bearer", "JWT"}
        }
    };

    // Build paths section
    for (const auto& route : routes_) {
        std::string method = method_to_string(route.method);

        openapi_operation operation;

        if (!route.tags.empty()) {
            operation.tags = route.tags;
        }

        if (!route.summary.empty()) {
            operation.summary = route.summary;
        }

        if (!route.description.empty()) {
            operation.description = route.description;
        }

        // Path parameters
        if (!route.param_names.empty()) {
            std::vector<openapi_parameter> params;
            for (const auto& param_name : route.param_names) {
                rfl::Object<std::string> schema;
                schema["type"] = "string";
                params.push_back(openapi_parameter{param_name, "path", true, schema});
            }
            operation.parameters = params;
        }

        // Security
        if (route.requires_auth) {
            rfl::Object<std::vector<std::string>> bearer_auth;
            bearer_auth["bearerAuth"] = std::vector<std::string>{};
            operation.security = std::vector<rfl::Object<std::vector<std::string>>>{bearer_auth};
        }

        // Responses
        operation.responses["200"] = openapi_response{"Successful response"};
        if (route.requires_auth) {
            operation.responses["401"] = openapi_response{"Unauthorized"};
            operation.responses["403"] = openapi_response{"Forbidden"};
        }
        operation.responses["500"] = openapi_response{"Internal server error"};

        spec.paths[route.pattern][method] = operation;
    }

    BOOST_LOG_SEV(lg(), debug) << "OpenAPI JSON spec generated";
    return rfl::json::write(spec);
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
