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
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.utility/codec/json_document.hpp"
#include <map>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <utility>

namespace ores::http::openapi {

using namespace ores::logging;

using ores::utility::codec::json_document;
using ores::utility::codec::json_value;

namespace {

/**
 * @brief Where a resolved value goes: the root of the document being built, a
 * member of an object in it, or an element of an array in it.
 *
 * A schema that states an object or an array needs a fresh place to be filled.
 * A schema that states any other value is copied to its place whole.
 */
class destination final {
public:
    static destination root(json_value value) {
        return destination{value, std::string{}, kind::root};
    }

    static destination member(json_value parent, std::string name) {
        return destination{parent, std::move(name), kind::member};
    }

    static destination element(json_value parent) {
        return destination{parent, std::string{}, kind::element};
    }

    void copy(const json_value& value) {
        if (kind_ == kind::member) {
            parent_.add(name_, value);
        } else if (kind_ == kind::element) {
            parent_.push(value);
        }
    }

    json_value object() {
        switch (kind_) {
            case kind::root:
                return parent_;
            case kind::member:
                return parent_.add_object(name_);
            case kind::element:
                return parent_.push_object();
        }
        return {};
    }

    json_value array() {
        switch (kind_) {
            case kind::root:
                return parent_;
            case kind::member:
                return parent_.add_array(name_);
            case kind::element:
                return parent_.push_array();
        }
        return {};
    }

private:
    enum class kind { root, member, element };

    destination(json_value parent, std::string name, kind where)
        : parent_(parent)
        , name_(std::move(name))
        , kind_(where) {}

    json_value parent_;
    std::string name_;
    kind kind_;
};

/**
 * @brief A response that states only a description, as the standard errors do.
 */
json_document make_response(std::string_view description) {
    auto response = json_document::make_object();
    response.root().set("description", description);
    return response;
}

/**
 * @brief States the example a route carries, when it states one.
 */
void add_example(json_value media_type, const std::string& example_json) {
    if (example_json.empty())
        return;
    auto example = json_document::parse(example_json);
    if (example)
        media_type.add("example", example->root());
}

/**
 * @brief Recursively resolves all $ref values in a schema by inlining them.
 *
 * OpenAPI 3.0.x interprets $ref paths relative to the document root, not the
 * schema object. This function walks through the schema and replaces all
 * $ref values with the actual type definitions from $defs.
 *
 * @param schema The schema value to process
 * @param defs The $defs value containing type definitions
 * @param visited Set of type names being resolved (for cycle detection)
 * @param where Where the resolved value goes in the document being built
 */
void resolve_refs_recursive(json_value schema,
                            json_value defs,
                            std::set<std::string>& visited,
                            destination where) {
    if (schema.is_array()) {
        // Handle arrays
        auto resolved = where.array();
        for (const auto& item : schema.elements()) {
            resolve_refs_recursive(item, defs, visited, destination::element(resolved));
        }
        return;
    }

    if (!schema.is_object()) {
        where.copy(schema);
        return;
    }

    // Check if this object is a $ref
    const auto ref = schema.member("$ref");
    if (ref.exists()) {
        constexpr std::string_view defs_prefix = "#/$defs/";
        const auto ref_text = ref.text();
        if (ref_text && ref_text->starts_with(defs_prefix)) {
            const auto type_name = std::string(ref_text->substr(defs_prefix.size()));

            // Prevent infinite recursion
            if (visited.contains(type_name)) {
                // Already resolving this type - return a simple object type
                where.object().set("type", "object");
                return;
            }

            // Find the type in $defs
            const auto definition = defs.member(type_name);
            if (definition.is_object()) {
                // Mark as being visited
                visited.insert(type_name);

                // Create new object with resolved contents
                const auto resolved = where.object();
                for (const auto& [name, value] : definition.members()) {
                    resolve_refs_recursive(
                        value, defs, visited, destination::member(resolved, name));
                }

                visited.erase(type_name);
                return;
            }
        }
        // Couldn't resolve - return original
        where.copy(schema);
        return;
    }

    // No $ref - recursively process all values in this object
    const auto resolved = where.object();
    for (const auto& [name, value] : schema.members()) {
        if (name == "$defs" || name == "$schema")
            continue;
        resolve_refs_recursive(value, defs, visited, destination::member(resolved, name));
    }
}

/**
 * @brief Processes a JSON schema, resolving $refs and removing $defs.
 *
 * @param schema The schema document to process
 * @return The processed schema with all $refs inlined
 */
json_document process_schema(json_document schema) {
    const auto root = schema.root();
    if (!root.is_object())
        return schema;

    // Find $defs if present
    const auto defs = root.member("$defs");

    // Nothing to resolve when the document declares no definitions.
    if (!defs.is_object())
        return schema;

    // Use the recursive resolver which handles everything
    std::set<std::string> visited;
    auto processed = json_document::make_object();
    resolve_refs_recursive(root, defs, visited, destination::root(processed.root()));
    return processed;
}

} // anonymous namespace

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
    BOOST_LOG_SEV(lg(), debug) << "Route registered: " << method_to_string(route.method) << " "
                               << route.pattern;
}

std::string endpoint_registry::method_to_string(domain::http_method method) const {
    switch (method) {
        case domain::http_method::get:
            return "get";
        case domain::http_method::post:
            return "post";
        case domain::http_method::put:
            return "put";
        case domain::http_method::patch:
            return "patch";
        case domain::http_method::delete_:
            return "delete";
        case domain::http_method::head:
            return "head";
        case domain::http_method::options:
            return "options";
        default:
            return "get";
    }
}

std::string endpoint_registry::generate_openapi_json() const {
    BOOST_LOG_SEV(lg(), debug) << "Generating OpenAPI JSON spec with " << routes_.size()
                               << " routes";

    auto spec = json_document::make_object();
    auto root = spec.root();
    root.set("openapi", "3.0.3");

    // Build info section
    auto info = root.add_object("info");
    info.set("title", info_.title);
    info.set("description", info_.description);
    info.set("version", info_.version);

    if (!info_.contact_name.empty() || !info_.contact_email.empty()) {
        auto contact = info.add_object("contact");
        if (!info_.contact_name.empty()) {
            contact.set("name", info_.contact_name);
        }
        if (!info_.contact_email.empty()) {
            contact.set("email", info_.contact_email);
        }
    }

    if (!info_.license_name.empty()) {
        auto license = info.add_object("license");
        license.set("name", info_.license_name);
        if (!info_.license_url.empty()) {
            license.set("url", info_.license_url);
        }
    }

    // Build servers section
    auto servers = root.add_array("servers");
    const auto add_server = [&servers](const server_info& server) {
        auto entry = servers.push_object();
        entry.set("url", server.url);
        entry.set("description", server.description);
    };
    if (servers_.empty()) {
        add_server(server_info{"/", "Default server"});
    } else {
        for (const auto& server : servers_) {
            add_server(server);
        }
    }

    // Build components section
    auto components = root.add_object("components");
    auto security_schemes = components.add_object("securitySchemes");
    auto bearer_auth = security_schemes.add_object("bearerAuth");
    bearer_auth.set("type", "http");
    bearer_auth.set("scheme", "bearer");
    bearer_auth.set("bearerFormat", "JWT");

    // Build paths section
    std::map<std::string, std::map<std::string, json_document>> paths;
    for (const auto& route : routes_) {
        const auto method = method_to_string(route.method);

        auto operation_document = json_document::make_object();
        auto operation = operation_document.root();

        if (!route.tags.empty()) {
            auto tags = operation.add_array("tags");
            for (const auto& tag : route.tags) {
                tags.push(tag);
            }
        }

        if (!route.summary.empty()) {
            operation.set("summary", route.summary);
        }

        if (!route.description.empty()) {
            operation.set("description", route.description);
        }

        // Parameters (path + query)
        if (!route.param_names.empty() || !route.query_params.empty()) {
            auto parameters = operation.add_array("parameters");

            // Path parameters
            for (const auto& param_name : route.param_names) {
                auto parameter = parameters.push_object();
                parameter.set("name", param_name);
                parameter.set("in", "path");
                parameter.set("required", true);
                parameter.add_object("schema").set("type", "string");
            }

            // Query parameters
            for (const auto& query_param : route.query_params) {
                auto parameter = parameters.push_object();
                parameter.set("name", query_param.name);
                parameter.set("in", "query");
                parameter.set("required", query_param.required);
                auto schema = parameter.add_object("schema");
                schema.set("type", query_param.type);
                if (!query_param.format.empty()) {
                    schema.set("format", query_param.format);
                }
            }
        }

        // The schema a route states, with its $refs inlined. A schema that is
        // not JSON becomes an empty one, as it always has.
        const auto schema_from = [&](const std::string& json_schema, std::string_view what) {
            auto parsed = json_document::parse(json_schema);
            if (!parsed) {
                BOOST_LOG_SEV(lg(), warn)
                    << "Failed to parse " << what << " for route: " << route.pattern;
                return json_document::make_object();
            }
            return process_schema(std::move(*parsed));
        };

        // Request body
        if (route.body_schema.has_value()) {
            const auto& body = route.body_schema.value();

            auto request_body = operation.add_object("requestBody");
            request_body.set("required", body.required);

            auto media_type = request_body.add_object("content").add_object(body.content_type);
            media_type.add("schema", schema_from(body.json_schema, "JSON schema").root());
            add_example(media_type, body.example_json);
        }

        // Security
        if (route.requires_auth) {
            auto security = operation.add_array("security");
            security.push_object().add_array("bearerAuth");
        }

        // Responses
        std::map<std::string, json_document> responses;
        if (route.success_response_schema.has_value()) {
            const auto& response_schema = route.success_response_schema.value();

            auto response_document = json_document::make_object();
            auto response = response_document.root();
            response.set("description", response_schema.description);

            auto media_type =
                response.add_object("content").add_object(response_schema.content_type);
            media_type.add("schema",
                           schema_from(response_schema.json_schema, "response JSON schema").root());
            add_example(media_type, response_schema.example_json);

            responses.emplace("200", std::move(response_document));
        } else {
            responses.emplace("200", make_response("Successful response"));
        }

        if (route.requires_auth) {
            responses.emplace("401", make_response("Unauthorized"));
            responses.emplace("403", make_response("Forbidden"));
        }
        responses.emplace("500", make_response("Internal server error"));

        auto response_members = operation.add_object("responses");
        for (const auto& [status, response] : responses) {
            response_members.add(status, response.root());
        }

        paths[route.pattern][method] = std::move(operation_document);
    }

    auto paths_members = root.add_object("paths");
    for (const auto& [pattern, operations] : paths) {
        auto path = paths_members.add_object(pattern);
        for (const auto& [method, operation] : operations) {
            path.add(method, operation.root());
        }
    }

    BOOST_LOG_SEV(lg(), debug) << "OpenAPI JSON spec generated";
    return spec.write();
}

std::string endpoint_registry::generate_swagger_ui_html(const std::string& spec_url) const {
    BOOST_LOG_SEV(lg(), debug) << "Generating Swagger UI HTML";

    std::ostringstream oss;
    oss << R"(<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>)"
        << info_.title << R"( - API Documentation</title>
    <link rel="stylesheet"
          href="https://cdnjs.cloudflare.com/ajax/libs/swagger-ui/5.29.1/swagger-ui.css"
          integrity="sha512-eZpfl9qlKnbDvlJ2brfdx3nhlP1FMsA23w65motxKdYsUcfMcdO2bcLPr7mXhvyzmDZwuzYCJKrl/sEo1ditVQ=="
          crossorigin="anonymous"
          referrerpolicy="no-referrer">
    <style>
        body { margin: 0; padding: 0; }
        .topbar { display: none; }
    </style>
</head>
<body>
    <div id="swagger-ui"></div>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/swagger-ui/5.29.1/swagger-ui-bundle.js"
            integrity="sha512-iDdi6WwSTimAFh1NhPyFZpHJWr16m/PupHztElqQL+gQoQDavUATsP9hcvgs9Yci+EUA//WvZYZsCTDhRDKc3g=="
            crossorigin="anonymous"
            referrerpolicy="no-referrer"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/swagger-ui/5.29.1/swagger-ui-standalone-preset.js"
            integrity="sha512-oGq+mP+3XUyYDm9vsprsd6YdhOokdkcLNUZnmEE6HJzG0F9fIaB8kgGZsjdfTWN8w/1f2kN4a/0xkla0hsetcQ=="
            crossorigin="anonymous"
            referrerpolicy="no-referrer"></script>
    <script>
        window.onload = function() {
            SwaggerUIBundle({
                url: ")"
        << spec_url << R"(",
                dom_id: '#swagger-ui',
                presets: [
                    SwaggerUIBundle.presets.apis,
                    SwaggerUIStandalonePreset
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
