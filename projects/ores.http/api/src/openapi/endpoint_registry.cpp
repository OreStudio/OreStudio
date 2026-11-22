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
#include <array>
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

/// The name a group is listed under, and the words that describe it.
struct tag_detail final {
    std::string_view name;
    std::string_view description;
};

/**
 * @brief The areas the document groups its operations by, in the order the UI lists them.
 *
 * A tag is what the Swagger UI collapses a long list into, so each one names an area a
 * person would look for rather than a package of the code that answers it. The order is
 * the order of the journey: signing in, the records everything is described against, the
 * markets, and then the work done with them.
 */
constexpr std::array<tag_detail, 29> tag_details{{
    {"auth", "Signing in and out, and the session a browser keeps."},
    {"setup", "The first-run journey: the administrator, the tenant and what it starts with."},
    {"iam", "Accounts, roles and the permissions a role holds."},
    {"tenancy", "Tenants, their statuses and their own setup."},
    {"access", "Who holds what, and who has signed in."},
    {"refdata", "The shared reference records, and the records built from them."},
    {"classifications", "The lists that classify a record, and the rows of each list."},
    {"conventions", "The conventions a trade is written under."},
    {"calendars", "The calendars and the business days they name."},
    {"tenors", "The tenors a curve is quoted at."},
    {"currencies", "The currencies and the pairs they are quoted in."},
    {"counterparties", "The counterparties, their identifiers and their netting."},
    {"parties", "The parties of a tenant, and the books they own."},
    {"books", "Books, portfolios and the desk that owns them."},
    {"labels", "The labels a record can be marked with."},
    {"market-data", "Market series, their quotes, and the screens that read them."},
    {"synthetic", "The generation of simulated market data and its calibration."},
    {"trading", "Trades, their versions and their lifecycle."},
    {"reporting", "Report definitions, their runs and the files they produce."},
    {"analytics", "Pricing, sensitivities and the shifts they are read under."},
    {"workflow", "Workflow instances, their steps and their plans."},
    {"assets", "Images, the policy that governs them and the map that binds them."},
    {"grid", "The compute grid and the operations run on it."},
    {"site", "The served site: its configuration and the bundle's own files."},
    {"changes", "Why a change was made: the reasons and their categories."},
    {"history", "The record of what changed, and when."},
    {"operations", "Operational reads: versions, health and counters."},
    {"system-settings", "The settings this installation reads."},
    {"system", "The document itself."},
}};

/// The tags more than one route spells differently, mapped to the one area they belong to.
constexpr std::array<std::pair<std::string_view, std::string_view>, 2> tag_aliases{{
    {"marketdata", "market-data"},
    {"change-reasons", "changes"},
}};

/// The area @p tag names, after the spellings that mean the same area are settled.
std::string canonical_tag(std::string_view tag) {
    for (const auto& [alias, area] : tag_aliases)
        if (tag == alias)
            return std::string(area);
    return std::string(tag);
}

/// The description of @p tag, or nothing for an area the table does not name.
std::string_view description_of(std::string_view tag) {
    for (const auto& detail : tag_details)
        if (detail.name == tag)
            return detail.description;
    return {};
}

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

} // anonymous namespace

endpoint_registry::endpoint_registry() {
    ORES_LOG_SEV(lg(), debug) << "Endpoint registry created";
}

void endpoint_registry::set_info(const api_info& info) {
    info_ = info;
    ORES_LOG_SEV(lg(), debug) << "API info set: " << info.title << " v" << info.version;
}

void endpoint_registry::add_server(const server_info& server) {
    servers_.push_back(server);
    ORES_LOG_SEV(lg(), debug) << "Server added: " << server.url;
}

void endpoint_registry::register_route(const domain::route& route) {
    routes_.push_back(route);
    ORES_LOG_SEV(lg(), debug) << "Route registered: " << method_to_string(route.method) << " "
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
    ORES_LOG_SEV(lg(), debug) << "Generating OpenAPI JSON spec with " << routes_.size()
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
    // A browser holds a session in a cookie rather than a bearer token, and every route
    // reads either, so the document states both or a generated client authenticates in a
    // way the web client never does.
    auto cookie_auth = security_schemes.add_object("cookieAuth");
    cookie_auth.set("type", "apiKey");
    cookie_auth.set("in", "cookie");
    cookie_auth.set("name", session_cookie_name_);

    /*
     * The groups the UI collapses the list into, in the order of the table. An area a
     * route states and the table does not name is still a group, listed after the named
     * ones so that a tag a person adds is never lost from the document.
     */
    std::set<std::string> observed;
    for (const auto& route : routes_)
        for (const auto& tag : route.tags)
            observed.insert(canonical_tag(tag));
    auto tags_array = root.add_array("tags");
    for (const auto& detail : tag_details) {
        if (!observed.contains(std::string(detail.name)))
            continue;
        auto entry = tags_array.push_object();
        entry.set("name", detail.name);
        entry.set("description", detail.description);
    }
    for (const auto& tag : observed)
        if (description_of(tag).empty())
            tags_array.push_object().set("name", tag);

    // Build paths section
    std::map<std::string, std::map<std::string, json_document>> paths;
    for (const auto& route : routes_) {
        const auto method = method_to_string(route.method);

        auto operation_document = json_document::make_object();
        auto operation = operation_document.root();

        if (!route.tags.empty()) {
            auto tags = operation.add_array("tags");
            for (const auto& tag : route.tags) {
                tags.push(canonical_tag(tag));
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

        /*
         * The schema a route states, as the codec writes it: one object per route, with
         * every member expanded in place. A schema that is not JSON becomes an empty one,
         * as it always has.
         */
        const auto schema_from = [&](const std::string& json_schema, std::string_view what) {
            auto parsed = json_document::parse(json_schema);
            if (!parsed) {
                ORES_LOG_SEV(lg(), warn)
                    << "Failed to parse " << what << " for route: " << route.pattern;
                return json_document::make_object();
            }
            return std::move(*parsed);
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

        // Security: either a bearer token or the browser's session cookie.
        if (route.requires_auth) {
            auto security = operation.add_array("security");
            security.push_object().add_array("bearerAuth");
            security.push_object().add_array("cookieAuth");
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

    ORES_LOG_SEV(lg(), debug) << "OpenAPI JSON spec generated";
    return spec.write();
}

std::string endpoint_registry::generate_swagger_ui_html(const std::string& spec_url) const {
    ORES_LOG_SEV(lg(), debug) << "Generating Swagger UI HTML";

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
                showCommonExtensions: true,
                /*
                 * An area holds hundreds of operations, so each is a closed section a
                 * person opens, the filter narrows the list by name, and the order is the
                 * document's own rather than the alphabet's.
                 */
                docExpansion: "none",
                filter: true,
                tagsSorter: "none",
                operationsSorter: "method",
                persistAuthorization: true
            });
        };
    </script>
</body>
</html>)";

    return oss.str();
}

}
