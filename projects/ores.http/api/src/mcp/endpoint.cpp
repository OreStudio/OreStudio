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
#include "ores.http.api/mcp/endpoint.hpp"
#include "ores.http.api/net/router.hpp"
#include "ores.http.api/openapi/endpoint_registry.hpp"
#include "ores.utility/codec/json_document.hpp"
#include <algorithm>
#include <array>
#include <cctype>

namespace ores::http::mcp {

using namespace ores::logging;
using domain::http_request;
using domain::http_response;
using domain::http_status;
using ores::utility::codec::json_document;
using ores::utility::codec::json_value;

namespace {

constexpr auto served_versions =
    std::to_array<std::string_view>({"2025-11-25", "2025-06-18", "2025-03-26"});

/// JSON-RPC 2.0's error codes.
namespace code {
constexpr std::int64_t parse_error = -32700;
constexpr std::int64_t invalid_request = -32600;
constexpr std::int64_t method_not_found = -32601;
constexpr std::int64_t invalid_params = -32602;
}

constexpr std::string_view session_header = "Mcp-Session-Id";

std::string owner_of(const http_request& req) {
    const auto& claims = *req.authenticated_user;
    return claims.subject + "\n" + claims.act.value_or("");
}

bool names_an_agent(const http_request& req) {
    return req.authenticated_user && req.authenticated_user->act &&
           req.authenticated_user->act->starts_with("agent:");
}

http_response with_json(const json_document& doc, http_status status = http_status::ok) {
    return http_response::json(doc.write(), status);
}

/// A JSON-RPC envelope answering the request whose id is given, or null.
json_document envelope(const json_value& id) {
    auto doc = json_document::make_object();
    auto root = doc.root();
    root.set("jsonrpc", "2.0");
    if (id.exists())
        root.add("id", id);
    else
        root.set_null("id");
    return doc;
}

http_response rpc_error(const json_value& id,
                        std::int64_t error_code,
                        std::string_view message,
                        http_status status = http_status::ok) {
    auto doc = envelope(id);
    auto error = doc.root().add_object("error");
    error.set("code", error_code);
    error.set("message", message);
    return with_json(doc, status);
}

http_response accepted() {
    http_response response;
    response.status = http_status::accepted;
    return response;
}

std::string lowercase(std::string_view text) {
    std::string out(text);
    std::ranges::transform(
        out, out.begin(), [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
    return out;
}

/// Whether an origin is this machine's: a page served from it is not the
/// stranger DNS rebinding disguises, which names its own host.
bool is_local_origin(std::string_view origin) {
    const auto scheme = origin.find("://");
    if (scheme == std::string_view::npos)
        return false;
    auto host = origin.substr(scheme + 3);
    if (host.starts_with("[::1]"))
        return host.size() == 5 || host[5] == ':';
    host = host.substr(0, host.find(':'));
    return host == "localhost" || host == "127.0.0.1";
}

}

endpoint::endpoint(std::shared_ptr<const tool_provider> tools,
                   endpoint_options options,
                   std::shared_ptr<session_registry> sessions)
    : tools_(std::move(tools))
    , options_(std::move(options))
    , sessions_(std::move(sessions)) {}

bool endpoint::serves_version(std::string_view version) {
    return std::ranges::find(served_versions, version) != served_versions.end();
}

std::optional<http_response> endpoint::refusal(const http_request& req) const {
    if (!names_an_agent(req)) {
        auto refused = http_response::unauthorized("MCP is served to an LLM agent's key.");
        refused.set_header("WWW-Authenticate", "Bearer");
        return refused;
    }
    const auto origin = req.get_header("origin");
    if (!origin.empty() && !is_local_origin(lowercase(origin)) &&
        std::ranges::none_of(options_.allowed_origins, [&](const std::string& allowed) {
            return lowercase(allowed) == lowercase(origin);
        })) {
        ORES_LOG_SEV(lg(), warn) << "Refused an MCP request from origin " << origin;
        return http_response::forbidden("The Origin is not one this endpoint serves.");
    }
    const auto version = req.get_header("mcp-protocol-version");
    if (!version.empty() && !serves_version(version))
        return http_response::bad_request(
            "The MCP-Protocol-Version is not one this endpoint serves.");
    return std::nullopt;
}

boost::asio::awaitable<http_response> endpoint::post(const http_request& req) const {
    if (auto refused = refusal(req))
        co_return *refused;

    const auto parsed = json_document::parse(req.body);
    if (!parsed)
        co_return rpc_error(
            {}, code::parse_error, "The body is not JSON.", http_status::bad_request);
    const auto message = parsed->root();
    if (message.is_array())
        co_return rpc_error(
            {}, code::invalid_request, "A batch is not served.", http_status::bad_request);
    if (!message.is_object() || message.member("jsonrpc").text_or() != "2.0")
        co_return rpc_error({},
                            code::invalid_request,
                            "The body is not a JSON-RPC 2.0 message.",
                            http_status::bad_request);

    const auto id = message.member("id");
    const auto method = message.member("method").text_or();
    const auto owner = owner_of(req);

    if (method == "initialize") {
        if (!id.exists())
            co_return rpc_error(
                {}, code::invalid_request, "initialize is a request.", http_status::bad_request);
        const auto asked = message.member("params").member("protocolVersion").text_or();
        const auto version = serves_version(asked) ? asked : std::string(latest_version);
        const auto session = sessions_->open(owner, version);

        auto doc = envelope(id);
        auto result = doc.root().add_object("result");
        result.set("protocolVersion", version);
        auto capabilities = result.add_object("capabilities");
        capabilities.add_object("tools").set("listChanged", false);
        auto server_info = result.add_object("serverInfo");
        server_info.set("name", options_.server_name);
        server_info.set("version", options_.server_version);
        if (!options_.instructions.empty())
            result.set("instructions", options_.instructions);
        ORES_LOG_SEV(lg(), info) << "Opened an MCP session at " << version << " for "
                                 << req.authenticated_user->act.value_or("");
        auto response = with_json(doc);
        response.set_header(std::string(session_header), session);
        co_return response;
    }

    const auto session = req.get_header("mcp-session-id");
    if (session.empty())
        co_return rpc_error(id,
                            code::invalid_request,
                            "The request names no Mcp-Session-Id.",
                            http_status::bad_request);
    if (sessions_->touch(session, owner) == session_registry::standing::absent)
        co_return rpc_error(
            id, code::invalid_request, "The session is not open.", http_status::not_found);

    if (!id.exists() || method.empty())
        co_return accepted();

    if (!id.is_string() && !id.is_integer())
        co_return rpc_error({},
                            code::invalid_request,
                            "A request's id is a string or an integer.",
                            http_status::bad_request);

    if (method == "ping") {
        auto doc = envelope(id);
        doc.root().add_object("result");
        co_return with_json(doc);
    }

    if (method == "tools/list") {
        auto doc = envelope(id);
        auto list = doc.root().add_object("result").add_array("tools");
        for (const auto& tool : tools_->list(req)) {
            auto entry = list.push_object();
            entry.set("name", tool.name);
            if (!tool.title.empty())
                entry.set("title", tool.title);
            entry.set("description", tool.description);
            const auto schema = json_document::parse(tool.input_schema);
            if (schema && schema->root().is_object())
                entry.add("inputSchema", schema->root());
            else
                entry.add_object("inputSchema").set("type", "object");
            entry.add_object("annotations").set("readOnlyHint", tool.read_only);
        }
        co_return with_json(doc);
    }

    if (method == "tools/call") {
        const auto params = message.member("params");
        const auto name = params.member("name").text_or();
        if (name.empty())
            co_return rpc_error(id, code::invalid_params, "tools/call names no tool.");
        const auto arguments = params.member("arguments");
        if (arguments.exists() && !arguments.is_object())
            co_return rpc_error(id, code::invalid_params, "A tool's arguments are an object.");

        std::optional<tool_result> answered;
        try {
            answered = co_await tools_->call(req, name, arguments);
        } catch (const std::exception& e) {
            ORES_LOG_SEV(lg(), error) << "The MCP tool " << name << " failed: " << e.what();
            answered =
                tool_result{.text = "The tool failed; nothing more is known.", .is_error = true};
        }
        if (!answered)
            co_return rpc_error(id, code::invalid_params, "No tool is named " + name + ".");

        auto doc = envelope(id);
        auto result = doc.root().add_object("result");
        auto content = result.add_array("content").push_object();
        content.set("type", "text");
        content.set("text", answered->text);
        result.set("isError", answered->is_error);
        co_return with_json(doc);
    }

    co_return rpc_error(id, code::method_not_found, "The method " + method + " is not served.");
}

http_response endpoint::get(const http_request& req) const {
    if (auto refused = refusal(req))
        return *refused;
    auto response =
        http_response::error(http_status::method_not_allowed,
                             "This endpoint sends nothing unasked, so it opens no stream.");
    response.set_header("Allow", "POST, DELETE");
    return response;
}

http_response endpoint::remove(const http_request& req) const {
    if (auto refused = refusal(req))
        return *refused;
    const auto session = req.get_header("mcp-session-id");
    if (session.empty())
        return http_response::bad_request("The request names no Mcp-Session-Id.");
    if (!sessions_->close(session, owner_of(req)))
        return http_response::not_found("The session is not open.");
    http_response response;
    response.status = http_status::no_content;
    return response;
}

void register_routes(const std::shared_ptr<const endpoint>& served,
                     const std::shared_ptr<net::router>& router,
                     const std::shared_ptr<openapi::endpoint_registry>& registry) {
    const auto add = [&](net::route_builder builder) {
        const auto built = builder.tags({"mcp"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };
    add(router->post("/mcp")
            .catalogue("mcp_post", "operations", "mcp", false)
            .transport()
            .json_body()
            .summary("Send one MCP message")
            .description("MCP's Streamable HTTP transport: one JSON-RPC 2.0 message, answered "
                         "with JSON. Served to an LLM agent's key.")
            .handler([served](const http_request& req) { return served->post(req); }));
    add(router->get("/mcp")
            .catalogue("mcp_get", "operations", "mcp", false)
            .transport()
            .summary("Open an MCP stream")
            .description("Answered with 405: the server sends nothing unasked.")
            .handler([served](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return served->get(req);
            }));
    add(router->delete_("/mcp")
            .catalogue("mcp_delete", "operations", "mcp", false)
            .transport()
            .no_body()
            .summary("End an MCP session")
            .description("Ends the session the Mcp-Session-Id header names.")
            .handler([served](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return served->remove(req);
            }));
}

}
