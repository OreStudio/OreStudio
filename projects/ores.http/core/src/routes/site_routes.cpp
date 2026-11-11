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
#include "ores.http.core/routes/site_routes.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <algorithm>
#include <array>
#include <format>
#include <regex>
#include <yyjson.h>

namespace ores::http::routes {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::site::loaded_site;
namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;

namespace {

struct environment_view final {
    std::string id;
    std::string displayName;
    std::string description;
    bool nonProduction = true;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<environment_view>) {
    using ores::utility::codec::field;
    return std::tuple{field{"id", &environment_view::id},
                      field{"displayName", &environment_view::displayName},
                      field{"description", &environment_view::description},
                      field{"nonProduction", &environment_view::nonProduction}};
}

struct site_state_view final {
    std::string appName = "ORE Studio";
    environment_view environment;
    bool developerTools = false;
    std::vector<ores::http::site::developer_account> developerAccounts;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<site_state_view>) {
    using ores::utility::codec::field;
    return std::tuple{field{"appName", &site_state_view::appName},
                      field{"environment", &site_state_view::environment},
                      field{"developerTools", &site_state_view::developerTools},
                      field{"developerAccounts", &site_state_view::developerAccounts}};
}

struct available_view final {
    std::string id;
    std::string displayName;
    bool nonProduction = true;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<available_view>) {
    using ores::utility::codec::field;
    return std::tuple{field{"id", &available_view::id},
                      field{"displayName", &available_view::displayName},
                      field{"nonProduction", &available_view::nonProduction}};
}

struct deployment_view final {
    ores::http::site::environment environment;
    std::string configFile;
    bool developerTools = false;
    std::vector<available_view> available;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<deployment_view>) {
    using ores::utility::codec::field;
    return std::tuple{field{"environment", &deployment_view::environment},
                      field{"configFile", &deployment_view::configFile},
                      field{"developerTools", &deployment_view::developerTools},
                      field{"available", &deployment_view::available}};
}

/// The keys a journal record already uses, which a browser fact may not replace.
constexpr std::array<std::string_view, 9> reserved{
    "level", "time", "msg", "name", "pid", "hostname", "req", "res", "err"};

ores::logging::boost_severity severity_of(const std::string& level) {
    using enum ores::logging::boost_severity;
    if (level == "trace")
        return trace;
    if (level == "debug")
        return debug;
    if (level == "info")
        return info;
    if (level == "warn")
        return warn;
    return error;
}

std::string text_of(yyjson_val* v) {
    if (yyjson_is_str(v))
        return std::string(yyjson_get_str(v), yyjson_get_len(v));
    if (yyjson_is_bool(v))
        return yyjson_get_bool(v) ? "true" : "false";
    if (yyjson_is_int(v))
        return std::to_string(yyjson_get_sint(v));
    return std::format("{}", yyjson_get_num(v));
}

std::optional<site_routes::client_entry> entry_of(yyjson_val* v) {
    static const std::regex logger_form(R"(^ores\.web\.client(\.[a-z][a-z0-9_]*)*$)");
    static const std::regex key_form("^[a-z][a-z0-9_]*$");
    static constexpr std::array<std::string_view, 5> levels{
        "trace", "debug", "info", "warn", "error"};
    if (!yyjson_is_obj(v))
        return std::nullopt;
    const auto text = [&](const char* name, std::size_t limit) -> std::optional<std::string> {
        auto* member = yyjson_obj_get(v, name);
        if (!yyjson_is_str(member) || yyjson_get_len(member) > limit)
            return std::nullopt;
        return std::string(yyjson_get_str(member), yyjson_get_len(member));
    };
    auto at = text("at", 40);
    auto level = text("level", 5);
    auto logger = text("logger", 200);
    auto message = text("message", 200);
    if (!at || !level || !logger || !message || std::ranges::find(levels, *level) == levels.end() ||
        !std::regex_match(*logger, logger_form))
        return std::nullopt;
    site_routes::client_entry entry{*at, *level, *logger, *message, {}};
    if (auto* fields = yyjson_obj_get(v, "fields")) {
        if (!yyjson_is_obj(fields))
            return std::nullopt;
        yyjson_val* key = nullptr;
        yyjson_obj_iter it = yyjson_obj_iter_with(fields);
        while ((key = yyjson_obj_iter_next(&it))) {
            auto* value = yyjson_obj_iter_get_val(key);
            const std::string name(yyjson_get_str(key), yyjson_get_len(key));
            const bool fits = (yyjson_is_str(value) && yyjson_get_len(value) <= 300) ||
                              yyjson_is_num(value) || yyjson_is_bool(value);
            if (!std::regex_match(name, key_form) || !fits)
                return std::nullopt;
            entry.fields.emplace_back(name, text_of(value));
        }
    }
    return entry;
}

http_response not_offered() {
    return rr::answer(rr::refusal{
        http_status::not_found, "invalid-request", "No developer surface on this deployment."});
}

}

http_response site_routes::site_state(const loaded_site& site) {
    site_state_view view;
    view.environment = {site.served.id,
                        site.served.displayName,
                        site.served.description,
                        site.served.nonProduction};
    view.developerTools = site.configuration.developerTools;
    if (view.developerTools)
        view.developerAccounts = site.configuration.developerAccounts;
    return http_response::json(json::write(view));
}

http_response site_routes::deployment(const loaded_site& site) {
    if (!site.configuration.developerTools)
        return not_offered();
    deployment_view view;
    view.environment = site.served;
    view.configFile = site.source;
    view.developerTools = true;
    for (const auto& e : site.configuration.environments)
        view.available.push_back({e.id, e.displayName, e.nonProduction});
    return http_response::json(json::write(view));
}

std::optional<std::vector<site_routes::client_entry>>
site_routes::read_client_log(const std::string& body) {
    std::unique_ptr<yyjson_doc, decltype(&yyjson_doc_free)> doc(
        yyjson_read(body.data(), body.size(), 0), &yyjson_doc_free);
    if (!doc)
        return std::nullopt;
    auto* entries = yyjson_obj_get(yyjson_doc_get_root(doc.get()), "entries");
    if (!yyjson_is_arr(entries) || yyjson_arr_size(entries) > 100)
        return std::nullopt;
    std::vector<client_entry> out;
    std::size_t index = 0, count = 0;
    yyjson_val* item = nullptr;
    yyjson_arr_foreach(entries, index, count, item) {
        auto entry = entry_of(item);
        if (!entry)
            return std::nullopt;
        out.push_back(std::move(*entry));
    }
    return out;
}

http_response site_routes::handle_client_log(const http_request& req) {
    const auto entries = read_client_log(req.body);
    if (!entries)
        return rr::answer(rr::refusal{http_status::bad_request,
                                      "invalid-request",
                                      "The client log is not one the browser sends."});
    static auto lg = ores::logging::make_logger("ores.web.client");
    const auto& claims = req.authenticated_user;
    const auto session_id = claims ? claims->session_id.value_or("") : std::string{};
    const auto account = claims ? claims->username.value_or("") : std::string{};
    for (const auto& entry : *entries) {
        std::string facts;
        for (const auto& [key, value] : entry.fields)
            if (std::ranges::find(reserved, key) == reserved.end())
                facts += " " + key + "=" + value;
        BOOST_LOG_SEV(lg, severity_of(entry.level))
            << "[" << entry.logger << "] " << entry.message << facts << " session_id=" << session_id
            << " account=" << account << " client_time=" << entry.at;
    }
    return http_response::json(R"({"ok":true})");
}

void site_routes::register_routes(std::shared_ptr<ores::http::net::router> router,
                                  std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                  std::optional<loaded_site> site) {
    const auto add = [&](auto builder) {
        const auto built = builder.build();
        router->add_route(built);
        registry->register_route(built);
    };
    add(router->get("/api/health")
            .summary("Answer that the server is alive")
            .tags({"site"})
            .auth_optional()
            .handler([](const http_request&) -> boost::asio::awaitable<http_response> {
                co_return http_response::json(R"({"status":"ok"})");
            }));
    add(router->post("/api/client-log")
            .summary("Write the browser's trail into the journal")
            .tags({"site"})
            .auth_required()
            .handler([](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return handle_client_log(req);
            }));
    if (!site)
        return;
    const auto shared = std::make_shared<const loaded_site>(std::move(*site));
    add(router->get("/api/site")
            .summary("Read what the client needs to render itself")
            .tags({"site"})
            .auth_optional()
            .handler([shared](const http_request&) -> boost::asio::awaitable<http_response> {
                co_return site_state(*shared);
            }));
    add(router->get("/api/site/deployment")
            .summary("Read the deployment's plumbing, where the developer surface is offered")
            .tags({"site"})
            .auth_optional()
            .handler([shared](const http_request&) -> boost::asio::awaitable<http_response> {
                co_return deployment(*shared);
            }));
}

}
