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
#include "ores.http/routes/iam/tenancy_catalogue_routes.hpp"
#include "ores.dq.api/messaging/badge_definition_protocol.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.iam.api/messaging/tenant_status_protocol.hpp"
#include "ores.iam.api/messaging/tenant_type_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <algorithm>
#include <charconv>
#include <functional>

namespace ores::http::routes::iam {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::http::forwarding::guarded;
using ores::http::net::record_routes::refused;
using ores::nats::service::nats_client;
namespace json = ores::utility::codec::json;

namespace {

/// The badge a code is painted with, or null when the catalogue holds none.
std::string badge_json(const std::string& code,
                       const std::vector<ores::dq::domain::badge_definition>& badges) {
    const auto found = std::ranges::find(badges, code, &ores::dq::domain::badge_definition::code);
    if (code.empty() || found == badges.end())
        return "null";
    return "{\"code\":" + json::write(found->code) + ",\"label\":" + json::write(found->name) +
           ",\"description\":" + json::write(found->description) +
           ",\"backgroundColour\":" + json::write(found->background_colour) +
           ",\"textColour\":" + json::write(found->text_colour) +
           ",\"severity\":" + json::write(found->severity_code) + "}";
}

template <typename Row>
std::string coded_row(const std::string& code,
                      const Row& row,
                      const std::vector<ores::dq::domain::badge_definition>& badges) {
    return "{\"code\":" + json::write(code) + ",\"name\":" + json::write(row.name) +
           ",\"description\":" + json::write(row.description) +
           ",\"badge\":" + badge_json(row.badge_code, badges) + "}";
}

/// The badge catalogue, read as the BFF read it: one page of 200, which holds
/// every badge the platform defines.
std::vector<ores::dq::domain::badge_definition> badges_of(const http_request& req,
                                                          nats_client& session) {
    ores::dq::messaging::list_badge_definitions_request list;
    list.limit = 200;
    return ores::http::forwarding::forward(req, session, list).definitions;
}

}

std::string tenancy_catalogue_routes::statuses_json(
    const std::vector<ores::iam::domain::tenant_status>& statuses,
    const std::vector<ores::dq::domain::badge_definition>& badges) {
    std::string out = "{\"statuses\":[";
    for (std::size_t i = 0; i < statuses.size(); ++i)
        out += (i == 0 ? "" : ",") + coded_row(statuses[i].status, statuses[i], badges);
    return out + "]}";
}

std::string tenancy_catalogue_routes::types_json(
    std::vector<ores::iam::domain::tenant_type> types,
    const std::vector<ores::dq::domain::badge_definition>& badges) {
    std::ranges::stable_sort(types, {}, &ores::iam::domain::tenant_type::display_order);
    std::string out = "{\"types\":[";
    for (std::size_t i = 0; i < types.size(); ++i)
        out += (i == 0 ? "" : ",") + coded_row(types[i].type, types[i], badges);
    return out + "]}";
}

std::string tenancy_catalogue_routes::entities_json(
    const ores::dq::messaging::search_lei_entities_response& reply) {
    std::string out = "{\"entities\":[";
    for (std::size_t i = 0; i < reply.entities.size(); ++i) {
        const auto& e = reply.entities[i];
        out += (i == 0 ? "{" : ",{");
        out += "\"lei\":" + json::write(e.lei);
        out += ",\"legalName\":" + json::write(e.entity_legal_name);
        out += ",\"country\":" + json::write(e.country);
        out += ",\"partyCount\":" + std::to_string(e.party_count) + "}";
    }
    return out + "]}";
}

int tenancy_catalogue_routes::lei_limit(const std::string& text) {
    int value = 0;
    const auto [end, failed] = std::from_chars(text.data(), text.data() + text.size(), value);
    const auto read = !text.empty() && failed == std::errc{} && end == text.data() + text.size();
    return read && value > 0 ? value : 20;
}

void tenancy_catalogue_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    const auto add = [&](ores::http::net::route_builder builder,
                         std::string summary,
                         std::function<http_response(const http_request&)> handle) {
        const auto built =
            builder.summary(std::move(summary))
                .tags({"tenancy"})
                .auth_required()
                .handler([handle = std::move(handle)](const http_request& req)
                             -> boost::asio::awaitable<http_response> { co_return handle(req); })
                .build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/tenant-statuses"),
        "Read the tenant statuses with their badges",
        [&session](const http_request& req) {
            return guarded<ores::iam::messaging::list_tenant_statuses_request>([&] {
                const auto listed = ores::http::forwarding::forward(
                    req, session, ores::iam::messaging::list_tenant_statuses_request{});
                return http_response::json(statuses_json(listed.statuses, badges_of(req, session)));
            });
        });

    add(router->get("/api/tenant-types"),
        "Read the tenant types with their badges",
        [&session](const http_request& req) {
            return guarded<ores::iam::messaging::list_tenant_types_request>([&] {
                const auto listed = ores::http::forwarding::forward(
                    req, session, ores::iam::messaging::list_tenant_types_request{});
                return http_response::json(types_json(listed.types, badges_of(req, session)));
            });
        });

    add(router->get("/api/lei-entities"),
        "Search the legal entities a tenant may start from",
        [&session](const http_request& req) {
            ores::dq::messaging::search_lei_entities_request search;
            search.search = req.get_query_param("search");
            search.country_filter = req.get_query_param("country");
            search.limit = lei_limit(req.get_query_param("limit"));
            return guarded<ores::dq::messaging::search_lei_entities_request>([&] {
                const auto reply = ores::http::forwarding::forward(req, session, search);
                if (!reply.success)
                    return refused(
                        http_status::bad_request, "invalid-request", reply.error_message);
                return http_response::json(entities_json(reply));
            });
        });
}

}
