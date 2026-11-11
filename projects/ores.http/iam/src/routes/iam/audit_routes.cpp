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
#include "ores.http/routes/iam/audit_routes.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/iam/access_read_routes.hpp"
#include "ores.iam.api/messaging/geo_operations_protocol.hpp"
#include "ores.iam.api/messaging/session_operations_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/string_generator.hpp>
#include <algorithm>
#include <charconv>
#include <cmath>
#include <cstdint>
#include <format>
#include <functional>
#include <limits>

namespace ores::http::routes::iam {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace json = ores::utility::codec::json;
namespace messaging = ores::iam::messaging;
using ores::http::net::record_routes::characters_of;
using ores::http::forwarding::guarded;
using ores::http::net::record_routes::refused;

namespace {

std::optional<std::uint32_t> page_number(const http_request& req,
                                         const std::string& name,
                                         std::uint32_t fallback,
                                         std::uint32_t least,
                                         std::uint32_t most) {
    const auto text = req.get_query_param(name);
    if (text.empty())
        return fallback;
    std::uint32_t value = 0;
    const auto [end, failed] = std::from_chars(text.data(), text.data() + text.size(), value);
    if (failed != std::errc{} || end != text.data() + text.size() || value < least || value > most)
        return std::nullopt;
    return value;
}

/// A number as JSON states it: a value JSON cannot hold, such as an average over
/// no sessions, is null, as the BFF's JSON.stringify wrote it.
std::string number_text(double value) {
    return std::isfinite(value) ? std::format("{}", value) : std::string("null");
}

}

std::optional<audit_routes::audit_query>
audit_routes::read_query(const http_request& req,
                         std::initializer_list<std::string_view> keys,
                         std::chrono::system_clock::time_point now) {
    for (const auto& [key, value] : req.query_params)
        if (std::ranges::find(keys, std::string_view(key)) == keys.end())
            return std::nullopt;
    audit_query out;
    out.account_id = req.get_query_param("accountId");
    out.event_type = req.get_query_param("eventType");
    const auto period = req.get_query_param("period");
    const auto offset = page_number(req, "offset", 0, 0, std::numeric_limits<std::uint32_t>::max());
    const auto limit = page_number(req, "limit", 100, 1, 500);
    if (characters_of(out.account_id) > 64 || characters_of(out.event_type) > 64 || !offset ||
        !limit)
        return std::nullopt;
    using namespace std::chrono;
    std::optional<seconds> reach;
    if (period.empty() || period == "day")
        reach = hours(24);
    else if (period == "hour")
        reach = hours(1);
    else if (period == "week")
        reach = hours(24 * 7);
    else if (period != "all")
        return std::nullopt;
    if (reach) {
        out.from = now - *reach;
        out.to = now;
    }
    out.offset = *offset;
    out.limit = *limit;
    return out;
}

std::string audit_routes::events_json(const messaging::list_auth_events_response& reply) {
    std::string out = "{\"events\":[";
    for (std::size_t i = 0; i < reply.events.size(); ++i) {
        const auto& e = reply.events[i];
        out += (i == 0 ? "{" : ",{");
        out += "\"id\":" + json::write(e.id);
        out += ",\"eventTime\":" + json::write(e.event_time);
        out += ",\"accountId\":" + json::write(e.account_id);
        out += ",\"eventType\":" + json::write(e.event_type);
        out += ",\"username\":" + json::write(e.username);
        out += ",\"sessionId\":" + json::write(e.session_id);
        out += ",\"partyId\":" + json::write(e.party_id);
        out += ",\"errorDetail\":" + json::write(e.error_detail) + "}";
    }
    return out + "]}";
}

std::string audit_routes::statistics_json(const messaging::get_session_statistics_response& reply) {
    std::string out = "{\"rows\":[";
    for (std::size_t i = 0; i < reply.rows.size(); ++i) {
        const auto& r = reply.rows[i];
        out += (i == 0 ? "{" : ",{");
        out += "\"day\":" + json::write(r.day);
        out += ",\"accountId\":" + json::write(r.account_id);
        out += ",\"sessionCount\":" + std::to_string(r.session_count);
        out += ",\"avgDurationSeconds\":" + number_text(r.avg_duration_seconds);
        out += ",\"totalBytesSent\":" + std::to_string(r.total_bytes_sent);
        out += ",\"totalBytesReceived\":" + std::to_string(r.total_bytes_received);
        out += ",\"avgBytesSent\":" + number_text(r.avg_bytes_sent);
        out += ",\"avgBytesReceived\":" + number_text(r.avg_bytes_received);
        out += ",\"uniqueCountries\":" + std::to_string(r.unique_countries) + "}";
    }
    return out + "]}";
}

void audit_routes::register_routes(std::shared_ptr<ores::http::net::router> router,
                                   std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                   nats_client& session) {
    const auto add = [&](ores::http::net::route_builder builder,
                         std::string summary,
                         std::function<http_response(const http_request&)> handle) {
        const auto built =
            builder.summary(std::move(summary))
                .tags({"auth"})
                .auth_required()
                .handler([handle = std::move(handle)](const http_request& req)
                             -> boost::asio::awaitable<http_response> { co_return handle(req); })
                .build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/auth-events"),
        "Read the sign-in events of a period",
        [&session](const http_request& req) {
            const auto query = read_query(req,
                                          {"accountId", "eventType", "period", "offset", "limit"},
                                          std::chrono::system_clock::now());
            if (!query)
                return refused(
                    http_status::bad_request,
                    "invalid-request",
                    "The auth events read takes an account, a period, an event and a page.");
            return guarded<messaging::list_auth_events_request>([&] {
                messaging::list_auth_events_request list;
                list.account_id = query->account_id;
                list.event_type = query->event_type;
                list.from_time = query->from;
                list.to_time = query->to;
                list.offset = query->offset;
                list.limit = query->limit;
                return http_response::json(
                    events_json(ores::http::forwarding::forward(req, session, list)));
            });
        });

    add(router->get("/api/session-statistics"),
        "Read the session statistics of a period",
        [&session](const http_request& req) {
            const auto query = read_query(
                req, {"accountId", "period", "offset", "limit"}, std::chrono::system_clock::now());
            if (!query)
                return refused(
                    http_status::bad_request,
                    "invalid-request",
                    "The session statistics read takes an account, a period and a page.");
            return guarded<messaging::get_session_statistics_request>([&] {
                messaging::get_session_statistics_request get;
                get.account_id = query->account_id;
                get.from_time = query->from;
                get.to_time = query->to;
                get.offset = query->offset;
                get.limit = query->limit;
                return http_response::json(
                    statistics_json(ores::http::forwarding::forward(req, session, get)));
            });
        });

    add(router->get("/api/geo/country"),
        "Read the country an address is in",
        [&session](const http_request& req) {
            const auto address = req.get_query_param("address");
            const auto only_address = std::ranges::all_of(
                req.query_params, [](const auto& entry) { return entry.first == "address"; });
            if (!only_address || address.empty() || characters_of(address) > 64)
                return refused(http_status::bad_request,
                               "invalid-request",
                               "The country lookup takes one address.");
            return guarded<messaging::lookup_country_request>([&] {
                const auto reply = ores::http::forwarding::forward(
                    req, session, messaging::lookup_country_request{address});
                if (!reply.success)
                    return refused(http_status::conflict,
                                   "invalid-request",
                                   reply.message.empty() ? "The country lookup failed." :
                                                           reply.message);
                return http_response::json(
                    "{\"found\":" + std::string(reply.found ? "true" : "false") +
                    ",\"countryCode\":" + json::write(reply.found ? reply.country_code : "") + "}");
            });
        });

    add(router->post("/api/sessions/{sessionId}/end"),
        "End one session",
        [&session](const http_request& req) {
            const auto id = req.get_path_param("sessionId");
            if (!access_read_routes::is_uuid(id))
                return refused(
                    http_status::bad_request, "invalid-request", "A session id is required.");
            return guarded<messaging::end_session_request>([&] {
                const auto reply = ores::http::forwarding::forward(
                    req,
                    session,
                    messaging::end_session_request{boost::uuids::string_generator()(id)});
                if (!reply.success)
                    return refused(http_status::conflict,
                                   "invalid-request",
                                   reply.message.empty() ?
                                       "The server refused to end the session." :
                                       reply.message);
                return http_response::json(R"({"success":true})");
            });
        });
}

}
