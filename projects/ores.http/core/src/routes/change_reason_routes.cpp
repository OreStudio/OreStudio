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
#include "ores.http.core/routes/change_reason_routes.hpp"
#include "ores.dq.api/messaging/change_reason_protocol.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <charconv>
#include <cstdint>
#include <limits>
#include <string>

namespace ores::http::routes {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;
namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;

namespace {

/// A whole number query parameter from lowest to highest, its default when left out, or a refusal.
std::uint32_t reason_page_number(const http_request& req,
                                 const std::string& name,
                                 std::uint32_t lowest,
                                 std::uint32_t highest,
                                 std::uint32_t fallback) {
    const auto raw = req.get_query_param(name);
    if (raw.empty())
        return fallback;
    std::uint32_t value = 0;
    const auto [end, error] = std::from_chars(raw.data(), raw.data() + raw.size(), value);
    if (error != std::errc{} || end != raw.data() + raw.size() || value < lowest || value > highest)
        rr::invalid("The " + name + " is a whole number from " + std::to_string(lowest) + " to " +
                    std::to_string(highest) + ".");
    return value;
}

}

void change_reason_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    const auto built =
        router->get("/api/change-reasons")
            .summary("Read the reasons a screen offers for a write")
            .tags({"change-reasons"})
            .query_param("offset", "integer", "", false, "Reasons to skip", "0")
            .query_param("limit", "integer", "", false, "Reasons to read, 1 to 1000", "200")
            .auth_required()
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return ores::http::forwarding::guarded<
                    ores::dq::messaging::list_change_reasons_request>([&] {
                    ores::dq::messaging::list_change_reasons_request msg;
                    msg.offset = reason_page_number(
                        req, "offset", 0, std::numeric_limits<std::uint32_t>::max(), 0);
                    msg.limit = reason_page_number(req, "limit", 1, 1000, 200);
                    const auto reply = ores::http::forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);
                    std::string out = "{\"reasons\":[";
                    for (std::size_t i = 0; i < reply.reasons.size(); ++i) {
                        const auto& reason = reply.reasons[i];
                        out += i == 0 ? "{" : ",{";
                        out += "\"code\":" + json::write(reason.code);
                        out += ",\"description\":" + json::write(reason.description);
                        out += ",\"categoryCode\":" + json::write(reason.category_code);
                        out += ",\"appliesToNew\":" + json::write(reason.applies_to_new);
                        out += ",\"appliesToAmend\":" + json::write(reason.applies_to_amend);
                        out += ",\"appliesToDelete\":" + json::write(reason.applies_to_delete);
                        out += ",\"requiresCommentary\":" + json::write(reason.requires_commentary);
                        out += ",\"displayOrder\":" + json::write(reason.display_order) + "}";
                    }
                    return http_response::json(out + "]}");
                });
            })
            .build();
    router->add_route(built);
    registry->register_route(built);
}

}
