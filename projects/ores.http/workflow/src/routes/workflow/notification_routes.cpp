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
#include "ores.http/routes/workflow/notification_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.inbox.api/messaging/notification_operations_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <charconv>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <vector>

namespace ores::http::routes::workflow {

namespace notices {

/// The notices a write names: empty means every one the write applies to.
struct ids_body final {
    std::vector<std::string> ids;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<ids_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"ids", &ids_body::ids}};
}

}

namespace {


namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace inbox_messaging = ores::inbox::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;

/// The most rows one page may ask for.
constexpr std::uint32_t notice_page_max = 100;

/// The page a read answers with when the caller states none.
constexpr std::uint32_t notice_page_default = 50;

/// The sentence a page states when it is not one.
constexpr std::string_view notice_page_refused = "A page names an offset and a limit of 1 to 100.";

/// The sentence a mark states when its body is not the shape it takes.
constexpr std::string_view read_refused =
    "Marking notifications read names the ones to mark, or an empty list for all unread.";

/// The sentence a clear states when its body is not the shape it takes.
constexpr std::string_view clear_refused =
    "Clearing notifications names the ones to clear, or an empty list for all read.";

/// A page of a list, and whether only the unread ones are asked for.
struct notice_page final {
    std::uint32_t offset = 0;
    std::uint32_t limit = notice_page_default;
    bool unread_only = false;
};

/// The whole number a page parameter states, or nothing when it states none.
std::optional<std::uint32_t> notice_whole_of(const std::string& raw) {
    if (raw.empty())
        return std::nullopt;
    std::uint32_t value = 0;
    const auto* first = raw.data();
    const auto* last = raw.data() + raw.size();
    const auto parsed = std::from_chars(first, last, value);
    if (parsed.ec != std::errc() || parsed.ptr != last)
        rr::invalid(std::string(notice_page_refused));
    return value;
}

notice_page notice_page_of(const http_request& req) {
    rr::refuse_unknown_params(req, {"offset", "limit", "unreadOnly"});
    notice_page out;
    if (const auto stated = notice_whole_of(req.get_query_param("offset")))
        out.offset = *stated;
    if (const auto stated = notice_whole_of(req.get_query_param("limit"))) {
        if (*stated < 1 || *stated > notice_page_max)
            rr::invalid(std::string(notice_page_refused));
        out.limit = *stated;
    }
    const auto unread = req.get_query_param("unreadOnly");
    if (unread == "true")
        out.unread_only = true;
    else if (!unread.empty() && unread != "false")
        rr::invalid(std::string(notice_page_refused));
    return out;
}

/// One notice as the bell draws it.
std::string notification_json(const inbox_messaging::inbox_notification& notice) {
    std::vector<std::string> written;
    written.reserve(notice.arguments.size());
    for (const auto& argument : notice.arguments)
        written.push_back("{\"name\":" + json::write(argument.name) +
                          ",\"value\":" + json::write(argument.value) + "}");

    return "{\"id\":" + json::write(notice.id) + ",\"kindCode\":" + json::write(notice.kind_code) +
           ",\"messageKey\":" + json::write(notice.message_key) +
           ",\"raisedBy\":" + json::write(notice.raised_by) +
           ",\"raisedAt\":" + json::write(notice.raised_at) +
           ",\"linkRoute\":" + json::write(notice.link_route) +
           ",\"linkId\":" + json::write(notice.link_id) +
           ",\"arguments\":" + rr::array_of(written) +
           ",\"readAt\":" + json::write(notice.read_at) + "}";
}

/// The notices a write names, refused when a member is not the shape it takes.
notices::ids_body ids_of(const http_request& req, std::string_view sentence) {
    const auto body = rr::body_of<notices::ids_body>(req, {"ids"}, std::string(sentence));
    if (body.ids.size() > notice_page_max)
        rr::invalid(std::string(sentence));
    for (const auto& id : body.ids)
        if (!rr::is_uuid(id))
            rr::invalid(std::string(sentence));
    return body;
}

}

void notification_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"workflow"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/me/notifications")
            .summary("The notices the caller is given, newest first")
            .description("A page of the notices the signed-in person holds, or only the unread "
                         "ones, each with the words its wording names.")
            .query_param("offset", "integer", "int32", false, "How many notices to skip")
            .query_param("limit", "integer", "int32", false, "How many notices to return")
            .query_param("unreadOnly", "boolean", "", false, "Only the notices not yet read")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<inbox_messaging::list_my_notifications_request>([&] {
                    const auto asked = notice_page_of(req);
                    inbox_messaging::list_my_notifications_request msg;
                    msg.unread_only = asked.unread_only;
                    msg.offset = static_cast<int>(asked.offset);
                    msg.limit = static_cast<int>(asked.limit);
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);

                    std::vector<std::string> written;
                    written.reserve(reply.notifications.size());
                    for (const auto& notice : reply.notifications)
                        written.push_back(notification_json(notice));
                    return http_response::json("{\"items\":" + rr::array_of(written) +
                                               ",\"total\":" + std::to_string(reply.total) + "}");
                });
            }));

    add(router->get("/api/me/notifications/unread-count")
            .summary("How many notices the caller has not read")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<inbox_messaging::count_unread_notifications_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto reply = forwarding::forward(
                        req, session, inbox_messaging::count_unread_notifications_request{});
                    rr::require_ok(reply.result);
                    return http_response::json("{\"unread\":" + std::to_string(reply.unread) + "}");
                });
            }));

    add(router->post("/api/me/notifications/read")
            .summary("Mark notices read, or every unread one when the list is empty")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<inbox_messaging::mark_notifications_read_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto body = ids_of(req, read_refused);
                    inbox_messaging::mark_notifications_read_request msg;
                    msg.notification_ids = body.ids;
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);
                    return http_response::json("{\"marked\":" + std::to_string(reply.marked) + "}");
                });
            }));

    add(router->post("/api/me/notifications/clear")
            .summary("Clear notices, or every read one when the list is empty")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<inbox_messaging::clear_notifications_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto body = ids_of(req, clear_refused);
                    inbox_messaging::clear_notifications_request msg;
                    msg.notification_ids = body.ids;
                    const auto reply = forwarding::forward(req, session, msg);
                    rr::require_ok(reply.result);
                    return http_response::json("{\"cleared\":" + std::to_string(reply.cleared) +
                                               "}");
                });
            }));
}

}
