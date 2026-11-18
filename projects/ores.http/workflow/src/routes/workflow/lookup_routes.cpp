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
#include "ores.http/routes/workflow/lookup_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.inbox.api/messaging/approval_kind_protocol.hpp"
#include "ores.inbox.api/messaging/approval_operations_protocol.hpp"
#include "ores.inbox.api/messaging/hold_reason_protocol.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <cstdint>
#include <string>
#include <string_view>
#include <vector>

namespace ores::http::routes::workflow {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace inbox_messaging = ores::inbox::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

/// The trail a read leaves when a lookup beside it is deliberately absent.
auto& lookup_lg() {
    static auto instance = ores::logging::make_logger("ores.http.workflow.lookup_routes");
    return instance;
}

/// The most rows a lookup read asks for; the services refuse a longer page.
constexpr std::uint32_t lookup_page = 1000;

}

void lookup_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"workflow"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/approvals-mode")
            .summary("Whether the tenant enforces approvals, and who said so")
            .description("The mode the inbox holds for the caller's tenant, with its scope and "
                         "the person who set it, when and why. The read is the service's: the "
                         "route answers the setting it holds.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<inbox_messaging::get_approvals_mode_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto reply = forwarding::forward(
                        req, session, inbox_messaging::get_approvals_mode_request{});
                    rr::require_ok(reply.result);
                    return http_response::json(
                        "{\"autoApproved\":" + json::write(reply.mode == "auto_approved") +
                        ",\"scope\":" + json::write(reply.scope) + ",\"setBy\":" +
                        json::write(reply.set_by) + ",\"setAt\":" + json::write(reply.set_at) +
                        ",\"reason\":" + json::write(reply.reason) + "}");
                });
            }));

    add(router->get("/api/hold-reasons")
            .summary("The codes a decider may park a request with, and which kinds allow a hold")
            .description("A hold names a code from this list rather than free text, so the "
                         "decision panel offers it. The list is a tenant-wide lookup and carries "
                         "no personal data, so the route is gated on a session and on nothing "
                         "else. The kinds' rules are a second read: a person who may hold a "
                         "request but may not read the kind catalogue still gets the reasons and "
                         "no kind that allows a hold, because that is a grant to ask for rather "
                         "than a fault to report.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<inbox_messaging::list_hold_reasons_request>([&] {
                    rr::refuse_unknown_params(req, {});

                    inbox_messaging::list_hold_reasons_request reasons_msg;
                    reasons_msg.limit = lookup_page;
                    const auto reasons_reply = forwarding::forward(req, session, reasons_msg);
                    rr::require_ok(reasons_reply.result);

                    std::vector<std::string> written;
                    written.reserve(reasons_reply.reasons.size());
                    for (const auto& reason : reasons_reply.reasons)
                        written.push_back("{\"code\":" + json::write(reason.code) +
                                          ",\"name\":" + json::write(reason.name) +
                                          ",\"description\":" + json::write(reason.description) +
                                          "}");

                    std::vector<std::string> rules;
                    try {
                        inbox_messaging::list_approval_kinds_request kinds_msg;
                        kinds_msg.limit = lookup_page;
                        const auto kinds_reply = forwarding::forward(req, session, kinds_msg);
                        rr::require_ok(kinds_reply.result);
                        rules.reserve(kinds_reply.kinds.size());
                        for (const auto& kind : kinds_reply.kinds)
                            if (!kind.code.empty())
                                rules.push_back(json::write(kind.code) + ":" +
                                                json::write(kind.allows_hold));
                    } catch (const rr::refusal& refused) {
                        if (refused.status != http_status::forbidden)
                            throw;
                        BOOST_LOG_SEV(lookup_lg(), ores::logging::warn)
                            << "The approval kinds are not readable by this account, so no kind "
                               "is offered a hold.";
                    }

                    return http_response::json("{\"holdReasons\":" + rr::array_of(written) +
                                               ",\"allowsHoldByKind\":" + rr::object_of(rules) +
                                               "}");
                });
            }));
}

}
