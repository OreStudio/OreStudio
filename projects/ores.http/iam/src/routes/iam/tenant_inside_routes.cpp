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
#include "ores.http/routes/iam/tenant_inside_routes.hpp"
#include "ores.assets.api/messaging/image_protocol.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/image_answer.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.http/routes/iam/account_screen_routes.hpp"
#include "ores.http/routes/iam/deployment_access.hpp"
#include "ores.http/routes/iam/sign_in_routes.hpp"
#include "ores.http/routes/iam/tenant_roster_routes.hpp"
#include "ores.http/routes/refdata/party_page_routes.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.iam.api/messaging/tenant_session_protocol.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <charconv>
#include <cstdint>
#include <exception>
#include <limits>
#include <optional>
#include <regex>
#include <vector>

namespace ores::http::routes::iam {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace messaging = ores::iam::messaging;
namespace assets = ores::assets::messaging;
namespace rr = ores::http::net::record_routes;
using ores::http::routes::refdata::party_page_routes;

namespace {

/// A page bound as the BFF read it: its default when the address leaves it out.
std::optional<std::uint32_t> inside_bound(const http_request& req,
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

http_response not_whole_page() {
    return rr::refused(http_status::bad_request,
                       "invalid-request",
                       "The page offset and limit must be whole numbers.");
}

bool is_image_id(const std::string& text) {
    static const std::regex form(
        "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$");
    return std::regex_match(text, form);
}

}

http_request tenant_inside_routes::as_token(const http_request& req,
                                            const std::string& token,
                                            const std::string& tenant_id) {
    auto inside = req;
    inside.headers["authorization"] = "Bearer " + token;
    if (inside.authenticated_user) {
        inside.authenticated_user->acting_from_tenant_id = inside.authenticated_user->tenant_id;
        inside.authenticated_user->tenant_id = tenant_id;
    }
    return inside;
}

http_response
tenant_inside_routes::read_inside(const http_request& req,
                                  nats_client& session,
                                  const std::function<http_response(const http_request&)>& read) {
    deployment_access::require_read(req, session, "A tenant of the deployment");
    const auto t = tenant_roster_routes::known_tenant(req, session, req.get_path_param("code"));
    const auto tenant_id = boost::uuids::to_string(t.id);
    const auto& claims = req.authenticated_user;
    // Both ids are canonical lowercase UUID text, so equal text is the same tenant.
    if (claims && claims->tenant_id && *claims->tenant_id == tenant_id)
        return read(req);

    const auto entered =
        ores::http::forwarding::forward(req, session, messaging::enter_tenant_request{tenant_id});
    if (!entered.success || entered.token.empty())
        throw rr::refusal{http_status::forbidden, "forbidden", entered.message};
    const auto inside = as_token(req, entered.token, tenant_id);
    const auto leave = [&] {
        using namespace ores::logging;
        try {
            ores::http::forwarding::forward(inside, session, messaging::leave_tenant_request{});
        } catch (const rr::refusal& r) {
            BOOST_LOG_SEV(lg(), warn) << "The exit from the tenant was not recorded: " << r.message;
        } catch (const std::exception& e) {
            BOOST_LOG_SEV(lg(), warn) << "The exit from the tenant was not recorded: " << e.what();
        }
    };
    try {
        auto answer = read(inside);
        leave();
        return answer;
    } catch (...) {
        leave();
        throw;
    }
}

void tenant_inside_routes::register_routes(
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
    using ores::http::forwarding::guarded;

    add(router->get("/api/tenants/{code}/parties"),
        "Read one page of a tenant's parties, inside the tenant",
        [&session](const http_request& req) {
            const auto query = party_page_routes::read_page_query(req);
            if (!query)
                return not_whole_page();
            return guarded<messaging::enter_tenant_request>([&] {
                return read_inside(req, session, [&](const http_request& inside) {
                    return http_response::json(
                        party_page_routes::read_page(inside, session, *query));
                });
            });
        });

    add(router->get("/api/tenants/{code}/people"),
        "Read one page of a tenant's people, inside the tenant",
        [&session](const http_request& req) {
            const auto offset =
                inside_bound(req, "offset", 0, 0, std::numeric_limits<std::uint32_t>::max());
            const auto limit = inside_bound(req, "limit", 100, 1, 1000);
            if (!offset || !limit)
                return not_whole_page();
            return guarded<messaging::enter_tenant_request>([&] {
                return read_inside(req, session, [&](const http_request& inside) {
                    messaging::list_accounts_request page;
                    page.offset = *offset;
                    page.limit = *limit;
                    return http_response::json(account_screen_routes::page_json(
                        ores::http::forwarding::forward(inside, session, page)));
                });
            });
        });

    add(router->get("/api/tenants/{code}/accounts/{username}/sign-ins"),
        "Read one account of a tenant and its sign-ins, inside the tenant",
        [&session](const http_request& req) {
            const auto page = sign_in_routes::read_sign_ins_page(req);
            if (!page)
                return not_whole_page();
            const auto username = req.get_path_param("username");
            return guarded<messaging::enter_tenant_request>([&] {
                return read_inside(req, session, [&](const http_request& inside) {
                    const auto answer =
                        sign_in_routes::sign_ins_json(inside, session, username, *page);
                    if (!answer)
                        return rr::refused(
                            http_status::not_found, "not-found", "No account has this username.");
                    return http_response::json(*answer);
                });
            });
        });

    add(router->get("/api/tenants/{code}/images/{id}"),
        "Read one picture or flag of a tenant, inside the tenant",
        [&session](const http_request& req) {
            const auto id = req.get_path_param("id");
            if (!is_image_id(id))
                return rr::refused(
                    http_status::not_found, "not-found", "No image has this identifier.");
            return guarded<messaging::enter_tenant_request>([&] {
                return read_inside(req, session, [&](const http_request& inside) {
                    assets::list_images_request list;
                    list.limit = 1;
                    list.filter = assets::images_filter{
                        std::vector<boost::uuids::uuid>{boost::uuids::string_generator()(id)},
                        std::nullopt};
                    const auto reply = ores::http::forwarding::forward(inside, session, list);
                    rr::require_done(reply.result);
                    if (reply.images.empty())
                        return rr::refused(
                            http_status::not_found, "not-found", "No image has this identifier.");
                    const auto& image = reply.images.front();
                    return ores::http::net::image_answer(
                        image.mime_type, std::string(image.data.begin(), image.data.end()));
                });
            });
        });
}

}
