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
#include "ores.http/routes/refdata/currency_ore_document_routes.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.ore.api/messaging/currency_config_protocol.hpp"
#include <boost/asio/awaitable.hpp>
#include <set>
#include <string>
#include <type_traits>
#include <vector>

namespace ores::http::routes::refdata {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;
namespace forwarding = ores::http::forwarding;
namespace ore_messaging = ores::ore::messaging;
namespace rr = ores::http::net::record_routes;

namespace {

/// The most choices one import may state.
constexpr std::size_t currency_import_most_choices = 10000;

/// The bodies the screen sends, beside the wire_fields each is read with.
namespace currency_ore_document {

/// What a preview reads: the text of the file.
struct preview_body final {
    std::string content;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<preview_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"content", &preview_body::content}};
}

/// What an import writes: the file, the choice made for each currency, and why.
struct import_body final {
    std::string content;
    std::vector<ore_messaging::currency_import_choice> choices;
    std::string reasonCode;
    std::string commentary;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<import_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"content", &import_body::content},
                                              field{"choices", &import_body::choices},
                                              field{"reasonCode", &import_body::reasonCode},
                                              field{"commentary", &import_body::commentary}};
}

}

/// Whether a choice names a currency and one of the four things an import may do with it.
bool is_choice(ore_messaging::currency_import_choice& choice) {
    static const std::set<std::string, std::less<>> actions{"take", "keep", "delete", "skip"};
    choice.iso_code = rr::trimmed(choice.iso_code);
    return !choice.iso_code.empty() && rr::characters_of(choice.iso_code) <= 10 &&
           actions.contains(choice.action);
}

}

void currency_ore_document_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"currencies"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/refdata/currencies/ore-document")
            .summary("Export every currency as the ORE currency configuration document")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return forwarding::respond(
                    req, session, ore_messaging::export_currency_config_request{});
            }));

    add(router->post("/api/refdata/currencies/ore-import/preview")
            .summary("Read what an ORE currency configuration file would change")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                const auto body = rr::read_body<currency_ore_document::preview_body>(req.body);
                if (!body || body->content.empty())
                    co_return rr::refused(ores::http::domain::http_status::bad_request,
                                          "invalid-request",
                                          "A preview needs the text of the file.");
                ore_messaging::preview_currency_config_import_request msg;
                msg.content = body->content;
                co_return forwarding::respond(req, session, msg);
            }));

    add(router->post("/api/refdata/currencies/ore-import")
            .summary("Import an ORE currency configuration file with a choice per currency")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                auto body = rr::read_body<currency_ore_document::import_body>(req.body);
                bool valid = body && !body->content.empty() &&
                             body->choices.size() <= currency_import_most_choices &&
                             rr::has_reason(body->reasonCode) &&
                             rr::characters_of(body->commentary) <= 2000;
                // The service takes the last choice for a currency, so two choices
                // for one currency would decide by their order; they are refused.
                std::set<std::string, std::less<>> chosen;
                for (std::size_t i = 0; valid && i < body->choices.size(); ++i)
                    valid = is_choice(body->choices[i]) &&
                            chosen.insert(body->choices[i].iso_code).second;
                if (!valid)
                    co_return rr::refused(ores::http::domain::http_status::bad_request,
                                          "invalid-request",
                                          "An import needs the file, one choice for each currency "
                                          "at most, and a reason.");
                ore_messaging::import_currency_config_request msg;
                msg.content = std::move(body->content);
                msg.choices = std::move(body->choices);
                msg.reason_code = rr::trimmed(body->reasonCode);
                msg.commentary = std::move(body->commentary);
                co_return forwarding::respond(req, session, msg);
            }));
}

}
