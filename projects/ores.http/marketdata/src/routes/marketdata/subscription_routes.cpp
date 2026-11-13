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
#include "ores.http/routes/marketdata/subscription_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.marketdata.api/messaging/route_protocol.hpp"
#include "ores.marketdata.api/messaging/subscription_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <vector>

namespace ores::http::routes::marketdata {

namespace {

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace marketdata_domain = ores::marketdata::domain;
namespace marketdata_messaging = ores::marketdata::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;

/// The unit's own helpers and bodies, named apart from the other units that
/// build into one translation unit with it.
namespace subscriptions {

/// Why every row a screen creates exists, as the BFF stated it.
constexpr std::string_view new_record = "system.new_record";

/// What a write states when the caller acts in no party.
constexpr std::string_view choose_party =
    "Choose the party you act in before changing market data.";

/// What a write states when the screen's body is not the shape it reads.
constexpr std::string_view malformed_body =
    "A market data write states the row it writes, and why, as this screen states them.";

/// What a removal states when its note is longer than a note may be.
constexpr std::string_view long_commentary = "A removal states a note of up to 2000 characters.";

/// A new subscription, as the subscriptions screen states it.
struct new_subscription {
    std::string provider_code;
    bool display{};
    bool non_display{};
    bool derived{};
};

inline auto wire_fields(std::type_identity<new_subscription>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{
        field{"provider_code", &new_subscription::provider_code},
        field{"display", &new_subscription::display},
        field{"non_display", &new_subscription::non_display},
        field{"derived", &new_subscription::derived}};
}

/// A new route, as the connectivity screen states it.
struct new_route {
    std::string publisher_code;
    std::string feed_id;
};

inline auto wire_fields(std::type_identity<new_route>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"publisher_code", &new_route::publisher_code},
                                            field{"feed_id", &new_route::feed_id}};
}

/// A removal, as the screen states it: the version the row was read at, and why
/// it goes.
struct removal {
    int version{};
    std::string reason_code;
    std::optional<std::string> commentary;
};

inline auto wire_fields(std::type_identity<removal>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"version", &removal::version},
                                            field{"reason_code", &removal::reason_code},
                                            field{"commentary", &removal::commentary}};
}

}

namespace subscriptions {

/// The party the write belongs to, as the caller's own token states it.
///
/// The service states the party of a subscription from the session and never
/// from the browser, so this is only ever the caller's own party.
std::string party_of(const http_request& req) {
    if (!req.authenticated_user)
        rr::invalid(std::string(choose_party));
    const auto& party = req.authenticated_user->party_id;
    if (!party || party->empty())
        rr::invalid(std::string(choose_party));
    return *party;
}

/// A code the screen names: 1 to 100 characters once trimmed.
std::string code_of(const std::string& raw, const std::string& sentence) {
    const auto text = rr::trimmed(raw);
    if (text.empty() || rr::characters_of(text) > 100)
        rr::invalid(sentence);
    return text;
}

/// The intent a new row carries, which is the only one that applies to a create.
ores::utility::domain::change_intent create_intent() {
    return {std::string(new_record), {}};
}

/// The intent a removal carries, refused when the screen states none.
ores::utility::domain::change_intent removal_intent(const removal& body) {
    const auto reason = rr::trimmed(body.reason_code);
    if (!rr::has_reason(reason))
        rr::invalid(std::string(malformed_body));
    const auto note = body.commentary.value_or(std::string());
    if (rr::characters_of(note) > 2000)
        rr::invalid(std::string(long_commentary));
    return {reason, note};
}

/// The version a removal names, refused when the screen states none.
std::uint32_t removal_version(const removal& body) {
    if (body.version < 0)
        rr::invalid(std::string(malformed_body));
    return static_cast<std::uint32_t>(body.version);
}

/// The body read as its own shape, refused by the sentence when it is another.
template <typename Body>
Body body_of(const http_request& req, std::initializer_list<std::string_view> members) {
    rr::refuse_unknown_members(req.body, members, std::string(malformed_body));
    const auto body = rr::read_body<Body>(req.body);
    if (!body)
        rr::invalid(std::string(malformed_body));
    return *body;
}

/// The body read as its own shape, with each member the screen leaves out at its
/// default, refused by the sentence when it is another shape.
template <typename Body>
Body lenient_body_of(const http_request& req, std::initializer_list<std::string_view> members) {
    rr::refuse_unknown_members(req.body, members, std::string(malformed_body));
    const auto body = json::read_lenient<Body>(req.body);
    if (!body)
        rr::invalid(std::string(malformed_body));
    return *body;
}

/// Every subscription the caller may read.
std::vector<marketdata_domain::subscription> all_subscriptions(const http_request& req,
                                                               nats_client& session) {
    return forwarding::forward_all(
        req,
        session,
        marketdata_messaging::list_subscriptions_request{},
        &marketdata_messaging::list_subscriptions_response::subscriptions);
}

/// Every route the caller may read.
std::vector<marketdata_domain::route> all_routes(const http_request& req, nats_client& session) {
    return forwarding::forward_all(req,
                                   session,
                                   marketdata_messaging::list_routes_request{},
                                   &marketdata_messaging::list_routes_response::routes);
}

}

}

std::string subscription_routes::subscriptions_json(
    const std::vector<ores::marketdata::domain::subscription>& subscriptions) {
    return "{\"subscriptions\":" + json::write(subscriptions) + "}";
}

std::string
subscription_routes::routes_json(const std::vector<ores::marketdata::domain::route>& routes) {
    return "{\"routes\":" + json::write(routes) + "}";
}

void subscription_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using namespace subscriptions;
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"market-data"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/marketdata/subscriptions")
            .summary("Every subscription the caller may read")
            .description("A subscription is the party's entitlement to one provider's feeds, "
                         "with the three rights the vendor licences name. The party is the "
                         "caller's own, so the answer holds what that party holds.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::list_subscriptions_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    return http_response::json(subscriptions_json(all_subscriptions(req, session)));
                });
            }));

    add(router->get("/api/marketdata/routes")
            .summary("Every route the caller may read")
            .description("A route names the one feed that serves the party for a publisher. The "
                         "feed travels by its id, and the catalogue names it.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::list_routes_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    return http_response::json(routes_json(all_routes(req, session)));
                });
            }));

    add(router->post("/api/marketdata/subscriptions")
            .summary("Add a subscription for the party the caller acts in")
            .description("A subscription holds at least one right: to display, to use without "
                         "displaying, or to compute new values from the data.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::put_subscription_request>([&] {
                    const auto body = lenient_body_of<new_subscription>(
                        req, {"provider_code", "display", "non_display", "derived"});
                    if (!body.display && !body.non_display && !body.derived)
                        rr::invalid("A subscription needs at least one right.");
                    marketdata_messaging::subscription_write write;
                    write.id = boost::uuids::random_generator()();
                    write.party_id = boost::uuids::string_generator()(party_of(req));
                    write.provider_code =
                        code_of(body.provider_code,
                                "A subscription names a provider in 1 to 100 characters.");
                    write.display = body.display;
                    write.non_display = body.non_display;
                    write.derived = body.derived;
                    const auto written =
                        forwarding::put_at_version<marketdata_messaging::put_subscription_request>(
                            req, session, write, std::nullopt, create_intent());
                    rr::require_done(written.result);
                    return http_response::json(
                        "{\"provider_code\":" + json::write(write.provider_code) + "}");
                });
            }));

    add(router->delete_("/api/marketdata/subscriptions/{provider}")
            .summary("Remove the caller party's subscription to one provider")
            .description("The removal names the version the subscription was read at, so a "
                         "subscription another person changed is not removed unseen.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::delete_subscription_request>([&] {
                    party_of(req);
                    const auto provider =
                        code_of(req.get_path_param("provider"),
                                "The address does not name a provider of 1 to 100 characters.");
                    const auto body =
                        body_of<removal>(req, {"version", "reason_code", "commentary"});
                    marketdata_messaging::delete_subscription_request msg;
                    msg.removal.key.provider_code = provider;
                    msg.removal.precondition.kind =
                        ores::utility::domain::precondition_kind::must_match_version;
                    msg.removal.precondition.version = removal_version(body);
                    msg.intent = removal_intent(body);
                    const auto written = forwarding::forward(req, session, msg);
                    rr::require_done(written.result);
                    return http_response::json("{\"provider_code\":" + json::write(provider) + "}");
                });
            }));

    add(router->post("/api/marketdata/routes")
            .summary("Add a route for the party the caller acts in")
            .description("The route names the publisher and the feed that serves it. The "
                         "service checks the feed belongs to that publisher and that the party "
                         "holds a subscription to it.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::put_route_request>([&] {
                    const auto body = body_of<new_route>(req, {"publisher_code", "feed_id"});
                    if (!rr::is_uuid(rr::trimmed(body.feed_id)))
                        rr::invalid("A route names the feed that serves it.");
                    marketdata_messaging::route_write write;
                    write.id = boost::uuids::random_generator()();
                    write.party_id = boost::uuids::string_generator()(party_of(req));
                    write.publisher_code = code_of(
                        body.publisher_code, "A route names a publisher in 1 to 100 characters.");
                    write.feed_id = boost::uuids::string_generator()(rr::trimmed(body.feed_id));
                    const auto written =
                        forwarding::put_at_version<marketdata_messaging::put_route_request>(
                            req, session, write, std::nullopt, create_intent());
                    rr::require_done(written.result);
                    return http_response::json(
                        "{\"publisher_code\":" + json::write(write.publisher_code) + "}");
                });
            }));

    add(router->delete_("/api/marketdata/routes/{publisher}")
            .summary("Remove the caller party's route for one publisher")
            .description("The removal names the version the route was read at, so a route "
                         "another person changed is not removed unseen.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::delete_route_request>([&] {
                    party_of(req);
                    const auto publisher =
                        code_of(req.get_path_param("publisher"),
                                "The address does not name a publisher of 1 to 100 characters.");
                    const auto body =
                        body_of<removal>(req, {"version", "reason_code", "commentary"});
                    marketdata_messaging::delete_route_request msg;
                    msg.removal.key.publisher_code = publisher;
                    msg.removal.precondition.kind =
                        ores::utility::domain::precondition_kind::must_match_version;
                    msg.removal.precondition.version = removal_version(body);
                    msg.intent = removal_intent(body);
                    const auto written = forwarding::forward(req, session, msg);
                    rr::require_done(written.result);
                    return http_response::json("{\"publisher_code\":" + json::write(publisher) +
                                               "}");
                });
            }));
}

}
