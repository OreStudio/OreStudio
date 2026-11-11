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
#include "ores.http/routes/refdata/tenor_screen_routes.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.refdata.api/domain/tenor_codec.hpp"
#include "ores.refdata.api/domain/tenor_convention_codec.hpp"
#include "ores.refdata.api/domain/tenor_convention_resolution_codec.hpp"
#include "ores.refdata.api/messaging/tenor_convention_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_convention_resolution_protocol.hpp"
#include "ores.refdata.api/messaging/tenor_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>

namespace ores::http::routes::refdata {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::nats::service::nats_client;
namespace forwarding = ores::http::forwarding;
namespace json = ores::utility::codec::json;
namespace messaging = ores::refdata::messaging;
namespace rr = ores::http::net::record_routes;
namespace utility_domain = ores::utility::domain;

namespace {

/// The bodies the screen sends, beside the wire_fields each is read with.
namespace tenor_screen {

/// The removal of a membership row: why, and the version it was read at.
struct removal_body final {
    rr::stated_intent intent;
    std::uint32_t version{};
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<removal_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"intent", &removal_body::intent},
                                              field{"version", &removal_body::version}};
}

}

constexpr std::string_view question_sentence =
    "A resolution names its codes in 1 to 100 characters, and its horizon and spot as dates.";

/// A question read as its own request, or a refusal when it is malformed.
template <typename Request>
Request question_of(const http_request& req) {
    auto question = rr::read_body<Request>(req.body);
    if (!question)
        rr::invalid(std::string(question_sentence));
    question->calendar_code = rr::code(question->calendar_code, std::string(question_sentence));
    return std::move(*question);
}

/// The convention named in the address.
std::string convention_code_of(const http_request& req) {
    return rr::code(req.get_path_param("code"),
                    "A tenor convention is named by its code, 1 to 100 characters.");
}

/// The tenor named in the address.
std::string tenor_code_of(const http_request& req) {
    return rr::code(req.get_path_param("tenor"),
                    "A tenor is named by its code, 1 to 100 characters.");
}

/// A one-row write as the screen states it, or a refusal when it is malformed.
template <typename Write>
rr::versioned_write<Write> write_of(std::string_view text) {
    auto body = rr::read_body<rr::versioned_write<Write>>(text);
    if (!body || !rr::intent_of(body->intent))
        rr::invalid("A write states the row, the version it was read at or null for a new row, "
                    "and a reason.");
    return std::move(*body);
}

}

void tenor_screen_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"tenors"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->post("/api/tenors/preview-resolution")
            .summary("Resolve a drafted tenor rule to a date before it is written")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::preview_tenor_resolution_request>([&] {
                    const auto question =
                        question_of<messaging::preview_tenor_resolution_request>(req);
                    return http_response::json(
                        json::write(forwarding::forward(req, session, question)));
                });
            }));

    add(router->post("/api/tenors/resolve")
            .summary("Resolve a stored tenor under a stored convention to a date")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::resolve_tenor_request>([&] {
                    auto question = question_of<messaging::resolve_tenor_request>(req);
                    question.tenor_code =
                        rr::code(question.tenor_code, std::string(question_sentence));
                    question.convention_code =
                        rr::code(question.convention_code, std::string(question_sentence));
                    return http_response::json(
                        json::write(forwarding::forward(req, session, question)));
                });
            }));

    add(router->post("/api/tenors/resolve-convention")
            .summary("Resolve every tenor of a stored convention to a date")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::resolve_convention_request>([&] {
                    auto question = question_of<messaging::resolve_convention_request>(req);
                    question.convention_code =
                        rr::code(question.convention_code, std::string(question_sentence));
                    return http_response::json(
                        json::write(forwarding::forward(req, session, question)));
                });
            }));

    add(router->get("/api/tenors/conventions/{code}/resolutions")
            .summary("Read the tenors one convention resolves, with each tenor's rule")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<
                    messaging::list_by_convention_code_tenor_convention_resolutions_request>([&] {
                    messaging::list_by_convention_code_tenor_convention_resolutions_request msg;
                    msg.convention_code = convention_code_of(req);
                    msg.scope = utility_domain::scope::direct;
                    const auto rows = forwarding::forward_all(
                        req,
                        session,
                        msg,
                        &messaging::list_by_convention_code_tenor_convention_resolutions_response::
                            tenor_convention_resolutions);
                    return http_response::json("{\"rows\":" + json::write(rows) + "}");
                });
            }));

    add(router->put("/api/tenors/tenors")
            .summary("Write a tenor")
            .description("Answers 200 with the service's result whatever its outcome.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::put_tenor_request>([&] {
                    const auto body = write_of<messaging::tenor_write>(req.body);
                    const auto reply = forwarding::put_at_version<messaging::put_tenor_request>(
                        req, session, body.write, body.version, *rr::intent_of(body.intent));
                    return http_response::json(rr::result_with(reply.result, "tenor", reply.tenor));
                });
            }));

    add(router->put("/api/tenors/conventions")
            .summary("Write a tenor convention")
            .description("Answers 200 with the service's result whatever its outcome.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::put_tenor_convention_request>([&] {
                    const auto body = write_of<messaging::tenor_convention_write>(req.body);
                    const auto reply =
                        forwarding::put_at_version<messaging::put_tenor_convention_request>(
                            req, session, body.write, body.version, *rr::intent_of(body.intent));
                    return http_response::json(
                        rr::result_with(reply.result, "convention", reply.tenor_convention));
                });
            }));

    add(router->put("/api/tenors/conventions/{code}/resolutions/{tenor}")
            .summary("Write the rule one convention resolves one tenor by")
            .description("Answers 200 with the service's result whatever its outcome. The "
                         "convention and tenor are the ones the address names.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::put_tenor_convention_resolution_request>([&] {
                    const auto convention_code = convention_code_of(req);
                    const auto tenor_code = tenor_code_of(req);
                    // The address names the row, so the body may leave its codes out.
                    auto body = write_of<messaging::tenor_convention_resolution_write>(
                        rr::with_write_defaults(
                            req.body, {{"convention_code", "\"\""}, {"tenor_code", "\"\""}}));
                    body.write.convention_code = convention_code;
                    body.write.tenor_code = tenor_code;
                    const auto reply = forwarding::put_at_version<
                        messaging::put_tenor_convention_resolution_request>(
                        req, session, body.write, body.version, *rr::intent_of(body.intent));
                    return http_response::json("{\"result\":" + json::write(reply.result) + "}");
                });
            }));

    add(router->delete_("/api/tenors/conventions/{code}/resolutions/{tenor}")
            .summary("Remove the rule one convention resolves one tenor by")
            .description("Answers 200 with the service's result whatever its outcome.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::delete_tenor_convention_resolution_request>([&] {
                    messaging::delete_tenor_convention_resolution_request msg;
                    msg.removal.key.convention_code = convention_code_of(req);
                    msg.removal.key.tenor_code = tenor_code_of(req);
                    const auto body = rr::read_body<tenor_screen::removal_body>(req.body);
                    const auto intent = body ? rr::intent_of(body->intent) : std::nullopt;
                    if (!body || !intent)
                        rr::invalid("A removal states the version the row was read at, and a "
                                    "reason.");
                    msg.removal.precondition.kind =
                        utility_domain::precondition_kind::must_match_version;
                    msg.removal.precondition.version = body->version;
                    msg.intent = *intent;
                    const auto reply = forwarding::forward(req, session, msg);
                    return http_response::json("{\"result\":" + json::write(reply.result) + "}");
                });
            }));
}

}
