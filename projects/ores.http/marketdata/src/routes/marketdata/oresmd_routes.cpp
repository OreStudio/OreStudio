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
#include "ores.http/routes/marketdata/oresmd_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <cstddef>
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
namespace marketdata_messaging = ores::marketdata::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

/// The unit's own helpers and bodies, named apart from the other units that
/// build into one translation unit with it.
namespace oresmd {

/// The most characters a URI, a subject or a field's text may hold. The
/// service's own limit.
constexpr std::size_t most_text = 2048;

/// The most fields a compose request may state.
constexpr std::size_t most_fields = 64;

/// The most URIs one batch read may name.
constexpr std::size_t most_batch = 200;

/// How long a browser may keep the schema. It is the codec's own table and
/// changes with a release.
constexpr std::string_view schema_max_age = "private, max-age=300";

/// What a route states when the body is not the shape the browser sends.
constexpr std::string_view malformed = "An ORE URI route states the body this screen sends.";

/// One field of a URI, as the editor states it.
struct uri_field {
    std::string name;
    std::string text;
};

/// One URI to read.
struct parse_body {
    std::string uri;
};

/// The URIs one batch read names.
struct parse_many_body {
    std::vector<std::string> uris;
};

/// The parts of a URI to write.
struct compose_body {
    std::string kind;
    std::string instrument;
    std::optional<std::string> quote;
    std::optional<std::string> subject;
    std::optional<std::vector<uri_field>> fields;
};

inline auto wire_fields(std::type_identity<uri_field>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"name", &uri_field::name},
                                            field{"text", &uri_field::text}};
}

inline auto wire_fields(std::type_identity<parse_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"uri", &parse_body::uri}};
}

inline auto wire_fields(std::type_identity<parse_many_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"uris", &parse_many_body::uris}};
}

inline auto wire_fields(std::type_identity<compose_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"kind", &compose_body::kind},
                                            field{"instrument", &compose_body::instrument},
                                            field{"quote", &compose_body::quote},
                                            field{"subject", &compose_body::subject},
                                            field{"fields", &compose_body::fields}};
}

}

namespace oresmd {

/// A text the browser states, refused when it is longer than the codec reads.
void check_text(const std::string& text) {
    if (rr::characters_of(text) > most_text)
        rr::invalid(std::string(malformed));
}

/// The body read as its own shape, refused by the sentence when it is another.
template <typename Body>
Body body_of(const http_request& req, std::initializer_list<std::string_view> members) {
    rr::refuse_unknown_members(req.body, members, std::string(malformed));
    const auto body = rr::read_body<Body>(req.body);
    if (!body)
        rr::invalid(std::string(malformed));
    return *body;
}

/// The parts of a URI as the codec writes them.
std::vector<marketdata_messaging::oresmd_uri_field> fields_of(const std::vector<uri_field>& in) {
    if (in.size() > most_fields)
        rr::invalid(std::string(malformed));
    std::vector<marketdata_messaging::oresmd_uri_field> out;
    out.reserve(in.size());
    for (const auto& field : in) {
        check_text(field.name);
        check_text(field.text);
        marketdata_messaging::oresmd_uri_field named;
        named.name = field.name;
        named.text = field.text;
        out.push_back(std::move(named));
    }
    return out;
}

}

}

void oresmd_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using namespace oresmd;
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"market-data"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/oresmd/schema")
            .summary("What an ORE URI may hold")
            .description("Every instrument type with its fields, the quotes and the index "
                         "families the codec holds, which an editor reads once and keeps for the "
                         "minutes a release lasts.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::describe_oresmd_schema_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    const auto reply = forwarding::forward(
                        req, session, marketdata_messaging::describe_oresmd_schema_request{});
                    if (!reply.success)
                        rr::refuse(
                            http_status::unprocessable_content, "invalid-request", reply.message);
                    auto answer = http_response::json(
                        rr::object_without_members(json::write(reply), {"success", "message"}));
                    answer.headers["Cache-Control"] = std::string(schema_max_age);
                    return answer;
                });
            }));

    add(router->post("/api/oresmd/parse")
            .summary("Read one ORE URI into its parts")
            .description("The codec's reader is strict, so a URI that is not in its own "
                         "spelling is refused with the rule that failed, and the answer says so "
                         "beside the success rather than as a fault.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::parse_oresmd_uri_request>([&] {
                    const auto body = body_of<parse_body>(req, {"uri"});
                    check_text(body.uri);
                    marketdata_messaging::parse_oresmd_uri_request msg;
                    msg.uri = body.uri;
                    return http_response::json(json::write(forwarding::forward(req, session, msg)));
                });
            }));

    add(router->post("/api/oresmd/parse-many")
            .summary("Read many ORE URIs into their parts, in the order asked")
            .description("One call per URI on the service and one answer to the browser, "
                         "because a list of rows names a URI each and a page would otherwise cost "
                         "a call for every row.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::parse_oresmd_uri_request>([&] {
                    const auto body = body_of<parse_many_body>(req, {"uris"});
                    if (body.uris.size() > most_batch)
                        rr::invalid(std::string(malformed));
                    std::vector<std::string> parts;
                    for (const auto& uri : body.uris) {
                        check_text(uri);
                        marketdata_messaging::parse_oresmd_uri_request msg;
                        msg.uri = uri;
                        parts.push_back(json::write(forwarding::forward(req, session, msg)));
                    }
                    return http_response::json("{\"parts\":" + rr::array_of(parts) + "}");
                });
            }));

    add(router->post("/api/oresmd/compose")
            .summary("Write the parts of an ORE URI into a URI")
            .description("The browser states the structure and the codec decides whether the "
                         "parts make a URI, so a refusal is the codec's words and not a fault.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::compose_oresmd_uri_request>([&] {
                    const auto body = body_of<compose_body>(
                        req, {"kind", "instrument", "quote", "subject", "fields"});
                    check_text(body.kind);
                    check_text(body.instrument);
                    marketdata_messaging::compose_oresmd_uri_request msg;
                    msg.kind = body.kind;
                    msg.instrument = body.instrument;
                    msg.quote = body.quote.value_or(std::string());
                    msg.subject = body.subject.value_or(std::string());
                    check_text(msg.quote);
                    check_text(msg.subject);
                    msg.fields = fields_of(body.fields.value_or(std::vector<uri_field>{}));
                    return http_response::json(json::write(forwarding::forward(req, session, msg)));
                });
            }));
}

}
