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
#include "ores.http/routes/marketdata/curve_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.marketdata.api/messaging/fx_forward_operation_protocol.hpp"
#include "ores.marketdata.api/messaging/market_series_protocol.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.marketdata.api/messaging/published_curves_operations_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/text_form.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <type_traits>
#include <utility>
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
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

/// The unit's own helpers and bodies, named apart from the other units that
/// build into one translation unit with it.
namespace curves {

/// The most instants one evolution may state, so a request cannot ask for an
/// unbounded matrix.
constexpr std::size_t most_instants = 500;

/// The most fields an identity may state.
constexpr std::size_t most_fields = 20;

/// The most coordinates a node may state, which is the widest shape.
constexpr std::size_t most_coordinates = 8;

/// The most series one read of a party's catalogue asks for, which is the
/// server's page.
constexpr std::uint32_t series_page = 200;

/// The most pages of series one read asks for, so a server that ignored the
/// offset cannot hold the route.
constexpr std::uint32_t series_pages = 50;

/// The instant the server ignores, stated where a set of instants replaces the
/// range.
const auto no_instant = std::chrono::system_clock::time_point{};

/// What a route states when the body is not the shape the screen sends.
constexpr std::string_view malformed = "A curve route states the body this screen sends.";

/// What a route states when an instant is not one.
constexpr std::string_view bad_instant = "An instant is written YYYY-MM-DD HH:MM:SSZ.";

/// What a route states when a range ends before it starts.
constexpr std::string_view backwards = "The range ends before it starts.";

/// What an evolution states when it names no instant at all.
constexpr std::string_view no_range = "State the instants, or state both ends of the range.";

/// One field of an identity, such as a currency or a curve id.
struct identity_field {
    std::string name;
    std::string text;
};

/// The typed identity of a curve, which is what every read of one takes.
struct identity {
    std::string asset;
    std::string scope;
    std::string instrument_type;
    std::string quote_type;
    std::string party_id;
    std::string provider_code;
    std::vector<identity_field> fields;
};

struct identity_body {
    identity identity;
};

struct series_list_body {
    std::string party_id;
};

struct series_by_body {
    std::vector<std::string> ids;
};

struct key_point_body {
    identity identity;
    std::vector<std::string> coordinates;
    std::string value;
    std::string change_reason_code;
    std::string change_commentary;
};

struct clear_point_body {
    identity identity;
    std::vector<std::string> coordinates;
};

struct snapshot_body {
    identity identity;
    bool include_provenance{};
};

struct slice_body {
    identity identity;
    std::string component_field;
    std::string component_value;
    std::string from_instant;
    std::string to_instant;
};

struct evolution_body {
    identity identity;
    std::optional<std::string> from_instant;
    std::optional<std::string> to_instant;
    std::vector<std::string> instants;
};

struct compute_body {
    std::string bootstrap_config_id;
    std::string as_of;
};

struct published_list_body {
    std::string party_id;
    std::string search;
    bool include_candidates{};
    bool include_ended{};
    std::vector<std::string> request_id_one_of;
    int offset{};
    int limit{50};
};

struct published_sets_body {
    std::string series_id;
    int limit{5};
    bool include_candidates{true};
};

struct ladder_body {
    std::string pair_code;
    std::string as_of;
};

struct withdraw_body {
    std::string publish_id;
    std::string reason_code;
    std::string comment;
    std::string approval_request_id;
};

inline auto wire_fields(std::type_identity<identity_field>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"name", &identity_field::name},
                                            field{"text", &identity_field::text}};
}

inline auto wire_fields(std::type_identity<identity>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"asset", &identity::asset},
                                            field{"scope", &identity::scope},
                                            field{"instrument_type", &identity::instrument_type},
                                            field{"quote_type", &identity::quote_type},
                                            field{"party_id", &identity::party_id},
                                            field{"provider_code", &identity::provider_code},
                                            field{"fields", &identity::fields}};
}

inline auto wire_fields(std::type_identity<identity_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"identity", &identity_body::identity}};
}

inline auto wire_fields(std::type_identity<series_list_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"party_id", &series_list_body::party_id}};
}

inline auto wire_fields(std::type_identity<series_by_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"ids", &series_by_body::ids}};
}

inline auto wire_fields(std::type_identity<key_point_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{
        field{"identity", &key_point_body::identity},
        field{"coordinates", &key_point_body::coordinates},
        field{"value", &key_point_body::value},
        field{"change_reason_code", &key_point_body::change_reason_code},
        field{"change_commentary", &key_point_body::change_commentary}};
}

inline auto wire_fields(std::type_identity<clear_point_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"identity", &clear_point_body::identity},
                                            field{"coordinates", &clear_point_body::coordinates}};
}

inline auto wire_fields(std::type_identity<snapshot_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{
        field{"identity", &snapshot_body::identity},
        field{"include_provenance", &snapshot_body::include_provenance}};
}

inline auto wire_fields(std::type_identity<slice_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"identity", &slice_body::identity},
                                            field{"component_field", &slice_body::component_field},
                                            field{"component_value", &slice_body::component_value},
                                            field{"from_instant", &slice_body::from_instant},
                                            field{"to_instant", &slice_body::to_instant}};
}

inline auto wire_fields(std::type_identity<evolution_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"identity", &evolution_body::identity},
                                            field{"from_instant", &evolution_body::from_instant},
                                            field{"to_instant", &evolution_body::to_instant},
                                            field{"instants", &evolution_body::instants}};
}

inline auto wire_fields(std::type_identity<compute_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{
        field{"bootstrap_config_id", &compute_body::bootstrap_config_id},
        field{"as_of", &compute_body::as_of}};
}

inline auto wire_fields(std::type_identity<published_list_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{
        field{"party_id", &published_list_body::party_id},
        field{"search", &published_list_body::search},
        field{"include_candidates", &published_list_body::include_candidates},
        field{"include_ended", &published_list_body::include_ended},
        field{"request_id_one_of", &published_list_body::request_id_one_of},
        field{"offset", &published_list_body::offset},
        field{"limit", &published_list_body::limit}};
}

inline auto wire_fields(std::type_identity<published_sets_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{
        field{"series_id", &published_sets_body::series_id},
        field{"limit", &published_sets_body::limit},
        field{"include_candidates", &published_sets_body::include_candidates}};
}

inline auto wire_fields(std::type_identity<ladder_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"pair_code", &ladder_body::pair_code},
                                            field{"as_of", &ladder_body::as_of}};
}

inline auto wire_fields(std::type_identity<withdraw_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{
        field{"publish_id", &withdraw_body::publish_id},
        field{"reason_code", &withdraw_body::reason_code},
        field{"comment", &withdraw_body::comment},
        field{"approval_request_id", &withdraw_body::approval_request_id}};
}

}

namespace curves {

/// A text the screen names in one word: 1 to 100 characters once trimmed.
std::string word_of(const std::string& raw) {
    const auto text = rr::trimmed(raw);
    if (text.empty() || rr::characters_of(text) > 100)
        rr::invalid(std::string(malformed));
    return text;
}

/// An identifier the screen states, refused unless it is one.
std::string uuid_of(const std::string& raw) {
    const auto text = rr::trimmed(raw);
    if (!rr::is_uuid(text))
        rr::invalid(std::string(malformed));
    return text;
}

/// An identifier the screen states or leaves empty, as a party or a request is.
std::string optional_uuid_of(const std::string& raw) {
    const auto text = rr::trimmed(raw);
    if (text.empty())
        return text;
    if (!rr::is_uuid(text))
        rr::invalid(std::string(malformed));
    return text;
}

/// An instant as the wire states it.
std::chrono::system_clock::time_point instant_of(const std::string& raw) {
    try {
        return ores::utility::codec::text_form<std::chrono::system_clock::time_point>::from_text(
            rr::trimmed(raw));
    } catch (const std::exception&) {
        rr::invalid(std::string(bad_instant));
    }
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

/// The body read as its own shape, with each member the screen leaves out at
/// its default.
template <typename Body>
Body lenient_body_of(const http_request& req, std::initializer_list<std::string_view> members) {
    rr::refuse_unknown_members(req.body, members, std::string(malformed));
    const auto body = json::read_lenient<Body>(req.body);
    if (!body)
        rr::invalid(std::string(malformed));
    return *body;
}

/// The typed identity as the service reads it.
marketdata_messaging::resolve_series_identity_request identity_of(const identity& in) {
    marketdata_messaging::resolve_series_identity_request out;
    out.asset = word_of(in.asset);
    out.scope = word_of(in.scope);
    out.instrument_type = word_of(in.instrument_type);
    out.quote_type = word_of(in.quote_type);
    out.party_id = optional_uuid_of(in.party_id);
    const auto provider = rr::trimmed(in.provider_code);
    if (rr::characters_of(provider) > 100)
        rr::invalid(std::string(malformed));
    out.provider_code = provider;
    if (in.fields.size() > most_fields)
        rr::invalid(std::string(malformed));
    for (const auto& field : in.fields) {
        marketdata_messaging::series_identity_field named;
        named.name = word_of(field.name);
        named.text = word_of(field.text);
        out.fields.push_back(std::move(named));
    }
    return out;
}

/// The node the screen names: one coordinate for each declared axis.
std::vector<std::string> coordinates_of(const std::vector<std::string>& in) {
    if (in.empty() || in.size() > most_coordinates)
        rr::invalid(std::string(malformed));
    std::vector<std::string> out;
    out.reserve(in.size());
    for (const auto& coordinate : in)
        out.push_back(word_of(coordinate));
    return out;
}

/// The answer an operation's reply states: its payload alone, or the server's
/// refusal at the status the BFF answered it with.
template <typename Reply>
http_response answered(const Reply& reply) {
    if (!reply.success)
        rr::refuse(http_status::unprocessable_content, "invalid-request", reply.message);
    return http_response::json(
        rr::object_without_members(json::write(reply), {"success", "message"}));
}

/// The series a screen asks for: the ids it states, or the party's whole
/// catalogue when it states none.
std::vector<marketdata_domain::market_series>
series_of(const http_request& req, nats_client& session, const std::vector<std::string>& stated) {
    std::vector<boost::uuids::uuid> ids;
    for (const auto& id : stated)
        ids.push_back(boost::uuids::string_generator()(uuid_of(id)));
    const bool named = !ids.empty();

    std::vector<marketdata_domain::market_series> all;
    for (std::uint32_t page = 0; page < series_pages; ++page) {
        marketdata_messaging::list_market_series_request msg;
        msg.offset = page * series_page;
        msg.limit = series_page;
        if (named) {
            msg.filter.emplace();
            msg.filter->id_one_of = ids;
        }
        const auto reply = forwarding::forward(req, session, msg);
        rr::require_ok(reply.result);
        all.insert(all.end(), reply.market_series.begin(), reply.market_series.end());
        if (named || reply.market_series.size() < series_page)
            break;
    }
    return all;
}

}

}

void curve_routes::register_routes(std::shared_ptr<ores::http::net::router> router,
                                   std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                   nats_client& session) {
    using namespace curves;
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"market-data"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->post("/api/curves/series")
            .summary("The series a typed identity names")
            .description("The caller states the identity as typed fields and the service resolves "
                         "it, so no caller builds an oresmd URI and no caller matches on one.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::resolve_series_identity_request>([&] {
                    const auto body = body_of<identity_body>(req, {"identity"});
                    marketdata_messaging::resolve_series_identity_request msg =
                        identity_of(body.identity);
                    return answered(forwarding::forward(req, session, msg));
                });
            }));

    add(router->post("/api/curves/series-by-id")
            .summary("The oresmd URI of each series a set of ids names")
            .description("A blueprint holds its output series as a soft reference, so the screen "
                         "that lists blueprints names each one and reads its URI here. No id "
                         "asks for every series the caller's party holds.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::list_market_series_request>([&] {
                    const auto body = lenient_body_of<series_by_body>(req, {"ids"});
                    std::vector<std::string> written;
                    for (const auto& series : series_of(req, session, body.ids))
                        written.push_back("{\"id\":" + json::write(rr::as_text(series.id)) +
                                          ",\"uri\":" + json::write(series.oresmd_uri) + "}");
                    return http_response::json("{\"series\":" + rr::array_of(written) + "}");
                });
            }));

    add(router->post("/api/curves/compute")
            .summary("Build a recipe's curve at an instant and echo its points")
            .description("Nothing is written and nothing is published; the check tab reads the "
                         "answer, so a refusal is the recipe's or the market's.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::compute_curve_request>([&] {
                    const auto body = body_of<compute_body>(req, {"bootstrap_config_id", "as_of"});
                    marketdata_messaging::compute_curve_request msg;
                    msg.bootstrap_config_id = uuid_of(body.bootstrap_config_id);
                    msg.as_of = instant_of(body.as_of);
                    return answered(forwarding::forward(req, session, msg));
                });
            }));

    add(router->post("/api/curves/series-list")
            .summary("The series the caller's party can see, each as its identity")
            .description("The body may be empty, which asks across the parties the caller's "
                         "session sees.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::list_series_identities_request>([&] {
                    const auto body = lenient_body_of<series_list_body>(req, {"party_id"});
                    marketdata_messaging::list_series_identities_request msg;
                    msg.party_id = optional_uuid_of(body.party_id);
                    return answered(forwarding::forward(req, session, msg));
                });
            }));

    add(router->post("/api/curves/manual-point")
            .summary("Key a manual point on one node")
            .description("The point holds the node until it is cleared, and the feed value shows "
                         "again only then.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::key_manual_point_request>([&] {
                    const auto body = body_of<key_point_body>(req,
                                                              {"identity",
                                                               "coordinates",
                                                               "value",
                                                               "change_reason_code",
                                                               "change_commentary"});
                    marketdata_messaging::key_manual_point_request msg;
                    msg.identity = identity_of(body.identity);
                    msg.coordinates = coordinates_of(body.coordinates);
                    const auto value = rr::trimmed(body.value);
                    if (value.empty() || rr::characters_of(value) > 40)
                        rr::invalid(std::string(malformed));
                    msg.value = value;
                    msg.change_reason_code = word_of(body.change_reason_code);
                    const auto note = rr::trimmed(body.change_commentary);
                    if (note.empty() || rr::characters_of(note) > 2000)
                        rr::invalid(std::string(malformed));
                    msg.change_commentary = note;
                    return answered(forwarding::forward(req, session, msg));
                });
            }));

    add(router->post("/api/curves/manual-point/clear")
            .summary("Clear the manual point on one node")
            .description("The feed value shows again once the manual point is cleared.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::clear_manual_point_request>([&] {
                    const auto body = body_of<clear_point_body>(req, {"identity", "coordinates"});
                    marketdata_messaging::clear_manual_point_request msg;
                    msg.identity = identity_of(body.identity);
                    msg.coordinates = coordinates_of(body.coordinates);
                    return answered(forwarding::forward(req, session, msg));
                });
            }));

    add(router->post("/api/curves/snapshot")
            .summary("The object at one instant, with the age of its oldest point")
            .description("The instant the answer is as of travels with it, so a reader measures "
                         "an age from the market and not from its own clock.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::get_curve_snapshot_request>([&] {
                    const auto body =
                        lenient_body_of<snapshot_body>(req, {"identity", "include_provenance"});
                    marketdata_messaging::get_curve_snapshot_request msg;
                    msg.identity = identity_of(body.identity);
                    msg.include_provenance = body.include_provenance;
                    return answered(forwarding::forward(req, session, msg));
                });
            }));

    add(router->post("/api/curves/slice")
            .summary("One slice of the object over a range, at one component")
            .description("The component fixes one axis and the slice varies over the other, "
                         "oldest instant first.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::get_series_slice_request>([&] {
                    const auto body = body_of<slice_body>(req,
                                                          {"identity",
                                                           "component_field",
                                                           "component_value",
                                                           "from_instant",
                                                           "to_instant"});
                    marketdata_messaging::get_series_slice_request msg;
                    msg.identity = identity_of(body.identity);
                    msg.component_field = rr::trimmed(body.component_field);
                    msg.component_value = rr::trimmed(body.component_value);
                    msg.from_instant = instant_of(body.from_instant);
                    msg.to_instant = instant_of(body.to_instant);
                    if (msg.to_instant < msg.from_instant)
                        rr::invalid(std::string(backwards));
                    return answered(forwarding::forward(req, session, msg));
                });
            }));

    add(router->post("/api/curves/evolution")
            .summary("The object's evolution over a range, or at the instants stated")
            .description("A stated set of instants replaces the range, which is why the range is "
                         "not sent with it.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::get_series_evolution_request>([&] {
                    const auto body = lenient_body_of<evolution_body>(
                        req, {"identity", "from_instant", "to_instant", "instants"});
                    if (body.instants.size() > most_instants)
                        rr::invalid(std::string(malformed));
                    marketdata_messaging::get_series_evolution_request msg;
                    msg.identity = identity_of(body.identity);
                    if (body.instants.empty()) {
                        if (!body.from_instant || !body.to_instant)
                            rr::invalid(std::string(no_range));
                        msg.from_instant = instant_of(*body.from_instant);
                        msg.to_instant = instant_of(*body.to_instant);
                        if (msg.to_instant < msg.from_instant)
                            rr::invalid(std::string(backwards));
                    } else {
                        msg.from_instant = no_instant;
                        msg.to_instant = no_instant;
                        for (const auto& instant : body.instants)
                            msg.instants.push_back(instant_of(instant));
                    }
                    return answered(forwarding::forward(req, session, msg));
                });
            }));

    add(router->post("/api/curves/published")
            .summary("The tenant's published sets, with the state each is in")
            .description("The read is the list screen's whole page: the filters and the pager are "
                         "the service's, so a screen never filters a page it was given.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::list_published_curves_request>([&] {
                    const auto body = lenient_body_of<published_list_body>(req,
                                                                           {"party_id",
                                                                            "search",
                                                                            "include_candidates",
                                                                            "include_ended",
                                                                            "request_id_one_of",
                                                                            "offset",
                                                                            "limit"});
                    marketdata_messaging::list_published_curves_request msg;
                    msg.party_id = optional_uuid_of(body.party_id);
                    const auto search = rr::trimmed(body.search);
                    if (rr::characters_of(search) > 200)
                        rr::invalid(std::string(malformed));
                    msg.search = search;
                    msg.include_candidates = body.include_candidates;
                    msg.include_ended = body.include_ended;
                    if (body.request_id_one_of.size() > 100)
                        rr::invalid(std::string(malformed));
                    for (const auto& id : body.request_id_one_of)
                        msg.request_id_one_of.push_back(uuid_of(id));
                    if (body.offset < 0 || body.limit < 1 || body.limit > 200)
                        rr::invalid(std::string(malformed));
                    msg.offset = static_cast<std::uint32_t>(body.offset);
                    msg.limit = static_cast<std::uint32_t>(body.limit);
                    return answered(forwarding::forward(req, session, msg));
                });
            }));

    add(router->post("/api/curves/published-sets")
            .summary("One curve's last published sets, with the pillars each holds")
            .description("The sets are read beside their points because the snapshot read serves "
                         "the market at the caller's clock, and a set built at an earlier as-of is "
                         "not that.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::get_published_curve_sets_request>([&] {
                    const auto body = lenient_body_of<published_sets_body>(
                        req, {"series_id", "limit", "include_candidates"});
                    marketdata_messaging::get_published_curve_sets_request msg;
                    const auto series = rr::trimmed(body.series_id);
                    if (series.empty() || rr::characters_of(series) > 100)
                        rr::invalid(std::string(malformed));
                    msg.series_id = series;
                    if (body.limit < 1 || body.limit > 20)
                        rr::invalid(std::string(malformed));
                    msg.limit = static_cast<std::uint32_t>(body.limit);
                    msg.include_candidates = body.include_candidates;
                    return answered(forwarding::forward(req, session, msg));
                });
            }));

    add(router->post("/api/curves/fx-forward-ladder")
            .summary("The caller party's published forward set of one pair, with where each "
                     "number came from")
            .description("The party is the session's, so the body names none.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::get_published_fx_forward_request>([&] {
                    const auto body = body_of<ladder_body>(req, {"pair_code", "as_of"});
                    marketdata_messaging::get_published_fx_forward_request msg;
                    msg.pair_code = word_of(body.pair_code);
                    msg.as_of = instant_of(body.as_of);
                    return answered(forwarding::forward(req, session, msg));
                });
            }));

    add(router->post("/api/curves/published/withdraw")
            .summary("Take a published set out of use")
            .description("The reason is a change reason code the service validates, and the "
                         "request is the one that authorised the act when a decision stands "
                         "behind it.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::withdraw_curve_request>([&] {
                    const auto body = lenient_body_of<withdraw_body>(
                        req, {"publish_id", "reason_code", "comment", "approval_request_id"});
                    marketdata_messaging::withdraw_curve_request msg;
                    msg.publish_id = uuid_of(body.publish_id);
                    msg.reason_code = word_of(body.reason_code);
                    const auto comment = rr::trimmed(body.comment);
                    if (rr::characters_of(comment) > 500)
                        rr::invalid(std::string(malformed));
                    msg.comment = comment;
                    msg.approval_request_id = optional_uuid_of(body.approval_request_id);
                    return answered(forwarding::forward(req, session, msg));
                });
            }));
}

}
