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
#include "ores.http/routes/marketdata/public_rate_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.marketdata.api/messaging/market_fixing_protocol.hpp"
#include "ores.marketdata.api/messaging/provider_terms_list_operations_protocol.hpp"
#include "ores.marketdata.api/messaging/provider_terms_operations_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstddef>
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
using ores::nats::service::nats_client;

/// The unit's own helpers and bodies, named apart from the other units that
/// build into one translation unit with it.
namespace rates {

/// The most series one read of fixings may name, which bounds a reply to a
/// page of a screen.
constexpr std::size_t most_series = 12;

/// The most providers one list accept may name.
constexpr std::size_t most_providers = 100;

/// What a write states when the screen's body is not the shape it reads.
constexpr std::string_view malformed_body =
    "A public rates write states the publisher and the version of it that was read.";

/// One provider's terms accepted at the version the person read.
struct terms_accept {
    int version{};
};

/// One provider in a list accept, at the version the person read.
struct terms_choice {
    std::string code;
    int version{};
};

/// The providers one list accept names.
struct terms_accept_many {
    std::vector<terms_choice> providers;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<terms_accept>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"version", &terms_accept::version}};
}

[[nodiscard]] constexpr auto wire_fields(std::type_identity<terms_choice>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"code", &terms_choice::code},
                                            field{"version", &terms_choice::version}};
}

[[nodiscard]] constexpr auto wire_fields(std::type_identity<terms_accept_many>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"providers", &terms_accept_many::providers}};
}

}

namespace rates {

/// A publisher named in the address.
std::string publisher_of(const http_request& req) {
    return rr::code(req.get_path_param("code"),
                    "The address does not name a publisher of 1 to 100 characters.");
}

/// A version the person read, which is at least one.
int read_version(int version) {
    if (version < 1)
        rr::invalid(std::string(malformed_body));
    return version;
}

/// The series a screen asks for, in the order it names them and without
/// repeats, refused when it names more than a page holds or a text that is no id.
std::vector<boost::uuids::uuid> series_ids_of(const http_request& req) {
    const auto raw = req.get_query_param("series");
    if (rr::trimmed(raw).empty())
        rr::invalid("A read of fixings names the series to read.");
    std::vector<boost::uuids::uuid> ids;
    std::size_t at = 0;
    while (at <= raw.size()) {
        const auto comma = raw.find(',', at);
        const auto part =
            raw.substr(at, comma == std::string::npos ? std::string::npos : comma - at);
        if (!part.empty()) {
            if (!rr::is_uuid(part))
                rr::invalid("A series is named by its id.");
            const auto id = boost::uuids::string_generator()(part);
            if (std::ranges::find(ids, id) == ids.end())
                ids.push_back(id);
        }
        if (comma == std::string::npos)
            break;
        at = comma + 1;
    }
    if (ids.empty())
        rr::invalid("A read of fixings names the series to read.");
    if (ids.size() > most_series)
        rr::invalid("A read names at most 12 series.");
    return ids;
}

/// One provider's outcome in a list accept, as the screen reads it.
std::string outcome_json(const marketdata_messaging::provider_terms_outcome& outcome) {
    return "{\"code\":" + json::write(outcome.provider_code) +
           ",\"success\":" + json::write(outcome.success) +
           ",\"message\":" + json::write(outcome.message) +
           ",\"accepted_by\":" + json::write(outcome.accepted_by) +
           ",\"accepted_at\":" + json::write(outcome.accepted_at) + "}";
}

}

}

std::string public_rate_routes::fixings_json(
    const std::vector<boost::uuids::uuid>& series,
    const std::vector<ores::marketdata::domain::market_fixing>& fixings) {
    std::vector<std::pair<std::string, std::string>> members;
    for (const auto& id : series) {
        std::vector<const marketdata_domain::market_fixing*> rows;
        for (const auto& fixing : fixings)
            if (fixing.series_id == id)
                rows.push_back(&fixing);
        std::ranges::sort(rows, [](const auto* left, const auto* right) {
            return left->fixing_date < right->fixing_date;
        });
        std::vector<std::string> written;
        for (const auto* row : rows)
            written.push_back("{\"date\":" + json::write(row->fixing_date) +
                              ",\"value\":" + json::write(row->value) + "}");
        members.emplace_back(rr::as_text(id), rr::array_of(written));
    }
    return "{\"fixings\":" + rr::object_with_members("{}", members) + "}";
}

void public_rate_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using namespace rates;
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"market-data"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/marketdata/public-rates/fixings")
            .summary("The fixings of up to a dozen series, oldest first")
            .description("A screen names the series it draws by their ids. Each series is "
                         "stated with its own list, empty when the store holds no fixing for "
                         "it, so a screen reads one series without testing the answer.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::list_market_fixings_request>([&] {
                    rr::refuse_unknown_params(req, {"series"});
                    const auto ids = series_ids_of(req);
                    marketdata_messaging::list_market_fixings_request msg;
                    msg.filter.emplace();
                    msg.filter->series_id_one_of = ids;
                    const auto rows = forwarding::forward_all(
                        req,
                        session,
                        msg,
                        &marketdata_messaging::list_market_fixings_response::market_fixings);
                    return http_response::json(fixings_json(ids, rows));
                });
            }));

    add(router->post("/api/marketdata/public-rates/{code}/accept-terms")
            .summary("Accept one publisher's terms at the version the person read")
            .description("The service refuses when the publisher changed in between, so a "
                         "person accepts the terms they read and no others.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::accept_provider_terms_request>([&] {
                    const auto body =
                        rr::body_of<terms_accept>(req, {"version"}, std::string(malformed_body));
                    marketdata_messaging::accept_provider_terms_request msg;
                    msg.provider_code = publisher_of(req);
                    msg.provider_version = read_version(body.version);
                    const auto reply = forwarding::forward(req, session, msg);
                    if (!reply.success)
                        rr::invalid(reply.message);
                    return http_response::json(
                        "{\"accepted_by\":" + json::write(reply.accepted_by) +
                        ",\"accepted_at\":" + json::write(reply.accepted_at) +
                        ",\"message\":" + json::write(reply.message) + "}");
                });
            }));

    add(router->post("/api/marketdata/public-rates/{code}/withdraw-terms")
            .summary("Withdraw a publisher's acceptance, so its adapter no longer fetches it")
            .description("A person who declines a publisher, or changes their mind, withdraws "
                         "the acceptance. The service clears the record of who accepted and "
                         "when.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::withdraw_provider_terms_request>([&] {
                    marketdata_messaging::withdraw_provider_terms_request msg;
                    msg.provider_code = publisher_of(req);
                    const auto reply = forwarding::forward(req, session, msg);
                    if (!reply.success)
                        rr::invalid(reply.message);
                    return http_response::json("{\"message\":" + json::write(reply.message) + "}");
                });
            }));

    add(router->post("/api/marketdata/public-rates/accept-terms")
            .summary("Accept the terms of several publishers, each at the version read")
            .description("Each publisher is accepted as the single accept would accept it, and "
                         "one refusal does not stop the rest, so the answer is every publisher's "
                         "outcome rather than an error.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::accept_many_provider_terms_request>([&] {
                    const auto body = rr::body_of<terms_accept_many>(
                        req, {"providers"}, std::string(malformed_body));
                    if (body.providers.empty() || body.providers.size() > most_providers)
                        rr::invalid(std::string(malformed_body));
                    marketdata_messaging::accept_many_provider_terms_request msg;
                    for (const auto& choice : body.providers) {
                        if (rr::trimmed(choice.code).empty() ||
                            rr::characters_of(rr::trimmed(choice.code)) > 100)
                            rr::invalid(std::string(malformed_body));
                        marketdata_messaging::provider_terms_choice named;
                        named.provider_code = rr::trimmed(choice.code);
                        named.provider_version = read_version(choice.version);
                        msg.providers.push_back(std::move(named));
                    }
                    const auto reply = forwarding::forward(req, session, msg);
                    std::vector<std::string> outcomes;
                    for (const auto& outcome : reply.outcomes)
                        outcomes.push_back(outcome_json(outcome));
                    return http_response::json("{\"success\":" + json::write(reply.success) +
                                               ",\"message\":" + json::write(reply.message) +
                                               ",\"outcomes\":" + rr::array_of(outcomes) + "}");
                });
            }));
}

}
