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
#include "ores.http/routes/marketdata/market_reads_routes.hpp"
#include "ores.http.api/domain/http_request.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.marketdata.api/messaging/operations_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/text_form.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include <boost/asio/awaitable.hpp>
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <exception>
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
namespace marketdata_messaging = ores::marketdata::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;

/// The unit's own helpers and bodies, named apart from the other units that
/// build into one translation unit with it.
namespace market_reads {

/// The most kinds one read names, as the screen's own schema held it.
constexpr std::size_t most_kinds = 20;

/// The most providers one read names.
constexpr std::size_t most_providers = 50;

/// The longest search text one read states.
constexpr std::size_t most_query = 200;

/// The longest word one read states: a kind, a provider, an instrument type.
constexpr std::size_t most_word = 100;

/// The longest forward start one read states.
constexpr std::size_t most_forward_start = 20;

/// The longest page the instrument catalogue answers.
constexpr std::int64_t instruments_limit = 500;

/// The longest ladder one read asks for.
constexpr std::int64_t ladder_limit = 200;

/// What a route states when the body is not the shape its screen sends.
constexpr std::string_view malformed = "A market read states the body its screen sends.";

/// What a route states when a value is outside the range its screen sends.
constexpr std::string_view out_of_range = "A market read states the values its screen sends.";

/// What a route states when a currency is not three letters.
constexpr std::string_view bad_currency = "A currency is three letters.";

/// What a route states when an instant is not one.
constexpr std::string_view bad_instant = "An instant is written YYYY-MM-DD HH:MM:SSZ.";

struct instruments_body {
    std::string query;
    std::vector<std::string> kinds;
    std::string provider_code;
    std::int64_t offset{};
    std::int64_t limit{200};
};

struct ladder_body {
    std::string currency;
    std::string instrument_type{"discount"};
    std::string forward_start;
    std::string as_of;
    std::vector<std::string> providers;
    std::int64_t limit{100};
};

struct montage_body {
    std::string base_currency;
    std::string as_of;
    std::vector<std::string> providers;
};

struct analytics_body {
    std::string instrument_id;
    std::string as_of;
    std::optional<std::string> compare_at;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<instruments_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{
        field{"query", &instruments_body::query},
        field{"kinds", &instruments_body::kinds},
        field{"provider_code", &instruments_body::provider_code},
        field{"offset", &instruments_body::offset},
        field{"limit", &instruments_body::limit}};
}

[[nodiscard]] constexpr auto wire_fields(std::type_identity<ladder_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"currency", &ladder_body::currency},
                                            field{"instrument_type", &ladder_body::instrument_type},
                                            field{"forward_start", &ladder_body::forward_start},
                                            field{"as_of", &ladder_body::as_of},
                                            field{"providers", &ladder_body::providers},
                                            field{"limit", &ladder_body::limit}};
}

[[nodiscard]] constexpr auto wire_fields(std::type_identity<montage_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"base_currency", &montage_body::base_currency},
                                            field{"as_of", &montage_body::as_of},
                                            field{"providers", &montage_body::providers}};
}

[[nodiscard]] constexpr auto wire_fields(std::type_identity<analytics_body>) {
    using ores::utility::codec::field;
    return ores::utility::codec::field_list{field{"instrument_id", &analytics_body::instrument_id},
                                            field{"as_of", &analytics_body::as_of},
                                            field{"compare_at", &analytics_body::compare_at}};
}

/// A word the screen names: 1 to the given count of characters once trimmed.
std::string word_of(const std::string& raw, std::size_t most) {
    const auto text = rr::trimmed(raw);
    if (text.empty() || rr::characters_of(text) > most)
        rr::invalid(std::string(out_of_range));
    return text;
}

/// A text the screen sends: at most the given count of characters once trimmed,
/// and empty when it sends nothing.
std::string text_of(const std::string& raw, std::size_t most) {
    const auto text = rr::trimmed(raw);
    if (rr::characters_of(text) > most)
        rr::invalid(std::string(out_of_range));
    return text;
}

/// A list of words the screen names, each 1 to 100 characters once trimmed.
std::vector<std::string> words_of(const std::vector<std::string>& raw, std::size_t most_entries) {
    if (raw.size() > most_entries)
        rr::invalid(std::string(out_of_range));
    std::vector<std::string> out;
    out.reserve(raw.size());
    for (const auto& one : raw)
        out.push_back(word_of(one, most_word));
    return out;
}

/// A currency, which is three letters.
std::string currency_of(const std::string& raw) {
    const auto text = rr::trimmed(raw);
    if (rr::characters_of(text) != 3)
        rr::invalid(std::string(bad_currency));
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

/// A page of the given length, or the refusal its screen states.
std::int64_t limit_of(std::int64_t limit, std::int64_t most) {
    if (limit < 1 || limit > most)
        rr::invalid(std::string(out_of_range));
    return limit;
}

/// The answer an operation's reply states: its payload alone, or the service's
/// refusal at the status the BFF answered it with.
template <typename Reply>
http_response payload_of(const Reply& reply) {
    if (!reply.success)
        rr::refuse(http_status::unprocessable_content, "invalid-request", reply.message);
    return http_response::json(
        rr::object_without_members(json::write(reply), {"success", "message"}));
}

}

}

void market_reads_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using namespace market_reads;
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"market-data"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->post("/api/market/instruments").catalogue("web_post_market_instruments", "market-data", "instruments", true)
            .summary("The session party's instrument catalogue")
            .description("The catalogue of the caller's party as one page: every instrument "
                         "with its axes in draw order and the verdict on each source. The "
                         "service derives the identity, the axes and the verdicts; the gateway "
                         "reads the body its screen sends and forwards one operation.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::list_instruments_request>([&] {
                    const auto body = rr::lenient_body_of<instruments_body>(
                        req,
                        {"query", "kinds", "provider_code", "offset", "limit"},
                        std::string(malformed));
                    marketdata_messaging::list_instruments_request msg;
                    msg.party_id = "";
                    msg.query = text_of(body.query, most_query);
                    msg.kinds = words_of(body.kinds, most_kinds);
                    msg.provider_code = text_of(body.provider_code, most_word);
                    if (body.offset < 0)
                        rr::invalid(std::string(out_of_range));
                    msg.offset = body.offset;
                    msg.limit = limit_of(body.limit, instruments_limit);
                    return payload_of(forwarding::forward(req, session, msg));
                });
            }));

    add(router->post("/api/market/rate-ladder").catalogue("web_post_market_rate_ladder", "market-data", "rate_ladder", true)
            .summary("One currency's rate ladder at an instant")
            .description("Each tenor's quotes from the providers the caller names, the "
                         "consensus the service derives and the conventions it holds the "
                         "currency to. The maturity dates, the year fractions and the "
                         "consensus are the service's facts.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::get_rate_ladder_request>([&] {
                    const auto body = rr::lenient_body_of<ladder_body>(req,
                                                                       {"currency",
                                                                        "instrument_type",
                                                                        "forward_start",
                                                                        "as_of",
                                                                        "providers",
                                                                        "limit"},
                                                                       std::string(malformed));
                    marketdata_messaging::get_rate_ladder_request msg;
                    msg.party_id = "";
                    msg.currency = currency_of(body.currency);
                    msg.instrument_type = word_of(body.instrument_type, most_word);
                    msg.forward_start = text_of(body.forward_start, most_forward_start);
                    msg.as_of = instant_of(body.as_of);
                    msg.providers = words_of(body.providers, most_providers);
                    msg.limit = limit_of(body.limit, ladder_limit);
                    return payload_of(forwarding::forward(req, session, msg));
                });
            }));

    add(router->post("/api/market/fx-montage").catalogue("web_post_market_fx_montage", "market-data", "fx_montage", true)
            .summary("One base currency's crosses at an instant")
            .description("Each pair's spot from every provider the caller names, the consensus, "
                         "and the day's high, low and change measured from the instant the "
                         "service states rather than one the browser chose.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::get_fx_montage_request>([&] {
                    const auto body = rr::lenient_body_of<montage_body>(
                        req, {"base_currency", "as_of", "providers"}, std::string(malformed));
                    marketdata_messaging::get_fx_montage_request msg;
                    msg.party_id = "";
                    msg.base_currency = currency_of(body.base_currency);
                    msg.providers = words_of(body.providers, most_providers);
                    msg.as_of = instant_of(body.as_of);
                    return payload_of(forwarding::forward(req, session, msg));
                });
            }));

    add(router->post("/api/market/curve-analytics").catalogue("web_post_market_curve_analytics", "market-data", "curve_analytics", true)
            .summary("One curve's conventions, market pillars and bootstrap")
            .description("The curve the id names at an instant: its conventions, the market "
                         "pillars behind it and what the bootstrap makes of them, beside the "
                         "same points at the instant the caller compares with. A market pillar "
                         "and a bootstrapped point of one tenor state the same maturity date.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<marketdata_messaging::get_curve_analytics_request>([&] {
                    const auto body = rr::lenient_body_of<analytics_body>(
                        req, {"instrument_id", "as_of", "compare_at"}, std::string(malformed));
                    marketdata_messaging::get_curve_analytics_request msg;
                    msg.party_id = "";
                    msg.instrument_id = rr::trimmed(body.instrument_id);
                    if (!rr::is_uuid(msg.instrument_id))
                        rr::invalid(std::string(malformed));
                    msg.as_of = instant_of(body.as_of);
                    if (body.compare_at)
                        msg.compare_at = instant_of(*body.compare_at);
                    return payload_of(forwarding::forward(req, session, msg));
                });
            }));
}

}
