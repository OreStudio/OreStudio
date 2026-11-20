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
#include "ores.http/routes/marketdata/quote_routes.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/struct_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <charconv>
#include <chrono>
#include <cstdint>
#include <set>
#include <stdexcept>
#include <string>
#include <vector>

namespace ores::http::routes::marketdata {

namespace {

/// A fit's body: the pair and each source's quotes. The instant is the route's own, now.
struct fit_body final {
    std::string pair_code;
    std::vector<ores::marketdata::messaging::fx_vol_quote_source> sources;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<fit_body>) {
    return ores::utility::codec::field_list{
        ores::utility::codec::field{"pair_code", &fit_body::pair_code},
        ores::utility::codec::field{"sources", &fit_body::sources},
    };
}

namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace forwarding = ores::http::forwarding;
namespace messaging = ores::marketdata::messaging;

using ores::http::domain::http_request;
using ores::http::domain::http_response;

/// The unit's own helpers, named apart from the other units that build into one translation
/// unit with it.
namespace quotes {

std::set<quote_board::feed_key> keys_of(const std::vector<messaging::watchable_feed>& feeds) {
    std::set<quote_board::feed_key> out;
    for (const auto& f : feeds)
        out.emplace(f.provider_code, f.feed_id);
    return out;
}

std::string quote_json(const quote& q) {
    return "{\"provider\":" + json::write(q.provider) + ",\"feed\":" + json::write(q.feed) +
           ",\"source\":" + json::write(q.source) + ",\"uri\":" + json::write(q.uri) +
           ",\"value\":" + json::write(q.value) + ",\"previous\":" + json::write(q.previous) +
           ",\"at\":" + json::write(q.at) + ",\"asset_class\":" + json::write(q.asset_class) +
           ",\"instrument_type\":" + json::write(q.instrument_type) +
           ",\"series_uri\":" + json::write(q.series_uri) + ",\"series\":" + json::write(q.series) +
           ",\"point\":" + json::write(q.point) + "}";
}

/// The sequence a read states with ?after=, or zero for the whole board.
std::uint64_t after_of(const http_request& req) {
    const auto raw = req.get_query_param("after");
    if (raw.empty())
        return 0;
    std::uint64_t value = 0;
    const auto [end, error] = std::from_chars(raw.data(), raw.data() + raw.size(), value);
    if (error != std::errc{} || end != raw.data() + raw.size())
        rr::invalid("The after parameter is not a board sequence: " + raw);
    return value;
}

/// The instant @p name holds. rr::invalid never returns, so no value follows the catch.
std::chrono::system_clock::time_point instant_of(const http_request& req, const std::string& name) {
    const auto raw = req.get_query_param(name);
    try {
        return ores::platform::time::datetime::from_iso8601_utc(raw);
    } catch (const std::exception&) {
        rr::invalid("The " + name + " parameter is not a UTC instant: " + raw);
    }
}

std::string history_json(const messaging::get_series_ticks_response& reply) {
    if (reply.gaps_us.size() != reply.values.size() ||
        reply.coordinate_indexes.size() != reply.values.size())
        throw std::runtime_error("The history reply's arrays differ in length");
    std::vector<std::string> rows;
    rows.reserve(reply.values.size());
    auto at = reply.first_instant.value_or(std::chrono::system_clock::time_point{});
    for (std::size_t i = 0; i < reply.values.size(); ++i) {
        at += std::chrono::microseconds(reply.gaps_us[i]);
        rows.push_back("{\"at\":" + json::write(at) + ",\"uri\":" +
                       json::write(reply.coordinates.at(reply.coordinate_indexes[i])) +
                       ",\"value\":" + json::write(reply.values[i]) + "}");
    }
    return "{\"ticks\":" + rr::array_of(rows) + "}";
}

}

}

void quote_routes::register_routes(std::shared_ptr<ores::http::net::router> router,
                                   std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                   ores::nats::service::nats_client& session) {
    register_routes(
        std::move(router),
        std::move(registry),
        std::make_shared<quote_board>(quote_board::nats_subscriber(session)),
        [&session](const http_request& req) {
            messaging::list_watchable_feeds_request msg;
            msg.for_display = true;
            const auto reply = forwarding::forward(req, session, msg);
            rr::require_ok(reply.result);
            return reply.feeds;
        },
        [&session](const http_request& req, const messaging::get_provider_ticks_request& msg) {
            return forwarding::forward(req, session, msg);
        },
        [&session](const http_request& req, const messaging::fit_fx_vol_quotes_request& msg) {
            return forwarding::forward(req, session, msg);
        });
}

void quote_routes::register_routes(std::shared_ptr<ores::http::net::router> router,
                                   std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
                                   std::shared_ptr<quote_board> board,
                                   entitled_feeds entitled,
                                   provider_history history,
                                   quote_fit fit) {
    using namespace quotes;
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"market-data"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/marketdata/quotes/feeds")
            .summary("The feeds the caller may watch, with the source and the last tick of each")
            .description("The feeds come from the market data service's answer for the caller, "
                         "for display; the source name and the last tick come from the board.")
            .handler([board,
                      entitled](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::list_watchable_feeds_request>([&] {
                    rr::refuse_unknown_params(req, {});
                    board->start();
                    const auto heard = board->feeds();
                    std::vector<std::string> rows;
                    for (const auto& f : entitled(req)) {
                        const auto found = heard.find({f.provider_code, f.feed_id});
                        rows.push_back("{\"provider\":" + json::write(f.provider_code) +
                                       ",\"feed\":" + json::write(f.feed_id) + ",\"precedence\":" +
                                       std::to_string(f.precedence) + ",\"source\":" +
                                       json::write(found == heard.end() ? std::string{} :
                                                                          found->second.first) +
                                       ",\"last_at\":" +
                                       (found == heard.end() ? std::string("null") :
                                                               json::write(found->second.second)) +
                                       "}");
                    }
                    return http_response::json("{\"feeds\":" + rr::array_of(rows) + "}");
                });
            }));

    add(router->get("/api/marketdata/quotes")
            .summary("The latest quote of each datum the caller may watch")
            .description("Every quote of the caller's feeds that arrived after the board "
                         "sequence ?after= states, oldest first, and the sequence now. The first "
                         "read states none and is given the whole board.")
            .handler([board,
                      entitled](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::list_watchable_feeds_request>([&] {
                    rr::refuse_unknown_params(req, {"after"});
                    const auto after = after_of(req);
                    board->start();
                    // The sequence is read after the entitlement's answer, with the rows, so a
                    // quote that arrives while it is asked is sent once and not again.
                    const auto feeds = keys_of(entitled(req));
                    auto now = board->sequence();
                    auto rows = board->after(after, feeds);
                    // A read over the ceiling sends the oldest and states the sequence of the
                    // last it sent, so the next read carries on from there and loses none.
                    if (rows.size() > most_quotes) {
                        rows.resize(most_quotes);
                        now = rows.back().sequence;
                    }
                    std::vector<std::string> out;
                    out.reserve(rows.size());
                    for (const auto& q : rows)
                        out.push_back(quote_json(q));
                    return http_response::json("{\"sequence\":" + std::to_string(now) +
                                               ",\"quotes\":" + rr::array_of(out) + "}");
                });
            }));

    add(router->get("/api/marketdata/quotes/history")
            .summary("One provider's series over a span, for display")
            .description("The ticks of the series ?series= names, or of its one point ?point=, "
                         "from ?provider=, from ?from= to ?to= or now, read through the caller's "
                         "subscription.")
            .handler([history](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::get_provider_ticks_request>([&] {
                    rr::refuse_unknown_params(req, {"provider", "series", "point", "from", "to"});
                    messaging::get_provider_ticks_request msg;
                    msg.provider_code = req.get_query_param("provider");
                    msg.series_uri = req.get_query_param("series");
                    if (msg.provider_code.empty() || msg.series_uri.empty())
                        rr::invalid("A history read names the provider and the series.");
                    if (const auto point = req.get_query_param("point"); !point.empty())
                        msg.coordinates.push_back(point);
                    msg.from_instant = instant_of(req, "from");
                    if (!req.get_query_param("to").empty())
                        msg.to_instant = instant_of(req, "to");
                    msg.for_display = true;
                    const auto reply = history(req, msg);
                    rr::require_ok(reply.result);
                    return http_response::json(history_json(reply));
                });
            }));

    add(router->post("/api/marketdata/quotes/vols-by-delta")
            .summary("Sources' quotes of an FX volatility surface, read as smiles by delta")
            .description("The body names the pair and each source's quotes: its expiries, its "
                         "labels such as ATM, 25RR and 10BF, and its volatilities. The market "
                         "data service fits each source in the pair's FX option convention on the "
                         "caller's party's published forwards now, and answers the volatility at "
                         "10P, 25P, ATM, 25C and 10C of each expiry. It writes nothing.")
            .handler([fit](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<messaging::fit_fx_vol_quotes_request>([&] {
                    if (!fit)
                        rr::refuse(ores::http::domain::http_status::service_unavailable,
                                   "upstream-unavailable",
                                   "Vols by delta are not served here.");
                    messaging::fit_fx_vol_quotes_request msg;
                    try {
                        auto body = ores::utility::codec::object::read_json<fit_body>(req.body);
                        msg.pair_code = std::move(body.pair_code);
                        msg.sources = std::move(body.sources);
                    } catch (const std::exception&) {
                        rr::invalid("A fit names the pair and each source's quotes.");
                    }
                    if (msg.pair_code.empty() || msg.sources.empty())
                        rr::invalid("A fit names the pair and each source's quotes.");
                    msg.as_of = std::chrono::system_clock::now();
                    const auto reply = fit(req, msg);
                    return http_response::json(
                        ores::utility::codec::wire_traits<
                            messaging::fit_fx_vol_quotes_response>::write_json(reply));
                });
            }));
}

}
