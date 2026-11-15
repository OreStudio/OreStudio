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
#include "ores.http/routes/synthetic/market_regime_routes.hpp"
#include "ores.http.api/domain/http_response.hpp"
#include "ores.http.api/net/record_routes.hpp"
#include "ores.http.api/net/view_answer.hpp"
#include "ores.http.nats/forwarding.hpp"
#include "ores.synthetic.api/messaging/market_regime_protocol.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include <boost/asio/awaitable.hpp>
#include <algorithm>
#include <charconv>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <string>
#include <type_traits>

namespace ores::http::routes::synthetic {

using ores::http::domain::http_request;
using ores::http::domain::http_response;
using ores::http::domain::http_status;
using ores::nats::service::nats_client;
namespace forwarding = ores::http::forwarding;
namespace json = ores::utility::codec::json;
namespace rr = ores::http::net::record_routes;
namespace synthetic_messaging = ores::synthetic::messaging;

namespace {

/// The bodies the market regime screen sends, beside the wire_fields each is read with.
namespace market_regime {

/// What a switch forces: a state, or control handed back to the chain.
struct pin_body final {
    std::string mode;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<pin_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"mode", &pin_body::mode}};
}

/// How the market moves from now on.
struct settings_body final {
    double wakeRate = 0.0;
    double calmRate = 0.0;
    double jump = 0.0;
    double decay = 0.0;
    double speed = 0.0;
    double tailLoading = 0.0;
};

[[nodiscard]] constexpr auto wire_fields(std::type_identity<settings_body>) {
    using ores::utility::codec::field;
    return ::ores::utility::codec::field_list{field{"wakeRate", &settings_body::wakeRate},
                                              field{"calmRate", &settings_body::calmRate},
                                              field{"jump", &settings_body::jump},
                                              field{"decay", &settings_body::decay},
                                              field{"speed", &settings_body::speed},
                                              field{"tailLoading", &settings_body::tailLoading}};
}

}

/// The seconds each history resolution reads: an hour by the second, ten minutes by the tenth.
double history_span(const std::string& resolution) {
    return resolution == "tenth" ? 600.0 : 3600.0;
}

/// The regime the service answered, in the shape the screen reads, or a 502 with its words.
template <typename Reply>
http_response regime_answer(const Reply& reply) {
    if (!reply.success)
        rr::refuse(http_status::bad_gateway,
                   "upstream-unavailable",
                   reply.message.empty() ? "The synthetic service refused the regime read." :
                                           reply.message);
    std::string samples = "[";
    for (const auto& sample : reply.samples) {
        samples += samples.size() == 1 ? "{" : ",{";
        samples += "\"at\":" + json::write(sample.at);
        samples += ",\"heat\":" + json::write(sample.heat);
        samples += ",\"intensity\":" + json::write(sample.intensity) + "}";
    }
    return http_response::json(
        "{\"state\":" + json::write(reply.state) + ",\"multiplier\":" +
        json::write(reply.multiplier) + ",\"pinned\":" + json::write(reply.pinned) +
        ",\"heat\":" + json::write(reply.heat) + ",\"jump\":" + json::write(reply.jump) +
        ",\"decay\":" + json::write(reply.decay) + ",\"samples\":" + samples + "]" +
        ",\"pace\":" + json::write(reply.pace) + ",\"tau\":" + json::write(reply.tau) +
        ",\"populationRate\":" + json::write(reply.population_rate) + ",\"loopGain\":" +
        json::write(reply.loop_gain) + ",\"runSeed\":" + json::write(reply.run_seed) +
        ",\"speed\":" + json::write(reply.speed) + ",\"wakeRate\":" + json::write(reply.wake_rate) +
        ",\"calmRate\":" + json::write(reply.calm_rate) +
        ",\"tailLoading\":" + json::write(reply.tail_loading) +
        ",\"activeMultiplier\":" + json::write(reply.active_multiplier) + "}");
}

/// The settings a body states, checked as the BFF checked them.
synthetic_messaging::configure_market_request settings_of(const http_request& req) {
    const std::string sentence =
        "The settings state a wake rate, a calm rate, a decay and a speed above zero, a jump of "
        "zero or more, and a tail loading from 0 to 10.";
    rr::refuse_unknown_members(
        req.body, {"wakeRate", "calmRate", "jump", "decay", "speed", "tailLoading"}, sentence);
    const auto body = rr::read_body<market_regime::settings_body>(req.body);
    if (!body)
        rr::invalid(sentence);
    const auto positive = [](double v) {
        return std::isfinite(v) && v > 0.0;
    };
    if (!positive(body->wakeRate) || !positive(body->calmRate) || !positive(body->decay) ||
        !positive(body->speed) || !std::isfinite(body->jump) || body->jump < 0.0 ||
        !std::isfinite(body->tailLoading) || body->tailLoading < 0.0 || body->tailLoading > 10.0)
        rr::invalid(sentence);
    synthetic_messaging::configure_market_request msg;
    msg.wake_rate = body->wakeRate;
    msg.calm_rate = body->calmRate;
    msg.jump = body->jump;
    msg.decay = body->decay;
    msg.speed = body->speed;
    msg.tail_loading = body->tailLoading;
    return msg;
}

/// The history a query asks for: a resolution, and the seconds it reads up to the resolution's
/// span.
synthetic_messaging::get_market_history_request history_of(const http_request& req) {
    const std::string sentence = "A history reads a resolution of second or tenth, and from 1 to "
                                 "3600 seconds.";
    rr::refuse_unknown_params(req, {"resolution", "seconds"});
    synthetic_messaging::get_market_history_request msg;
    msg.resolution = req.query_params.contains("resolution") ? req.get_query_param("resolution") :
                                                               std::string("second");
    if (msg.resolution != "second" && msg.resolution != "tenth")
        rr::invalid(sentence);
    msg.seconds = history_span(msg.resolution);
    if (req.query_params.contains("seconds")) {
        const auto raw = req.get_query_param("seconds");
        double seconds = 0.0;
        const auto [end, error] = std::from_chars(raw.data(), raw.data() + raw.size(), seconds);
        if (raw.empty() || error != std::errc{} || end != raw.data() + raw.size() ||
            !std::isfinite(seconds) || seconds < 1.0 || seconds > 3600.0)
            rr::invalid(sentence);
        msg.seconds = std::min(seconds, msg.seconds);
    }
    return msg;
}

/// A number named in the query, from lowest to highest, or its default when left out.
double trace_query_number(const http_request& req,
                          const std::string& name,
                          double lowest,
                          double highest,
                          double fallback,
                          bool whole) {
    if (!req.query_params.contains(name))
        return fallback;
    const auto raw = req.get_query_param(name);
    double value = 0.0;
    const auto [end, error] = std::from_chars(raw.data(), raw.data() + raw.size(), value);
    if (raw.empty() || error != std::errc{} || end != raw.data() + raw.size() ||
        !std::isfinite(value) || value < lowest || value > highest ||
        (whole && value != std::floor(value)))
        rr::invalid("A trace reads a source of up to 256 characters, from 1 to 3600 seconds, "
                    "and from 1 to 20000 events.");
    return value;
}

/// The most events one trace read returns; the newest are kept.
constexpr double trace_most_events = 20000;

}

market_trace
market_regime_routes::trace_of(const synthetic_messaging::get_market_trace_response& reply) {
    market_trace out;
    out.runSeed = reply.run_seed;
    out.ticksPerSecond = reply.ticks_per_second;
    out.heapSize = reply.heap_size;
    out.latenessSeconds = reply.lateness_seconds;
    out.maxLatenessSeconds = reply.max_lateness_seconds;
    out.series = reply.series;
    out.events.reserve(reply.events.size());
    for (const auto& e : reply.events)
        out.events.push_back({e.kind,
                              e.source_name,
                              e.at,
                              e.tau,
                              e.operational_gap,
                              e.wall_gap,
                              e.compensated_gap,
                              e.heat,
                              e.multiplier,
                              e.pace,
                              e.state});
    out.feeds.reserve(reply.series_states.size());
    for (const auto& f : reply.series_states)
        out.feeds.push_back({f.source_name,
                             f.mode,
                             f.rate,
                             f.sensitivity,
                             f.effective_rate,
                             f.phase,
                             f.excess,
                             f.branching_ratio,
                             f.decay});
    return out;
}

void market_regime_routes::register_routes(
    std::shared_ptr<ores::http::net::router> router,
    std::shared_ptr<ores::http::openapi::endpoint_registry> registry,
    nats_client& session) {
    using forwarding::guarded;
    const auto add = [&](ores::http::net::route_builder builder) {
        const auto built = builder.tags({"synthetic"}).auth_required().build();
        router->add_route(built);
        registry->register_route(built);
    };

    add(router->get("/api/synthetic/market-regime")
            .summary("Read the market regime in force")
            .description("The state, the multiplier every feed's rate is scaled by, whether a "
                         "switch holds it, the heat and its readings, and the settings the "
                         "market moves by.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::get_market_regime_request>([&] {
                    return regime_answer(forwarding::forward(
                        req, session, synthetic_messaging::get_market_regime_request{}));
                });
            }));

    add(router->post("/api/synthetic/market-regime")
            .summary("Force the market state, or hand it back to the chain")
            .description("The mode is auto, calm or active. The answer is the regime after the "
                         "switch, so a screen redraws from what the service says.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::pin_market_regime_request>([&] {
                    const std::string sentence = "A switch states a mode of auto, calm or active.";
                    rr::refuse_unknown_members(req.body, {"mode"}, sentence);
                    const auto body = rr::read_body<market_regime::pin_body>(req.body);
                    if (!body ||
                        (body->mode != "auto" && body->mode != "calm" && body->mode != "active"))
                        rr::invalid(sentence);
                    synthetic_messaging::pin_market_regime_request msg;
                    msg.mode = body->mode;
                    return regime_answer(forwarding::forward(req, session, msg));
                });
            }));

    add(router->put("/api/synthetic/market-regime/settings")
            .summary("Set how the market moves from now on")
            .description("The chain's two rates, the heat's jump and decay, the speed and the "
                         "tail loading. Settings the service refuses answer 400 with its words, "
                         "which name the number.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::configure_market_request>([&] {
                    const auto reply = forwarding::forward(req, session, settings_of(req));
                    if (!reply.success)
                        rr::invalid(reply.message.empty() ?
                                        "The synthetic service refused the settings." :
                                        reply.message);
                    return regime_answer(reply);
                });
            }));

    add(router->get("/api/synthetic/market-trace")
            .summary("The market trace")
            .description("A composed view owned by the synthetic service: the scheduler's recent "
                         "decisions, every tick with the gap its law drew and the wall gap it "
                         "took, every regime turn, pin and change of the feeds, with every "
                         "running feed's law now. One feed's events and the market's, or all of "
                         "them. One trace call. The answer carries an ETag and stays fresh for "
                         "10 seconds.")
            .response<market_trace>()
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::get_market_trace_request>([&] {
                    rr::refuse_unknown_params(req, {"source", "seconds", "limit"});
                    synthetic_messaging::get_market_trace_request msg;
                    msg.source_name = req.get_query_param("source");
                    if (rr::characters_of(msg.source_name) > 256)
                        rr::invalid("A trace reads a source of up to 256 characters, from 1 to "
                                    "3600 seconds, and from 1 to 20000 events.");
                    msg.seconds = trace_query_number(req, "seconds", 1, 3600, 120, false);
                    msg.limit = static_cast<std::uint32_t>(
                        trace_query_number(req, "limit", 1, trace_most_events, 5000, true));
                    const auto reply = forwarding::forward(req, session, msg);
                    if (!reply.success)
                        rr::refuse(http_status::bad_gateway,
                                   "upstream-unavailable",
                                   reply.message.empty() ?
                                       "The synthetic service refused the trace read." :
                                       reply.message);
                    return ores::http::net::view_answer(
                        req,
                        json::write(trace_of(reply)),
                        ores::http::net::view_cache_policy::fresh_for(std::chrono::seconds(10)));
                });
            }));

    add(router->get("/api/synthetic/market-history")
            .summary("Read the tick counts of the last stretch of the run, by asset class")
            .description("The last hour by the second, or the last ten minutes by the tenth, "
                         "with the pace and the active share of each slot. Counts rather than "
                         "ticks, so the read costs the same at any tick rate. The seconds "
                         "asked for are capped at the resolution's span, so a read by the "
                         "tenth answers at most 600.")
            .handler([&session](const http_request& req) -> boost::asio::awaitable<http_response> {
                co_return guarded<synthetic_messaging::get_market_history_request>([&] {
                    const auto reply = forwarding::forward(req, session, history_of(req));
                    if (!reply.success)
                        rr::refuse(http_status::bad_gateway,
                                   "upstream-unavailable",
                                   reply.message.empty() ?
                                       "The synthetic service refused the history read." :
                                       reply.message);
                    std::string bins = "[";
                    for (const auto& bin : reply.bins) {
                        bins += bins.size() == 1 ? "{" : ",{";
                        bins += "\"at\":" + json::write(bin.at);
                        bins += ",\"counts\":" + json::write(bin.counts);
                        bins += ",\"pace\":" + json::write(bin.pace);
                        bins += ",\"activeShare\":" + json::write(bin.active_share) + "}";
                    }
                    return http_response::json(
                        "{\"slotSeconds\":" + json::write(reply.slot_seconds) +
                        ",\"groups\":" + json::write(reply.groups) + ",\"bins\":" + bins + "]}");
                });
            }));
}

}
