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
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#ifndef ORES_SYNTHETIC_SERVICE_MARKET_REGIME_HANDLER_HPP
#define ORES_SYNTHETIC_SERVICE_MARKET_REGIME_HANDLER_HPP

#include "feed_kind_registry.hpp"
#include "ores.analytics.quant/service/tick_scheduler.hpp"
#include "ores.synthetic.api/messaging/market_regime_protocol.hpp"
#include <boost/log/trivial.hpp>
#include <chrono>
#include <cmath>
#include <cstddef>
#include <optional>
#include <string>
#include <utility>

namespace ores::synthetic::service {

namespace {
inline auto& market_regime_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.synthetic.service.market_regime_handler");
    return instance;
}
} // namespace

/**
 * @brief Reads and pins the market-wide arrival factor.
 *
 * The factor belongs to the run, not to a row, so it is set here rather than in
 * a generation config: a user pins it once and every feed follows. Pinning is
 * the operational switch a demonstration or a stress run uses, which is why it
 * takes effect at once instead of waiting for the chain to turn.
 *
 * A read needs a session, so it is attributable. A pin needs what starting a
 * feed needs, because it changes what every feed publishes.
 */
class market_regime_handler {
public:
    market_regime_handler(ores::nats::service::client& nats,
                          ores::analytics::quant::service::tick_scheduler& scheduler,
                          ores::database::context ctx,
                          std::optional<ores::security::jwt::jwt_authenticator> verifier)
        : nats_(nats)
        , scheduler_(scheduler)
        , ctx_(std::move(ctx))
        , verifier_(std::move(verifier)) {}

    void get(ores::nats::message msg) {
        using ores::service::messaging::error_reply;
        using ores::service::messaging::reply;
        using namespace ores::logging;

        BOOST_LOG_SEV(market_regime_handler_lg(), info)
            << "market_regime get: a request arrived on subject '" << msg.subject << "'.";
        auto ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx) {
            BOOST_LOG_SEV(market_regime_handler_lg(), warn)
                << "Rejecting get_market_regime request: auth failed.";
            error_reply(nats_, msg, ctx.error());
            return;
        }

        ores::synthetic::messaging::get_market_regime_response response;
        response.success = true;
        fill(response);
        fill_heat(response);
        BOOST_LOG_SEV(market_regime_handler_lg(), info)
            << "market_regime get: replying to '" << msg.reply_subject << "' with state '"
            << response.state << "'.";
        const auto market = scheduler_.snapshot();
        BOOST_LOG_SEV(market_regime_handler_lg(), debug)
            << "market_regime get: pace " << market.pace << ", tau " << market.tau << ", heat "
            << market.heat << ", enrolled rate " << market.population_rate << " a second, "
            << scheduler_.size() << " series.";
        try {
            reply(nats_, msg, response);
        } catch (const std::exception& ex) {
            BOOST_LOG_SEV(market_regime_handler_lg(), error)
                << "market_regime get: the reply failed: " << ex.what();
            throw;
        }
    }

    void pin(ores::nats::message msg) {
        using ores::service::messaging::error_reply;
        using ores::service::messaging::reply;
        using namespace ores::logging;

        auto ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx) {
            BOOST_LOG_SEV(market_regime_handler_lg(), warn)
                << "Rejecting pin_market_regime request: auth failed.";
            error_reply(nats_, msg, ctx.error());
            return;
        }
        if (!feed_kind_registry::permits_run(*ctx)) {
            BOOST_LOG_SEV(market_regime_handler_lg(), warn)
                << "Rejecting pin_market_regime request: the caller may not run feeds.";
            error_reply(nats_, msg, ores::service::error_code::forbidden);
            return;
        }

        const auto decoded =
            ores::nats::default_wire_codec()
                .decode<ores::synthetic::messaging::pin_market_regime_request>(msg.data);
        if (!decoded) {
            BOOST_LOG_SEV(market_regime_handler_lg(), warn)
                << "Rejecting pin_market_regime request: the payload did not decode.";
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        const auto& mode = decoded->mode;
        using ores::analytics::quant::domain::market_state;
        if (mode == "auto") {
            scheduler_.pin(std::nullopt);
        } else if (mode == "calm") {
            scheduler_.pin(market_state::calm);
        } else if (mode == "active") {
            scheduler_.pin(market_state::active);
        } else {
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        BOOST_LOG_SEV(market_regime_handler_lg(), info)
            << "The market regime was set to '" << mode << "'.";

        ores::synthetic::messaging::pin_market_regime_response response;
        response.success = true;
        fill(response);
        reply(nats_, msg, response);
    }

    /**
     * Replies with the scheduler's recent decisions, one feed's and the market's or all of
     * them, and the loop's own numbers.
     */
    void trace(ores::nats::message msg) {
        using ores::service::messaging::error_reply;
        using ores::service::messaging::reply;
        using namespace ores::logging;
        namespace quant = ores::analytics::quant;

        auto ctx = ores::service::service::make_request_context(ctx_, msg, verifier_);
        if (!ctx) {
            BOOST_LOG_SEV(market_regime_handler_lg(), warn)
                << "Rejecting get_market_trace request: auth failed.";
            error_reply(nats_, msg, ctx.error());
            return;
        }
        const auto decoded =
            ores::nats::default_wire_codec()
                .decode<ores::synthetic::messaging::get_market_trace_request>(msg.data);
        if (!decoded || !std::isfinite(decoded->seconds) || decoded->seconds <= 0.0) {
            BOOST_LOG_SEV(market_regime_handler_lg(), warn)
                << "Rejecting get_market_trace request: the payload did not decode or names no "
                   "window.";
            error_reply(nats_, msg, ores::service::error_code::bad_request);
            return;
        }

        const auto series = decoded->source_name.empty() ?
                                std::nullopt :
                                std::optional(quant::math::series_key(decoded->source_name));
        const auto page = scheduler_.trace(series, decoded->seconds);
        const auto stats = scheduler_.stats();

        ores::synthetic::messaging::get_market_trace_response response;
        response.success = true;
        response.run_seed = scheduler_.run_seed();
        response.ticks_per_second = stats.ticks_per_second;
        response.heap_size = stats.heap_size;
        response.lateness_seconds = stats.lateness_seconds;
        response.max_lateness_seconds = stats.max_lateness_seconds;
        response.series = stats.series;
        const auto skip =
            page.events.size() > decoded->limit ? page.events.size() - decoded->limit : 0;
        response.events.reserve(page.events.size() - skip);
        for (auto it = page.events.begin() + static_cast<std::ptrdiff_t>(skip);
             it != page.events.end();
             ++it) {
            const auto name = page.names.find(it->series);
            response.events.push_back(ores::synthetic::messaging::market_trace_event{
                .kind = std::string(quant::service::to_string(it->kind)),
                .source_name = name != page.names.end() ? name->second : std::string{},
                .at = scheduler_.wall_at(it->at),
                .tau = it->tau,
                .operational_gap = it->operational_gap,
                .wall_gap = it->wall_gap,
                .heat = it->heat,
                .state = it->state == quant::domain::market_state::active ? "active" : "calm"});
        }
        reply(nats_, msg, response);
    }

private:
    template <typename Response>
    void fill(Response& response) const {
        const auto market = scheduler_.snapshot();
        response.state = market.state == ores::analytics::quant::domain::market_state::active ?
                             "active" :
                             "calm";
        response.multiplier = market.multiplier;
        response.pinned = market.pinned;
    }

    /**
     * Fills the endogenous half: the heat now, the two numbers that move it, and
     * the readings the scheduler took on its sample period.
     *
     * The window is the last two minutes, which is what a raster draws, and the
     * service's own history bounds it: a reader asks for a window and receives at
     * most what the scheduler kept.
     */
    void fill_heat(ores::synthetic::messaging::get_market_regime_response& response) const {
        const auto market = scheduler_.snapshot();
        response.heat = market.heat;
        response.jump = market.jump;
        response.decay = market.decay;
        response.pace = market.pace;
        response.tau = market.tau;
        response.population_rate = market.population_rate;
        response.loop_gain = market.loop_gain;
        response.run_seed = scheduler_.run_seed();
        constexpr auto window = std::chrono::minutes(2);
        const auto from = std::chrono::system_clock::now() - window;
        for (const auto& reading : scheduler_.readings_since(from)) {
            response.samples.push_back(ores::synthetic::messaging::market_regime_sample{
                .at = reading.at, .heat = reading.heat});
        }
    }

    ores::nats::service::client& nats_;
    ores::analytics::quant::service::tick_scheduler& scheduler_;
    ores::database::context ctx_;
    std::optional<ores::security::jwt::jwt_authenticator> verifier_;
};

}

#endif
