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
#ifndef ORES_SYNTHETIC_SERVICE_MARKET_FEED_CONFIG_HANDLER_HPP
#define ORES_SYNTHETIC_SERVICE_MARKET_FEED_CONFIG_HANDLER_HPP

#include "feed_controller.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/messaging/market_feed_config_protocol.hpp"
#include "ores.nats/domain/message.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include <memory>

namespace ores::synthetic::service {

namespace {
inline auto& market_feed_config_handler_lg() {
    static auto instance =
        ores::logging::make_logger("ores.synthetic.service.market_feed_config_handler");
    return instance;
}
} // namespace

using ores::service::messaging::decode;
using ores::service::messaging::log_handler_entry;
using ores::service::messaging::reply;
using namespace ores::logging;

/**
 * @brief NATS handler for market feed config start/stop control messages.
 *
 * These are internal control-plane messages: no JWT auth or DB context is
 * required. The handler delegates to feed_controller for the actual
 * feed lifecycle management.
 *
 * PoC scope: a single EUR/USD GMM feed. The ore_key in the request is
 * informational; the handler uses the series_id pre-resolved at startup.
 */
class market_feed_config_handler {
public:
    market_feed_config_handler(ores::nats::service::client& nats,
                                std::shared_ptr<feed_controller> ctrl)
        : nats_(nats)
        , ctrl_(std::move(ctrl)) {}

    void start(ores::nats::message msg) {
        using namespace ores::marketdata::messaging;
        [[maybe_unused]] const auto cid =
            log_handler_entry(market_feed_config_handler_lg(), msg);

        // Decode the request; fall back to defaults if body is empty or malformed.
        auto req = decode<start_market_feed_config_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_feed_config_handler_lg(), warn)
                << msg.subject << " — empty or malformed body; using defaults";
            req = start_market_feed_config_request{};
        }

        start_market_feed_config_response resp;
        const bool started = ctrl_->start(
            req->ore_key,
            req->gmm_means,
            req->gmm_stdevs,
            req->gmm_weights,
            req->gmm_initial_price,
            req->ticks_per_hour);

        if (started) {
            resp.success = true;
            resp.message = "Feed started: " + req->ore_key;
            BOOST_LOG_SEV(market_feed_config_handler_lg(), info)
                << msg.subject << " — feed started: " << req->ore_key
                << "  ticks/h=" << req->ticks_per_hour;
        } else {
            resp.success = false;
            resp.message = "Feed already running: " + req->ore_key;
            BOOST_LOG_SEV(market_feed_config_handler_lg(), warn)
                << msg.subject << " — feed already running: " << req->ore_key;
        }
        reply(nats_, msg, resp);
    }

    void stop(ores::nats::message msg) {
        using namespace ores::marketdata::messaging;
        [[maybe_unused]] const auto cid =
            log_handler_entry(market_feed_config_handler_lg(), msg);

        stop_market_feed_config_response resp;
        const bool stopped = ctrl_->stop_signal();

        if (stopped) {
            resp.success = true;
            resp.message = "Feed stop signalled";
            BOOST_LOG_SEV(market_feed_config_handler_lg(), info)
                << msg.subject << " — feed stop signalled";
        } else {
            resp.success = false;
            resp.message = "No feed is running";
            BOOST_LOG_SEV(market_feed_config_handler_lg(), warn)
                << msg.subject << " — stop requested but no feed is running";
        }
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    std::shared_ptr<feed_controller> ctrl_;
};

}

#endif
