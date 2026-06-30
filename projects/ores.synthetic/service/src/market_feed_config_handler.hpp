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
 * required. The handler delegates the full multi-feed lifecycle to
 * feed_controller — keyed by source_name, with the market series resolved
 * per feed inside the controller (not pre-resolved at startup).
 */
class market_feed_config_handler {
public:
    market_feed_config_handler(ores::nats::service::client& nats,
                               std::shared_ptr<feed_controller> ctrl)
        : nats_(nats)
        , ctrl_(std::move(ctrl)) {}

    void start(ores::nats::message msg) {
        using namespace ores::marketdata::messaging;
        [[maybe_unused]] const auto cid = log_handler_entry(market_feed_config_handler_lg(), msg);

        // Reject a malformed body rather than silently starting a default feed.
        auto req = decode<start_market_feed_config_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_feed_config_handler_lg(), warn)
                << msg.subject << " — empty or malformed start body; rejecting";
            reply(nats_,
                  msg,
                  start_market_feed_config_response{.success = false,
                                                    .message = "Malformed start request"});
            return;
        }

        start_market_feed_config_response resp;
        const bool started = ctrl_->start(req->ore_key,
                                          req->source_name,
                                          req->gmm_means,
                                          req->gmm_stdevs,
                                          req->gmm_weights,
                                          req->gmm_initial_price,
                                          req->ticks_per_hour,
                                          req->process_type);

        const std::string id = req->source_name.empty() ? req->ore_key : req->source_name;
        if (started) {
            resp.success = true;
            resp.message = "Feed started: " + id;
            BOOST_LOG_SEV(market_feed_config_handler_lg(), info)
                << msg.subject << " — feed started: " << id << "  ticks/h=" << req->ticks_per_hour;
        } else {
            resp.success = false;
            resp.message = "Feed already running or series unresolved: " + id;
            BOOST_LOG_SEV(market_feed_config_handler_lg(), warn)
                << msg.subject << " — feed not started: " << id;
        }
        reply(nats_, msg, resp);
    }

    void stop(ores::nats::message msg) {
        using namespace ores::marketdata::messaging;
        [[maybe_unused]] const auto cid = log_handler_entry(market_feed_config_handler_lg(), msg);

        // Reject a malformed body rather than treating it as "stop all".
        auto req = decode<stop_market_feed_config_request>(msg);
        if (!req) {
            BOOST_LOG_SEV(market_feed_config_handler_lg(), warn)
                << msg.subject << " — empty or malformed stop body; rejecting";
            reply(nats_,
                  msg,
                  stop_market_feed_config_response{.success = false,
                                                   .message = "Malformed stop request"});
            return;
        }
        const std::string key = req->source_name;

        stop_market_feed_config_response resp;
        const auto stopped = ctrl_->stop(key);

        resp.success = stopped > 0;
        resp.message = std::to_string(stopped) + " feed(s) stopped";
        BOOST_LOG_SEV(market_feed_config_handler_lg(), info)
            << msg.subject << " — " << resp.message << (key.empty() ? " (all)" : " (" + key + ")");
        reply(nats_, msg, resp);
    }

private:
    ores::nats::service::client& nats_;
    std::shared_ptr<feed_controller> ctrl_;
};

}

#endif
