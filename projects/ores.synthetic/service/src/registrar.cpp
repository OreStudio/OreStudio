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
#include "registrar.hpp"
#include "market_feed_config_handler.hpp"
#include "ores.marketdata.api/messaging/market_feed_config_protocol.hpp"
#include "ores.synthetic.api/messaging/simulate_fx_spot_paths_protocol.hpp"
#include "simulate_handler.hpp"

namespace ores::synthetic::service {

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             std::shared_ptr<feed_controller> ctrl,
                             ores::database::context ctx,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;
    constexpr auto queue = "ores.synthetic.service";

    using namespace ores::marketdata::messaging;

    subs.push_back(nats.queue_subscribe(std::string(start_market_feed_config_request::nats_subject),
                                        queue,
                                        [&nats, ctrl](ores::nats::message msg) {
                                            market_feed_config_handler h(nats, ctrl);
                                            h.start(std::move(msg));
                                        }));

    subs.push_back(nats.queue_subscribe(std::string(stop_market_feed_config_request::nats_subject),
                                        queue,
                                        [&nats, ctrl](ores::nats::message msg) {
                                            market_feed_config_handler h(nats, ctrl);
                                            h.stop(std::move(msg));
                                        }));

    subs.push_back(nats.queue_subscribe(std::string(list_market_feed_configs_request::nats_subject),
                                        queue,
                                        [&nats, ctrl](ores::nats::message msg) {
                                            market_feed_config_handler h(nats, ctrl);
                                            h.list(std::move(msg));
                                        }));

    subs.push_back(nats.queue_subscribe(
        std::string(validate_market_feed_config_request::nats_subject),
        queue,
        [&nats, ctrl](ores::nats::message msg) {
            market_feed_config_handler h(nats, ctrl);
            h.validate(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(ores::synthetic::messaging::simulate_fx_spot_paths_request::nats_subject),
        queue,
        [&nats, ctx, verifier](ores::nats::message msg) {
            simulate_handler h(nats, ctx, verifier);
            h.simulate(std::move(msg));
        }));

    return subs;
}

}
