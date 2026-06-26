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

namespace ores::synthetic::service {

std::vector<ores::nats::service::subscription>
registrar::register_handlers(ores::nats::service::client& nats,
                             ores::database::context /*ctx*/,
                             std::shared_ptr<feed_controller> ctrl,
                             std::optional<ores::security::jwt::jwt_authenticator> verifier) {
    std::vector<ores::nats::service::subscription> subs;
    constexpr auto queue = "ores.synthetic.service";

    using namespace ores::marketdata::messaging;

    subs.push_back(nats.queue_subscribe(
        std::string(start_market_feed_config_request::nats_subject),
        queue,
        [&nats, ctrl, verifier](ores::nats::message msg) mutable {
            market_feed_config_handler h(nats, ctrl, verifier);
            h.start(std::move(msg));
        }));

    subs.push_back(nats.queue_subscribe(
        std::string(stop_market_feed_config_request::nats_subject),
        queue,
        [&nats, ctrl, verifier](ores::nats::message msg) mutable {
            market_feed_config_handler h(nats, ctrl, verifier);
            h.stop(std::move(msg));
        }));

    return subs;
}

}
