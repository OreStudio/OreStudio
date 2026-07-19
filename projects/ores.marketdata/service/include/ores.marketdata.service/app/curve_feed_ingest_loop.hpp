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
#ifndef ORES_MARKETDATA_SERVICE_APP_CURVE_FEED_INGEST_LOOP_HPP
#define ORES_MARKETDATA_SERVICE_APP_CURVE_FEED_INGEST_LOOP_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.service/export.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.nats/service/subscription.hpp"
#include <optional>

namespace ores::marketdata::service::app {

/**
 * @brief Subscribes to the IR curve family wildcard and persists each tick-batch point as its
 * own market_observation.
 *
 * Unlike feed_ingest_loop (fx_spot_tick), a curve family feed has no per-source feed_binding
 * registry to reconcile against — see the tick-batch-publishing task's "Two subjects, not two
 * message shapes" analysis: the family subject is a producer/subject-routing decision, and every
 * ir_curve_tick is fully self-describing (tenant, party, series identity, point_id, subclass all
 * travel on the wire). So start() opens one persistent wildcard subscription
 * (synthetic.v1.curve_family.*) covering every current and future curve producer, with no
 * refresh()/reconciliation step needed.
 */
class ORES_MARKETDATA_SERVICE_EXPORT curve_feed_ingest_loop {
private:
    [[nodiscard]] static auto& lg() {
        static auto instance =
            ores::logging::make_logger("ores.marketdata.service.app.curve_feed_ingest_loop");
        return instance;
    }

public:
    curve_feed_ingest_loop(ores::nats::service::client& nats, ores::database::context ctx);

    void start();

private:
    ores::nats::service::client& nats_;
    ores::database::context ctx_;
    std::optional<ores::nats::service::subscription> sub_;
};

} // namespace ores::marketdata::service::app

#endif
