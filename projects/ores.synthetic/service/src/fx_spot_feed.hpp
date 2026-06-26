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
#ifndef ORES_SYNTHETIC_SERVICE_FX_SPOT_FEED_HPP
#define ORES_SYNTHETIC_SERVICE_FX_SPOT_FEED_HPP

#include "ores.marketdata.api/domain/i_fx_spot_feed.hpp"
#include "ores.marketdata.api/domain/i_stochastic_process.hpp"
#include "ores.nats/service/client.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <atomic>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <memory>
#include <string>

namespace ores::synthetic::service {

/**
 * @brief Concrete FX spot feed: fixed-mode tick clock + GmmProcess.
 *
 * Implements IFxSpotFeed. On start(), runs a tick loop on the calling
 * thread (caller must run it on a dedicated std::thread). Each tick:
 *   1. Advances the stochastic process to get a new price.
 *   2. Builds an fx_spot_tick (ore_key, utc now, price).
 *   3. Calls the on_tick handler.
 *   4. Publishes the tick JSON to the NATS fan-out subject.
 *   5. Saves a market_observation to the DB via NATS request_sync (step 3).
 *
 * The NATS subject is derived from the ORE key: lowercase, '/' → '.',
 * prefixed with "marketdata.v1.tick.", e.g. "FX/RATE/EUR/USD" →
 * "marketdata.v1.tick.fx.rate.eur.usd".
 *
 * Threading: see IFxSpotFeed class-level doc.
 *
 * PoC limitation: the clock period includes the NATS publish latency,
 * so the actual tick rate falls below the configured rate under load.
 */
class fx_spot_feed final : public ores::marketdata::domain::IFxSpotFeed {
public:
    fx_spot_feed(ores::nats::service::client& nats,
                 std::string ore_key,
                 std::unique_ptr<ores::marketdata::domain::IStochasticProcess> process,
                 double ticks_per_hour,
                 boost::uuids::uuid series_id,
                 ores::utility::uuid::tenant_id tenant_id);

    std::string ore_key() const override;
    void start(handler on_tick) override;
    void stop() override;

private:
    static std::string derive_nats_subject(const std::string& ore_key);

    ores::nats::service::client& nats_;
    std::string ore_key_;
    std::unique_ptr<ores::marketdata::domain::IStochasticProcess> process_;
    double ticks_per_hour_;
    std::string nats_subject_;
    boost::uuids::uuid series_id_;
    ores::utility::uuid::tenant_id tenant_id_;
    boost::uuids::random_generator uuid_gen_;
    std::atomic<bool> stop_flag_{false};
};

}

#endif
