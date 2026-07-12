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

#include "ores.analytics.quant/domain/i_stochastic_process.hpp"
#include "ores.marketdata.api/domain/i_fx_spot_feed.hpp"
#include "ores.nats/service/client.hpp"
#include <atomic>
#include <memory>
#include <string>

namespace ores::synthetic::service {

/**
 * @brief Concrete FX spot feed: fixed-mode tick clock + stochastic process.
 *
 * Implements IFxSpotFeed. On start(), runs a tick loop on the calling thread
 * (caller must run it on a dedicated std::thread). Each tick:
 *   1. Advances the stochastic process to get a new price.
 *   2. Builds an fx_spot_tick (ore_key, utc now, price).
 *   3. Calls the on_tick handler.
 *   4. Publishes the tick JSON to the NATS JetStream subject.
 *
 * Persistence is handled by the marketdata ingest loop, which subscribes to
 * the JetStream subject, lazily creates the market series on first tick, and
 * writes market observations. The synthetic service has no marketdata writes.
 *
 * The NATS publish subject is supplied by the caller (derived per-producer
 * from its source name) so that multiple producers for the same ORE key
 * publish on distinct subjects.
 */
class fx_spot_feed final : public ores::marketdata::domain::IFxSpotFeed {
public:
    fx_spot_feed(ores::nats::service::client& nats,
                 std::string ore_key,
                 std::string nats_subject,
                 std::unique_ptr<ores::analytics::quant::domain::IStochasticProcess> process,
                 double ticks_per_hour);

    std::string ore_key() const override;
    void start(handler on_tick) override;
    void stop() override;
    std::uint64_t publish_count() const {
        return publish_count_.load(std::memory_order_relaxed);
    }

private:
    ores::nats::service::client& nats_;
    std::string ore_key_;
    std::unique_ptr<ores::analytics::quant::domain::IStochasticProcess> process_;
    double ticks_per_hour_;
    std::string nats_subject_;
    std::atomic<bool> stop_flag_{false};
    std::atomic<std::uint64_t> publish_count_{0};
};

}

#endif
