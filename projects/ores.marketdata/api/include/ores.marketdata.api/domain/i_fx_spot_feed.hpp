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
#ifndef ORES_MARKETDATA_API_DOMAIN_I_FX_SPOT_FEED_HPP
#define ORES_MARKETDATA_API_DOMAIN_I_FX_SPOT_FEED_HPP

#include "ores.marketdata.api/domain/fx_spot_tick.hpp"
#include <functional>
#include <string>

namespace ores::marketdata::domain {

/**
 * @brief Pure interface for an FX spot synthetic data feed.
 *
 * Lives in ores.marketdata.api so it is visible to both the generation
 * service (ores.synthetic) and any future calibration service without
 * creating a circular dependency.
 *
 * The feed manager pattern (a future ores.marketdata.service concern,
 * see architecture doc open design question 8) will acquire IFxSpotFeed
 * instances via a registered factory; for the PoC, ores.synthetic.service
 * calls start/stop directly on its own internal tick loop.
 *
 * Threading model: start() blocks the calling thread until stop() is
 * called from a different thread. stop() is thread-safe and returns
 * immediately; the tick loop exits asynchronously. All other methods are
 * single-threaded. Callers must not invoke the on_tick handler after
 * stop() returns (the implementation must drain before yielding).
 */
class IFxSpotFeed {
public:
    virtual ~IFxSpotFeed() = default;

    using handler = std::function<void(const fx_spot_tick&)>;

    /**
     * @brief ORE canonical key this feed produces, e.g. "FX/RATE/EUR/USD".
     */
    virtual std::string ore_key() const = 0;

    /**
     * @brief Start the tick loop; call on_tick for each generated tick.
     *
     * Blocks until stop() is called from another thread.
     */
    virtual void start(handler on_tick) = 0;

    /**
     * @brief Signal the tick loop to stop.
     *
     * Thread-safe; may be called from any thread. Returns immediately;
     * the tick loop thread exits asynchronously.
     */
    virtual void stop() = 0;
};

}

#endif
