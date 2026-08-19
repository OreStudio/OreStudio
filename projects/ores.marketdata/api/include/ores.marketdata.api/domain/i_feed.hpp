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
#ifndef ORES_MARKETDATA_API_DOMAIN_I_FEED_HPP
#define ORES_MARKETDATA_API_DOMAIN_I_FEED_HPP

#include "ores.marketdata.api/export.hpp"
#include <cstdint>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief The feed's qualifier plus role, joined with a unit separator -- the
 * identity a feed's consumers look observations up by. Two feeds conflict when
 * their conflict keys are equal (same qualifier AND same role; a discount curve
 * and a projection curve for the same qualifier coexist). A feed with no role
 * (e.g. FX spot) still produces a deterministic key.
 */
inline std::string feed_conflict_key(const std::string& qualifier, const std::string& role) {
    return qualifier + '\x1f' + role;
}

/**
 * @brief Common interface for a synthetic market-data producer feed.
 *
 * Lives in ores.marketdata.api so it is visible to both the generation
 * service (ores.synthetic) and any future calibration service without
 * creating a circular dependency.
 *
 * The feed manager pattern (a future ores.marketdata.service concern,
 * see architecture doc open design question 8) acquires IFeed instances
 * via a registered factory; ores.synthetic.api registers its producers
 * (fx_spot_feed, ir_curve_feed) under per-kind factory registrations.
 *
 * The interface exposes the identity every producer already carries:
 * source_name (unique per producer), qualifier (the published
 * market-data key consumers look up), and role (oresmd's curve_role for
 * curves; empty for FX). Lifecycle is start()/stop().
 *
 * Threading model: start() blocks the calling thread until stop() is
 * called from a different thread. stop() is thread-safe and returns
 * immediately; the tick loop exits asynchronously. All other methods are
 * single-threaded. Feeds publish their ticks to NATS themselves; no
 * handler is invoked by the loop.
 *
 * The default ctor and virtual dtor are declared here but defined
 * out-of-line in the DLL: consumers import them (ORES_MARKETDATA_API_EXPORT
 * is dllimport on their side), and an abstract class that is never
 * instantiated in the producer would otherwise never emit or export
 * them.
 */
class ORES_MARKETDATA_API_EXPORT IFeed {
public:
    IFeed();
    virtual ~IFeed();

    /**
     * @brief The producer's unique identity, e.g. "eur_usd_gbm_1".
     */
    virtual const std::string& source_name() const = 0;

    /**
     * @brief The published market-data key consumers look up, e.g. "EUR/USD"
     * for FX spot, "USD/SOFR" for an IR curve family.
     */
    virtual const std::string& qualifier() const = 0;

    /**
     * @brief The series role this feed publishes under (oresmd's curve_role);
     * empty for feeds without roles (FX spot).
     */
    virtual const std::string& role() const = 0;

    /**
     * @brief The factory kind string this producer registers under (the
     * asset-class discriminator of the factory seam, e.g. "fx_spot",
     * "ir_curve"). Control-plane code scopes running-feed listings by kind;
     * a producer's kind is fixed for its lifetime.
     */
    virtual std::string_view kind() const = 0;

    /**
     * @brief The identity two feeds must not share while both run (see
     * feed_conflict_key). Controllers reject a start whose conflict key is
     * already held by a different running feed.
     */
    virtual std::string conflict_key() const = 0;

    /**
     * @brief Start the tick loop; blocks until stop() is called from another
     * thread. Publishes each generated tick to NATS.
     */
    virtual void start() = 0;

    /**
     * @brief Signal the tick loop to stop.
     *
     * Thread-safe; may be called from any thread. Returns immediately;
     * the tick loop thread exits asynchronously.
     */
    virtual void stop() = 0;

    /**
     * @brief Number of ticks published since start.
     */
    virtual std::uint64_t publish_count() const = 0;
};

}

#endif
