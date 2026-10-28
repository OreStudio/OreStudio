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

#include "ores.marketdata.api/domain/feed_start.hpp"
#include "ores.marketdata.api/export.hpp"
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::domain {

/**
 * @brief Common interface for a synthetic market-data producer feed.
 *
 * Lives in ores.marketdata.api so it is visible to both the generation
 * service (ores.synthetic) and any future calibration service without
 * creating a circular dependency.
 *
 * The feed manager pattern (a future ores.marketdata.service concern,
 * see architecture doc open design question 8) acquires i_feed instances
 * via a registered factory; ores.synthetic.api registers its producers
 * (fx_spot_feed, ir_curve_feed) under per-kind factory registrations.
 *
 * The interface exposes the identity every producer already carries:
 * source_name (unique per producer), series_uris (the oresmd series the feed
 * publishes into, which consumers look up), and role (oresmd's curve_role for
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
class ORES_MARKETDATA_API_EXPORT i_feed {
public:
    i_feed();
    virtual ~i_feed();

    /**
     * @brief The producer's unique identity, e.g. "eur_usd_gbm_1".
     */
    virtual const std::string& source_name() const = 0;

    /**
     * @brief The oresmd series this feed publishes points into, as series URIs.
     *
     * A curve, a surface or a rate is a series (see the oresmd URI note), so this
     * is the identity consumers look the feed's data up by. An FX spot feed names
     * one series, e.g.
     * "oresmd://fx/EUR?type=series&instrument=fx_spot&quote=rate&ccy=USD". An IR
     * curve feed names one series for the pillars that start at spot and one for
     * each later start date. Two feeds must not run while their lists share a
     * series. A feed that names no series has an empty list and conflicts with
     * nothing.
     */
    virtual const std::vector<std::string>& series_uris() const = 0;

    /**
     * @brief The use this feed's curve is built for, such as "discount" or
     * "projection"; empty for feeds without roles (FX spot). The oresmd grammar
     * holds no role, so the role is the feed's own data and plays no part in
     * which series the feed owns.
     */
    virtual const std::string& role() const = 0;

    /**
     * @brief The NATS subject this feed publishes its ticks on, derived from
     * its source_name: "synthetic.v1.ops.tick.<source>".
     */
    virtual const std::string& nats_subject() const = 0;

    /**
     * @brief Where this run's starting value came from, or why it kept its fixed level.
     *
     * A kind that starts from a published value records it when the feed is built, so a
     * reader sees the observation the run took rather than the one a later run would
     * take. A kind that starts from no such value answers nothing, and a reader shows
     * only that the feed runs.
     */
    virtual std::optional<feed_start> start_source() const;

    /**
     * @brief The factory kind string this producer registers under (the
     * asset-class discriminator of the factory seam, e.g. "fx_spot",
     * "ir_curve"). Control-plane code scopes running-feed listings by kind;
     * a producer's kind is fixed for its lifetime.
     */
    virtual std::string_view kind() const = 0;

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
