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
#ifndef ORES_MARKETDATA_SERVICE_APP_CURVE_REPUBLISH_SERVICE_HPP
#define ORES_MARKETDATA_SERVICE_APP_CURVE_REPUBLISH_SERVICE_HPP

#include "ores.analytics.quant/service/curve_bootstrap_engine.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.service/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief Computes and publishes an IR curve bootstrap config's output -- the "republish/remap"
 * step, analogous to feed_ingest_loop's raw-tick remap but a transform rather than a
 * passthrough.
 *
 * On-demand, not NATS-per-tick and not scheduler-only: a bootstrap is a batch recompute over a
 * presumed-complete grid at a given as-of, unlike feed_ingest_loop's per-tick remap.
 * compute()/republish() are pure orchestration methods callable identically from a NATS
 * request/reply handler, shell/CLI, or a scheduler job.
 *
 * compute() and republish() are deliberately separate: compute() is read-only (resolves pillars,
 * reads the raw grid, runs the engine -- itself already side-effect-free) and never touches
 * market_observations/observation_lineage, letting a caller preview a bootstrap's result before
 * committing to it. republish() calls compute() internally and then does the actual write --
 * this is what a Curve Builder Workbench's separate Bootstrap (preview) and Publish actions each
 * call, and what the future Curve review/sign-off UI task's approval gate sits in front of
 * republish() alone, not compute(). Neither method gates on any such approval today -- Bootstrap
 * is a compute-only preview, and Publish (via republish()) still always auto-publishes.
 */
class ORES_MARKETDATA_SERVICE_EXPORT curve_republish_service {
private:
    inline static std::string_view logger_name = "ores.marketdata.service.curve_republish_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Resolves and bootstraps @p bootstrap_config_id's pillars as of @p as_of, without
     * writing anything -- the preview half of the compute/publish split (see class docs).
     *
     * @param ctx Database context, already tenant-scoped by the caller.
     * @param bootstrap_config_id The ir_curve_bootstrap_config to bootstrap.
     * @param as_of The raw grid's as-of snapshot to bootstrap from.
     * @return One bootstrapped_point per pillar.
     * @throws std::invalid_argument if the config, its pillars, or the raw grid's observed rates
     * are missing or malformed.
     * @throws ores::analytics::quant::service::discount_curve_required_error if @p
     * bootstrap_config_id is a PROJECTION config and its discount_curve_config_id's own output
     * does not yet cover every pillar's dates as of @p as_of -- propagated uncaught, per the
     * engine's own "fails/defers cleanly" contract; callers must not treat this as a bug.
     */
    static std::vector<ores::analytics::quant::service::bootstrapped_point>
    compute(context ctx,
            const boost::uuids::uuid& bootstrap_config_id,
            std::chrono::system_clock::time_point as_of);

    /**
     * @brief Bootstraps (via compute()) and publishes @p bootstrap_config_id's output as of
     * @p as_of -- the publish half of the compute/publish split (see class docs).
     *
     * @param ctx Database context, already tenant-scoped by the caller.
     * @param bootstrap_config_id The ir_curve_bootstrap_config to republish.
     * @param as_of The raw grid's as-of snapshot to bootstrap from; also stamped as both the
     * published observations' own observation_datetime and observation_lineage's source_as_of.
     * @throws std::invalid_argument if the config, its pillars, its output market_series row, or
     * the raw grid's observed rates are missing or malformed.
     * @throws ores::analytics::quant::service::discount_curve_required_error -- see compute().
     */
    static void republish(context ctx,
                          const boost::uuids::uuid& bootstrap_config_id,
                          std::chrono::system_clock::time_point as_of);
};

}

#endif
