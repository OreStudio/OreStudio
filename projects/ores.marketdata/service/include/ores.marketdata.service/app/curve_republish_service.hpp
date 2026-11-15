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
#include "ores.database/domain/context_fwd.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.service/export.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <expected>
#include <string>
#include <vector>

namespace ores::marketdata::service::app {

/**
 * @brief Why a build did not run: one of its declared inputs is not there.
 *
 * A deferral is not a failure. The build did not start, so the curve keeps the
 * set it published before, and the next firing tries again: a projection curve
 * is deferred in the pass that builds its funding curve, and any curve is
 * deferred until its quotes arrive.
 */
struct ORES_MARKETDATA_SERVICE_EXPORT curve_deferral final {
    /// The input that was not there, named so a read of the log says which.
    std::string reason;
};

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
 * call; an approval gate, when one exists, belongs in front of republish() alone, not
 * compute(). Neither method gates on approval: Bootstrap is a compute-only preview, and Publish
 * (via republish()) always publishes.
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
     * @return One bootstrapped_point per pillar, or the deferral that says which
     * input is not there yet.
     * @throws std::invalid_argument if the config, its pillars, or the raw grid's observed rates
     * are missing or malformed.
     */
    static std::expected<std::vector<ores::analytics::quant::service::bootstrapped_point>,
                         curve_deferral>
    compute(const context& ctx,
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
     * @return Nothing on a publish, or the deferral that says which input is not there yet. A
     * deferred build writes nothing, so the curve keeps its previous set.
     * @throws std::invalid_argument if the config, its pillars, its output market_series row, or
     * the raw grid's observed rates are missing or malformed.
     */
    static std::expected<void, curve_deferral>
    republish(const context& ctx,
              const boost::uuids::uuid& bootstrap_config_id,
              std::chrono::system_clock::time_point as_of);
};

}

#endif
