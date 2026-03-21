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
#ifndef ORES_COMPUTE_REPOSITORY_COMPUTE_TELEMETRY_REPOSITORY_HPP
#define ORES_COMPUTE_REPOSITORY_COMPUTE_TELEMETRY_REPOSITORY_HPP

#include <optional>
#include <vector>
#include <sqlgen/postgres.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.compute/domain/grid_sample.hpp"
#include "ores.compute/domain/node_sample.hpp"

namespace ores::compute::repository {

/**
 * @brief Persistence for compute grid telemetry time-series samples.
 *
 * Writes to ores_compute_grid_samples_tbl and
 * ores_compute_node_samples_tbl (TimescaleDB hypertables).
 */
class compute_telemetry_repository {
private:
    inline static std::string_view logger_name =
        "ores.compute.repository.compute_telemetry_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Insert one grid-level sample row.
     */
    void insert_grid_sample(context ctx, const domain::grid_sample& sample);

    /**
     * @brief Insert one node-level sample row.
     */
    void insert_node_sample(context ctx, const domain::node_sample& sample);

    /**
     * @brief Return the most recent grid sample for the context's tenant.
     *
     * Returns nullopt if no samples exist yet.
     */
    std::optional<domain::grid_sample>
    latest_grid_sample(context ctx);

    /**
     * @brief Return the most recent sample per node for the context's tenant.
     *
     * Uses DISTINCT ON (host_id) to return exactly one row per host,
     * the one with the most recent sampled_at, in a single database query.
     */
    std::vector<domain::node_sample>
    latest_node_samples(context ctx);

    /**
     * @brief Compute current grid statistics using SQL aggregations.
     *
     * Runs a single parameterized CTE query against the compute domain tables
     * (hosts, results, workunits, batches) to return counts and breakdowns
     * without loading full tables into memory. Sets sampled_at to now().
     */
    domain::grid_sample compute_grid_stats(context ctx);
};

}

#endif
