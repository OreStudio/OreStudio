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

#include "ores.compute.api/domain/grid_sample.hpp"
#include "ores.compute.api/domain/node_sample.hpp"
#include "ores.compute.core/export.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <vector>

namespace ores::compute::repository {

/**
 * @brief The telemetry reads no generated repository expresses.
 *
 * The writes and the newest grid sample come from
 * grid_sample_repository and node_sample_repository, which the entity models
 * generate. Two reads are left, and neither has a generated form:
 *
 * - The newest row per node is a DISTINCT ON (host_id). A read scoped to one
 *   host returns that node's rows, and a read of the newest returns the
 *   grid's newest, so one row per node is a third shape.
 * - The live summary is a call to ores_compute_grid_stats_fn, a SQL function
 *   the codegen has no expression for.
 */
class ORES_COMPUTE_CORE_EXPORT compute_telemetry_repository {
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
     * @brief Return the most recent sample per node for the context's tenant.
     *
     * Uses DISTINCT ON (host_id) to return exactly one row per host, the one
     * with the most recent sampled_at, in a single database query.
     */
    std::vector<domain::node_sample> latest_node_samples(context ctx);

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
