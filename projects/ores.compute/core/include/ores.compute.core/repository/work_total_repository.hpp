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
#ifndef ORES_COMPUTE_REPOSITORY_WORK_TOTAL_REPOSITORY_HPP
#define ORES_COMPUTE_REPOSITORY_WORK_TOTAL_REPOSITORY_HPP

#include "ores.compute.api/domain/grid_sample.hpp"
#include "ores.compute.core/export.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <optional>
#include <string_view>

namespace ores::compute::repository {

/**
 * @brief The work of every tenant on the grid, summed at one instant.
 *
 * Counts only: the totals name no tenant, party, batch or result.
 */
struct ORES_COMPUTE_CORE_EXPORT work_totals {
    std::chrono::system_clock::time_point sampled_at;
    int tenants_counted = 0;
    int results_inactive = 0;
    int results_unsent = 0;
    int results_in_progress = 0;
    int results_done = 0;
    int total_workunits = 0;
    int total_batches = 0;
    int active_batches = 0;
    int outcomes_success = 0;
    int outcomes_client_error = 0;
    int outcomes_no_reply = 0;

    /**
     * @brief Adds the job counters of one sample row to the totals.
     */
    void add(const domain::grid_sample& sample);
};

/**
 * @brief The series of the work totals.
 *
 * The series belongs to the installation. Only a session of the system tenant
 * reads or writes it, and the policy decides that, so every method here runs
 * with the service's own context. The poller of the grid samples writes one row
 * for each poll, after it has read the counters inside every tenant, and the
 * grid reads for a system tenant session take the job counters from it.
 */
class ORES_COMPUTE_CORE_EXPORT work_total_repository {
private:
    inline static std::string_view logger_name = "ores.compute.repository.work_total_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Records the totals of one poll.
     */
    void write(context ctx, const work_totals& totals);

    /**
     * @brief The newest totals, or nothing when none was written.
     */
    std::optional<work_totals> read_newest(context ctx);
};

}

#endif
