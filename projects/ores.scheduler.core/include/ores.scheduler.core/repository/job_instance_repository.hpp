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
#pragma once

#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>
#include <boost/uuid/uuid.hpp>
#include "ores.logging/make_logger.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.scheduler.api/domain/job_instance.hpp"
#include "ores.scheduler.api/domain/job_status.hpp"

namespace ores::scheduler::repository {

/**
 * @brief Reads and writes job instance records to ores_scheduler_job_instances_tbl.
 */
class job_instance_repository final {
private:
    inline static std::string_view logger_name =
        "ores.scheduler.repository.job_instance_repository";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Inserts a new job instance row with status='started' and returns the new id.
     */
    std::int64_t write_started(context ctx, const domain::job_instance& inst);

    /**
     * @brief Updates completed_at, duration_ms, status and error_message for the given id.
     */
    void write_completed(context ctx, std::int64_t id,
        const std::chrono::system_clock::time_point& triggered_at,
        domain::job_status status, const std::string& error = "");

    /**
     * @brief Returns the most recent job instances for a given job_definition_id.
     */
    std::vector<domain::job_instance> read_latest(context ctx,
        const boost::uuids::uuid& job_definition_id, std::size_t limit = 100);

    /**
     * @brief Returns the triggered_at of the most recent run for a job definition, if any.
     */
    std::optional<std::chrono::system_clock::time_point> last_run_at(
        context ctx, const boost::uuids::uuid& job_definition_id);
};

} // namespace ores::scheduler::repository
