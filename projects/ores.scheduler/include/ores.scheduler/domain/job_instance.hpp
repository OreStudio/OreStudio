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
#include <boost/uuid/uuid.hpp>
#include "ores.scheduler/domain/job_status.hpp"

namespace ores::scheduler::domain {

/**
 * @brief A record of a single in-process job execution.
 *
 * Written to ores_scheduler_job_instances_tbl by the scheduler_loop.
 * Tenant and party isolation is enforced at the application layer.
 */
struct job_instance final {
    std::int64_t id = 0;
    std::optional<boost::uuids::uuid> tenant_id;
    std::optional<boost::uuids::uuid> party_id;
    boost::uuids::uuid job_definition_id;
    std::string action_type;
    job_status status = job_status::starting;
    std::chrono::system_clock::time_point triggered_at;
    std::chrono::system_clock::time_point started_at;
    std::optional<std::chrono::system_clock::time_point> completed_at;
    std::optional<std::int64_t> duration_ms;
    std::string error_message;

    /**
     * @brief Wall-clock duration of the execution, if it has completed.
     */
    [[nodiscard]] std::optional<std::chrono::milliseconds> duration() const noexcept {
        if (!duration_ms) return std::nullopt;
        return std::chrono::milliseconds(*duration_ms);
    }
};

} // namespace ores::scheduler::domain
