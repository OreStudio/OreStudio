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
#ifndef ORES_SCHEDULER_REPOSITORY_JOB_INSTANCE_ENTITY_HPP
#define ORES_SCHEDULER_REPOSITORY_JOB_INSTANCE_ENTITY_HPP

#include <string>
#include <optional>

namespace ores::scheduler::repository {

/**
 * @brief Raw database row for a job instance.
 *
 * All fields are strings as returned by the SQL query. The mapper converts
 * this to the domain::job_instance type with proper C++ types.
 */
struct job_instance_entity {
    std::optional<std::string> id;
    std::optional<std::string> tenant_id;
    std::optional<std::string> party_id;
    std::optional<std::string> job_definition_id;
    std::optional<std::string> action_type;
    std::optional<std::string> status;
    std::optional<std::string> triggered_at;
    std::optional<std::string> started_at;
    std::optional<std::string> completed_at;
    std::optional<std::string> duration_ms;
    std::optional<std::string> error_message;
};

}

#endif
