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

#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <boost/uuid/uuid.hpp>
#include "ores.utility/uuid/tenant_id.hpp"
#include "ores.scheduler/domain/cron_expression.hpp"
#include "ores.scheduler/domain/job_definition.hpp"

namespace ores::scheduler::builder {

/**
 * @brief Fluent builder for job_definition (Quartz.NET-style API).
 *
 * Validates each field on set and accumulates errors. Call build() at the end
 * to obtain either a valid job_definition or an error string.
 *
 * Example:
 * @code
 *   auto result = job_definition_builder{}
 *       .with_name("nightly_aggregation")
 *       .with_description("Aggregate daily stats")
 *       .with_command("SELECT ores_telemetry_aggregate_fn()")
 *       .with_cron_schedule("0 0 * * *")
 *       .with_database("ores_default")
 *       .with_tenant(tenant_id)
 *       .with_party(party_id)
 *       .with_modified_by("alice")
 *       .build();
 * @endcode
 */
class job_definition_builder final {
public:
    job_definition_builder() = default;

    /// Unique name passed to cron.schedule() as the job identifier.
    job_definition_builder& with_name(std::string_view name);

    /// Human-readable label shown in the job management UI.
    job_definition_builder& with_description(std::string_view description);

    /// The SQL command pg_cron will execute.
    job_definition_builder& with_command(std::string_view command);

    /// Cron expression string (e.g. "0 0 * * *"). Validated immediately.
    job_definition_builder& with_cron_schedule(std::string_view expr);

    /// Target PostgreSQL database name for pg_cron.
    job_definition_builder& with_database(std::string_view database_name);

    /// Tenant that owns this job.
    job_definition_builder& with_tenant(const utility::uuid::tenant_id& tenant_id);

    /// Party within the tenant that owns this job.
    job_definition_builder& with_party(const boost::uuids::uuid& party_id);

    /// Username of the user creating or modifying this job.
    job_definition_builder& with_modified_by(std::string_view modified_by);

    /**
     * @brief Validate all fields and produce a job_definition.
     *
     * Returns an error string if any required field is missing or if the cron
     * expression was rejected during with_cron_schedule().
     */
    [[nodiscard]] std::expected<domain::job_definition, std::string> build() const;

private:
    std::string name_;
    std::string description_;
    std::string command_;
    std::optional<domain::cron_expression> schedule_expression_; ///< Validated on with_cron_schedule().
    std::string error_;               ///< First validation error, if any.
    std::string database_name_;
    utility::uuid::tenant_id tenant_id_ = utility::uuid::tenant_id::system();
    boost::uuids::uuid party_id_;
    std::string modified_by_;
};

} // namespace ores::scheduler::builder
