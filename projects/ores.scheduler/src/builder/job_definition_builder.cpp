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
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include "ores.scheduler/builder/job_definition_builder.hpp"

namespace ores::scheduler::builder {

job_definition_builder&
job_definition_builder::with_name(std::string_view name) {
    if (name.empty() && error_.empty())
        error_ = "job name must not be empty";
    name_ = std::string(name);
    return *this;
}

job_definition_builder&
job_definition_builder::with_description(std::string_view description) {
    description_ = std::string(description);
    return *this;
}

job_definition_builder&
job_definition_builder::with_command(std::string_view command) {
    if (command.empty() && error_.empty())
        error_ = "command must not be empty";
    command_ = std::string(command);
    return *this;
}

job_definition_builder&
job_definition_builder::with_cron_schedule(std::string_view expr) {
    auto result = domain::cron_expression::from_string(expr);
    if (!result) {
        if (error_.empty())
            error_ = std::move(result.error());
    } else {
        schedule_expression_ = std::move(result).value();
    }
    return *this;
}

job_definition_builder&
job_definition_builder::with_database(std::string_view database_name) {
    if (database_name.empty() && error_.empty())
        error_ = "database name must not be empty";
    database_name_ = std::string(database_name);
    return *this;
}

job_definition_builder&
job_definition_builder::with_tenant(const utility::uuid::tenant_id& tenant_id) {
    tenant_id_ = tenant_id;
    return *this;
}

job_definition_builder&
job_definition_builder::with_party(const boost::uuids::uuid& party_id) {
    party_id_ = party_id;
    return *this;
}

job_definition_builder&
job_definition_builder::with_modified_by(std::string_view modified_by) {
    if (modified_by.empty() && error_.empty())
        error_ = "modified_by must not be empty";
    modified_by_ = std::string(modified_by);
    return *this;
}

std::expected<domain::job_definition, std::string>
job_definition_builder::build() const {
    if (!error_.empty())
        return std::unexpected(error_);

    if (name_.empty())
        return std::unexpected("job name is required");
    if (command_.empty())
        return std::unexpected("command is required");
    if (!schedule_expression_)
        return std::unexpected("schedule expression is required");
    if (database_name_.empty())
        return std::unexpected("database name is required");
    if (modified_by_.empty())
        return std::unexpected("modified_by is required");

    utility::uuid::uuid_v7_generator gen;
    return domain::job_definition{
        .id = gen(),
        .tenant_id = tenant_id_,
        .party_id = party_id_,
        .cron_job_id = std::nullopt,
        .job_name = name_,
        .description = description_,
        .command = command_,
        .schedule_expression = *schedule_expression_,
        .database_name = database_name_,
        .is_active = true,
        .version = 0,
        .modified_by = modified_by_,
    };
}

} // namespace ores::scheduler::builder
