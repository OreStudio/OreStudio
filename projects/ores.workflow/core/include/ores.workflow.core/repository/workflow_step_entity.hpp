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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_domain_type_entity.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_WORKFLOW_CORE_REPOSITORY_WORKFLOW_STEP_ENTITY_HPP
#define ORES_WORKFLOW_CORE_REPOSITORY_WORKFLOW_STEP_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::workflow::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a workflow step in the database.
 */
struct workflow_step_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_workflow_workflow_steps_tbl";

    std::string id;
    std::string tenant_id;
    int version = 0;
    std::string workflow_id;
    int step_index = 0;
    std::string name;
    std::string state_id;
    std::string request_json;
    std::optional<std::string> response_json;
    std::optional<std::string> error;
    std::optional<std::string> step_log_json;
    std::string command_subject;
    std::string command_json;
    std::optional<db_timestamp> command_published_at;
    std::string idempotency_key;
    std::string compensation_subject;
    std::string compensation_json;
    std::optional<db_timestamp> started_at;
    std::optional<db_timestamp> completed_at;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = workflow_step_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("workflow_id", &e::workflow_id) |
               query_builder::bind("step_index", &e::step_index) |
               query_builder::bind("name", &e::name) |
               query_builder::bind("state_id", &e::state_id) |
               query_builder::bind("request_json", &e::request_json) |
               query_builder::bind("response_json", &e::response_json) |
               query_builder::bind("error", &e::error) |
               query_builder::bind("step_log_json", &e::step_log_json) |
               query_builder::bind("command_subject", &e::command_subject) |
               query_builder::bind("command_json", &e::command_json) |
               query_builder::bind("command_published_at", &e::command_published_at) |
               query_builder::bind("idempotency_key", &e::idempotency_key) |
               query_builder::bind("compensation_subject", &e::compensation_subject) |
               query_builder::bind("compensation_json", &e::compensation_json) |
               query_builder::bind("started_at", &e::started_at) |
               query_builder::bind("completed_at", &e::completed_at) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const workflow_step_entity& v);

}

#endif
