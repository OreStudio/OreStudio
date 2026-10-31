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
#ifndef ORES_REPORTING_CORE_REPOSITORY_REPORT_DEFINITION_ENTITY_HPP
#define ORES_REPORTING_CORE_REPOSITORY_REPORT_DEFINITION_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::reporting::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a report definition in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct report_definition_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_reporting_report_definitions_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;

    std::string name;

    boost::uuids::uuid party_id;

    std::string description;
    std::string report_type;
    std::optional<boost::uuids::uuid> fsm_state_id;
    std::string schedule_expression;
    std::string concurrency_policy;
    std::optional<boost::uuids::uuid> scheduler_job_id;
    std::optional<boost::uuids::uuid> run_grant_id;
    std::string pre_processing;
    std::optional<std::string> prepared_input_key;
    std::string post_processing;
    bool is_official = true;
    std::optional<std::string> regulatory_book_type;
    std::optional<std::string> cut_kind_code;
    std::optional<boost::uuids::uuid> cut_id;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = db_timestamp::max();
    db_timestamp valid_to = db_timestamp::max();

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = report_definition_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) | query_builder::bind("name", &e::name) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("description", &e::description) |
               query_builder::bind("report_type", &e::report_type) |
               query_builder::bind("fsm_state_id", &e::fsm_state_id) |
               query_builder::bind("schedule_expression", &e::schedule_expression) |
               query_builder::bind("concurrency_policy", &e::concurrency_policy) |
               query_builder::bind("scheduler_job_id", &e::scheduler_job_id) |
               query_builder::bind("run_grant_id", &e::run_grant_id) |
               query_builder::bind("pre_processing", &e::pre_processing) |
               query_builder::bind("prepared_input_key", &e::prepared_input_key) |
               query_builder::bind("post_processing", &e::post_processing) |
               query_builder::bind("is_official", &e::is_official) |
               query_builder::bind("regulatory_book_type", &e::regulatory_book_type) |
               query_builder::bind("cut_kind_code", &e::cut_kind_code) |
               query_builder::bind("cut_id", &e::cut_id) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const report_definition_entity& v);

}

#endif
