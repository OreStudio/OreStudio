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
#ifndef ORES_REPORTING_CORE_REPOSITORY_REPORT_TRIGGER_EVENT_ENTITY_HPP
#define ORES_REPORTING_CORE_REPOSITORY_REPORT_TRIGGER_EVENT_ENTITY_HPP

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
 * @brief Represents a report trigger event in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct report_trigger_event_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_reporting_report_trigger_events_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    boost::uuids::uuid report_definition_id;
    std::string trigger_type;
    std::optional<boost::uuids::uuid> scheduler_job_id;
    std::optional<std::int64_t> schedule_job_run_id;
    std::string triggered_by;
    db_timestamp triggered_at;
    boost::uuids::uuid party_id;

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = report_trigger_event_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("report_definition_id", &e::report_definition_id) |
               query_builder::bind("trigger_type", &e::trigger_type) |
               query_builder::bind("scheduler_job_id", &e::scheduler_job_id) |
               query_builder::bind("schedule_job_run_id", &e::schedule_job_run_id) |
               query_builder::bind("triggered_by", &e::triggered_by) |
               query_builder::bind("triggered_at", &e::triggered_at) |
               query_builder::bind("party_id", &e::party_id);
    }
};

std::ostream& operator<<(std::ostream& s, const report_trigger_event_entity& v);

}

#endif
