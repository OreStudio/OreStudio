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
#ifndef ORES_REPORTING_CORE_REPOSITORY_REPORT_ERROR_EVENT_ENTITY_HPP
#define ORES_REPORTING_CORE_REPOSITORY_REPORT_ERROR_EVENT_ENTITY_HPP

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
 * @brief Represents a report error event in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct report_error_event_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_reporting_report_error_events_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    boost::uuids::uuid report_instance_id;
    boost::uuids::uuid party_id;
    std::string scope_code = "Report";
    std::string severity = "error";
    std::string message;
    std::optional<std::string> subject;
    std::optional<std::string> subject_id;
    std::optional<int> subject_version;
    std::optional<db_timestamp> subject_as_of;
    std::optional<std::string> cause;
    std::optional<std::string> detail;
    db_timestamp event_time;

    /**
     * @brief The columns of the table, named as the members, with the key.
     */
    static constexpr auto columns() {
        using e = report_error_event_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("report_instance_id", &e::report_instance_id) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("scope_code", &e::scope_code) |
               query_builder::bind("severity", &e::severity) |
               query_builder::bind("message", &e::message) |
               query_builder::bind("subject", &e::subject) |
               query_builder::bind("subject_id", &e::subject_id) |
               query_builder::bind("subject_version", &e::subject_version) |
               query_builder::bind("subject_as_of", &e::subject_as_of) |
               query_builder::bind("cause", &e::cause) |
               query_builder::bind_text("detail", &e::detail) |
               query_builder::bind("event_time", &e::event_time);
    }
};

std::ostream& operator<<(std::ostream& s, const report_error_event_entity& v);

}

#endif
