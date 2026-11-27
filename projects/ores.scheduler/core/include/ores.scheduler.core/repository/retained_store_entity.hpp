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
#ifndef ORES_SCHEDULER_CORE_REPOSITORY_RETAINED_STORE_ENTITY_HPP
#define ORES_SCHEDULER_CORE_REPOSITORY_RETAINED_STORE_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.scheduler.core/export.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::scheduler::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a retained store in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
// compass:except(wide_types/over_ceiling) expires=2027-05-26 approver=marco reason=the row is the store with the name and the description a person reads and the shape and target of what it keeps and the bound it ships with, beside the audit tail and the tenant that holds it
struct retained_store_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_scheduler_retained_stores_tbl";

    std::string code;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    std::string display_name;
    std::string description;
    std::string owner_component;
    std::string scope = std::string("platform");
    std::string target_kind;
    std::string target;
    std::optional<std::string> time_column;
    std::string measure_kind;
    std::string bound_kind = std::string("policy");
    std::optional<std::string> default_action;
    std::optional<int> default_retention_days;
    int default_cadence_hours = 24;
    std::optional<std::string> default_reason;
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
        using e = retained_store_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("code", &e::code) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("display_name", &e::display_name) |
               query_builder::bind("description", &e::description) |
               query_builder::bind("owner_component", &e::owner_component) |
               query_builder::bind("scope", &e::scope) |
               query_builder::bind("target_kind", &e::target_kind) |
               query_builder::bind("target", &e::target) |
               query_builder::bind("time_column", &e::time_column) |
               query_builder::bind("measure_kind", &e::measure_kind) |
               query_builder::bind("bound_kind", &e::bound_kind) |
               query_builder::bind("default_action", &e::default_action) |
               query_builder::bind("default_retention_days", &e::default_retention_days) |
               query_builder::bind("default_cadence_hours", &e::default_cadence_hours) |
               query_builder::bind("default_reason", &e::default_reason) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

ORES_SCHEDULER_CORE_EXPORT std::ostream& operator<<(std::ostream& s,
                                                    const retained_store_entity& v);

}

#endif
