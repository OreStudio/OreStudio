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
#ifndef ORES_REPORTING_CORE_REPOSITORY_ENGINE_SETTINGS_FIXINGS_ENTITY_HPP
#define ORES_REPORTING_CORE_REPOSITORY_ENGINE_SETTINGS_FIXINGS_ENTITY_HPP

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
 * @brief Represents a engine settings fixings in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct engine_settings_fixings_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_reporting_engine_settings_fixings_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    boost::uuids::uuid engine_settings_id;
    boost::uuids::uuid party_id;
    std::optional<bool> imply_todays_fixings;
    std::optional<bool> include_todays_cash_flows;
    std::optional<bool> include_reference_date_events;
    std::optional<bool> enrich_index_fixings;
    std::optional<int> ignore_fixing_lag;
    std::optional<int> ignore_fixing_lead;
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
        using e = engine_settings_fixings_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("engine_settings_id", &e::engine_settings_id) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("imply_todays_fixings", &e::imply_todays_fixings) |
               query_builder::bind("include_todays_cash_flows", &e::include_todays_cash_flows) |
               query_builder::bind("include_reference_date_events",
                                   &e::include_reference_date_events) |
               query_builder::bind("enrich_index_fixings", &e::enrich_index_fixings) |
               query_builder::bind("ignore_fixing_lag", &e::ignore_fixing_lag) |
               query_builder::bind("ignore_fixing_lead", &e::ignore_fixing_lead) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const engine_settings_fixings_entity& v);

}

#endif
