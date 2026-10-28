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
#ifndef ORES_REFDATA_CORE_REPOSITORY_COMMODITY_FORWARD_CONVENTION_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_COMMODITY_FORWARD_CONVENTION_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::refdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a commodity forward convention in the database.
 */
struct commodity_forward_convention_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_commodity_forward_conventions_tbl";

    std::string id;
    std::string tenant_id;
    int version = 0;
    std::string party_id;
    std::optional<int> spot_days;
    std::optional<double> points_factor;
    std::optional<std::string> advance_calendar;
    std::optional<bool> spot_relative;
    std::optional<std::string> delivery_location;
    std::optional<std::string> business_day_convention;
    std::optional<bool> outright;
    std::optional<std::string> oresmd_uri;
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
        using e = commodity_forward_convention_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("spot_days", &e::spot_days) |
               query_builder::bind("points_factor", &e::points_factor) |
               query_builder::bind("advance_calendar", &e::advance_calendar) |
               query_builder::bind("spot_relative", &e::spot_relative) |
               query_builder::bind("delivery_location", &e::delivery_location) |
               query_builder::bind("business_day_convention", &e::business_day_convention) |
               query_builder::bind("outright", &e::outright) |
               query_builder::bind("oresmd_uri", &e::oresmd_uri) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const commodity_forward_convention_entity& v);

}

#endif
