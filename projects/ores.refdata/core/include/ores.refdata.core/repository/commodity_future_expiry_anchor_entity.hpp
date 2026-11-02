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
#ifndef ORES_REFDATA_CORE_REPOSITORY_COMMODITY_FUTURE_EXPIRY_ANCHOR_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_COMMODITY_FUTURE_EXPIRY_ANCHOR_ENTITY_HPP

#include "ores.database/query_builder/columns.hpp"
#include "ores.database/repository/db_types.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <ostream>
#include <string>

namespace ores::refdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a commodity future expiry anchor in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct commodity_future_expiry_anchor_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_commodity_future_expiry_anchors_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    boost::uuids::uuid convention_id;

    boost::uuids::uuid party_id;
    std::optional<int> anchor_day_of_month;
    std::optional<int> anchor_calendar_days_before;
    std::optional<int> anchor_business_days_after;
    std::optional<int> anchor_nth_nth;
    std::optional<std::string> anchor_nth_weekday;
    std::optional<std::string> anchor_last_weekday;
    std::optional<std::string> anchor_weekly_day_of_the_week;
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
        using e = commodity_future_expiry_anchor_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("convention_id", &e::convention_id) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("anchor_day_of_month", &e::anchor_day_of_month) |
               query_builder::bind("anchor_calendar_days_before", &e::anchor_calendar_days_before) |
               query_builder::bind("anchor_business_days_after", &e::anchor_business_days_after) |
               query_builder::bind("anchor_nth_nth", &e::anchor_nth_nth) |
               query_builder::bind("anchor_nth_weekday", &e::anchor_nth_weekday) |
               query_builder::bind("anchor_last_weekday", &e::anchor_last_weekday) |
               query_builder::bind("anchor_weekly_day_of_the_week",
                                   &e::anchor_weekly_day_of_the_week) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const commodity_future_expiry_anchor_entity& v);

}

#endif
