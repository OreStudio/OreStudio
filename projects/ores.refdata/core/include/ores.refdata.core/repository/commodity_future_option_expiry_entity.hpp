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
#ifndef ORES_REFDATA_CORE_REPOSITORY_COMMODITY_FUTURE_OPTION_EXPIRY_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_COMMODITY_FUTURE_OPTION_EXPIRY_ENTITY_HPP

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
 * @brief Represents a commodity future option expiry in the database.
 *
 * Each member holds the type its column has, so the layer binds and reads it
 * in the binary format and the mapper copies it to the domain member. A column
 * whose type the layer has no codec for keeps its text, and the mapper converts
 * it.
 */
struct commodity_future_option_expiry_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_commodity_future_option_expiries_tbl";

    boost::uuids::uuid id;
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();
    int version = 0;
    boost::uuids::uuid convention_id;

    boost::uuids::uuid party_id;
    std::optional<int> option_expiry_month_lag;
    std::optional<std::string> option_contract_frequency;
    std::optional<int> option_expiry_offset;
    std::optional<int> option_calendar_days_before;
    std::optional<int> option_min_business_days_before;
    std::optional<int> option_expiry_day;
    std::optional<int> option_nth_nth;
    std::optional<std::string> option_nth_weekday;
    std::optional<std::string> option_expiry_last_weekday_of_month;
    std::optional<std::string> option_expiry_weekly_day_of_the_week;
    std::optional<std::string> option_business_day_convention;
    std::optional<std::string> option_underlying_future_convention;
    std::optional<std::string> option_continuation_mappings;
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
        using e = commodity_future_option_expiry_entity;
        namespace query_builder = ores::database::query_builder;
        return query_builder::columns<e>() | query_builder::key("id", &e::id) |
               query_builder::bind("tenant_id", &e::tenant_id) |
               query_builder::bind("version", &e::version) |
               query_builder::bind("convention_id", &e::convention_id) |
               query_builder::bind("party_id", &e::party_id) |
               query_builder::bind("option_expiry_month_lag", &e::option_expiry_month_lag) |
               query_builder::bind("option_contract_frequency", &e::option_contract_frequency) |
               query_builder::bind("option_expiry_offset", &e::option_expiry_offset) |
               query_builder::bind("option_calendar_days_before", &e::option_calendar_days_before) |
               query_builder::bind("option_min_business_days_before",
                                   &e::option_min_business_days_before) |
               query_builder::bind("option_expiry_day", &e::option_expiry_day) |
               query_builder::bind("option_nth_nth", &e::option_nth_nth) |
               query_builder::bind("option_nth_weekday", &e::option_nth_weekday) |
               query_builder::bind("option_expiry_last_weekday_of_month",
                                   &e::option_expiry_last_weekday_of_month) |
               query_builder::bind("option_expiry_weekly_day_of_the_week",
                                   &e::option_expiry_weekly_day_of_the_week) |
               query_builder::bind("option_business_day_convention",
                                   &e::option_business_day_convention) |
               query_builder::bind("option_underlying_future_convention",
                                   &e::option_underlying_future_convention) |
               query_builder::bind("option_continuation_mappings",
                                   &e::option_continuation_mappings) |
               query_builder::bind("modified_by", &e::modified_by) |
               query_builder::bind("performed_by", &e::performed_by) |
               query_builder::bind("change_reason_code", &e::change_reason_code) |
               query_builder::bind("change_commentary", &e::change_commentary) |
               query_builder::bind("valid_from", &e::valid_from) |
               query_builder::bind("valid_to", &e::valid_to);
    }
};

std::ostream& operator<<(std::ostream& s, const commodity_future_option_expiry_entity& v);

}

#endif
