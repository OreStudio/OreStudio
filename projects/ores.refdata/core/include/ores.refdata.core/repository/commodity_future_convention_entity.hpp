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
#ifndef ORES_REFDATA_CORE_REPOSITORY_COMMODITY_FUTURE_CONVENTION_ENTITY_HPP
#define ORES_REFDATA_CORE_REPOSITORY_COMMODITY_FUTURE_CONVENTION_ENTITY_HPP

#include "ores.database/repository/db_types.hpp"
#include "sqlgen/PrimaryKey.hpp"
#include <optional>
#include <ostream>
#include <string>

namespace ores::refdata::repository {

using db_timestamp = ores::database::repository::db_timestamp;

/**
 * @brief Represents a commodity future convention in the database.
 */
struct commodity_future_convention_entity {
    constexpr static const char* schema = "public";
    constexpr static const char* tablename = "ores_refdata_commodity_future_conventions_tbl";

    sqlgen::PrimaryKey<std::string> id;
    std::string tenant_id;
    int version = 0;
    std::string party_id;
    std::string contract_frequency;
    std::string calendar;
    std::optional<std::string> expiry_calendar;
    std::optional<int> expiry_month_lag;
    std::optional<std::string> one_contract_month;
    std::optional<int> offset_days;
    std::optional<std::string> business_day_convention;
    std::optional<bool> adjust_before_offset;
    std::optional<bool> is_averaging;
    std::optional<std::string> valid_contract_months;
    std::optional<int> anchor_day_of_month;
    std::optional<int> anchor_calendar_days_before;
    std::optional<int> anchor_business_days_after;
    std::optional<int> anchor_nth_nth;
    std::optional<std::string> anchor_nth_weekday;
    std::optional<std::string> anchor_last_weekday;
    std::optional<std::string> anchor_weekly_day_of_the_week;
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
    std::optional<int> hours_per_day;
    std::optional<std::string> off_peak_index;
    std::optional<std::string> peak_index;
    std::optional<double> off_peak_hours;
    std::optional<std::string> peak_calendar;
    std::optional<std::string> index_name;
    std::optional<std::string> savings_time;
    std::optional<std::string> delivery_location;
    std::optional<bool> balance_of_the_month;
    std::optional<std::string> balance_of_the_month_pricing_calendar;
    std::optional<std::string> option_underlying_future_convention;
    std::optional<std::string> averaging_commodity_name;
    std::optional<std::string> averaging_period;
    std::optional<std::string> averaging_pricing_calendar;
    std::optional<std::string> averaging_conventions;
    std::optional<bool> averaging_use_business_days;
    std::optional<int> averaging_delivery_roll_days;
    std::optional<int> averaging_future_month_offset;
    std::optional<int> averaging_daily_expiry_offset;
    std::optional<std::string> prohibited_expiries;
    std::optional<std::string> future_continuation_mappings;
    std::optional<std::string> option_continuation_mappings;
    std::optional<std::string> oresmd_uri;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = "9999-12-31 23:59:59";
    db_timestamp valid_to = "9999-12-31 23:59:59";
};

std::ostream& operator<<(std::ostream& s, const commodity_future_convention_entity& v);

}

#endif
