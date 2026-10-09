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
 * Template: cpp_domain_type_mapper.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.refdata.core/repository/commodity_future_convention_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/commodity_future_convention.hpp"
#include "ores.refdata.api/domain/commodity_future_convention_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/commodity_future_convention_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::commodity_future_convention
commodity_future_convention_mapper::map(const commodity_future_convention_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::commodity_future_convention r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = v.id.value();
    r.party_id = boost::lexical_cast<boost::uuids::uuid>(v.party_id);
    r.contract_frequency = v.contract_frequency;
    r.calendar = v.calendar;
    r.expiry_calendar = v.expiry_calendar;
    r.expiry_month_lag = v.expiry_month_lag;
    r.one_contract_month = v.one_contract_month;
    r.offset_days = v.offset_days;
    r.business_day_convention = v.business_day_convention;
    r.adjust_before_offset = v.adjust_before_offset;
    r.is_averaging = v.is_averaging;
    r.valid_contract_months = v.valid_contract_months;
    r.anchor_day_of_month = v.anchor_day_of_month;
    r.anchor_calendar_days_before = v.anchor_calendar_days_before;
    r.anchor_business_days_after = v.anchor_business_days_after;
    r.anchor_nth_nth = v.anchor_nth_nth;
    r.anchor_nth_weekday = v.anchor_nth_weekday;
    r.anchor_last_weekday = v.anchor_last_weekday;
    r.anchor_weekly_day_of_the_week = v.anchor_weekly_day_of_the_week;
    r.option_expiry_month_lag = v.option_expiry_month_lag;
    r.option_contract_frequency = v.option_contract_frequency;
    r.option_expiry_offset = v.option_expiry_offset;
    r.option_calendar_days_before = v.option_calendar_days_before;
    r.option_min_business_days_before = v.option_min_business_days_before;
    r.option_expiry_day = v.option_expiry_day;
    r.option_nth_nth = v.option_nth_nth;
    r.option_nth_weekday = v.option_nth_weekday;
    r.option_expiry_last_weekday_of_month = v.option_expiry_last_weekday_of_month;
    r.option_expiry_weekly_day_of_the_week = v.option_expiry_weekly_day_of_the_week;
    r.option_business_day_convention = v.option_business_day_convention;
    r.hours_per_day = v.hours_per_day;
    r.off_peak_index = v.off_peak_index;
    r.peak_index = v.peak_index;
    r.off_peak_hours = v.off_peak_hours;
    r.peak_calendar = v.peak_calendar;
    r.index_name = v.index_name;
    r.savings_time = v.savings_time;
    r.delivery_location = v.delivery_location;
    r.balance_of_the_month = v.balance_of_the_month;
    r.balance_of_the_month_pricing_calendar = v.balance_of_the_month_pricing_calendar;
    r.option_underlying_future_convention = v.option_underlying_future_convention;
    r.averaging_commodity_name = v.averaging_commodity_name;
    r.averaging_period = v.averaging_period;
    r.averaging_pricing_calendar = v.averaging_pricing_calendar;
    r.averaging_conventions = v.averaging_conventions;
    r.averaging_use_business_days = v.averaging_use_business_days;
    r.averaging_delivery_roll_days = v.averaging_delivery_roll_days;
    r.averaging_future_month_offset = v.averaging_future_month_offset;
    r.averaging_daily_expiry_offset = v.averaging_daily_expiry_offset;
    r.prohibited_expiries = v.prohibited_expiries;
    r.future_continuation_mappings = v.future_continuation_mappings;
    r.option_continuation_mappings = v.option_continuation_mappings;
    r.oresmd_uri = v.oresmd_uri;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

commodity_future_convention_entity
commodity_future_convention_mapper::map(const domain::commodity_future_convention& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    commodity_future_convention_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.party_id = boost::uuids::to_string(v.party_id);
    r.contract_frequency = v.contract_frequency;
    r.calendar = v.calendar;
    r.expiry_calendar = v.expiry_calendar;
    r.expiry_month_lag = v.expiry_month_lag;
    r.one_contract_month = v.one_contract_month;
    r.offset_days = v.offset_days;
    r.business_day_convention = v.business_day_convention;
    r.adjust_before_offset = v.adjust_before_offset;
    r.is_averaging = v.is_averaging;
    r.valid_contract_months = v.valid_contract_months;
    r.anchor_day_of_month = v.anchor_day_of_month;
    r.anchor_calendar_days_before = v.anchor_calendar_days_before;
    r.anchor_business_days_after = v.anchor_business_days_after;
    r.anchor_nth_nth = v.anchor_nth_nth;
    r.anchor_nth_weekday = v.anchor_nth_weekday;
    r.anchor_last_weekday = v.anchor_last_weekday;
    r.anchor_weekly_day_of_the_week = v.anchor_weekly_day_of_the_week;
    r.option_expiry_month_lag = v.option_expiry_month_lag;
    r.option_contract_frequency = v.option_contract_frequency;
    r.option_expiry_offset = v.option_expiry_offset;
    r.option_calendar_days_before = v.option_calendar_days_before;
    r.option_min_business_days_before = v.option_min_business_days_before;
    r.option_expiry_day = v.option_expiry_day;
    r.option_nth_nth = v.option_nth_nth;
    r.option_nth_weekday = v.option_nth_weekday;
    r.option_expiry_last_weekday_of_month = v.option_expiry_last_weekday_of_month;
    r.option_expiry_weekly_day_of_the_week = v.option_expiry_weekly_day_of_the_week;
    r.option_business_day_convention = v.option_business_day_convention;
    r.hours_per_day = v.hours_per_day;
    r.off_peak_index = v.off_peak_index;
    r.peak_index = v.peak_index;
    r.off_peak_hours = v.off_peak_hours;
    r.peak_calendar = v.peak_calendar;
    r.index_name = v.index_name;
    r.savings_time = v.savings_time;
    r.delivery_location = v.delivery_location;
    r.balance_of_the_month = v.balance_of_the_month;
    r.balance_of_the_month_pricing_calendar = v.balance_of_the_month_pricing_calendar;
    r.option_underlying_future_convention = v.option_underlying_future_convention;
    r.averaging_commodity_name = v.averaging_commodity_name;
    r.averaging_period = v.averaging_period;
    r.averaging_pricing_calendar = v.averaging_pricing_calendar;
    r.averaging_conventions = v.averaging_conventions;
    r.averaging_use_business_days = v.averaging_use_business_days;
    r.averaging_delivery_roll_days = v.averaging_delivery_roll_days;
    r.averaging_future_month_offset = v.averaging_future_month_offset;
    r.averaging_daily_expiry_offset = v.averaging_daily_expiry_offset;
    r.prohibited_expiries = v.prohibited_expiries;
    r.future_continuation_mappings = v.future_continuation_mappings;
    r.option_continuation_mappings = v.option_continuation_mappings;
    r.oresmd_uri = v.oresmd_uri;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::commodity_future_convention>
commodity_future_convention_mapper::map(const std::vector<commodity_future_convention_entity>& v) {
    return map_vector<commodity_future_convention_entity, domain::commodity_future_convention>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<commodity_future_convention_entity>
commodity_future_convention_mapper::map(const std::vector<domain::commodity_future_convention>& v) {
    return map_vector<domain::commodity_future_convention, commodity_future_convention_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
