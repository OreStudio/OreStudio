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
#include "ores.refdata.core/repository/commodity_future_option_expiry_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.logging/boost_severity.hpp"
#include "ores.refdata.api/domain/commodity_future_option_expiry.hpp"
#include "ores.refdata.api/domain/commodity_future_option_expiry_json_io.hpp" // IWYU pragma: keep.
#include "ores.refdata.core/repository/commodity_future_option_expiry_entity.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/log/sources/severity_feature.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <vector>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::commodity_future_option_expiry
commodity_future_option_expiry_mapper::map(const commodity_future_option_expiry_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::commodity_future_option_expiry r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.id = v.id;
    r.convention_id = v.convention_id;
    r.party_id = v.party_id;
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
    r.option_underlying_future_convention = v.option_underlying_future_convention;
    r.option_continuation_mappings = v.option_continuation_mappings;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = v.valid_from;

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

commodity_future_option_expiry_entity
commodity_future_option_expiry_mapper::map(const domain::commodity_future_option_expiry& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    commodity_future_option_expiry_entity r;
    r.id = v.id;
    r.tenant_id = v.tenant_id;
    r.version = v.version;
    r.convention_id = v.convention_id;
    r.party_id = v.party_id;
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
    r.option_underlying_future_convention = v.option_underlying_future_convention;
    r.option_continuation_mappings = v.option_continuation_mappings;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::commodity_future_option_expiry> commodity_future_option_expiry_mapper::map(
    const std::vector<commodity_future_option_expiry_entity>& v) {
    return map_vector<commodity_future_option_expiry_entity,
                      domain::commodity_future_option_expiry>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<commodity_future_option_expiry_entity> commodity_future_option_expiry_mapper::map(
    const std::vector<domain::commodity_future_option_expiry>& v) {
    return map_vector<domain::commodity_future_option_expiry,
                      commodity_future_option_expiry_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
