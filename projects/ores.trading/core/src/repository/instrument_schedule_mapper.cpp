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
#include "ores.trading.core/repository/instrument_schedule_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.trading.api/domain/instrument_schedule_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::trading::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::instrument_schedule instrument_schedule_mapper::map(const instrument_schedule_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::instrument_schedule r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.instrument_id = boost::lexical_cast<boost::uuids::uuid>(v.instrument_id.value());
    r.leg_role = v.leg_role.value();
    r.leg_number = boost::lexical_cast<int>(v.leg_number.value());
    r.schedule_role = v.schedule_role.value();
    r.schedule_kind = v.schedule_kind;
    r.start_date = v.start_date;
    r.end_date = v.end_date;
    r.adjust_end_date_to_previous_month_end = v.adjust_end_date_to_previous_month_end;
    r.tenor = v.tenor;
    r.calendar = v.calendar;
    r.convention = v.convention;
    r.term_convention = v.term_convention;
    r.rule = v.rule;
    r.end_of_month = v.end_of_month;
    r.end_of_month_convention = v.end_of_month_convention;
    r.first_date = v.first_date;
    r.last_date = v.last_date;
    r.remove_first_date = v.remove_first_date;
    r.remove_last_date = v.remove_last_date;
    r.include_duplicate_dates = v.include_duplicate_dates;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

instrument_schedule_entity instrument_schedule_mapper::map(const domain::instrument_schedule& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    instrument_schedule_entity r;
    r.instrument_id = boost::uuids::to_string(v.instrument_id);
    r.leg_role = v.leg_role;
    r.leg_number = std::to_string(v.leg_number);
    r.schedule_role = v.schedule_role;
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;
    r.schedule_kind = v.schedule_kind;
    r.start_date = v.start_date;
    r.end_date = v.end_date;
    r.adjust_end_date_to_previous_month_end = v.adjust_end_date_to_previous_month_end;
    r.tenor = v.tenor;
    r.calendar = v.calendar;
    r.convention = v.convention;
    r.term_convention = v.term_convention;
    r.rule = v.rule;
    r.end_of_month = v.end_of_month;
    r.end_of_month_convention = v.end_of_month_convention;
    r.first_date = v.first_date;
    r.last_date = v.last_date;
    r.remove_first_date = v.remove_first_date;
    r.remove_last_date = v.remove_last_date;
    r.include_duplicate_dates = v.include_duplicate_dates;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::instrument_schedule>
instrument_schedule_mapper::map(const std::vector<instrument_schedule_entity>& v) {
    return map_vector<instrument_schedule_entity, domain::instrument_schedule>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<instrument_schedule_entity>
instrument_schedule_mapper::map(const std::vector<domain::instrument_schedule>& v) {
    return map_vector<domain::instrument_schedule, instrument_schedule_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
