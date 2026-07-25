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
#include "ores.refdata.core/repository/calendar_date_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/calendar_date_json_io.hpp" // IWYU pragma: keep.

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::calendar_date calendar_date_mapper::map(const calendar_date_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::calendar_date r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.calendar_code = v.calendar_code.value();
    r.date = ores::platform::time::datetime::from_iso8601_date(v.date);
    r.is_business_day = v.is_business_day;
    r.source = v.source;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

calendar_date_entity calendar_date_mapper::map(const domain::calendar_date& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    calendar_date_entity r;
    r.calendar_code = v.calendar_code;
    r.tenant_id = v.tenant_id;
    r.date = ores::platform::time::datetime::to_iso8601_date(v.date);
    r.version = v.version;
    r.is_business_day = v.is_business_day;
    r.source = v.source;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::calendar_date>
calendar_date_mapper::map(const std::vector<calendar_date_entity>& v) {
    return map_vector<calendar_date_entity, domain::calendar_date>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<calendar_date_entity>
calendar_date_mapper::map(const std::vector<domain::calendar_date>& v) {
    return map_vector<domain::calendar_date, calendar_date_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
