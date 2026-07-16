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
#include "ores.refdata.core/repository/currency_pair_convention_calendar_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.refdata.api/domain/currency_pair_convention_calendar_json_io.hpp" // IWYU pragma: keep.

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::currency_pair_convention_calendar
currency_pair_convention_calendar_mapper::map(const currency_pair_convention_calendar_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::currency_pair_convention_calendar r;
    r.version = v.version;
    r.tenant_id = v.tenant_id;
    r.pair_code = v.pair_code.value();
    r.calendar_code = v.calendar_code;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

currency_pair_convention_calendar_entity
currency_pair_convention_calendar_mapper::map(const domain::currency_pair_convention_calendar& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    currency_pair_convention_calendar_entity r;
    r.pair_code = v.pair_code;
    r.tenant_id = v.tenant_id;
    r.calendar_code = v.calendar_code;
    r.version = v.version;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::currency_pair_convention_calendar>
currency_pair_convention_calendar_mapper::map(
    const std::vector<currency_pair_convention_calendar_entity>& v) {
    return map_vector<currency_pair_convention_calendar_entity,
                      domain::currency_pair_convention_calendar>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<currency_pair_convention_calendar_entity> currency_pair_convention_calendar_mapper::map(
    const std::vector<domain::currency_pair_convention_calendar>& v) {
    return map_vector<domain::currency_pair_convention_calendar,
                      currency_pair_convention_calendar_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
