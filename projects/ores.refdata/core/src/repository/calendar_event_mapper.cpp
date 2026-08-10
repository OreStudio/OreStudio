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
#include "ores.refdata.core/repository/calendar_event_mapper.hpp"
#include "ores.database/repository/mapper_helpers.hpp"
#include "ores.platform/time/datetime.hpp"
#include "ores.refdata.api/domain/calendar_event_json_io.hpp" // IWYU pragma: keep.
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <chrono>
#include <format>
#include <sstream>

namespace ores::refdata::repository {

using namespace ores::logging;
using namespace ores::database::repository;

domain::calendar_event calendar_event_mapper::map(const calendar_event_entity& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping db entity: " << v;

    domain::calendar_event r;
    r.version = v.version;
    r.tenant_id = utility::uuid::tenant_id::from_string(v.tenant_id).value();
    r.id = boost::lexical_cast<boost::uuids::uuid>(v.id.value());

    r.calendar_code = v.calendar_code;

    {
        int yy{}, mm{}, dd{};
        char s1{}, s2{};
        std::istringstream ss(v.event_date);
        ss >> yy >> s1 >> mm >> s2 >> dd;
        r.event_date = std::chrono::year{yy} / std::chrono::month{static_cast<unsigned>(mm)} /
                       std::chrono::day{static_cast<unsigned>(dd)};
    }


    r.diary_entry_type = v.diary_entry_type;

    r.name = v.name;
    r.description = v.description;
    r.source = v.source;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;
    r.recorded_at = timestamp_to_timepoint(v.valid_from);

    BOOST_LOG_SEV(lg(), trace) << "Mapped db entity. Result: " << r;
    return r;
}

calendar_event_entity calendar_event_mapper::map(const domain::calendar_event& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping domain entity: " << v;

    calendar_event_entity r;
    r.id = boost::uuids::to_string(v.id);
    r.tenant_id = v.tenant_id.to_string();
    r.version = v.version;

    r.calendar_code = v.calendar_code;

    r.event_date = std::format("{:%Y-%m-%d}", v.event_date);


    r.diary_entry_type = v.diary_entry_type;

    r.name = v.name;
    r.description = v.description;
    r.source = v.source;
    r.modified_by = v.modified_by;
    r.performed_by = v.performed_by;
    r.change_reason_code = v.change_reason_code;
    r.change_commentary = v.change_commentary;

    BOOST_LOG_SEV(lg(), trace) << "Mapped domain entity. Result: " << r;
    return r;
}

std::vector<domain::calendar_event>
calendar_event_mapper::map(const std::vector<calendar_event_entity>& v) {
    return map_vector<calendar_event_entity, domain::calendar_event>(
        v, [](const auto& ve) { return map(ve); }, lg(), "db entities");
}

std::vector<calendar_event_entity>
calendar_event_mapper::map(const std::vector<domain::calendar_event>& v) {
    return map_vector<domain::calendar_event, calendar_event_entity>(
        v, [](const auto& ve) { return map(ve); }, lg(), "domain entities");
}

}
