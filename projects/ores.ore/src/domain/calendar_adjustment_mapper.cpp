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
#include "ores.ore/domain/calendar_adjustment_mapper.hpp"

#include <algorithm>

namespace ores::ore::domain {

using namespace ores::logging;

refdata::domain::calendar_adjustment
calendar_adjustment_mapper::map(const newcalendar& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping ORE calendar: " << std::string(v.name);

    refdata::domain::calendar_adjustment r;
    r.calendar_name = std::string(v.name);

    if (v.BaseCalendar)
        r.base_calendar = std::string(*v.BaseCalendar);

    if (v.AdditionalHolidays) {
        r.additional_holidays.reserve(v.AdditionalHolidays->Date.size());
        for (const auto& d : v.AdditionalHolidays->Date)
            r.additional_holidays.push_back(std::string(d));
    }

    if (v.AdditionalBusinessDays) {
        r.additional_business_days.reserve(v.AdditionalBusinessDays->Date.size());
        for (const auto& d : v.AdditionalBusinessDays->Date)
            r.additional_business_days.push_back(std::string(d));
    }

    r.modified_by = "ores";
    r.change_reason_code = "system.external_data_import";
    r.change_commentary = "Imported from ORE XML";

    BOOST_LOG_SEV(lg(), trace) << "Mapped calendar: " << r.calendar_name
                               << " holidays=" << r.additional_holidays.size()
                               << " business_days=" << r.additional_business_days.size();
    return r;
}

std::vector<refdata::domain::calendar_adjustment>
calendar_adjustment_mapper::map(const calendaradjustment& v) {
    BOOST_LOG_SEV(lg(), trace) << "Mapping ORE calendar adjustments. Total: "
                               << v.Calendar.size();

    std::vector<refdata::domain::calendar_adjustment> r;
    r.reserve(v.Calendar.size());
    std::ranges::transform(v.Calendar, std::back_inserter(r),
        [](const auto& c) { return map(c); });

    BOOST_LOG_SEV(lg(), trace) << "Mapped " << r.size() << " calendar adjustments.";
    return r;
}

}
