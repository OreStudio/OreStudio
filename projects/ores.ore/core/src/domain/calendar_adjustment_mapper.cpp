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
#include "ores.ore.core/domain/calendar_adjustment_mapper.hpp"

namespace ores::ore::domain {

using namespace ores::logging;
namespace xg = xml::generated::calendar_adjustments;

namespace {

constexpr std::string_view audit_modified_by = "ores";
constexpr std::string_view audit_reason_code = "system.external_data_import";
constexpr std::string_view audit_commentary = "Imported from ORE XML";

/**
 * @brief The dates a nested record holds, or nothing when it never arrived.
 *
 * The table says whether the element appeared and the member holds what it
 * said, so the two are read together rather than by testing a vector's
 * emptiness: an element that arrived with no dates is not an absent element.
 */
auto dates_of(const xg::dates& v) -> std::vector<std::string> {
    return {v.date.begin(), v.date.end()};
}

/**
 * @brief The table the Calendar element is described by.
 *
 * The document type names only the root; the repeated child's table is the
 * type the root's own column points at, so a projection asks rather than
 * repeating the schema.
 */
auto calendar_type() -> const xml::record_type& {
    static const auto* found =
        xml::find_field(*xg::calendar_adjustments_document().root, "Calendar", false);
    return *found->type;
}

}

refdata::messaging::calendar_adjustment calendar_adjustment_mapper::map(const record& v) {
    BOOST_LOG_SEV(lg(), trace) << "Projecting ORE calendar: " << v.name;

    refdata::messaging::calendar_adjustment r;
    r.calendar_name = v.name;

    if (is_present(calendar_type(), &v, "BaseCalendar"))
        r.base_calendar = v.base_calendar;

    if (is_present(calendar_type(), &v, "AdditionalHolidays"))
        r.additional_holidays = dates_of(v.additional_holidays);

    if (is_present(calendar_type(), &v, "AdditionalBusinessDays"))
        r.additional_business_days = dates_of(v.additional_business_days);

    r.modified_by = std::string(audit_modified_by);
    r.change_reason_code = std::string(audit_reason_code);
    r.change_commentary = std::string(audit_commentary);

    BOOST_LOG_SEV(lg(), trace) << "Projected calendar: " << r.calendar_name
                               << " holidays=" << r.additional_holidays.size()
                               << " business_days=" << r.additional_business_days.size();
    return r;
}

std::vector<refdata::messaging::calendar_adjustment>
calendar_adjustment_mapper::map(const document& v) {
    BOOST_LOG_SEV(lg(), trace) << "Projecting ORE calendar adjustments. Total: "
                               << v.calendar.size();

    std::vector<refdata::messaging::calendar_adjustment> r;
    r.reserve(v.calendar.size());
    for (const auto& c : v.calendar)
        r.push_back(map(c));

    BOOST_LOG_SEV(lg(), trace) << "Projected " << r.size() << " calendar adjustments.";
    return r;
}

calendar_adjustment_mapper::record
calendar_adjustment_mapper::reverse(const refdata::messaging::calendar_adjustment& v) {
    BOOST_LOG_SEV(lg(), trace) << "Building calendar record: " << v.calendar_name;

    const auto& type = calendar_type();
    record r;
    r.name = v.calendar_name;
    mark_present(type, &r, "name");

    if (v.base_calendar) {
        r.base_calendar = *v.base_calendar;
        mark_present(type, &r, "BaseCalendar");
    }

    // A date list is two rows: the element's own, and the Date row inside the
    // record it points at. The writer writes only what its row says appeared,
    // so both are stated.
    const auto set_dates = [&type, &r](std::string_view name, auto& nested,
                                       const std::vector<std::string>& values) {
        nested.date = values;
        mark_present(type, &r, name);
        const auto* dates_field = find_field(type, name, false);
        if (dates_field != nullptr && dates_field->type != nullptr)
            mark_present(*dates_field->type, &nested, "Date");
    };

    if (!v.additional_holidays.empty())
        set_dates("AdditionalHolidays", r.additional_holidays, v.additional_holidays);

    if (!v.additional_business_days.empty())
        set_dates("AdditionalBusinessDays", r.additional_business_days,
                  v.additional_business_days);

    return r;
}

calendar_adjustment_mapper::document calendar_adjustment_mapper::reverse(
    const std::vector<refdata::messaging::calendar_adjustment>& v) {
    BOOST_LOG_SEV(lg(), trace) << "Building " << v.size() << " calendar adjustment records.";

    const auto& type = *xg::calendar_adjustments_document().root;
    document r;
    r.calendar.reserve(v.size());
    for (const auto& ca : v)
        r.calendar.push_back(reverse(ca));
    // The Calendar element is repeated and may be absent; the writer needs the
    // bit either way, because a document with no calendar states an empty list
    // rather than nothing.
    mark_present(type, &r, "Calendar");

    return r;
}

}
