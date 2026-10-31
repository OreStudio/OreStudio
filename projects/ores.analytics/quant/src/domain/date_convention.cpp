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
#include "ores.analytics.quant/domain/date_convention.hpp"
#include <array>
#include <stdexcept>

namespace ores::analytics::quant::domain {

namespace {

struct convention_entry final {
    std::string_view code;
    business_day_convention convention;
};

/// The persisted codes are QuantLib's own enumerator names, so this table
/// reads as the QuantLib enum with ores_refdata's spelling attached. Order
/// matches @c business_day_convention, which matches QuantLib.
constexpr std::array<convention_entry, 7> conventions{{
    {"Unadjusted", business_day_convention::unadjusted},
    {"Following", business_day_convention::following},
    {"ModifiedFollowing", business_day_convention::modified_following},
    {"Preceding", business_day_convention::preceding},
    {"ModifiedPreceding", business_day_convention::modified_preceding},
    {"Nearest", business_day_convention::nearest},
    {"HalfMonthModifiedFollowing", business_day_convention::half_month_modified_following},
}};

struct unit_entry final {
    std::string_view code;
    period_unit unit;
};

constexpr std::array<unit_entry, 5> units{{
    {"DAY", period_unit::day},
    {"WEEK", period_unit::week},
    {"MONTH", period_unit::month},
    {"YEAR", period_unit::year},
    {"NONE", period_unit::none},
}};

} // namespace

business_day_convention parse_business_day_convention_code(std::string_view code) {
    for (const auto& entry : conventions) {
        if (entry.code == code)
            return entry.convention;
    }
    throw std::invalid_argument("Unrecognised business day convention code: " + std::string(code));
}

std::string_view to_business_day_convention_code(business_day_convention convention) {
    for (const auto& entry : conventions) {
        if (entry.convention == convention)
            return entry.code;
    }
    throw std::invalid_argument("Business day convention has no persisted code");
}

period_unit parse_period_unit_code(std::string_view code) {
    for (const auto& entry : units) {
        if (entry.code == code)
            return entry.unit;
    }
    throw std::invalid_argument("Unrecognised tenor unit code: " + std::string(code));
}

std::string_view to_period_unit_code(period_unit unit) {
    for (const auto& entry : units) {
        if (entry.unit == unit)
            return entry.code;
    }
    throw std::invalid_argument("Period unit has no persisted code");
}

} // namespace ores::analytics::quant::domain
