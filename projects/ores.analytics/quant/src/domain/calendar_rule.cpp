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
#include "ores.analytics.quant/domain/calendar_rule.hpp"
#include <array>
#include <stdexcept>
#include <string>

namespace ores::analytics::quant::domain {

namespace {

struct shift_entry final {
    std::string_view code;
    observance_shift shift;
};

/// The spellings @c ores_refdata_calendar_rules_tbl.shift carries, in the
/// order the enum declares them.
constexpr std::array<shift_entry, 4> shifts{{
    {"none", observance_shift::none},
    {"nearest_weekday", observance_shift::nearest_weekday},
    {"roll_forward_to_monday", observance_shift::roll_forward_to_monday},
    {"sunday_to_monday", observance_shift::sunday_to_monday},
}};

} // namespace

observance_shift parse_observance_shift_code(std::string_view code) {
    for (const auto& entry : shifts) {
        if (entry.code == code)
            return entry.shift;
    }
    throw std::invalid_argument("Unrecognised observance shift code: " + std::string(code));
}

std::string_view to_observance_shift_code(observance_shift shift) {
    for (const auto& entry : shifts) {
        if (entry.shift == shift)
            return entry.code;
    }
    throw std::invalid_argument("Observance shift has no persisted code");
}

} // namespace ores::analytics::quant::domain
