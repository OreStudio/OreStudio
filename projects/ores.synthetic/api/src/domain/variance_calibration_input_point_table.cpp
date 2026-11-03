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
 * Template: cpp_domain_type_table.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.synthetic.api/domain/variance_calibration_input_point_table.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <fort.hpp>
#include <sstream>

namespace ores::synthetic::domain {

namespace {
template <typename T>
std::string variance_calibration_input_point_opt_str(const std::optional<T>& o) {
    if (!o)
        return {};
    std::ostringstream s;
    if constexpr (std::is_same_v<T, bool>)
        s << std::boolalpha;
    s << *o;
    return s.str();
}
}

std::string convert_to_table(const std::vector<variance_calibration_input_point>& v) {
    fort::char_table table;
    table.set_border_style(FT_BASIC_STYLE);

    table << fort::header << "Date" << "Fixing" << "Return" << "Used" << "Left Out" << "Version"
          << fort::endr;

    for ([[maybe_unused]] const auto& p : v) {
        table << p.fixing_date << p.fixing_text
              << variance_calibration_input_point_opt_str(p.log_return)
              << (p.used ? "true" : "false") << p.exclusion_reason << p.version << fort::endr;
    }
    return table.to_string();
}

}
