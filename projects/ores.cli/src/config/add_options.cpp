/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.cli/config/add_options.hpp"

#include <ostream>
#include <magic_enum/magic_enum.hpp>

namespace ores::cli::config {

std::ostream& operator<<(std::ostream& s, const add_options& v) {
    s << "{ target_entity: " << magic_enum::enum_name(v.target_entity);

    // Print currency fields if present
    if (v.iso_code) s << ", iso_code: " << *v.iso_code;
    if (v.name) s << ", name: " << *v.name;
    if (v.numeric_code) s << ", numeric_code: " << *v.numeric_code;
    if (v.symbol) s << ", symbol: " << *v.symbol;
    if (v.fraction_symbol) s << ", fraction_symbol: " << *v.fraction_symbol;
    if (v.fractions_per_unit) s << ", fractions_per_unit: " << *v.fractions_per_unit;
    if (v.rounding_type) s << ", rounding_type: " << *v.rounding_type;
    if (v.rounding_precision) s << ", rounding_precision: " << *v.rounding_precision;
    if (v.format) s << ", format: " << *v.format;
    if (v.currency_type) s << ", currency_type: " << *v.currency_type;
    if (v.modified_by) s << ", modified_by: " << *v.modified_by;

    // Print account fields if present
    if (v.username) s << ", username: " << *v.username;
    if (v.email) s << ", email: " << *v.email;
    if (v.password) s << ", password: [REDACTED]";
    if (v.is_admin) s << ", is_admin: " << (*v.is_admin ? "true" : "false");

    // Print feature flag fields if present
    if (v.flag_name) s << ", flag_name: " << *v.flag_name;
    if (v.description) s << ", description: " << *v.description;
    if (v.enabled) s << ", enabled: " << (*v.enabled ? "true" : "false");

    s << " }";
    return s;
}

}
