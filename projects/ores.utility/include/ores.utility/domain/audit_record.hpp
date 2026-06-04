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
#ifndef ORES_UTILITY_DOMAIN_AUDIT_RECORD_HPP
#define ORES_UTILITY_DOMAIN_AUDIT_RECORD_HPP

#include <chrono>
#include <string>

namespace ores::utility::domain {

/**
 * @brief Provenance and audit trail shared by all temporal domain entities.
 *
 * Extracted as a plain nested sub-struct so that rfl::internal::no_duplicate_field_names
 * never instantiates a Literal wider than 5 fields for this block.
 * See doc/investigations/msvc_c1202_rfl_complexity.org.
 */
struct audit_record final {
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
