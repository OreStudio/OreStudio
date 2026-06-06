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
#ifndef ORES_DQ_API_DOMAIN_AUDIT_RECORD_HPP
#define ORES_DQ_API_DOMAIN_AUDIT_RECORD_HPP

#include <chrono>
#include <string>

namespace ores::dq::domain {

/**
 * @brief Provenance and audit trail shared by all temporal domain entities.
 *
 * Lives in data quality alongside change_reason, which governs the
 * vocabulary for change_reason_code.
 *
 * Extracted as a plain nested sub-struct so that rfl::internal::no_duplicate_field_names
 * never instantiates a Literal wider than 5 fields for this block.
 * See doc/investigations/msvc_c1202_rfl_complexity.org.
 */
struct audit_record {
    /**
     * @brief Username of the system actor that wrote this record.
     */
    std::string modified_by;

    /**
     * @brief Username of the business user on whose behalf the change was made.
     */
    std::string performed_by;

    /**
     * @brief Structured reason code for the change (vocabulary owned by dq change_reason).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary describing the reason for the change.
     */
    std::string change_commentary;

    /**
     * @brief Wall-clock timestamp at which this version of the record was persisted.
     */
    std::chrono::system_clock::time_point recorded_at;
};

}

#endif
