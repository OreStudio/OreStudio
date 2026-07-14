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
#ifndef ORES_HISTORY_API_DOMAIN_PROVENANCE_FIELDS_HPP
#define ORES_HISTORY_API_DOMAIN_PROVENANCE_FIELDS_HPP

namespace ores::history::domain {

/**
 * @brief Field names every history_field_mapper appends to describe
 * who/why/when a version was recorded, rather than what changed in
 * it. Shared between the field mappers that populate them and any
 * presentation layer that renders them separately from the entity's
 * own diffable fields (e.g. HistoryDialog's timeline cards).
 */
struct provenance_fields {
    static constexpr const char* recorded_at = "Recorded At";
    static constexpr const char* modified_by = "Modified By";
    static constexpr const char* performed_by = "Performed By";
    static constexpr const char* change_reason_code = "Change Reason Code";
    static constexpr const char* change_commentary = "Change Commentary";
};

}

#endif
