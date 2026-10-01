/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
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
 * Template: domain_types.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * The bond issue leg schedule wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface BondIssueLegSchedule {
    version: number;
    tenant_id: string;
    issue_id: string;
    leg_number: number;
    schedule_role: string;
    sequence_number: number;
    schedule_kind: string;
    start_date: string | null;
    end_date: string | null;
    adjust_end_date_to_previous_month_end: string | null;
    tenor: string | null;
    calendar: string | null;
    convention: string | null;
    term_convention: string | null;
    rule: string | null;
    end_of_month: string | null;
    end_of_month_convention: string | null;
    first_date: string | null;
    last_date: string | null;
    remove_first_date: boolean | null;
    remove_last_date: boolean | null;
    include_duplicate_dates: string | null;
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
    recorded_at: string;
}
