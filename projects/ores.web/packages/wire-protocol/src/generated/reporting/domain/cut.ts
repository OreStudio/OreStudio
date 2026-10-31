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
 * The cut wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface Cut {
    version: number;
    tenant_id: string;
    id: string;
    kind_code: string;
    scope_code: string;
    timepoint: string;
    take_ordinal: number;
    party_id: string;
    business_centre_code: string;
    timepoint_timezone: string;
    cut_time_code: string | null;
    market_data_snapshot_id: string | null;
    deal_population_id: string | null;
    reference_data_version: string | null;
    engine_version: string | null;
    code_version: string | null;
    taken_by: string;
    taken_at: string | null;
    requires_authorisation: boolean;
    approval_request_id: string | null;
    approved_by: string;
    approved_at: string | null;
    refused_at: string | null;
    refusal_comment: string;
    retention_months: number;
    retention_destination_code: string;
    legal_hold: boolean;
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
    recorded_at: string;
}
