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
 * The curve segment wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface CurveSegment {
    version: number;
    tenant_id: string;
    id: string;
    party_id: string;
    curve_definition_id: string;
    segment_type: string;
    position: number;
    conventions: string | null;
    pillar_choice: string | null;
    priority: number | null;
    min_distance: number | null;
    projection_curve: string | null;
    discount_curve: string | null;
    spot_rate: string | null;
    projection_curve_domestic: string | null;
    projection_curve_foreign: string | null;
    projection_curve_pay: string | null;
    projection_curve_receive: string | null;
    projection_curve_long: string | null;
    projection_curve_short: string | null;
    reference_curve: string | null;
    reference_curve_2: string | null;
    weight_1: number | null;
    weight_2: number | null;
    ibor_index: string | null;
    rfr_curve: string | null;
    rfr_index: string | null;
    spread: number | null;
    base_curve: string | null;
    base_curve_currency: string | null;
    numerator_curve: string | null;
    numerator_curve_currency: string | null;
    denominator_curve: string | null;
    denominator_curve_currency: string | null;
    extrapolate_flat: boolean | null;
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
    recorded_at: string;
}
