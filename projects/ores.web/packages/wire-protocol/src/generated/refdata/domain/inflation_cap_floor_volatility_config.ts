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
 * The inflation cap floor volatility config wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface InflationCapFloorVolatilityConfig {
    version: number;
    tenant_id: string;
    id: string;
    curve_definition_id: string;
    inflation_type: string;
    quote_type: string;
    volatility_type: string;
    extrapolation: string;
    tenors: string;
    settlement_days: number | null;
    cap_strikes: string | null;
    floor_strikes: string | null;
    strikes: string | null;
    calendar: string;
    day_counter: string;
    business_day_convention: string;
    index: string;
    index_curve: string;
    index_interpolated: string | null;
    observation_lag: string;
    yield_term_structure: string;
    quote_index: string | null;
    conventions: string | null;
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
    recorded_at: string;
}
