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
 * The default curve configuration wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface DefaultCurveConfiguration {
    version: number;
    tenant_id: string;
    id: string;
    party_id: string;
    curve_definition_id: string;
    is_inline: boolean;
    priority: number | null;
    default_curve_type: string | null;
    discount_curve: string | null;
    day_counter: string | null;
    recovery_rate: string | null;
    start_date: string | null;
    has_quotes: boolean;
    benchmark_curve: string | null;
    reinterpreted_yield_curve: string | null;
    source_curve: string | null;
    pillars: string | null;
    spot_lag: number | null;
    calendar: string | null;
    conventions: string | null;
    extrapolation: string | null;
    running_spread: string | null;
    index_term: string | null;
    imply_default_from_market: string | null;
    allow_negative_rates: string | null;
    price_is_upfront: string | null;
    initial_state: string | null;
    states: string | null;
    position: number;
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
    recorded_at: string;
}
