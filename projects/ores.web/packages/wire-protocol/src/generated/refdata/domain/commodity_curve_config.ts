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
 * The commodity curve config wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface CommodityCurveConfig {
    version: number;
    tenant_id: string;
    id: string;
    curve_definition_id: string;
    currency: string;
    base_price_curve: string | null;
    base_yield_curve: string | null;
    yield_curve: string | null;
    spot_quote: string | null;
    has_quotes: boolean;
    day_counter: string | null;
    interpolation_method: string | null;
    conventions: string | null;
    extrapolation: string | null;
    has_basis_configuration: boolean;
    basis_base_price_curve: string | null;
    basis_base_price_conventions: string | null;
    basis_conventions: string | null;
    basis_day_counter: string | null;
    basis_interpolation_method: string | null;
    basis_add_basis: string | null;
    basis_month_offset: number | null;
    basis_average_base: string | null;
    basis_price_as_historical_fixing: string | null;
    has_price_segments: boolean;
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
    recorded_at: string;
}
