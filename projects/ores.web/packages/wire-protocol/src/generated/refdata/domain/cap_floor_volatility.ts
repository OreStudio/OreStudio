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
 * The cap floor volatility wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface CapFloorVolatility {
    version: number;
    tenant_id: string;
    id: string;
    curve_definition_id: string;
    volatility_type: string | null;
    output_volatility_type: string | null;
    model_shift: number | null;
    output_shift: number | null;
    extrapolation: string | null;
    interpolation_method: string | null;
    include_atm: string | null;
    day_counter: string | null;
    calendar: string | null;
    business_day_convention: string | null;
    tenors: string | null;
    strikes: string | null;
    optional_quotes: string | null;
    ibor_index: string | null;
    index: string | null;
    rate_computation_period: string | null;
    on_cap_settlement_days: number | null;
    discount_curve: string | null;
    atm_tenors: string | null;
    settlement_days: number | null;
    interpolate_on: string | null;
    time_interpolation: string | null;
    strike_interpolation: string | null;
    input_type: string | null;
    quote_includes_index_name: string | null;
    flat_first_period: string | null;
    use_effecive_volatility: string | null;
    use_effective_volatility: string | null;
    has_proxy_config: boolean;
    proxy_source_curve_id: string | null;
    proxy_source_index: string | null;
    proxy_source_rate_computation_period: string | null;
    proxy_target_index: string | null;
    proxy_target_rate_computation_period: string | null;
    proxy_target_on_cap_settlement_days: number | null;
    proxy_scaling_factor: number | null;
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
    recorded_at: string;
}
