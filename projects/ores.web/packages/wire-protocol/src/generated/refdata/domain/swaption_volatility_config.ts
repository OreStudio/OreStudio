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
 * The swaption volatility config wire shape.
 *
 * Field names are the C++ member names, because they are the keys rfl::json
 * writes. Renaming them breaks the wire silently, so they are not renamed.
 *
 * See the sibling protocol module for the messages that carry this type.
 */
export interface SwaptionVolatilityConfig {
    version: number;
    tenant_id: string;
    id: string;
    party_id: string;
    curve_definition_id: string;
    dimension: string | null;
    volatility_type: string | null;
    interpolation: string | null;
    extrapolation: string | null;
    output_volatility_type: string | null;
    model_shift: string | null;
    output_shift: string | null;
    day_counter: string | null;
    calendar: string | null;
    business_day_convention: string | null;
    option_tenors: string | null;
    swap_tenors: string | null;
    short_swap_index_base: string | null;
    swap_index_base: string | null;
    smile_option_tenors: string | null;
    smile_swap_tenors: string | null;
    smile_spreads: string | null;
    quote_tag: string | null;
    has_proxy_config: boolean;
    proxy_source_curve_id: string | null;
    proxy_source_short_swap_index_base: string | null;
    proxy_source_swap_index_base: string | null;
    proxy_target_short_swap_index_base: string | null;
    proxy_target_swap_index_base: string | null;
    modified_by: string;
    performed_by: string;
    change_reason_code: string;
    change_commentary: string;
    recorded_at: string;
}
