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
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
/**
 * @brief Request the parameters of one view in the terms of the other.
 *
 * Stateless: the caller names the values as they stand on its screen, so an
 * unsaved set converts as readily as a saved one. The simple names are
 * start_rate, long_run_rate, half_life_years and yearly_move_bp.
 */
export interface ConvertIrParametersRequest {
    /** @brief The engine the parameters are for, as its code: VASICEK, HULL_WHITE and so on. */
    process_type: string;
    /** @brief True to read the engine's parameters as the simple view; false for the other way. */
    to_simple: boolean;
    /** @brief The names of the values given; names and values have equal size. */
    parameter_names: string[];
    /** @brief The values given, rates as decimals. */
    parameter_values: number[];
}

/**
 * @brief The converted values, and why there are none when there are none.
 */
export interface ConvertIrParametersResponse {
    success: boolean;
    message: string;
    /** @brief False when the parameters given have no simple view; message then says why. */
    has_simple_form: boolean;
    /** @brief A risk the parameters carry, as a code; empty when there is none. */
    caution: string;
    /** @brief The names of the converted values. */
    parameter_names: string[];
    /** @brief The converted values. */
    parameter_values: number[];
}

export const subjects = {
    convert_ir_parameters_request: 'synthetic.v1.ops.convert_ir_parameters',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    convert_ir_parameters_request: true,
} as const;
