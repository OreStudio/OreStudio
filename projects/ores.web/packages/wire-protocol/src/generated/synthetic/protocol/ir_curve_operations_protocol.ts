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
 * @brief One named process parameter, as edited in an IR curve editor's
 * parameter table.
 *
 * The wire-shaped analogue of a {parameter_definition_id, value} row: the
 * row-based store joins values onto the system-tenant parameter definitions
 * catalogue, but a stateless preview request carries the name itself, because
 * the screen already has the definitions on screen, and lets the handler
 * re-materialise the catalogue entry on the fly.
 */
export interface ParameterSpec {
    parameter_name: string;
    parameter_value: number;
}

/**
 * @brief Request a batch (dry-run) simulation of short-rate sample paths.
 *
 * The IR curve analogue of simulate_fx_spot_paths_request. Stateless: the
 * caller names the process parameters rather than a stored config, so an
 * unsaved editor can preview what it is about to save.
 */
export interface SimulateIrCurvePathsRequest {
    /**
     * @brief Short-rate process engine: "vasicek", "cox_ingersoll_ross",
     * "hull_white", or "two_factor_gaussian".
     */
    process_type: string;
    /** @brief The process's named parameters, one entry per parameter. */
    parameters: ParameterSpec[];
    /** @brief Number of ticks (steps) to generate per path. */
    num_ticks: number;
    /** @brief Number of independent sample paths to generate (for contrast). */
    num_paths: number;
    /** @brief Base RNG seed; path i uses seed + i. Change to reseed. */
    seed: number;
}

/**
 * @brief The generated batch, and why it is empty when it is.
 */
export interface SimulateIrCurvePathsResponse {
    success: boolean;
    message: string;
    /** @brief num_paths series, each of num_ticks short-rate values. */
    paths: number[][];
}

/**
 * @brief One (possibly unsaved) Curve Template row, as edited in an IR curve
 * editor's Curve Template tab.
 *
 * Deliberately not the persisted ir_curve_template_entry domain struct: a row
 * being edited has no id, party_id or tenant_id yet.
 */
export interface PreviewIrCurveTemplateRow {
    sequence_index: number;
    start_tenor_code: string;
    end_tenor_code: string;
    instrument_code: string;
}

/**
 * @brief Request a stateless preview of the curve shape (rate per Curve
 * Template entry) implied by the given process parameters and template rows.
 *
 * Derives each row's rate the same way ir_curve_feed does
 * (IYieldCurveProcess::discount_factor() + curve_instrument_pricer, via the
 * shared price_ir_curve_entry()), from the process's current state, with no
 * ticking, so an editor can show "if I save these parameters and this
 * template, here is the curve you would publish" before committing anything.
 * Nothing is persisted or published.
 */
export interface PreviewIrCurveShapeRequest {
    /**
     * @brief Short-rate process engine: "vasicek", "cox_ingersoll_ross",
     * "hull_white", or "two_factor_gaussian".
     */
    process_type: string;
    /** @brief The process's named parameters, one entry per parameter. */
    parameters: ParameterSpec[];
    /**
     * @brief References ores.refdata.payment_frequency.code; needed to build a
     * Swap row's fixed-leg schedule.
     */
    fixed_leg_payment_frequency_code: string;
    /** @brief The template rows to price, in the order the editor shows them. */
    entries: PreviewIrCurveTemplateRow[];
}

/**
 * @brief One priced Curve Template row.
 */
export interface PreviewIrCurveShapePoint {
    sequence_index: number;
    start_tenor_code: string;
    end_tenor_code: string;
    rate: number;
}

/**
 * @brief The priced curve, and why it is empty when it is.
 */
export interface PreviewIrCurveShapeResponse {
    success: boolean;
    message: string;
    /** @brief One point per request entry, sorted by sequence_index. */
    points: PreviewIrCurveShapePoint[];
}

export const subjects = {
    simulate_ir_curve_paths_request: 'synthetic.v1.ops.simulate_ir_curve_paths',
    preview_ir_curve_shape_request: 'synthetic.v1.ops.preview_ir_curve_shape',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    simulate_ir_curve_paths_request: true,
    preview_ir_curve_shape_request: true,
} as const;
