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
 * @brief How far one quoted pillar is from the calibrated surface.
 */
export interface FxVolPillarFit {
    point: string;
    expiry: string;
    label: string;
    quoted: number;
    fitted: number;
    error: number;
    beyond_tolerance: boolean;
}

/**
 * @brief One point of the candidate's grid, as written to the output series.
 */
export interface FxVolGridPoint {
    expiry: string;
    strike: string;
    log_moneyness: number;
    volatility: number;
}

/**
 * @brief One arbitrage condition the calibrated surface breaks.
 */
export interface FxVolViolation {
    kind: string;
    expiry: string;
    description: string;
}

/**
 * @brief A pillar no contributor quoted at the as-of, with the series it read.
 */
export interface FxVolMissingPillar {
    point: string;
    series_ids: string[];
}

/**
 * @brief Calibrates one FX volatility surface blueprint of the caller's party at an as-of and writes the candidate. The recipe is checked before any quote is read. A refusal names what failed, and writes nothing.
 */
export interface CalibrateFxVolSurfaceRequest {
    /**
     * @brief The blueprint to calibrate.
     */
    blueprint_id: string;
    /**
     * @brief The instant the calibration reads its quotes and forwards at.
     */
    as_of: string;
}

/**
 * @brief The candidate the calibration wrote, or why it refused.
 */
export interface CalibrateFxVolSurfaceResponse {
    success: boolean;
    message: string;
    calibration_id: string;
    blueprint_name: string;
    blueprint_version: number;
    as_of: string;
    forward_publish_id: string;
    rho: number;
    eta: number;
    gamma: number;
    converged: boolean;
    max_abs_error: number;
    pillars: FxVolPillarFit[];
    grid: FxVolGridPoint[];
    violations: FxVolViolation[];
    missing: FxVolMissingPillar[];
}

/**
 * @brief Raises, or applies, the publish of a calibrated surface of the caller's party. A call that names no approved request raises a marketdata.fx_vol_surface_publish request for the calibration of the blueprint at the as-of, and records its checks for the decider. A call that names the approved request publishes that calibration and ends the set it replaces. A blueprint with no calibration at the as-of is refused, and nothing is written.
 */
export interface PublishFxVolSurfaceRequest {
    blueprint_id: string;
    as_of: string;
    /**
     * @brief The approved request this call applies, or empty to raise one.
     */
    approval_request_id: string;
    /**
     * @brief Why the surface should be published, for the decider to read.
     */
    reason: string;
}

/**
 * @brief What the call did: raised a request, published a surface, or refused and why.
 */
export interface PublishFxVolSurfaceResponse {
    success: boolean;
    message: string;
    approval_request_id: string;
    state_code: string;
    published_at: string | null;
    published_by: string;
}

/**
 * @brief Reads the subject of a surface publish request: the candidate it authorises, the checks the raise recorded, and what has happened to it since.
 */
export interface GetFxVolSurfacePublishSubjectRequest {
    approval_request_id: string;
}

/**
 * @brief The subject of a surface publish request, or why it cannot be read.
 */
export interface GetFxVolSurfacePublishSubjectResponse {
    success: boolean;
    message: string;
    approval_request_id: string;
    state_code: string;
    blueprint_name: string;
    party_id: string;
    as_of: string;
    calibration_id: string;
    calibration_version: number;
    converged: boolean;
    max_abs_error: number;
    pillars_beyond_tolerance: number;
    violation_count: number;
    published_at: string | null;
    published_by: string;
    superseded_at: string | null;
    superseded_by_request_id: string;
}

export const subjects = {
    calibrate_fx_vol_surface_request: 'marketdata.v1.ops.calibrate_fx_vol_surface',
    publish_fx_vol_surface_request: 'marketdata.v1.ops.publish_fx_vol_surface',
    get_fx_vol_surface_publish_subject_request:
        'marketdata.v1.ops.get_fx_vol_surface_publish_subject',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    calibrate_fx_vol_surface_request: true,
    publish_fx_vol_surface_request: true,
    get_fx_vol_surface_publish_subject_request: true,
} as const;
