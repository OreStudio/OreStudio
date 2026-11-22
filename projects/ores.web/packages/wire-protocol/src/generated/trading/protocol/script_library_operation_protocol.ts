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
import type { Script } from '../domain/script.js';
import type { ScriptCalibration } from '../domain/script_calibration.js';
import type { ScriptCalibrationStrike } from '../domain/script_calibration_strike.js';
import type { ScriptCoarsening } from '../domain/script_coarsening.js';
import type { ScriptResult } from '../domain/script_result.js';
import type { ScriptVariant } from '../domain/script_variant.js';

/**
 * @brief One version of a library script, as a scripted trade records it.
 */
export interface ScriptPin {
    /**
     * @brief The tenant that holds the script: the trade's own, or the system tenant for the world library.
     */
    tenant_id: string;
    /**
     * @brief The script's name.
     */
    name: string;
    /**
     * @brief The version of the script.
     */
    version: number;
}

/**
 * @brief Reads the rows of stated versions of library scripts.
 */
export interface ExportScriptLibraryRequest {
    /**
     * @brief The versions to read.
     */
    pins: ScriptPin[];
}

/**
 * @brief The rows of the stated versions, and the versions the store does not hold.
 */
export interface ExportScriptLibraryResponse {
    /**
     * @brief Whether the read ran.
     */
    success: boolean;
    /**
     * @brief Why the read failed, when it did.
     */
    message: string;
    /**
     * @brief The script row of each version held.
     */
    scripts: Script[];
    /**
     * @brief The variants of each version held.
     */
    variants: ScriptVariant[];
    /**
     * @brief The results of each version held.
     */
    results: ScriptResult[];
    /**
     * @brief The calibrations of each version held.
     */
    calibrations: ScriptCalibration[];
    /**
     * @brief The calibration strikes of each version held.
     */
    calibration_strikes: ScriptCalibrationStrike[];
    /**
     * @brief The schedule coarsenings of each version held.
     */
    coarsenings: ScriptCoarsening[];
    /**
     * @brief The stated versions the store does not hold, in the order stated.
     */
    missing: ScriptPin[];
}

export const subjects = {
    export_script_library_request: 'trading.v1.ops.export_script_library',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    export_script_library_request: true,
} as const;
