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
import type { Trade } from '../domain/trade.js';
import type { TradeBooking } from '../domain/trade_booking.js';
import type { TradeIdentifier } from '../domain/trade_identifier.js';
import type { Result } from '../../../utility/protocol.js';

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

/**
 * @brief One result a variant of a published script reports, in the order the script states it.
 */
export interface PublishedResult {
    /**
     * @brief The variable whose value the engine reports.
     */
    variable: string;
    /**
     * @brief The name the engine reports it under, when it differs.
     */
    rename: string | null;
}

/**
 * @brief One calibration of a variant of a published script.
 */
export interface PublishedCalibration {
    /**
     * @brief The variable naming the index the engine calibrates.
     */
    index_variable: string;
    /**
     * @brief The variables naming the strikes the engine calibrates at, in order.
     */
    strikes: string[];
}

/**
 * @brief One variant of a published script: its code and what the engine reads beside it.
 */
export interface PublishedVariant {
    /**
     * @brief What the variant is for: default for the variant a trade prices with, or the purpose ORE names, such as AMC.
     */
    purpose: string;
    /**
     * @brief The variant's text in ORE's payoff language.
     */
    code: string;
    /**
     * @brief The variable whose value is the trade's NPV.
     */
    npv_variable: string;
    /**
     * @brief The results the variant reports.
     */
    results: PublishedResult[];
    /**
     * @brief The calibrations the variant asks for.
     */
    calibrations: PublishedCalibration[];
    /**
     * @brief The schedules the engine may coarsen, in order.
     */
    coarsenings: string[];
}

/**
 * @brief One finding of the parse of a published script, located in the variant it concerns.
 */
export interface ScriptDiagnostic {
    /**
     * @brief The variant the finding is in.
     */
    purpose: string;
    /**
     * @brief error or warning; an error refuses the publish.
     */
    severity: string;
    /**
     * @brief The diagnostic's code, such as SCR1001.
     */
    code: string;
    /**
     * @brief The one-based line of the finding's start, or 0 where its position is unknown.
     */
    line: number;
    /**
     * @brief The one-based column of the finding's start, where known.
     */
    column: number | null;
    /**
     * @brief What the finding says.
     */
    message: string;
    /**
     * @brief The variable a finding about the trade's variables is about; empty for a finding located in the text.
     */
    subject: string;
}

/**
 * @brief One variable a trade states for its script.
 */
export interface ScriptVariableValue {
    /**
     * @brief The variable's name, as the script reads it.
     */
    name: string;
    /**
     * @brief The type ORE reads it as: Number, Event, Index, Currency or Daycounter.
     */
    type: string;
    /**
     * @brief Whether the variable is a list.
     */
    is_array: boolean;
    /**
     * @brief Its value, or its values when it is a list, as text.
     */
    values: string[];
}

/**
 * @brief Validates a script against the variables a trade states, booking nothing.
 */
export interface ValidateScriptRequest {
    /**
     * @brief The library script to validate, as the caller's tenant resolves the name; empty to validate code instead.
     */
    script_name: string;
    /**
     * @brief The version of the library script; 0 for its current version. A negative version names no script.
     */
    script_version: number;
    /**
     * @brief An inline script's code, read when script_name is empty.
     */
    code: string;
    /**
     * @brief The variables the trade states.
     */
    variables: ScriptVariableValue[];
}

/**
 * @brief The findings of a script's validation.
 */
export interface ValidateScriptResponse {
    /**
     * @brief Whether the validation ran. It does not run for a script that is not held, or that has no variant to parse; the message then says which, and is_valid is false.
     */
    success: boolean;
    /**
     * @brief Why it did not, when it did not, or a summary of the findings.
     */
    message: string;
    /**
     * @brief Whether no finding is an error, so a booking on these variables would pass.
     */
    is_valid: boolean;
    /**
     * @brief Every finding: those in the text by line and column, then those about a variable by its name.
     */
    diagnostics: ScriptDiagnostic[];
}

/**
 * @brief Books a scripted trade: a library script by name and version, or an inline script, on the variables the trade states, refused with the script's findings when they hold an error.
 */
export interface BookScriptedRequest {
    /**
     * @brief The trade's anchor; its trade type is ScriptedTrade.
     */
    anchor: Trade;
    /**
     * @brief The trade's booking, as book_trade takes it.
     */
    booking: TradeBooking;
    /**
     * @brief The activity that books the trade, as book_trade takes it.
     */
    activity_type_code: string;
    /**
     * @brief Why a draft is not live, as book_trade takes it.
     */
    draft_reason_code: string;
    /**
     * @brief The trade's identifiers, as book_trade takes them.
     */
    identifiers: TradeIdentifier[];
    /**
     * @brief The library script, as the caller's tenant resolves the name; for an inline script, the name its code is booked under.
     */
    script_name: string;
    /**
     * @brief The library script's version; 0 for its current version.
     */
    script_version: number;
    /**
     * @brief An inline script's code; empty to book the library script.
     */
    code: string;
    /**
     * @brief The variable an inline script's value lands in.
     */
    npv_variable: string;
    /**
     * @brief The variables the trade states for the script.
     */
    variables: ScriptVariableValue[];
    /**
     * @brief The product the trade is booked as; empty for a script-only product, which is the script.
     */
    product_code: string;
}

/**
 * @brief The outcome of a scripted booking, and the script's findings.
 */
export interface BookScriptedResponse {
    /**
     * @brief The booking's outcome, as book_trade states it; script_invalid when the script's findings hold an error.
     */
    result: Result;
    /**
     * @brief The activity that booked the trade, when it was booked.
     */
    activity_id: string | null;
    /**
     * @brief The script's findings, also when it is booked with warnings only.
     */
    diagnostics: ScriptDiagnostic[];
}

/**
 * @brief Publishes a script of the caller's tenant as its next version.
 *
 * The trading service parses each variant with ORE's grammar and derives the
 * script's inputs from the variants, so the inputs are never stated by the
 * caller. A script whose parse reports an error is refused with the
 * diagnostics, and nothing is written.
 */
export interface PublishScriptRequest {
    /**
     * @brief The script's name, which a scripted trade of the tenant names.
     */
    name: string;
    /**
     * @brief The product tag the engine prices the script with.
     */
    product_tag: string | null;
    /**
     * @brief What the script pays.
     */
    description: string | null;
    /**
     * @brief The script's variants; one must be the default.
     */
    variants: PublishedVariant[];
}

/**
 * @brief The version a publish wrote, or why it wrote none.
 */
export interface PublishScriptResponse {
    /**
     * @brief Whether the script is published.
     */
    success: boolean;
    /**
     * @brief What the publish did, or why it refused.
     */
    message: string;
    /**
     * @brief The script's version in the tenant after the publish; unchanged when the script equals its current version.
     */
    version: number;
    /**
     * @brief The parse's findings, errors and warnings, by variant.
     */
    diagnostics: ScriptDiagnostic[];
    /**
     * @brief What the caller should know that does not refuse the publish, such as a name the world library also holds.
     */
    warnings: string[];
}

export const subjects = {
    export_script_library_request: 'trading.v1.ops.export_script_library',
    validate_script_request: 'trading.v1.ops.validate_script',
    book_scripted_request: 'trading.v1.ops.book_scripted',
    publish_script_request: 'trading.v1.ops.publish_script',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    export_script_library_request: true,
    validate_script_request: true,
    book_scripted_request: true,
    publish_script_request: true,
} as const;
