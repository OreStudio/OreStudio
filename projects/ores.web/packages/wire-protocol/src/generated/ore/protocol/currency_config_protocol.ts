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
 * @brief One ORE document, as the browser saves it: its file name and its text.
 */
export interface OreDocument {
    name: string;
    content: string;
}

/**
 * @brief Writes the tenant's currencies as an ORE currency configuration.
 *
 * An empty list of codes exports every currency; a list exports those alone.
 */
export interface ExportCurrencyConfigRequest {
    iso_codes: string[];
}

/**
 * @brief The document, one file, and how many currencies it holds.
 */
export interface ExportCurrencyConfigResponse {
    success: boolean;
    message: string;
    count: number;
    files: OreDocument[];
}

/**
 * @brief One currency the import would touch: added, changed or removed, and
 * in words what differs.
 */
export interface CurrencyImportRow {
    iso_code: string;
    name: string;
    /** One of added, changed, removed. */
    change: string;
    what: string;
}

/**
 * @brief Something in the file the system does not know: a value with no row,
 * an element with no column, or a currency the file names twice.
 */
export interface CurrencyImportIssue {
    /** One of value, dropped, duplicate. */
    kind: string;
    subject: string;
    detail: string;
}

/**
 * @brief Parses a currency configuration and says what importing it would do.
 *
 * Nothing is written. A file of another kind is refused, and the refusal
 * names the root element found.
 */
export interface PreviewCurrencyConfigImportRequest {
    content: string;
}

/**
 * @brief The counts, the difference against what the tenant holds, and what
 * the system does not know.
 */
export interface PreviewCurrencyConfigImportResponse {
    success: boolean;
    message: string;
    root_element: string;
    in_file: number;
    /** Currencies the tenant holds with the same values. */
    held: number;
    changed: number;
    added: number;
    /** Currencies the tenant holds and the file omits. */
    removed: number;
    rows: CurrencyImportRow[];
    issues: CurrencyImportIssue[];
}

/**
 * @brief What the person chose for one currency of the difference.
 */
export interface CurrencyImportChoice {
    iso_code: string;
    /** One of take, keep, delete, skip. Take writes the file's row; keep leaves ours; delete removes a row the file omits; skip leaves a new row out. */
    action: string;
}

/**
 * @brief Writes a currency configuration with the person's choices.
 *
 * A currency the choices do not name takes the file's row when it is new or
 * changed, and is kept when the file omits it.
 */
export interface ImportCurrencyConfigRequest {
    content: string;
    choices: CurrencyImportChoice[];
    reason_code: string;
    commentary: string;
}

/**
 * @brief A currency the import could not write, and the owner's reason.
 */
export interface CurrencyImportError {
    iso_code: string;
    message: string;
}

/**
 * @brief What the import wrote, kept and could not write.
 */
export interface ImportCurrencyConfigResponse {
    success: boolean;
    message: string;
    added: number;
    changed: number;
    deleted: number;
    kept: number;
    errors: CurrencyImportError[];
}

export const subjects = {
    export_currency_config_request: 'ore.v1.ops.export_currency_config',
    preview_currency_config_import_request: 'ore.v1.ops.preview_currency_config_import',
    import_currency_config_request: 'ore.v1.ops.import_currency_config',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    export_currency_config_request: true,
    preview_currency_config_import_request: true,
    import_currency_config_request: true,
} as const;
