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
 * @brief Error for a single item within an ORE import.
 *
 * Carries the exact source file and item identifier so the caller can present
 * a precise failure location, such as "trades.xml / trade-0042".
 */
export interface OreImportItemError {
    source_file: string;
    item_id: string;
    message: string;
}

/**
 * @brief Request to import an ORE directory already uploaded to storage.
 *
 * The caller uploads the packed directory to the ore-imports bucket as
 * {request_id}.tar.gz before sending this message.
 */
export interface OreImportRequest {
    /** UUID; also the storage key root. */
    request_id: string;
    import_choices_json: string;
    correlation_id: string;
}

/**
 * @brief Response for an ore_import_request.
 *
 * On partial failure success is still true and item_errors is non-empty; a
 * false success means the saga itself failed and compensation ran.
 */
export interface OreImportResponse {
    success: boolean;
    message: string;
    item_errors: OreImportItemError[];
    correlation_id: string;
    /**
     * Set when the import was dispatched asynchronously through the workflow
     * engine. The import then runs in the background, item_errors is empty in
     * this response, and this id queries the workflow's status.
     */
    workflow_instance_id: string;
}

export const subjects = {
    ore_import_request: "workflow.v1.ore.import",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    ore_import_request: true,
} as const;
