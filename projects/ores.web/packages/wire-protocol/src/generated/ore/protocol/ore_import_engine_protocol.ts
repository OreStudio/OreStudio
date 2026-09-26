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
 * @brief Workflow step command: execute the full ORE import.
 *
 * Published by the workflow engine when the ore_import_workflow starts. The
 * handler fetches the packed tarball, scans, plans and saves every item, then
 * calls publish_step_completion.
 */
export interface OreImportExecuteRequest {
    request_id: string;
    import_choices_json: string;
    correlation_id: string;
    /** The caller's JWT, so the handler can delegate the caller's identity downstream. */
    bearer_token: string;
}

/**
 * @brief The step's stored response.
 *
 * Carries everything compensation needs -- the identifiers of every entity
 * the step saved -- alongside the caller-facing item_errors list.
 */
export interface OreImportExecuteResult {
    success: boolean;
    message: string;
    correlation_id: string;
    item_errors: OreImportItemError[];
    saved_currency_iso_codes: string[];
    saved_portfolio_ids: string[];
    saved_book_ids: string[];
    saved_trade_ids: string[];
}

/**
 * @brief Workflow compensation command: roll back a completed ORE import.
 *
 * Published by the workflow engine when step 0's compensation is triggered.
 * The handler deletes every saved entity in reverse order and calls
 * publish_step_completion.
 */
export interface OreImportRollbackRequest {
    correlation_id: string;
    /** The caller's JWT, so the handler can delegate the caller's identity downstream. */
    bearer_token: string;
    saved_currency_iso_codes: string[];
    saved_portfolio_ids: string[];
    saved_book_ids: string[];
    saved_trade_ids: string[];
}

export const subjects = {
    ore_import_execute_request: "ore.v1.ore.import.execute",
    ore_import_rollback_request: "ore.v1.ore.import.rollback",
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    ore_import_execute_request: true,
    ore_import_rollback_request: true,
} as const;
