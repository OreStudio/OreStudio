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
import type { CounterpartyChange } from '../domain/counterparty_change.js';
import type { CounterpartyIdentifierChange } from '../domain/counterparty_identifier_change.js';
import type { PartyCounterpartyChange } from '../domain/party_counterparty_change.js';
import type { Result } from '../../../utility/protocol.js';

/**
 * @brief What the real write said about one proposed line.
 *
 * The refusal is empty when the write would stand. The columns are the ones the
 * line changes, and the parts are the ones the policy names for the line alone.
 */
export interface CounterpartyLineOutcome {
    line_no: number;
    /**
     * @brief counterparty, counterparty_identifier or party_counterparty.
     */
    entity_type: string;
    operation: string;
    /**
     * @brief The row the line is about: its id, or the party and the counterparty of a link.
     */
    subject_id: string;
    columns: string[];
    refusal: string;
    part_codes: string[];
}

/**
 * @brief Runs each proposed line through the real write and writes nothing.
 *
 * A put line needs the permission to write what it names, a delete line the
 * permission to delete it. The lines are checked in order inside one transaction
 * that is never committed.
 */
export interface PreviewCounterpartyChangesRequest {
    /**
     * @brief The proposed writes to counterparties. The request, line number and part of a line are not read: the server numbers the lines and the policy names the parts.
     */
    counterparties: CounterpartyChange[];
    identifiers: CounterpartyIdentifierChange[];
    links: PartyCounterpartyChange[];
}

export interface PreviewCounterpartyChangesResponse {
    result: Result;
    lines: CounterpartyLineOutcome[];
    /**
     * @brief The parts the policy needs for the lines together. Empty when no policy row gates them.
     */
    part_codes: string[];
}

/**
 * @brief Holds the proposed lines in an approval request.
 *
 * Previews first. A refused line, or lines no policy row gates, raise nothing.
 */
export interface RaiseCounterpartyChangesRequest {
    /**
     * @brief Why the person asks, in their words.
     */
    reason: string;
    counterparties: CounterpartyChange[];
    identifiers: CounterpartyIdentifierChange[];
    links: PartyCounterpartyChange[];
}

export interface RaiseCounterpartyChangesResponse {
    result: Result;
    /**
     * @brief The id of the request as raised, when the outcome is ok. The inbox operations read the request itself.
     */
    request_id: string | null;
    lines: CounterpartyLineOutcome[];
    part_codes: string[];
}

/**
 * @brief One column of a proposed line beside the live row's value.
 *
 * The values are text whatever the column's type, because a screen draws them and
 * does not compute with them. The before is empty when the line creates the row.
 */
export interface CounterpartyFieldDelta {
    name: string;
    before: string;
    after: string;
    changed: boolean;
}

/**
 * @brief One line of a request as the decider reads it: the kind of row, the
 * operation, the row it names, and each column beside the value it replaces.
 */
export interface CounterpartyChangeDelta {
    line_no: number;
    entity_type: string;
    operation: string;
    subject_id: string;
    base_version: number;
    applied: boolean;
    fields: CounterpartyFieldDelta[];
}

/**
 * @brief Reads the lines a request holds, each beside the row it changes.
 *
 * Answered to the person who raised the request, to whoever may decide its kind,
 * and to whoever may read the tenant's requests. Answered as not found otherwise,
 * as opening the request is.
 */
export interface GetCounterpartyChangesRequest {
    request_id: string;
}

export interface GetCounterpartyChangesResponse {
    result: Result;
    /**
     * @brief The lines in line order.
     */
    changes: CounterpartyChangeDelta[];
}

/**
 * @brief Runs the lines a request holds through the real write again and writes
 * nothing.
 *
 * The live rows may have moved since the request was raised, so the checks a
 * decider reads are run now. Answered to the same callers as the lines are.
 */
export interface RecheckCounterpartyChangesRequest {
    request_id: string;
}

export interface RecheckCounterpartyChangesResponse {
    result: Result;
    lines: CounterpartyLineOutcome[];
    part_codes: string[];
}

export const subjects = {
    preview_counterparty_changes_request: 'refdata.v1.ops.preview_counterparty_changes',
    raise_counterparty_changes_request: 'refdata.v1.ops.raise_counterparty_changes',
    get_counterparty_changes_request: 'refdata.v1.ops.get_counterparty_changes',
    recheck_counterparty_changes_request: 'refdata.v1.ops.recheck_counterparty_changes',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    preview_counterparty_changes_request: true,
    raise_counterparty_changes_request: true,
    get_counterparty_changes_request: true,
    recheck_counterparty_changes_request: true,
} as const;
