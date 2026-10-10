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
import type { BookChange } from '../domain/book_change.js';
import type { Result } from '../../../utility/protocol.js';

/**
 * @brief What the real write said about one proposed line.
 *
 * The refusal is empty when the write would stand. The columns are the ones
 * the line changes, which is what the policy reads to name the parts.
 */
export interface BookLineOutcome {
    line_no: number;
    operation: string;
    entity_id: string;
    columns: string[];
    refusal: string;
}

/**
 * @brief Runs each proposed line through the real write and writes nothing.
 *
 * A put line needs the permission to write books, a delete line the permission
 * to delete them. The lines are checked in order inside one transaction that is
 * never committed.
 */
export interface PreviewBookChangesRequest {
    /**
     * @brief The proposed writes. The request, line number and part of a line are
     * not read: the server numbers the lines and the policy names the parts.
     */
    lines: BookChange[];
}

export interface PreviewBookChangesResponse {
    result: Result;
    lines: BookLineOutcome[];
    /**
     * @brief The parts the policy needs for the lines together. Empty when no
     * policy row gates them.
     */
    part_codes: string[];
}

/**
 * @brief Holds the proposed lines in an approval request.
 *
 * Previews first. A refused line, or lines no policy row gates, raise nothing.
 */
export interface RaiseBookChangesRequest {
    /**
     * @brief Why the person asks, in their words.
     */
    reason: string;
    lines: BookChange[];
}

export interface RaiseBookChangesResponse {
    result: Result;
    /**
     * @brief The id of the request as raised, when the outcome is ok. The inbox
     * operations read the request itself.
     */
    request_id: string | null;
    lines: BookLineOutcome[];
    part_codes: string[];
}

export const subjects = {
    preview_book_changes_request: 'refdata.v1.ops.preview_book_changes',
    raise_book_changes_request: 'refdata.v1.ops.raise_book_changes',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    preview_book_changes_request: true,
    raise_book_changes_request: true,
} as const;
