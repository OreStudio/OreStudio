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
import type { Result } from '../../../utility/protocol.js';

/**
 * @brief The product types one book permits, as the chain resolves them.
 *
 * The set is absent when the book, its desk and its party all restrict nothing,
 * because no level then names a type the catalogue does not: every type is
 * permitted. The set is empty when a level restricts and names nothing, which
 * permits none. A caller that folded the two together would read an open book
 * and a shut one alike.
 */
export interface BookProductApproval {
    book_id: string;
    allowed_product_types: string[] | null;
}

/**
 * @brief Reads the resolved set of each book the request names.
 *
 * A book of another tenant is a row the caller cannot see through RLS, and the
 * resolution answers an empty set for it, so a caller reads it as permitting
 * nothing rather than permitting everything.
 */
export interface GetBookProductApprovalsRequest {
    /**
     * @brief The books to resolve, in the order the answer states them.
     */
    book_ids: string[];
}

export interface GetBookProductApprovalsResponse {
    result: Result;
    /**
     * @brief One entry per book named, in the order the request named them.
     *
     * A book that is not the tenant's resolves to the empty set, which is the
     * resolution's answer for a row the caller cannot see.
     */
    approvals: BookProductApproval[];
}

export const subjects = {
    get_book_product_approvals_request: 'refdata.v1.ops.get_book_product_approvals',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    get_book_product_approvals_request: true,
} as const;
