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
 * The TypeScript twins of the records in
 * `ores::utility::domain` that every common entity message shares.
 *
 * The C++ types are hand-written utility structs, not codegen models, so no
 * `ores.ts.domain` facet emits them. The generated protocol modules that carry
 * one import these interfaces instead. Keep the members in step with
 * `projects/ores.utility/include/ores.utility/domain/protocol.hpp`.
 */

/**
 * How a request ended. Closed on purpose: a caller branches on these seven and
 * on nothing else. A failure that stops an operation being reached at all is
 * reported by the envelope instead, because there is no body to carry it.
 */
export type Outcome =
    | 'ok'
    | 'invalid'
    | 'denied'
    | 'missing'
    | 'conflict'
    | 'unavailable'
    | 'failed';

/** One field a request got wrong, named so a caller can act on it. */
export interface FieldFailure {
    field: string;
    code: string;
    message: string;
}

/**
 * The result every response carries. A payload's data is meaningful only when
 * `outcome` is `ok`; on any other outcome there is no data, and a caller that
 * reads data without checking the outcome is wrong.
 */
export interface Result {
    outcome: Outcome;
    code: string;
    message: string;
    fields: FieldFailure[];
}

/**
 * What a write believes about the row it is about to change. Three distinct
 * claims, so that no integer has to double as a mode.
 */
export type PreconditionKind = 'any' | 'must_not_exist' | 'must_match_version';

export interface Precondition {
    kind: PreconditionKind;
    version: number | null;
}

/**
 * Why a write is being made. User-owned, unlike the audit provenance, which
 * the service derives from the authenticated context.
 */
export interface ChangeIntent {
    reason_code: string;
    commentary: string;
}

/**
 * The order a page is returned in. An empty `field` means the order by key,
 * which is what makes a caller that names no order still get a stable page.
 */
export interface Order {
    field: string;
    descending: boolean;
}

/**
 * How much of a tree a scoped read covers. One verb with two answers rather
 * than two operations: reading a node's children and reading everything beneath
 * it differ only in reach.
 */
export type Scope = 'direct' | 'subtree';
