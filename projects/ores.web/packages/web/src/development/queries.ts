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
 *
 */

import { queryOptions } from '@tanstack/react-query';
import { agile } from '../api/agile.js';
import { qa } from '../api/qa.js';

/**
 * The reads of the QA validation runner, with their keys.
 *
 * Every one of them is quiet: its screen states the failure itself, in a notice
 * where the answer would have been, so the app's banner for a request that did
 * not finish would only repeat it. A link to a doc the runner does not show is
 * an expected answer, not a fault.
 */
export const QA_SCENARIOS_KEY = ['qa-scenarios'] as const;

export function docKey(id: string): readonly [string, string] {
    return ['qa-doc', id];
}

export function scenarioKey(id: string): readonly [string, string] {
    return ['qa-scenario', id];
}

const QUIET = { quiet: true } as const;

export const qaQueries = {
    scenarios: () =>
        queryOptions({
            queryKey: QA_SCENARIOS_KEY,
            queryFn: () => qa.scenarios(),
            meta: QUIET,
        }),

    /**
     * The runner works on a snapshot taken when the page opens, so the query is
     * never kept for a later visit and never refetched behind the tester's back.
     * A save replaces the snapshot with the server's answer.
     */
    scenario: (id: string) =>
        queryOptions({
            queryKey: scenarioKey(id),
            queryFn: () => qa.scenario(id),
            meta: QUIET,
            gcTime: 0,
            refetchOnWindowFocus: false,
            refetchOnReconnect: false,
        }),

    doc: (id: string) =>
        queryOptions({
            queryKey: docKey(id),
            queryFn: () => qa.doc(id),
            meta: QUIET,
        }),
};

export function boardKey(sprint: string | undefined): readonly [string, string] {
    return ['agile-board', sprint ?? ''];
}

export const AGILE_SPRINTS_KEY = ['agile-sprints'] as const;

/** The reads of the agile browser. Quiet for the same reason as the QA reads. */
export const agileQueries = {
    sprints: () =>
        queryOptions({
            queryKey: AGILE_SPRINTS_KEY,
            queryFn: () => agile.sprints(),
            meta: QUIET,
        }),

    board: (sprint: string | undefined) =>
        queryOptions({
            queryKey: boardKey(sprint),
            queryFn: () => agile.board(sprint),
            meta: QUIET,
        }),
};
