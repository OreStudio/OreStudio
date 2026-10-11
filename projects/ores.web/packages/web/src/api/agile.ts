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

import {
    agileDocResponseSchema,
    backlogSchema,
    chartsResponseSchema,
    boardResponseSchema,
    sprintListSchema,
    type Backlog,
    type Board,
    type Charts,
    type QaDoc,
} from '@ores/contracts';
import { request } from './transport.js';

/** The sprints of the agile tree, as the BFF serves them. */
export interface SprintRef {
    readonly version: string;
    readonly name: string;
    readonly title: string;
    readonly path: string;
}

/**
 * The agile browser's reads. The shapes are the BFF's own contract, so the
 * browser parses what the server serialised with the schema it was built from.
 */
export const agile = {
    /** The sprints, newest first. */
    async sprints(): Promise<readonly SprintRef[]> {
        return sprintListSchema.parse(await request('/api/agile/sprints', { method: 'GET' }))
            .sprints;
    },

    /** One sprint's board. With no name the newest sprint answers. */
    async board(sprint?: string): Promise<Board> {
        const query = sprint === undefined ? '' : `?sprint=${encodeURIComponent(sprint)}`;
        return boardResponseSchema.parse(
            await request(`/api/agile/board${query}`, { method: 'GET' }),
        ).board;
    },

    /** One sprint's chart series, and the velocity of every sprint. */
    async charts(sprint?: string): Promise<Charts> {
        const query = sprint === undefined ? '' : `?sprint=${encodeURIComponent(sprint)}`;
        return chartsResponseSchema.parse(
            await request(`/api/agile/charts${query}`, { method: 'GET' }),
        ).charts;
    },

    /** The four backlog buckets, each newest first. */
    async backlog(): Promise<Backlog> {
        return backlogSchema.parse(await request('/api/agile/backlog', { method: 'GET' }));
    },

    /** A sprint, story, task, capture or scenario by its :ID:, as org text. */
    async doc(id: string): Promise<QaDoc> {
        return agileDocResponseSchema.parse(
            await request(`/api/agile/docs/${encodeURIComponent(id)}`, { method: 'GET' }),
        ).doc;
    },
};
