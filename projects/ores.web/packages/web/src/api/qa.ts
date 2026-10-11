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
    imageUploadResponseSchema,
    qaDocResponseSchema,
    runResponseSchema,
    scenarioListSchema,
    scenarioResponseSchema,
    type QaDoc,
    type RunRequest,
    type RunResponse,
    type Scenario,
    type ScenarioSummary,
} from '@ores/contracts';
import { base64Of } from './base64.js';
import { request } from './transport.js';

/**
 * The QA validation runner's reads.
 *
 * The shapes are the BFF's own contract, so the browser parses what the server
 * serialised with the schema the server serialised it from.
 */
export const qa = {
    /** Every scenario under the doc root, waiting and done. */
    async scenarios(): Promise<readonly ScenarioSummary[]> {
        return scenarioListSchema.parse(await request('/api/qa/scenarios', { method: 'GET' }))
            .scenarios;
    },

    /** One scenario with its steps. */
    async scenario(id: string): Promise<Scenario> {
        return scenarioResponseSchema.parse(
            await request(`/api/qa/scenarios/${encodeURIComponent(id)}`, { method: 'GET' }),
        ).scenario;
    },

    /** A story, a task or a scenario by its :ID:, as org text. */
    async doc(id: string): Promise<QaDoc> {
        return qaDocResponseSchema.parse(
            await request(`/api/qa/docs/${encodeURIComponent(id)}`, { method: 'GET' }),
        ).doc;
    },

    /**
     * Keep a screenshot of step number `step`, counted from one. It answers the
     * name the server gave the file, which the step's note links to.
     */
    async uploadImage(id: string, step: number, file: Blob): Promise<string> {
        const bytes = new Uint8Array(await file.arrayBuffer());
        return imageUploadResponseSchema.parse(
            await request(`/api/qa/scenarios/${encodeURIComponent(id)}/steps/${step}/images`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ mediaType: file.type, data: base64Of(bytes) }),
            }),
        ).name;
    },

    /** Save a run into a scenario. The server says whether the save closed it. */
    async record(id: string, run: RunRequest): Promise<RunResponse> {
        return runResponseSchema.parse(
            await request(`/api/qa/scenarios/${encodeURIComponent(id)}/run`, {
                method: 'PUT',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(run),
            }),
        );
    },
};
