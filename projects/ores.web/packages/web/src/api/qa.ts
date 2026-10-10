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
    scenarioListSchema,
    scenarioResponseSchema,
    type Scenario,
    type ScenarioSummary,
} from '@ores/contracts';
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
};
