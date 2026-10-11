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
    curveEvolutionSchema,
    curveSeriesListSchema,
    curveSnapshotSchema,
    type CurveEvolution,
    type CurveIdentity,
    type CurveSeriesList,
    type CurveSnapshot,
} from '@ores/contracts';
import { request } from './transport.js';

/** The two ends of a range, as wire timestamps. */
export interface CurveRange {
    readonly from: string;
    readonly to: string;
}

function post(path: string, body: unknown): Promise<unknown> {
    return request(path, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(body),
    });
}

/**
 * The curve reads: an object over a range, and the object now.
 *
 * Both name the object by its typed identity. A read the server refuses comes
 * back as a failure that carries the server's reason.
 */
export const curves = {
    /** The series the session's party can see, each as the identity that names it. */
    async seriesList(): Promise<CurveSeriesList> {
        return curveSeriesListSchema.parse(await post('/api/curves/series-list', {}));
    },

    /** The aligned matrix of instants by nodes over the range. */
    async evolution(identity: CurveIdentity, range: CurveRange): Promise<CurveEvolution> {
        return curveEvolutionSchema.parse(
            await post('/api/curves/evolution', {
                identity,
                from_instant: range.from,
                to_instant: range.to,
            }),
        );
    },

    /** The object as it stands now, with where each value came from when asked. */
    async snapshot(identity: CurveIdentity, provenance: boolean): Promise<CurveSnapshot> {
        return curveSnapshotSchema.parse(
            await post('/api/curves/snapshot', { identity, include_provenance: provenance }),
        );
    },
};
