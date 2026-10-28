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
    storeUuidSchema,
    type CurveEvolution,
    type CurveIdentity,
    type CurveSeriesList,
    type CurveSnapshot,
} from '@ores/contracts';
import { z } from 'zod';
import { request } from './transport.js';

/** The two ends of a range, as wire timestamps. */
export interface CurveRange {
    readonly from: string;
    readonly to: string;
}

/** A write answers with nothing; the server's refusal arrives as a failure. */
const emptyReply = z.object({});

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
/**
 * A series named by its id: the oresmd URI it is published under.
 *
 * The id is the one the store wrote, which does not always carry the RFC
 * version nibbles, so it is parsed with the shape the contracts own rather
 * than with `z.uuid()`.
 */
const seriesUriSchema = z.object({
    series: z.array(z.object({ id: storeUuidSchema, uri: z.string() })),
});

/** One point of a built curve: the pillar it answers, its date and its factor. */
export interface ComputedCurvePoint {
    readonly pointId: string;
    readonly date: string;
    readonly discountFactor: number;
}

const computedSchema = z.object({
    points: z.array(
        z.object({ point_id: z.string(), date: z.string(), discount_factor: z.number() }),
    ),
});

export const curves = {
    /** The series the session's party can see, each as the identity that names it. */
    async seriesList(): Promise<CurveSeriesList> {
        return curveSeriesListSchema.parse(await post('/api/curves/series-list', {}));
    },

    /**
     * Builds one blueprint's curve at an instant and answers its points.
     *
     * The test writes nothing, so a re-run replaces the answer a person reads
     * and leaves the ledger as it was.
     */
    async compute(bootstrapConfigId: string, asOf: string): Promise<readonly ComputedCurvePoint[]> {
        const reply = computedSchema.parse(
            await post('/api/curves/compute', {
                bootstrap_config_id: bootstrapConfigId,
                as_of: asOf,
            }),
        );
        return reply.points.map((point) => ({
            pointId: point.point_id,
            date: point.date,
            discountFactor: point.discount_factor,
        }));
    },

    /**
     * The URI of each series a set of ids names, for the rows of a screen that
     * holds the series by reference.
     */
    async seriesUris(ids: readonly string[]): Promise<ReadonlyMap<string, string>> {
        const reply = seriesUriSchema.parse(await post('/api/curves/series-by-id', { ids }));
        return new Map(reply.series.map((row) => [row.id, row.uri]));
    },

    /**
     * Every series the session's party holds, by id, which is the list a person
     * chooses a blueprint's output series from.
     */
    async heldSeries(): Promise<ReadonlyMap<string, string>> {
        const reply = seriesUriSchema.parse(await post('/api/curves/series-by-id', { ids: [] }));
        return new Map(reply.series.map((row) => [row.id, row.uri]));
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

    /**
     * Keys a manual point on one node. The coordinates are in the order the shape declares its
     * axes, which is the order the reads return them in. The point owns the node until it is
     * cleared.
     */
    async keyPoint(
        identity: CurveIdentity,
        coordinates: readonly string[],
        value: string,
        reasonCode: string,
        commentary: string,
    ): Promise<void> {
        emptyReply.parse(
            await post('/api/curves/manual-point', {
                identity,
                coordinates,
                value,
                change_reason_code: reasonCode,
                change_commentary: commentary,
            }),
        );
    },

    /** Clears the manual point on one node, so the feed value shows again. */
    async clearPoint(identity: CurveIdentity, coordinates: readonly string[]): Promise<void> {
        emptyReply.parse(await post('/api/curves/manual-point/clear', { identity, coordinates }));
    },

    /** The object as it stands now, with where each value came from when asked. */
    async snapshot(identity: CurveIdentity, provenance: boolean): Promise<CurveSnapshot> {
        return curveSnapshotSchema.parse(
            await post('/api/curves/snapshot', { identity, include_provenance: provenance }),
        );
    },
};
