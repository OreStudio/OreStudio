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

import { wireTimestampSchema } from '@ores/wire-protocol/time';
import { z } from 'zod';

/**
 * The HTTP contract of the curve reads.
 *
 * The BFF forwards the identity of a composite object to the server and returns
 * the server's answer without its success and message fields. These shapes are
 * what the browser parses, so a change on either side fails loudly on the side
 * that is stale. A value is text, as the server holds it. A node no point
 * states has an empty value. Every market instant here was a `time_point` on
 * the server, so it is an instant in the wire shape and nothing else.
 */

/** A node of the declared grid, with the change its row shows over a range. */
export const curveNodeSchema = z.object({
    coordinates: z.array(z.string()),
    status: z.string(),
});
export type CurveNode = z.infer<typeof curveNodeSchema>;

/** One market instant of an evolution: a value for every node, empty where none stands. */
export const curveInstantSchema = z.object({
    as_of: wireTimestampSchema,
    values: z.array(z.string()),
});
export type CurveInstant = z.infer<typeof curveInstantSchema>;

export const curveEvolutionSchema = z.object({
    coordinate_fields: z.array(z.string()),
    nodes: z.array(curveNodeSchema),
    instants: z.array(curveInstantSchema),
});
export type CurveEvolution = z.infer<typeof curveEvolutionSchema>;

/** Where one value of a snapshot came from. */
export const pointProvenanceSchema = z.object({
    source_kind: z.string(),
    modified_by: z.string(),
    change_reason_code: z.string(),
    change_commentary: z.string(),
    recorded_at: wireTimestampSchema,
    derivation_config_id: z.string(),
    derivation_config_version: z.number(),
    source_as_of: wireTimestampSchema,
});
export type PointProvenance = z.infer<typeof pointProvenanceSchema>;

export const curveSnapshotSchema = z.object({
    as_of: wireTimestampSchema,
    coordinate_fields: z.array(z.string()),
    nodes: z.array(curveNodeSchema),
    values: z.array(z.string()),
    recorded_at: z.array(wireTimestampSchema),
    provenance: z.array(pointProvenanceSchema),
    oldest_age_seconds: z.number(),
    spread_seconds: z.number(),
    /** Whether the values mix several market states, so a view must not draw them silently. */
    warning: z.boolean(),
});
export type CurveSnapshot = z.infer<typeof curveSnapshotSchema>;

/** The typed identity of the object, as the server resolves it. */
export const curveIdentitySchema = z.object({
    asset: z.string(),
    scope: z.string(),
    instrument_type: z.string(),
    quote_type: z.string(),
    party_id: z.string(),
    /** The provider of the series, or empty for every provider. */
    provider_code: z.string(),
    fields: z.array(z.object({ name: z.string(), text: z.string() })),
});
export type CurveIdentity = z.infer<typeof curveIdentitySchema>;

/** One series a party can see: the identity that names it, and the axes it declares. */
export const listedSeriesSchema = z.object({
    series_id: z.string(),
    identity: curveIdentitySchema,
    /** Empty when the series declares no shape, which the evolution read refuses. */
    axes: z.array(z.string()),
});
export type ListedSeries = z.infer<typeof listedSeriesSchema>;

export const curveSeriesListSchema = z.object({
    series: z.array(listedSeriesSchema),
    /** True when the party can see more series than the list holds. */
    truncated: z.boolean(),
});
export type CurveSeriesList = z.infer<typeof curveSeriesListSchema>;
