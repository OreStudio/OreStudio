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

import { z } from 'zod';
import { request } from './transport.js';

/**
 * The identity shapes: the fields each market series family requires and
 * forbids. Rows are the market data service's, under its own field names.
 */

const JSON_HEADERS = { 'Content-Type': 'application/json' } as const;

export const REQUIREMENTS = ['required', 'forbidden'] as const;

export type Requirement = (typeof REQUIREMENTS)[number];

/** The four parts that name a row. The index family is empty for a family that is not a fixing. */
export interface ShapeKey {
    readonly asset_class: string;
    readonly series_subclass: string;
    readonly index_family: string;
    readonly field: string;
}

export interface ShapeWrite extends ShapeKey {
    readonly requirement: Requirement;
    readonly description: string;
}

export interface Shape extends ShapeWrite {
    readonly version: number;
    /** Whether the row is the shared catalogue's, which every tenant reads. */
    readonly system: boolean;
    readonly modified_by: string;
    readonly recorded_at: string;
    readonly change_reason_code: string;
    readonly change_commentary: string;
}

export interface ShapeIntent {
    readonly reason_code: string;
    readonly commentary: string;
}

export interface ShapeResult {
    readonly outcome: string;
    readonly message: string;
}

const shapeSchema: z.ZodType<Shape> = z.object({
    asset_class: z.string(),
    series_subclass: z.string(),
    index_family: z.string(),
    field: z.string(),
    requirement: z.enum(REQUIREMENTS),
    description: z.string(),
    version: z.int(),
    system: z.boolean(),
    modified_by: z.string().default(''),
    recorded_at: z.string().default(''),
    change_reason_code: z.string().default(''),
    change_commentary: z.string().default(''),
});

const resultSchema: z.ZodType<ShapeResult> = z.object({
    outcome: z.string(),
    message: z.string().default(''),
});

const PATH = '/api/marketdata/series-identity-shapes';

/** The family a row belongs to, as one text: class, subclass and, for a fixing, index family. */
export function familyOf(key: ShapeKey): string {
    return [key.asset_class, key.series_subclass, key.index_family]
        .filter((part) => part !== '')
        .join(' / ');
}

export const identityShapes = {
    /** Every shape the session reads: the shared catalogue's and the tenant's own. */
    async list(): Promise<readonly Shape[]> {
        return z
            .object({ shapes: z.array(shapeSchema) })
            .parse(await request(PATH, { method: 'GET' })).shapes;
    },

    /** The versions of one row, newest first. */
    async versions(key: ShapeKey): Promise<readonly Shape[]> {
        const query = new URLSearchParams({
            asset_class: key.asset_class,
            series_subclass: key.series_subclass,
            index_family: key.index_family,
            field: key.field,
        });
        return z
            .object({ versions: z.array(shapeSchema) })
            .parse(await request(`${PATH}/versions?${query.toString()}`, { method: 'GET' }))
            .versions;
    },

    /** Writes a row: a new one when no version is given, else a correction of the version read. */
    async put(
        write: ShapeWrite,
        version: number | null,
        intent: ShapeIntent,
    ): Promise<ShapeResult> {
        return z.object({ result: resultSchema }).parse(
            await request(PATH, {
                method: 'PUT',
                headers: JSON_HEADERS,
                body: JSON.stringify({ write, version, intent }),
            }),
        ).result;
    },

    /** Removes a row, against the version read. */
    async remove(key: ShapeKey, version: number, intent: ShapeIntent): Promise<ShapeResult> {
        return z.object({ result: resultSchema }).parse(
            await request(PATH, {
                method: 'DELETE',
                headers: JSON_HEADERS,
                body: JSON.stringify({ key, version, intent }),
            }),
        ).result;
    },
};
