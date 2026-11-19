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

import type { FastifyInstance, FastifyRequest } from 'fastify';
import { z } from 'zod';
import { SYSTEM_TENANT_ID } from '@ores/wire-protocol';
import {
    subjects as shapeSubjects,
    type ListSeriesIdentityShapesResponse,
    type ListSeriesIdentityShapeVersionsResponse,
} from '@ores/wire-protocol/generated/marketdata/protocol/series_identity_shape_protocol';
import {
    subjects as ruleSubjects,
    type ListSeriesClassificationRulesResponse,
} from '@ores/wire-protocol/generated/marketdata/protocol/series_classification_rule_protocol';
import { input, intentSchema, readAll, resultSchema, text } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/*
 * The identity shapes: the fields each market series family requires and
 * forbids. A family is an asset class and a series subclass, narrowed by the
 * index family for a fixing, and a row states one field of one family.
 *
 * The catalogue is the system tenant's, and every tenant reads it beside its
 * own rows, so each row says whose it is. A tenant writes and removes only its
 * own rows; the service refuses the rest.
 */

const partSchema = z.string().trim().max(100);

/** The four parts that name one row. The index family is empty for a family that is not a fixing. */
const keySchema = z.object({
    asset_class: partSchema.min(1),
    series_subclass: partSchema.min(1),
    index_family: partSchema,
    field: partSchema.min(1),
});

const writeSchema = keySchema.extend({
    requirement: z.enum(['required', 'forbidden']),
    description: z.string().trim().min(1).max(2000),
});

/** A write: the row, and the version the person read. No version claims the row is new. */
const writeBodySchema = z.object({
    write: writeSchema,
    version: z.int().min(1).nullable(),
    intent: intentSchema,
});

const removeBodySchema = z.object({
    key: keySchema,
    version: z.int().min(1),
    intent: intentSchema,
});

function precondition(version: number | null): unknown {
    return version === null
        ? { kind: 'must_not_exist', version: null }
        : { kind: 'must_match_version', version };
}

/** A row as the screen reads it, with whether it is the shared catalogue's. */
function shapeOf(row: Readonly<Record<string, unknown>>): Readonly<Record<string, unknown>> {
    return { ...row, system: text(row['tenant_id']) === SYSTEM_TENANT_ID };
}

export function registerSeriesIdentityShapeRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /**
     * The rules that give a series its subclass, which the oresmd editor reads
     * to name the family of the URI on screen and so the shape it is held to.
     */
    server.get('/api/marketdata/series-classification-rules', async (request) => {
        const session = requireSession(request);
        const rows = await readAll<ListSeriesClassificationRulesResponse>(
            session,
            ruleSubjects.list_series_classification_rules_request,
            'rules',
            { as_of: null },
        );
        return {
            rules: rows.map((rule) => ({
                instrument: text(rule['instrument']),
                quote: text(rule['quote']),
                key_kind: text(rule['key_kind']),
                series_subclass_code: text(rule['series_subclass_code']),
            })),
        };
    });

    server.get('/api/marketdata/series-identity-shapes', async (request) => {
        const session = requireSession(request);
        const rows = await readAll<ListSeriesIdentityShapesResponse>(
            session,
            shapeSubjects.list_series_identity_shapes_request,
            'shapes',
            { as_of: null },
        );
        return { shapes: rows.map(shapeOf) };
    });

    /** The versions of one row, newest first. */
    server.get('/api/marketdata/series-identity-shapes/versions', async (request) => {
        const session = requireSession(request);
        const key = input(keySchema, request.query);
        const rows = await readAll<ListSeriesIdentityShapeVersionsResponse>(
            session,
            shapeSubjects.list_series_identity_shape_versions_request,
            'versions',
            { key, filter: null },
        );
        return {
            versions: [...rows]
                .map(shapeOf)
                .sort((left, right) => Number(right['version']) - Number(left['version'])),
        };
    });

    server.put('/api/marketdata/series-identity-shapes', async (request) => {
        const session = requireSession(request);
        const body = input(writeBodySchema, request.body);
        const reply = await session.client.callAuthenticated(
            shapeSubjects.put_series_identity_shape_request,
            {
                change: { write: body.write, precondition: precondition(body.version) },
                intent: body.intent,
            },
            z.looseObject({ result: resultSchema }),
        );
        const written = reply['series_identity_shape'];
        return {
            result: reply.result,
            shape:
                written === null || written === undefined
                    ? null
                    : shapeOf(written as Record<string, unknown>),
        };
    });

    server.delete('/api/marketdata/series-identity-shapes', async (request) => {
        const session = requireSession(request);
        const body = input(removeBodySchema, request.body);
        const reply = await session.client.callAuthenticated(
            shapeSubjects.delete_series_identity_shape_request,
            {
                removal: { key: body.key, precondition: precondition(body.version) },
                intent: body.intent,
            },
            z.looseObject({ result: resultSchema }),
        );
        return { result: reply.result };
    });
}
