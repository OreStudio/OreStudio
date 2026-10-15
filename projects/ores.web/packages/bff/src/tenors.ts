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
import { subjects as businessDayConventionSubjects } from '@ores/wire-protocol/generated/refdata/protocol/business_day_convention_type_protocol';
import { subjects as tenorAnchorSubjects } from '@ores/wire-protocol/generated/refdata/protocol/tenor_anchor_protocol';
import { subjects as tenorConventionSubjects } from '@ores/wire-protocol/generated/refdata/protocol/tenor_convention_protocol';
import { subjects as tenorKindSubjects } from '@ores/wire-protocol/generated/refdata/protocol/tenor_kind_protocol';
import { subjects as tenorResolutionSubjects } from '@ores/wire-protocol/generated/refdata/protocol/tenor_convention_resolution_protocol';
import { subjects as tenorScheduleSubjects } from '@ores/wire-protocol/generated/refdata/protocol/tenor_schedule_protocol';
import { subjects as tenorSubjects } from '@ores/wire-protocol/generated/refdata/protocol/tenor_protocol';
import { subjects as tenorUnitSubjects } from '@ores/wire-protocol/generated/refdata/protocol/tenor_unit_protocol';
import { subjects as algorithmSubjects } from '@ores/wire-protocol/generated/refdata/protocol/tenor_resolution_algorithm_protocol';
import { input, intentSchema, readAll, resultSchema, write } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

const codeSchema = z.string().trim().min(1).max(100);

/**
 * A write of a tenor, a convention or a membership row: the row, and the
 * version the person read. No version claims the row is new.
 */
const writeBodySchema = z.object({
    intent: intentSchema,
    version: z.int().nonnegative().nullable(),
    write: z.looseObject({}),
});

const removeBodySchema = z.object({
    intent: intentSchema,
    version: z.int().nonnegative(),
});

const PICK_LISTS = [
    { key: 'kinds', subject: tenorKindSubjects.list_tenor_kinds_request, rows: 'kinds' },
    { key: 'units', subject: tenorUnitSubjects.list_tenor_units_request, rows: 'units' },
    { key: 'anchors', subject: tenorAnchorSubjects.list_tenor_anchors_request, rows: 'anchors' },
    {
        key: 'algorithms',
        subject: algorithmSubjects.list_tenor_resolution_algorithms_request,
        rows: 'algorithms',
    },
    {
        key: 'rollRules',
        subject: businessDayConventionSubjects.list_business_day_convention_types_request,
        rows: 'types',
    },
] as const;

function precondition(version: number | null): unknown {
    return version === null
        ? { kind: 'must_not_exist', version: null }
        : { kind: 'must_match_version', version };
}

/**
 * The tenor routes: the catalogue with its conventions and schedules, the pick
 * lists, one convention's set, and the writes of a tenor, a convention and a
 * membership row.
 *
 * The server serves no operation that resolves a tenor to a date, so no route
 * here resolves one. The resolution column and the resolved-dates step wait for
 * that operation.
 */
export function registerTenorRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/tenors/catalogue', async (request) => {
        const session = requireSession(request);
        return {
            tenors: await readAll(session, tenorSubjects.list_tenors_request, 'tenors', {
                filter: null,
            }),
            conventions: await readAll(
                session,
                tenorConventionSubjects.list_tenor_conventions_request,
                'conventions',
                { filter: null },
            ),
            schedules: await readAll(
                session,
                tenorScheduleSubjects.list_tenor_schedules_request,
                'schedules',
                { filter: null },
            ),
        };
    });

    server.get('/api/tenors/pick-lists', async (request) => {
        const session = requireSession(request);
        const lists: Record<string, readonly Record<string, unknown>[]> = {};
        for (const list of PICK_LISTS) {
            lists[list.key] = await readAll(session, list.subject, list.rows, { filter: null });
        }
        return lists;
    });

    server.get('/api/tenors/conventions/:code/resolutions', async (request) => {
        const session = requireSession(request);
        const code = input(codeSchema, (request.params as { code: string }).code);
        return {
            rows: await readAll(
                session,
                tenorResolutionSubjects.list_by_convention_code_tenor_convention_resolutions_request,
                'tenor_convention_resolutions',
                { convention_code: code, scope: 'direct', filter: null },
            ),
        };
    });

    server.put('/api/tenors/tenors', async (request) => {
        const session = requireSession(request);
        const body = input(writeBodySchema, request.body);
        const reply = await session.client.callAuthenticated(
            tenorSubjects.put_tenor_request,
            {
                change: { write: body.write, precondition: precondition(body.version) },
                intent: body.intent,
            },
            z.looseObject({ result: resultSchema }),
        );
        return { result: reply.result, tenor: reply['tenor'] ?? null };
    });

    server.put('/api/tenors/conventions', async (request) => {
        const session = requireSession(request);
        const body = input(writeBodySchema, request.body);
        const reply = await session.client.callAuthenticated(
            tenorConventionSubjects.put_tenor_convention_request,
            {
                change: { write: body.write, precondition: precondition(body.version) },
                intent: body.intent,
            },
            z.looseObject({ result: resultSchema }),
        );
        return { result: reply.result, convention: reply['tenor_convention'] ?? null };
    });

    server.put('/api/tenors/conventions/:code/resolutions/:tenor', async (request) => {
        const session = requireSession(request);
        const params = request.params as { code: string; tenor: string };
        const conventionCode = input(codeSchema, params.code);
        const tenorCode = input(codeSchema, params.tenor);
        const body = input(writeBodySchema, request.body);
        const result = await write(
            session,
            tenorResolutionSubjects.put_tenor_convention_resolution_request,
            {
                change: {
                    write: {
                        ...body.write,
                        convention_code: conventionCode,
                        tenor_code: tenorCode,
                    },
                    precondition: precondition(body.version),
                },
                intent: body.intent,
            },
        );
        return { result };
    });

    server.delete('/api/tenors/conventions/:code/resolutions/:tenor', async (request) => {
        const session = requireSession(request);
        const params = request.params as { code: string; tenor: string };
        const conventionCode = input(codeSchema, params.code);
        const tenorCode = input(codeSchema, params.tenor);
        const body = input(removeBodySchema, request.body);
        const result = await write(
            session,
            tenorResolutionSubjects.delete_tenor_convention_resolution_request,
            {
                removal: {
                    key: { convention_code: conventionCode, tenor_code: tenorCode },
                    precondition: { kind: 'must_match_version', version: body.version },
                },
                intent: body.intent,
            },
        );
        return { result };
    });
}
