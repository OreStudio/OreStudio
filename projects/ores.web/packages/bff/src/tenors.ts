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
import { subjects as calendarSubjects } from '@ores/wire-protocol/generated/refdata/protocol/calendar_protocol';
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

/**
 * A drafted rule asking what date it resolves to. The three rows arrive whole
 * because they have not been written yet, and the calendar is named because
 * resolution rolls on one.
 */
const previewBodySchema = z.object({
    tenor: z.looseObject({}),
    convention: z.looseObject({}),
    resolution: z.looseObject({}),
    calendar_code: codeSchema,
    horizon: z.string().trim().min(1).max(40),
    spot: z.string().trim().min(1).max(40).nullable().default(null),
});

/**
 * A question about the rules as the store holds them: which tenor, under which
 * convention, on which calendar, counted from which date.
 */
const resolveBodySchema = z.object({
    tenor_code: codeSchema,
    convention_code: codeSchema,
    calendar_code: codeSchema,
    horizon: z.string().trim().min(1).max(40),
    spot: z.string().trim().min(1).max(40).nullable().default(null),
});

/**
 * A question about every tenor of one convention: which convention, on which
 * calendar, counted from which date.
 */
const resolveConventionBodySchema = z.object({
    convention_code: codeSchema,
    calendar_code: codeSchema,
    horizon: z.string().trim().min(1).max(40),
    spot: z.string().trim().min(1).max(40).nullable().default(null),
});

/**
 * What one tenor of a convention resolved to, or why it declined. Each entry
 * carries its own result, so one refusal does not hide the other tenors.
 */
const conventionTenorSchema = z.looseObject({
    tenor_code: z.string(),
    result: resultSchema,
    anchor_date: z.string().default(''),
    date_before_roll: z.string().default(''),
    adjusted_date: z.string().default(''),
});

const conventionReplySchema = z.looseObject({
    result: resultSchema,
    roll_rule: z.string().default(''),
    calendar: z.string().default(''),
    tenors: z.array(conventionTenorSchema).default([]),
});

/**
 * What the resolution answered, or why it declined. The dates travel on a
 * refusal too: a refusal that says only "no" cannot be acted on.
 */
const resolutionReplySchema = z.looseObject({
    result: resultSchema,
    anchor_date: z.string().default(''),
    date_before_roll: z.string().default(''),
    adjusted_date: z.string().default(''),
    roll_rule: z.string().default(''),
    calendar: z.string().default(''),
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
    {
        key: 'calendars',
        subject: calendarSubjects.list_calendars_request,
        rows: 'calendars',
    },
] as const;

/**
 * The resolution's answer as the screen reads it.
 *
 * The preview and resolve operations answer with the same reply, so the two
 * routes shape it here once rather than each naming the six fields.
 */
function resolutionReply(
    reply: z.infer<typeof resolutionReplySchema>,
): z.infer<typeof resolutionReplySchema> {
    return {
        result: reply.result,
        anchor_date: reply.anchor_date,
        date_before_roll: reply.date_before_roll,
        adjusted_date: reply.adjusted_date,
        roll_rule: reply.roll_rule,
        calendar: reply.calendar,
    };
}

function precondition(version: number | null): unknown {
    return version === null
        ? { kind: 'must_not_exist', version: null }
        : { kind: 'must_match_version', version };
}

/**
 * The tenors in their own order: O/N before T/N before 1M, not alphabetically.
 *
 * The table holds that order in `sort_order`, and the server refuses to order a
 * tenor list by it ("A list of tenors cannot be ordered by sort_order"), so the
 * order is applied here, where the list is shaped for the screen.
 */
function orderBy(rows: readonly Record<string, unknown>[]): readonly Record<string, unknown>[] {
    return [...rows].sort(
        (left, right) => Number(left['sort_order'] ?? 0) - Number(right['sort_order'] ?? 0),
    );
}

/**
 * The tenor routes: the catalogue with its conventions and schedules, the pick
 * lists, one convention's set, the resolution of a drafted rule, the resolution
 * of the rules as they are stored, and the writes of a tenor, a convention and
 * a membership row.
 *
 * The preview route answers about the rows it is handed, so a person authoring
 * a rule reads its answer before writing it. The resolve route answers about the
 * rows the store holds, so a person who only wants a date never authors
 * anything. A rule the server declines comes back as a 200 whose result is not
 * ok: the refusal is the answer, not a transport failure.
 */
export function registerTenorRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/tenors/catalogue', async (request) => {
        const session = requireSession(request);
        return {
            tenors: orderBy(
                await readAll(session, tenorSubjects.list_tenors_request, 'tenors', {
                    filter: null,
                    as_of: null,
                }),
            ),
            conventions: await readAll(
                session,
                tenorConventionSubjects.list_tenor_conventions_request,
                'conventions',
                { filter: null, as_of: null },
            ),
            schedules: await readAll(
                session,
                tenorScheduleSubjects.list_tenor_schedules_request,
                'schedules',
                { filter: null, as_of: null },
            ),
        };
    });

    server.get('/api/tenors/pick-lists', async (request) => {
        const session = requireSession(request);
        const lists: Record<string, readonly Record<string, unknown>[]> = {};
        for (const list of PICK_LISTS) {
            lists[list.key] = await readAll(session, list.subject, list.rows, {
                filter: null,
                as_of: null,
            });
        }
        return lists;
    });

    server.post('/api/tenors/preview-resolution', async (request) => {
        const session = requireSession(request);
        const body = input(previewBodySchema, request.body);
        const reply = await session.client.callAuthenticated(
            tenorSubjects.preview_tenor_resolution_request,
            {
                tenor: body.tenor,
                convention: body.convention,
                resolution: body.resolution,
                calendar_code: body.calendar_code,
                horizon: body.horizon,
                spot: body.spot,
            },
            resolutionReplySchema,
        );
        return resolutionReply(reply);
    });

    server.post('/api/tenors/resolve', async (request) => {
        const session = requireSession(request);
        const body = input(resolveBodySchema, request.body);
        const reply = await session.client.callAuthenticated(
            tenorSubjects.resolve_tenor_request,
            {
                tenor_code: body.tenor_code,
                convention_code: body.convention_code,
                calendar_code: body.calendar_code,
                horizon: body.horizon,
                spot: body.spot,
            },
            resolutionReplySchema,
        );
        return resolutionReply(reply);
    });

    server.post('/api/tenors/resolve-convention', async (request) => {
        const session = requireSession(request);
        const body = input(resolveConventionBodySchema, request.body);
        const reply = await session.client.callAuthenticated(
            tenorSubjects.resolve_convention_request,
            {
                convention_code: body.convention_code,
                calendar_code: body.calendar_code,
                horizon: body.horizon,
                spot: body.spot,
            },
            conventionReplySchema,
        );
        return {
            result: reply.result,
            roll_rule: reply.roll_rule,
            calendar: reply.calendar,
            tenors: reply.tenors.map((entry) => ({
                tenor_code: entry.tenor_code,
                result: entry.result,
                anchor_date: entry.anchor_date,
                date_before_roll: entry.date_before_roll,
                adjusted_date: entry.adjusted_date,
            })),
        };
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
