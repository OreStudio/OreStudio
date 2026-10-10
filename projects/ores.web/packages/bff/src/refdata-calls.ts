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

import type { FastifyRequest } from 'fastify';
import { z } from 'zod';
import { HttpFailure, invalidRequest, notFound, notPermitted } from './errors.js';
import type { LiveSession } from './sessions.js';

/*
 * What the refdata routes share: reading a list whole, writing one row, and
 * reading a request's input so a bad one is a 400 and not a 500.
 */

export const PAGE = 1000;

export const NO_ORDER = { field: '', descending: false } as const;

export const resultSchema = z.object({
    outcome: z.enum(['ok', 'invalid', 'denied', 'missing', 'conflict', 'unavailable', 'failed']),
    code: z.string().default(''),
    message: z.string().default(''),
    fields: z
        .array(
            z.object({
                field: z.string().default(''),
                code: z.string().default(''),
                message: z.string().default(''),
            }),
        )
        .default([]),
});

export const row = z.looseObject({});

export const intentSchema = z.object({
    reason_code: z.string().trim().min(1).max(200),
    commentary: z.string().max(2000).default(''),
});

export const idSchema = z.uuid();

/** A request's input read against its schema, or a 400 that says what was wrong. */
export function input<Schema extends z.ZodType>(schema: Schema, value: unknown): z.infer<Schema> {
    const parsed = schema.safeParse(value);
    if (!parsed.success) {
        throw invalidRequest(parsed.error.issues.map((issue) => issue.message).join(' '));
    }
    return parsed.data;
}

/**
 * Turns a refused read into the status that says why, so the browser can tell a
 * missing permission from a bad request.
 */
export function refusal(result: z.infer<typeof resultSchema>): HttpFailure {
    switch (result.outcome) {
        case 'denied':
            return notPermitted(result.message);
        case 'missing':
            return notFound(result.message);
        case 'unavailable':
        case 'failed':
            return new HttpFailure(502, { code: 'upstream-unavailable', message: result.message });
        default:
            return invalidRequest(result.message);
    }
}

/** Every row of a list, read a page at a time until a page comes back short. */
export async function readAll(
    session: LiveSession,
    subject: string,
    rows: string,
    request: Readonly<Record<string, unknown>>,
): Promise<readonly Record<string, unknown>[]> {
    const all: Record<string, unknown>[] = [];
    for (let offset = 0; ; offset += PAGE) {
        const reply = await session.client.callAuthenticated(
            subject,
            { ...request, offset, limit: PAGE, order: NO_ORDER },
            z.looseObject({ result: resultSchema }),
        );
        if (reply.result.outcome !== 'ok') {
            throw refusal(reply.result);
        }
        const page = z.array(row).default([]).parse(reply[rows]);
        all.push(...page);
        if (page.length < PAGE) {
            return all;
        }
    }
}

/** A write's result, with the row it wrote when the server returns one. */
export async function write(
    session: LiveSession,
    subject: string,
    body: unknown,
): Promise<z.infer<typeof resultSchema>> {
    const reply = await session.client.callAuthenticated(
        subject,
        body,
        z.looseObject({ result: resultSchema }),
    );
    return reply.result;
}

export function text(value: unknown): string {
    return typeof value === 'string' ? value : '';
}

/** The id the address names, or a 400. */
export function paramId(request: FastifyRequest): string {
    return input(idSchema, (request.params as { id: string }).id);
}
