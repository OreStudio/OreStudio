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
 */

import type { FastifyInstance, FastifyRequest } from 'fastify';
import { z } from 'zod';
import { readEntityHistory } from '@ores/wire-protocol';
import { subjects } from '@ores/wire-protocol/generated/analytics/protocol/sensitivity_template_protocol';
import { HttpFailure, invalidRequest, notFound, notPermitted } from './errors.js';
import { input, resultSchema, row, text } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/*
 * The keys a pricing engine configuration puts on a product with the
 * SensitivityTemplate setting, and nothing more: the shift sizes a key implies
 * belong to the sensitivity configuration, so this screen manages the keys.
 *
 * The table is the tenant's, with no party, and every row keeps its versions. A
 * create states a key the table does not hold and a correction states the
 * version it read, so a key written meanwhile is refused rather than replaced.
 *
 * The method is held as two fields: the words a person reads, and the token ORE
 * spells in a key such as IR_FD. The token follows from the words and is never
 * typed, so a row cannot state a pair the table refuses.
 */

/** The entity type one key's history is read by. */
const ENTITY_TYPE = 'ores.analytics.sensitivity_template';

/** The asset classes a template belongs to, as the table holds them. */
const ASSET_CLASSES = ['IR', 'FX', 'EQ', 'COMM', 'CR', 'ALL'] as const;

/**
 * The five methods, each with the token ORE puts in a template key. The table
 * holds the pair in a check, so this list is the one place a screen reads it.
 */
export const METHODS = [
    { label: 'Analytical', code: 'Analytical' },
    { label: 'Semianalytical', code: 'Semianalytical' },
    { label: 'Finite difference', code: 'FD' },
    { label: 'Monte Carlo', code: 'MC' },
    { label: 'Trade level', code: 'Trade' },
] as const;

const codeSchema = z.string().trim().min(1).max(100);
const textSchema = z.string().max(2000).default('');
const reasonSchema = z.object({
    reasonCode: z.string().trim().min(1).max(200),
    commentary: textSchema,
});

/** A method's token, or a refusal that names the method the request stated. */
function methodCodeOf(method: string): string {
    const found = METHODS.find((candidate) => candidate.label === method);
    if (found === undefined) {
        throw invalidRequest(
            `${method === '' ? 'A template' : method} is not a method a template may state. The methods are ${METHODS.map((candidate) => candidate.label).join(', ')}.`,
        );
    }
    return found.code;
}

const templateBodySchema = z.object({
    assetClass: z.enum(ASSET_CLASSES),
    method: z.string().trim().min(1).max(100),
    description: textSchema,
});

const createBodySchema = templateBodySchema.merge(reasonSchema).extend({ code: codeSchema });

const updateBodySchema = templateBodySchema.merge(reasonSchema).extend({
    version: z.int().nonnegative(),
});

const removeBodySchema = z.object({
    version: z.int().nonnegative(),
    reasonCode: z.string().trim().min(1).max(200),
    commentary: textSchema,
});

/** What the list request carries: where the page starts, how long, and what it searches. */
const listQuerySchema = z.object({
    offset: z.coerce.number().pipe(z.int().min(0)).default(0),
    limit: z.coerce.number().pipe(z.int().min(1).max(1000)).default(15),
    search: z.string().trim().max(256).default(''),
    sort: z.string().max(100).default(''),
    descending: z
        .enum(['true', 'false'])
        .default('false')
        .transform((value) => value === 'true'),
});

const listReplySchema = z.looseObject({
    result: resultSchema,
    templates: z.array(row).default([]),
    total: z.number().default(0),
});

const putReplySchema = z.looseObject({
    result: resultSchema,
    sensitivity_template: row.nullable().default(null),
});

const getReplySchema = z.looseObject({
    result: resultSchema,
    sensitivity_template: row.nullable().default(null),
});

/** One key as the screen reads it. */
function templateOf(stored: Readonly<Record<string, unknown>>): Readonly<Record<string, unknown>> {
    return {
        code: text(stored['code']),
        assetClass: text(stored['asset_class']),
        method: text(stored['method']),
        methodCode: text(stored['method_code']),
        description: text(stored['description']),
        version: typeof stored['version'] === 'number' ? stored['version'] : 0,
        modifiedBy: text(stored['modified_by']),
        performedBy: text(stored['performed_by']),
        reasonCode: text(stored['change_reason_code']),
        commentary: text(stored['change_commentary']),
        recordedAt: text(stored['recorded_at']),
    };
}

/**
 * The status a refused write or read is told as.
 *
 * The service decides, and states the code it refused with. A key the table
 * already holds, and a key that moved on since it was read, are both a conflict:
 * both are answered by reading the rows again.
 */
function refusalOf(result: z.infer<typeof resultSchema>): HttpFailure {
    const message = result.message === '' ? 'The server refused the request.' : result.message;
    switch (result.code) {
        case 'already_exists':
            return new HttpFailure(409, { code: 'conflict', message });
        case 'version_conflict':
            return new HttpFailure(409, { code: 'conflict', message });
        case 'not_found':
            return notFound(message);
        case 'order_not_supported':
        case 'filter_not_supported':
        case 'filter_too_large':
        case 'as_of_invalid':
        case 'filter_range_invalid':
        case 'precondition_not_supported':
        case 'precondition_incomplete':
        case 'missing_field':
            return invalidRequest(message);
        default:
            break;
    }
    switch (result.outcome) {
        case 'denied':
            return notPermitted(message);
        case 'missing':
            return notFound(message);
        case 'conflict':
            return new HttpFailure(409, { code: 'conflict', message });
        case 'unavailable':
        case 'failed':
            return new HttpFailure(502, { code: 'upstream-unavailable', message });
        default:
            return invalidRequest(message);
    }
}

/** One key of the tenant, or a not-found that names it. */
async function readOne(
    session: LiveSession,
    code: string,
): Promise<Readonly<Record<string, unknown>>> {
    const reply = await session.client.callAuthenticated(
        subjects.get_sensitivity_template_request,
        { key: { code } },
        getReplySchema,
    );
    if (reply.result.outcome !== 'ok') {
        throw refusalOf(reply.result);
    }
    const found = reply.sensitivity_template;
    if (found === null) {
        throw notFound(`There is no sensitivity template ${code}.`);
    }
    return templateOf(found);
}

/** The sensitivity template routes: the tenant's keys, the writes, and one key's history. */
export function registerSensitivityTemplateRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /** One page of the tenant's keys, searched and ordered by the server. */
    server.get('/api/analytics/sensitivity-templates', async (request) => {
        const session = requireSession(request);
        const query = input(listQuerySchema, request.query);
        const reply = await session.client.callAuthenticated(
            subjects.list_sensitivity_templates_request,
            {
                offset: query.offset,
                limit: query.limit,
                order: { field: query.sort, descending: query.descending },
                filter: { code_one_of: null, search: query.search === '' ? null : query.search },
                as_of: null,
            },
            listReplySchema,
        );
        if (reply.result.outcome !== 'ok') {
            throw refusalOf(reply.result);
        }
        return { rows: reply.templates.map(templateOf), total: reply.total };
    });

    /**
     * The values a template states. The method's token comes with its label,
     * because the dialog shows the token the chosen method implies.
     */
    server.get('/api/analytics/sensitivity-templates/choices', async () => ({
        assetClasses: [...ASSET_CLASSES],
        methods: METHODS.map((method) => ({ ...method })),
    }));

    /** One key, as its page reads it. */
    server.get('/api/analytics/sensitivity-templates/:code', async (request) => {
        const session = requireSession(request);
        const { code } = input(z.object({ code: codeSchema }), request.params);
        return { template: await readOne(session, code) };
    });

    /** Every version of one key, newest first, with what changed in each. */
    server.get('/api/analytics/sensitivity-templates/:code/history', async (request) => {
        const session = requireSession(request);
        const { code } = input(z.object({ code: codeSchema }), request.params);
        return { versions: await readEntityHistory(session.client, ENTITY_TYPE, code) };
    });

    /**
     * Adds a key. A code the tenant already holds is refused, and the refusal
     * names it, so a create never replaces a row the person did not read.
     */
    server.post('/api/analytics/sensitivity-templates', async (request, reply) => {
        const session = requireSession(request);
        const body = input(createBodySchema, request.body);
        const answer = await session.client.callAuthenticated(
            subjects.put_sensitivity_template_request,
            {
                change: {
                    write: {
                        code: body.code,
                        asset_class: body.assetClass,
                        method: body.method,
                        method_code: methodCodeOf(body.method),
                        description: body.description,
                    },
                    precondition: { kind: 'must_not_exist', version: null },
                },
                intent: { reason_code: body.reasonCode, commentary: body.commentary },
            },
            putReplySchema,
        );
        if (answer.result.outcome !== 'ok') {
            throw refusalOf(answer.result);
        }
        void reply.code(201);
        const written = answer.sensitivity_template;
        return {
            template: written === null ? await readOne(session, body.code) : templateOf(written),
        };
    });

    /**
     * Corrects a key against the version the screen read. The key is fixed once
     * it exists, so the body carries no code.
     */
    server.put('/api/analytics/sensitivity-templates/:code', async (request) => {
        const session = requireSession(request);
        const { code } = input(z.object({ code: codeSchema }), request.params);
        const body = input(updateBodySchema, request.body);
        const answer = await session.client.callAuthenticated(
            subjects.put_sensitivity_template_request,
            {
                change: {
                    write: {
                        code,
                        asset_class: body.assetClass,
                        method: body.method,
                        method_code: methodCodeOf(body.method),
                        description: body.description,
                    },
                    precondition: { kind: 'must_match_version', version: body.version },
                },
                intent: { reason_code: body.reasonCode, commentary: body.commentary },
            },
            putReplySchema,
        );
        if (answer.result.outcome !== 'ok') {
            throw refusalOf(answer.result);
        }
        const written = answer.sensitivity_template;
        return { template: written === null ? await readOne(session, code) : templateOf(written) };
    });

    /**
     * Removes a key. Its versions stay in the history, so the answer states the
     * row it removed rather than a bare acknowledgement.
     */
    server.delete('/api/analytics/sensitivity-templates/:code', async (request) => {
        const session = requireSession(request);
        const { code } = input(z.object({ code: codeSchema }), request.params);
        const body = input(removeBodySchema, request.body);
        const removed = await readOne(session, code);
        const answer = await session.client.callAuthenticated(
            subjects.delete_sensitivity_template_request,
            {
                removal: {
                    key: { code },
                    precondition: { kind: 'must_match_version', version: body.version },
                },
                intent: { reason_code: body.reasonCode, commentary: body.commentary },
            },
            z.looseObject({ result: resultSchema }),
        );
        if (answer.result.outcome !== 'ok') {
            throw refusalOf(answer.result);
        }
        return { removed };
    });
}
