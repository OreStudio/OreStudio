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
import { subjects as methodSubjects } from '@ores/wire-protocol/generated/analytics/protocol/sensitivity_method_protocol';
import { subjects as assetClassSubjects } from '@ores/wire-protocol/generated/analytics/protocol/sensitivity_asset_class_protocol';
import { subjects } from '@ores/wire-protocol/generated/analytics/protocol/sensitivity_template_protocol';
import { HttpFailure, invalidRequest, notFound, notPermitted } from './errors.js';
import { NO_ORDER, input, resultSchema, row, text } from './refdata-calls.js';
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
 * Two lookups hold what a key names. The method is one of the five ORE defines,
 * and the asset class is the reference data's code, with the prefix a key spells
 * and the words a person reads held beside it. A trade-level template states no
 * class, because the trade computes its own sensitivities; the methods table says
 * which method is that one, so this module asks it rather than repeating the
 * exception.
 *
 * Nothing here derives a value a person typed: the two codes travel as they were
 * chosen, and the row states them.
 */

/** The entity type one key's history is read by. */
const ENTITY_TYPE = 'ores.analytics.sensitivity_template';

/** One method, as the screen chooses it. */
export interface MethodChoice {
    /** The token ORE spells in a template key, such as FD. */
    readonly code: string;
    /** The words a person reads, such as Finite difference. */
    readonly name: string;
    /** Whether a template that states this method also states an asset class. */
    readonly needsAssetClass: boolean;
}

/** One asset class ORE presents a template prefix for. */
export interface AssetClassChoice {
    /** The reference data's asset class code, such as interest_rates. */
    readonly code: string;
    /** The prefix a template key spells, such as IR. */
    readonly prefix: string;
    /** The words a person reads, such as Interest rate. */
    readonly name: string;
}

/** The two lookups, in the order the screen offers them. */
export interface Choices {
    readonly assetClasses: readonly AssetClassChoice[];
    readonly methods: readonly MethodChoice[];
}

const methodsReplySchema = z.looseObject({
    result: resultSchema,
    methods: z.array(row).default([]),
    total: z.number().default(0),
});

const assetClassesReplySchema = z.looseObject({
    result: resultSchema,
    asset_classes: z.array(row).default([]),
    total: z.number().default(0),
});

/**
 * The methods and the asset classes a template may name, read from the tables
 * that hold them. A screen offers what the store holds and never a list of its
 * own, so a method or a class added to a table reaches the form without a change
 * here.
 */
export async function readChoices(session: LiveSession): Promise<Choices> {
    const reply = await session.client.callAuthenticated(
        methodSubjects.list_sensitivity_methods_request,
        { filter: null, as_of: null, offset: 0, limit: 1000, order: NO_ORDER },
        methodsReplySchema,
    );
    if (reply.result.outcome !== 'ok') {
        throw refusalOf(reply.result);
    }
    const classes = await session.client.callAuthenticated(
        assetClassSubjects.list_sensitivity_asset_classes_request,
        { filter: null, as_of: null, offset: 0, limit: 1000, order: NO_ORDER },
        assetClassesReplySchema,
    );
    if (classes.result.outcome !== 'ok') {
        throw refusalOf(classes.result);
    }
    const order = (
        left: Readonly<Record<string, unknown>>,
        right: Readonly<Record<string, unknown>>,
    ) => Number(left['display_order'] ?? 0) - Number(right['display_order'] ?? 0);
    return {
        assetClasses: [...classes.asset_classes].sort(order).map((stored) => ({
            code: text(stored['asset_class_code']),
            prefix: text(stored['template_prefix']),
            name: text(stored['name']),
        })),
        methods: [...reply.methods].sort(order).map((stored) => ({
            code: text(stored['code']),
            name: text(stored['name']),
            needsAssetClass: stored['needs_asset_class'] === true,
        })),
    };
}

/**
 * The pair a write states, refused when it is not one the tables allow.
 *
 * The rule is the methods table's: a method that needs no class states none. The
 * store refuses it too, and this says which of the two is wrong before the write
 * reaches it.
 */
function pairOf(
    choices: Choices,
    methodCode: string,
    assetClassCode: string | null,
): { methodCode: string; assetClassCode: string | null } {
    const method = choices.methods.find((candidate) => candidate.code === methodCode);
    if (method === undefined) {
        throw invalidRequest(
            `${methodCode === '' ? 'A template' : methodCode} is not a method a template may state. The methods are ${choices.methods.map((candidate) => candidate.name).join(', ')}.`,
        );
    }
    if (method.needsAssetClass && assetClassCode === null) {
        throw invalidRequest(
            `A ${method.name} template states the asset class of the product it belongs to.`,
        );
    }
    if (!method.needsAssetClass && assetClassCode !== null) {
        throw invalidRequest(`A ${method.name} template states no asset class.`);
    }
    return { methodCode: method.code, assetClassCode };
}

const codeSchema = z.string().trim().min(1).max(100);
const textSchema = z.string().max(2000).default('');
const reasonSchema = z.object({
    reasonCode: z.string().trim().min(1).max(200),
    commentary: textSchema,
});

const templateBodySchema = z.object({
    methodCode: z.string().trim().min(1).max(100),
    // A trade-level key states no class, and a client that read one back as an
    // empty string must be able to save it without stating a class it has not.
    assetClassCode: z
        .string()
        .trim()
        .nullable()
        .default(null)
        .transform((value) => (value === '' ? null : value)),
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

/**
 * One key as the screen reads it: the two codes it states, and the words and the
 * prefix the lookups hold for them.
 *
 * A class the bridge does not present still reads as its code, so a row written
 * before the bridge held the class shows what it holds rather than a blank. A
 * trade-level key has no class, and its three class fields are empty.
 */
function templateOf(
    stored: Readonly<Record<string, unknown>>,
    choices: Choices,
): Readonly<Record<string, unknown>> {
    const methodCode = text(stored['method_code']);
    const assetClassCode = text(stored['asset_class_code']);
    const method = choices.methods.find((candidate) => candidate.code === methodCode);
    const assetClass = choices.assetClasses.find((candidate) => candidate.code === assetClassCode);
    return {
        code: text(stored['code']),
        methodCode,
        methodName: method?.name ?? methodCode,
        assetClassCode,
        assetClassName: assetClass?.name ?? assetClassCode,
        templatePrefix: assetClass?.prefix ?? '',
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
    choices: Choices,
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
    return templateOf(found, choices);
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
        const choices = await readChoices(session);
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
        return {
            rows: reply.templates.map((stored) => templateOf(stored, choices)),
            total: reply.total,
        };
    });

    /**
     * The two lookups a template states, in the order the screen offers them.
     * They are read from their tables rather than stated here, so a method or a
     * class added to one reaches the form without a change to this module.
     */
    server.get('/api/analytics/sensitivity-templates/choices', async (request) =>
        readChoices(requireSession(request)),
    );

    /** One key, as its page reads it. */
    server.get('/api/analytics/sensitivity-templates/:code', async (request) => {
        const session = requireSession(request);
        const { code } = input(z.object({ code: codeSchema }), request.params);
        return { template: await readOne(session, code, await readChoices(session)) };
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
        const choices = await readChoices(session);
        const pair = pairOf(choices, body.methodCode, body.assetClassCode);
        const answer = await session.client.callAuthenticated(
            subjects.put_sensitivity_template_request,
            {
                change: {
                    write: {
                        code: body.code,
                        method_code: pair.methodCode,
                        asset_class_code: pair.assetClassCode ?? '',
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
            template:
                written === null
                    ? await readOne(session, body.code, choices)
                    : templateOf(written, choices),
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
        const choices = await readChoices(session);
        const pair = pairOf(choices, body.methodCode, body.assetClassCode);
        const answer = await session.client.callAuthenticated(
            subjects.put_sensitivity_template_request,
            {
                change: {
                    write: {
                        code,
                        method_code: pair.methodCode,
                        asset_class_code: pair.assetClassCode ?? '',
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
        return {
            template:
                written === null
                    ? await readOne(session, code, choices)
                    : templateOf(written, choices),
        };
    });

    /**
     * Removes a key. Its versions stay in the history, so the answer states the
     * row it removed rather than a bare acknowledgement.
     */
    server.delete('/api/analytics/sensitivity-templates/:code', async (request) => {
        const session = requireSession(request);
        const { code } = input(z.object({ code: codeSchema }), request.params);
        const body = input(removeBodySchema, request.body);
        const choices = await readChoices(session);
        const removed = await readOne(session, code, choices);
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
