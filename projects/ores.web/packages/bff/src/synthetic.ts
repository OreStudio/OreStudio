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
import { subjects as feedSubjects } from '@ores/wire-protocol/generated/synthetic/protocol/feed_config_protocol';
import { subjects as folderSubjects } from '@ores/wire-protocol/generated/synthetic/protocol/folder_protocol';
import {
    subjects as fxFeedSubjects,
    type FxSpotGenerationConfigWrite,
} from '@ores/wire-protocol/generated/synthetic/protocol/fx_spot_generation_config_protocol';
import {
    subjects as gmmSubjects,
    type GmmComponentWrite,
} from '@ores/wire-protocol/generated/synthetic/protocol/gmm_component_protocol';
import {
    subjects as irFeedSubjects,
    type IrCurveGenerationConfigWrite,
} from '@ores/wire-protocol/generated/synthetic/protocol/ir_curve_generation_config_protocol';
import { subjects as irOperationSubjects } from '@ores/wire-protocol/generated/synthetic/protocol/ir_curve_operations_protocol';
import {
    subjects as parameterValueSubjects,
    type IrCurveGenerationConfigProcessParameterValueWrite,
} from '@ores/wire-protocol/generated/synthetic/protocol/ir_curve_generation_config_process_parameter_value_protocol';
import {
    subjects as templateEntrySubjects,
    type IrCurveTemplateEntryWrite,
} from '@ores/wire-protocol/generated/synthetic/protocol/ir_curve_template_entry_protocol';
import { subjects as collectionSubjects } from '@ores/wire-protocol/generated/synthetic/protocol/market_data_generation_config_protocol';
import { subjects as fxSimulateSubjects } from '@ores/wire-protocol/generated/synthetic/protocol/simulate_fx_spot_paths_protocol';
import { subjects as definitionSubjects } from '@ores/wire-protocol/generated/synthetic/protocol/yield_curve_process_parameter_definition_protocol';
import {
    subjects as processTypeSubjects,
    type YieldCurveProcessTypeWrite,
} from '@ores/wire-protocol/generated/synthetic/protocol/yield_curve_process_type_protocol';
import { subjects as marketdataOperationSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/operations_protocol';
import { HttpFailure, invalidRequest, notFound, notPermitted } from './errors.js';
import {
    NO_ORDER,
    idSchema,
    input,
    intentSchema,
    paramId,
    readAll,
    refusal,
    resultSchema,
    row,
} from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

const code = z.string().trim().min(1).max(100);
const label = z.string().trim().min(1).max(200);
const text = z.string().max(2000);

/**
 * One synthetic resource the screens read, and write when it has a schema.
 *
 * `oneOf` is the list filter that reads rows by key. It is not always the key
 * field: a parameter definition is keyed by its name on the wire but filtered
 * by its id, and its id is what names one row.
 *
 * A system-owned resource is one catalogue every tenant reads. The server
 * reads and writes it in the system tenant, so a write from a tenant session
 * would change every tenant's catalogue; only a system session writes it.
 */
interface SyntheticResource {
    readonly key: string;
    readonly entityType: string;
    readonly keyField: 'id' | 'code';
    readonly oneOf: 'id_one_of' | 'code_one_of';
    readonly rows: string;
    readonly subjects: {
        readonly list: string;
        readonly putMany: string;
        readonly remove: string;
        readonly removeMany: string;
    };
    readonly writes?: z.ZodType<Record<string, unknown>>;
    readonly readOnlyBecause?: string;
    readonly systemOwned?: true;
}

/**
 * The synthetic resources the four journeys read and write.
 *
 * This table is the BFF's authority: a resource it does not name cannot be
 * reached through the synthetic routes. Each write schema checks structure
 * and satisfies the generated write type; the table checks enforce the domain
 * rules, so the enumerations are not copied here.
 */
const SYNTHETIC_RECORDS: readonly SyntheticResource[] = [
    {
        key: 'folders',
        entityType: 'ores.synthetic.folder',
        keyField: 'id',
        oneOf: 'id_one_of',
        rows: 'folders',
        subjects: {
            list: folderSubjects.list_folders_request,
            putMany: folderSubjects.put_many_folders_request,
            remove: folderSubjects.delete_folder_request,
            removeMany: folderSubjects.delete_many_folders_request,
        },
        readOnlyBecause: 'No journey writes a folder from the web.',
    },
    {
        key: 'collections',
        entityType: 'ores.synthetic.market_data_generation_config',
        keyField: 'id',
        oneOf: 'id_one_of',
        rows: 'market_data_generation_configs',
        subjects: {
            list: collectionSubjects.list_market_data_generation_configs_request,
            putMany: collectionSubjects.put_many_market_data_generation_configs_request,
            remove: collectionSubjects.delete_market_data_generation_config_request,
            removeMany: collectionSubjects.delete_many_market_data_generation_configs_request,
        },
        readOnlyBecause: 'No journey writes a collection from the web.',
    },
    {
        key: 'fx-feeds',
        entityType: 'ores.synthetic.fx_spot_generation_config',
        keyField: 'id',
        oneOf: 'id_one_of',
        rows: 'fx_spot_generation_configs',
        subjects: {
            list: fxFeedSubjects.list_fx_spot_generation_configs_request,
            putMany: fxFeedSubjects.put_many_fx_spot_generation_configs_request,
            remove: fxFeedSubjects.delete_fx_spot_generation_config_request,
            removeMany: fxFeedSubjects.delete_many_fx_spot_generation_configs_request,
        },
        writes: z.object({
            id: idSchema,
            party_id: idSchema,
            config_id: idSchema,
            base_currency_code: code,
            quote_currency_code: code,
            source_name: label,
            ore_key: label,
            price_source: code,
            gmm_initial_price: z.number(),
            ticks_per_hour: z.int().positive(),
            process_type: code,
            enabled: z.boolean(),
            auto_start: z.boolean(),
            vintage_source: z.string().max(200),
            vintage_date: z.string().max(40),
            folder_id: idSchema.nullable(),
        }) satisfies z.ZodType<FxSpotGenerationConfigWrite>,
    },
    {
        key: 'gmm-components',
        entityType: 'ores.synthetic.gmm_component',
        keyField: 'id',
        oneOf: 'id_one_of',
        rows: 'gmm_components',
        subjects: {
            list: gmmSubjects.list_gmm_components_request,
            putMany: gmmSubjects.put_many_gmm_components_request,
            remove: gmmSubjects.delete_gmm_component_request,
            removeMany: gmmSubjects.delete_many_gmm_components_request,
        },
        writes: z.object({
            id: idSchema,
            party_id: idSchema,
            fx_spot_config_id: idSchema,
            component_index: z.int().nonnegative(),
            description: text,
            mean: z.number(),
            stdev: z.number(),
            weight: z.number(),
        }) satisfies z.ZodType<GmmComponentWrite>,
    },
    {
        key: 'ir-curve-feeds',
        entityType: 'ores.synthetic.ir_curve_generation_config',
        keyField: 'id',
        oneOf: 'id_one_of',
        rows: 'ir_curve_generation_configs',
        subjects: {
            list: irFeedSubjects.list_ir_curve_generation_configs_request,
            putMany: irFeedSubjects.put_many_ir_curve_generation_configs_request,
            remove: irFeedSubjects.delete_ir_curve_generation_config_request,
            removeMany: irFeedSubjects.delete_many_ir_curve_generation_configs_request,
        },
        writes: z.object({
            id: idSchema,
            party_id: idSchema,
            config_id: idSchema,
            currency_code: code,
            index_family: code,
            tenor: z.string().max(20),
            role: code,
            process_type: code,
            ticks_per_hour: z.int().positive(),
            enabled: z.boolean(),
            auto_start: z.boolean(),
            price_source: code,
            vintage_source: z.string().max(200),
            vintage_date: z.string().max(40),
            vintage_series_uri: z.string().max(2000),
            description: text,
            fixed_leg_payment_frequency_code: z.string().max(100),
            source_name: label,
            folder_id: idSchema.nullable(),
        }) satisfies z.ZodType<IrCurveGenerationConfigWrite>,
    },
    {
        key: 'ir-parameter-values',
        entityType: 'ores.synthetic.ir_curve_generation_config_process_parameter_value',
        keyField: 'id',
        oneOf: 'id_one_of',
        rows: 'process_parameter_values',
        subjects: {
            list: parameterValueSubjects.list_ir_curve_generation_config_process_parameter_values_request,
            putMany:
                parameterValueSubjects.put_many_ir_curve_generation_config_process_parameter_values_request,
            remove: parameterValueSubjects.delete_ir_curve_generation_config_process_parameter_value_request,
            removeMany:
                parameterValueSubjects.delete_many_ir_curve_generation_config_process_parameter_values_request,
        },
        writes: z.object({
            id: idSchema,
            config_id: idSchema,
            parameter_definition_id: idSchema,
            parameter_value: z.number(),
        }) satisfies z.ZodType<IrCurveGenerationConfigProcessParameterValueWrite>,
    },
    {
        key: 'ir-template-entries',
        entityType: 'ores.synthetic.ir_curve_template_entry',
        keyField: 'id',
        oneOf: 'id_one_of',
        rows: 'ir_curve_template_entries',
        subjects: {
            list: templateEntrySubjects.list_ir_curve_template_entries_request,
            putMany: templateEntrySubjects.put_many_ir_curve_template_entries_request,
            remove: templateEntrySubjects.delete_ir_curve_template_entry_request,
            removeMany: templateEntrySubjects.delete_many_ir_curve_template_entries_request,
        },
        writes: z.object({
            id: idSchema,
            party_id: idSchema,
            ir_curve_config_id: idSchema,
            sequence_index: z.int().nonnegative(),
            start_tenor_code: code,
            end_tenor_code: code,
            instrument_code: code,
        }) satisfies z.ZodType<IrCurveTemplateEntryWrite>,
    },
    {
        key: 'process-types',
        systemOwned: true,
        entityType: 'ores.synthetic.yield_curve_process_type',
        keyField: 'code',
        oneOf: 'code_one_of',
        rows: 'process_types',
        subjects: {
            list: processTypeSubjects.list_yield_curve_process_types_request,
            putMany: processTypeSubjects.put_many_yield_curve_process_types_request,
            remove: processTypeSubjects.delete_yield_curve_process_type_request,
            removeMany: processTypeSubjects.delete_many_yield_curve_process_types_request,
        },
        writes: z.object({
            code,
            name: label,
            description: text,
            display_order: z.int().nonnegative(),
        }) satisfies z.ZodType<YieldCurveProcessTypeWrite>,
    },
    {
        key: 'parameter-definitions',
        entityType: 'ores.synthetic.yield_curve_process_parameter_definition',
        keyField: 'id',
        oneOf: 'id_one_of',
        rows: 'parameter_definitions',
        subjects: {
            list: definitionSubjects.list_yield_curve_process_parameter_definitions_request,
            putMany: definitionSubjects.put_many_yield_curve_process_parameter_definitions_request,
            remove: definitionSubjects.delete_yield_curve_process_parameter_definition_request,
            removeMany:
                definitionSubjects.delete_many_yield_curve_process_parameter_definitions_request,
        },
        readOnlyBecause:
            'The server addresses a parameter definition by its name alone, which several process types share, so a write would reach whichever type it found first.',
    },
];

/** The synthetic entity types whose history the history route serves. */
export const SYNTHETIC_HISTORY_TYPES: readonly string[] = SYNTHETIC_RECORDS.map(
    (resource) => resource.entityType,
);

/** The name a resource's subjects and permissions share, such as `gmm_components`. */
function resourceName(resource: SyntheticResource): string {
    return resource.subjects.list.split('.')[2] ?? '';
}

function resourceFor(request: FastifyRequest): SyntheticResource {
    const { resource } = request.params as { resource: string };
    const found = SYNTHETIC_RECORDS.find((candidate) => candidate.key === resource);
    if (found === undefined) {
        throw notFound(`There is no synthetic resource ${resource}.`);
    }
    return found;
}

/** Why this session cannot write the resource, or undefined when it can. */
function readOnlyFor(resource: SyntheticResource, session: LiveSession): string | undefined {
    if (resource.writes === undefined) {
        return resource.readOnlyBecause;
    }
    if (resource.systemOwned === true && session.tenantId !== SYSTEM_TENANT_ID) {
        return 'Every tenant shares this catalogue, so only the system tenant changes it.';
    }
    return undefined;
}

function writableFor(
    request: FastifyRequest,
    session: LiveSession,
): {
    readonly resource: SyntheticResource;
    readonly writes: z.ZodType<Record<string, unknown>>;
} {
    const resource = resourceFor(request);
    const because = readOnlyFor(resource, session);
    if (resource.writes === undefined || because !== undefined) {
        throw notPermitted(`${resource.key} is read only here. ${because ?? ''}`);
    }
    return { resource, writes: resource.writes };
}

function keySchema(resource: SyntheticResource): z.ZodType<string> {
    return resource.keyField === 'id' ? idSchema : code;
}

/**
 * Turns a refused write into the status that says why. The server sends no
 * words with some conflicts, so a conflict says what it means.
 */
function answer(result: z.infer<typeof resultSchema>): void {
    if (result.outcome === 'ok') {
        return;
    }
    if (result.outcome === 'conflict') {
        throw conflict(
            result.message === ''
                ? 'A row changed since it was read, or it already exists. Reload and try again.'
                : result.message,
        );
    }
    throw refusal(result);
}

/*
 * An operation answers success and a message. A refused preview is a bad
 * input, a refused start or stop is a conflict with the feed's state, and a
 * refused read is the service failing.
 */
function upstreamFailure(message: string): HttpFailure {
    return new HttpFailure(502, { code: 'upstream-unavailable', message });
}

function conflict(message: string): HttpFailure {
    return new HttpFailure(409, { code: 'conflict', message });
}

function refuseUnless(
    reply: { readonly success: boolean; readonly message: string },
    failure: (message: string) => HttpFailure,
): void {
    if (!reply.success) {
        throw failure(reply.message);
    }
}

/**
 * A save of one or many rows, each with the version the person read. No
 * version claims the row is new.
 */
const saveBodySchema = z.object({
    intent: intentSchema,
    changes: z
        .array(
            z.object({
                write: z.record(z.string(), z.unknown()),
                version: z.int().nonnegative().nullable(),
            }),
        )
        .min(1)
        .max(1000),
});

/**
 * A removal of one or many rows by key. One removal may name the version it
 * read. The server removes a batch in one statement with no per-row version,
 * so a batch names none.
 */
const removeBodySchema = z.object({
    intent: intentSchema,
    removals: z
        .array(
            z.object({
                key: z.string(),
                version: z.int().nonnegative().nullable().default(null),
            }),
        )
        .min(1)
        .max(1000),
});

const operationReplySchema = z.object({
    success: z.boolean(),
    message: z.string().default(''),
});

const pathsReplySchema = operationReplySchema.extend({
    paths: z.array(z.array(z.number())).default([]),
});

const runSize = {
    num_ticks: z.int().positive(),
    num_paths: z.int().positive(),
    seed: z.int().nonnegative(),
};

const parameterSpecs = z
    .array(z.object({ parameter_name: code, parameter_value: z.number() }))
    .max(100);

const fxSimulateSchema = z
    .object({
        gmm_means: z.array(z.number()).min(1).max(100),
        gmm_stdevs: z.array(z.number()).min(1).max(100),
        gmm_weights: z.array(z.number()).min(1).max(100),
        process_type: code,
        initial_price: z.number(),
        ...runSize,
    })
    .refine(
        (request) =>
            request.gmm_means.length === request.gmm_stdevs.length &&
            request.gmm_means.length === request.gmm_weights.length,
        { message: 'The mixture has one mean, one deviation and one weight per component.' },
    );

const irSimulateSchema = z.object({
    process_type: code,
    parameters: parameterSpecs,
    ...runSize,
});

const irShapeSchema = z.object({
    process_type: code,
    parameters: parameterSpecs,
    fixed_leg_payment_frequency_code: z.string().max(100),
    entries: z
        .array(
            z.object({
                sequence_index: z.int().nonnegative(),
                start_tenor_code: code,
                end_tenor_code: code,
                instrument_code: code,
            }),
        )
        .min(1)
        .max(200),
});

/**
 * The synthetic routes: the resources the four synthetic journeys read and
 * write, the previews, and the feed and folder operations.
 *
 * A save of many rows is one call: the server checks every row's version and
 * writes the set in one statement, so a reordered mixture lands whole or not
 * at all.
 */
export function registerSyntheticRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/synthetic', async (request) => {
        const session = requireSession(request);
        return {
            resources: SYNTHETIC_RECORDS.map((resource) => ({
                key: resource.key,
                entityType: resource.entityType,
                keyField: resource.keyField,
                writable: readOnlyFor(resource, session) === undefined,
                readOnlyBecause: readOnlyFor(resource, session) ?? null,
                readPermission: `synthetic::${resourceName(resource)}:read`,
                writePermission: `synthetic::${resourceName(resource)}:write`,
                deletePermission: `synthetic::${resourceName(resource)}:delete`,
            })),
        };
    });

    /** The source names of the feeds running now, of every kind the caller may see. */
    server.get('/api/synthetic/running', async (request) => {
        const session = requireSession(request);
        const reply = await session.client.callAuthenticated(
            feedSubjects.list_feeds_request,
            {},
            z.object({
                success: z.boolean(),
                running_source_names: z.array(z.string()).default([]),
            }),
        );
        refuseUnless(
            { success: reply.success, message: 'The running feeds could not be read.' },
            upstreamFailure,
        );
        return { sourceNames: reply.running_source_names };
    });

    /** Whether each vintage a feed draws from is still valid. */
    server.get('/api/synthetic/vintages', async (request) => {
        const session = requireSession(request);
        const reply = await session.client.callAuthenticated(
            marketdataOperationSubjects.get_vintage_validity_request,
            {},
            operationReplySchema.extend({ entries: z.array(row).default([]) }),
        );
        refuseUnless(reply, upstreamFailure);
        return { entries: reply.entries };
    });

    server.get('/api/synthetic/:resource', async (request) => {
        const session = requireSession(request);
        const resource = resourceFor(request);
        return {
            rows: await readAll(session, resource.subjects.list, resource.rows, {
                filter: null,
                as_of: null,
            }),
        };
    });

    server.get('/api/synthetic/:resource/key/:key', async (request) => {
        const session = requireSession(request);
        const resource = resourceFor(request);
        const key = input(keySchema(resource), (request.params as { key: string }).key);
        const reply = await session.client.callAuthenticated(
            resource.subjects.list,
            {
                offset: 0,
                limit: 1,
                order: NO_ORDER,
                filter: { [resource.oneOf]: [key] },
                as_of: null,
            },
            z.looseObject({ result: resultSchema }),
        );
        if (reply.result.outcome !== 'ok') {
            throw refusal(reply.result);
        }
        const found = z.array(row).default([]).parse(reply[resource.rows])[0];
        if (found === undefined) {
            throw notFound(`There is no ${resource.key} row ${key}.`);
        }
        return { row: found };
    });

    /** Writes one or many rows in one call, and answers with the rows as written. */
    server.put('/api/synthetic/:resource', async (request) => {
        const session = requireSession(request);
        const { resource, writes } = writableFor(request, session);
        const body = input(saveBodySchema, request.body);
        const changes = body.changes.map((change) => ({
            write: input(writes, change.write),
            precondition:
                change.version === null
                    ? { kind: 'must_not_exist', version: null }
                    : { kind: 'must_match_version', version: change.version },
        }));
        const reply = await session.client.callAuthenticated(
            resource.subjects.putMany,
            { changes, intent: body.intent },
            z.looseObject({ result: resultSchema }),
        );
        answer(reply.result);
        return { rows: z.array(row).default([]).parse(reply[resource.rows]) };
    });

    /**
     * Removes one row, refused with 409 when it moved on since the version
     * named, or many rows unconditionally in one statement.
     */
    server.delete('/api/synthetic/:resource', async (request, reply) => {
        const session = requireSession(request);
        const { resource } = writableFor(request, session);
        const body = input(removeBodySchema, request.body);
        const removals = body.removals.map((removal) => ({
            key: { [resource.keyField]: input(keySchema(resource), removal.key) },
            precondition:
                removal.version === null
                    ? { kind: 'any', version: null }
                    : { kind: 'must_match_version', version: removal.version },
        }));
        if (removals.length === 1) {
            const answered = await session.client.callAuthenticated(
                resource.subjects.remove,
                { removal: removals[0], intent: body.intent },
                z.looseObject({ result: resultSchema }),
            );
            answer(answered.result);
            return reply.code(204).send();
        }
        if (body.removals.some((removal) => removal.version !== null)) {
            throw invalidRequest(
                'A removal of many rows is unconditional: it names no version. Remove one row to name its version.',
            );
        }
        const answered = await session.client.callAuthenticated(
            resource.subjects.removeMany,
            { removals, intent: body.intent },
            z.looseObject({ result: resultSchema }),
        );
        answer(answered.result);
        return reply.code(204).send();
    });

    server.post('/api/synthetic/feeds/:id/start', async (request) => {
        const session = requireSession(request);
        const reply = await session.client.callAuthenticated(
            feedSubjects.start_feed_request,
            { config_id: paramId(request) },
            operationReplySchema,
        );
        refuseUnless(reply, conflict);
        return { message: reply.message };
    });

    server.post('/api/synthetic/feeds/:id/stop', async (request) => {
        const session = requireSession(request);
        const reply = await session.client.callAuthenticated(
            feedSubjects.stop_feed_request,
            { config_id: paramId(request), source_name: '' },
            operationReplySchema,
        );
        refuseUnless(reply, conflict);
        return { message: reply.message };
    });

    /**
     * Starts every feed under a folder, its subtree included. The answer
     * counts what started, what was already running and what was skipped; the
     * server does not name the skipped feeds.
     */
    server.post('/api/synthetic/folders/:id/start', async (request) => {
        const session = requireSession(request);
        const counts = z.object({
            started: z.int().default(0),
            already_running: z.int().default(0),
            skipped: z.int().default(0),
        });
        const reply = await session.client.callAuthenticated(
            marketdataOperationSubjects.start_feeds_under_folder_request,
            { folder_id: paramId(request) },
            operationReplySchema.extend({
                ...counts.shape,
                by_kind: z.record(z.string(), counts).default({}),
            }),
        );
        refuseUnless(reply, conflict);
        return {
            message: reply.message,
            started: reply.started,
            alreadyRunning: reply.already_running,
            skipped: reply.skipped,
            byKind: reply.by_kind,
        };
    });

    server.post('/api/synthetic/folders/:id/stop', async (request) => {
        const session = requireSession(request);
        const reply = await session.client.callAuthenticated(
            marketdataOperationSubjects.stop_feeds_under_folder_request,
            { folder_id: paramId(request) },
            operationReplySchema.extend({
                stopped: z.int().default(0),
                stopped_by_kind: z.record(z.string(), z.int()).default({}),
            }),
        );
        refuseUnless(reply, conflict);
        return { message: reply.message, stopped: reply.stopped, byKind: reply.stopped_by_kind };
    });

    /** Sample FX spot paths from the values on the screen, never from a saved feed. */
    server.post('/api/synthetic/simulate/fx', async (request) => {
        const session = requireSession(request);
        const reply = await session.client.callAuthenticated(
            fxSimulateSubjects.simulate_fx_spot_paths_request,
            input(fxSimulateSchema, request.body),
            pathsReplySchema,
        );
        refuseUnless(reply, invalidRequest);
        return { paths: reply.paths };
    });

    /** Sample short-rate paths from the process and parameters on the screen. */
    server.post('/api/synthetic/simulate/ir', async (request) => {
        const session = requireSession(request);
        const reply = await session.client.callAuthenticated(
            irOperationSubjects.simulate_ir_curve_paths_request,
            input(irSimulateSchema, request.body),
            pathsReplySchema,
        );
        refuseUnless(reply, invalidRequest);
        return { paths: reply.paths };
    });

    /** The rate at each ladder row, priced from the process and parameters on the screen. */
    server.post('/api/synthetic/preview/ir-shape', async (request) => {
        const session = requireSession(request);
        const reply = await session.client.callAuthenticated(
            irOperationSubjects.preview_ir_curve_shape_request,
            input(irShapeSchema, request.body),
            operationReplySchema.extend({ points: z.array(row).default([]) }),
        );
        refuseUnless(reply, invalidRequest);
        return { points: reply.points };
    });
}
