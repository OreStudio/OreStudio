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
import {
    subjects as folderSubjects,
    type FoldersFilter,
} from '@ores/wire-protocol/generated/synthetic/protocol/folder_protocol';
import {
    subjects as fxFeedSubjects,
    type FxSpotGenerationConfigWrite,
    type FxSpotGenerationConfigsFilter,
} from '@ores/wire-protocol/generated/synthetic/protocol/fx_spot_generation_config_protocol';
import {
    subjects as gmmSubjects,
    type GmmComponentWrite,
    type GmmComponentsFilter,
} from '@ores/wire-protocol/generated/synthetic/protocol/gmm_component_protocol';
import {
    subjects as irFeedSubjects,
    type IrCurveGenerationConfigWrite,
    type IrCurveGenerationConfigsFilter,
} from '@ores/wire-protocol/generated/synthetic/protocol/ir_curve_generation_config_protocol';
import { subjects as tenorSubjects } from '@ores/wire-protocol/generated/refdata/protocol/tenor_protocol';
import { subjects as instrumentCodeSubjects } from '@ores/wire-protocol/generated/refdata/protocol/instrument_code_protocol';
import { subjects as paymentFrequencySubjects } from '@ores/wire-protocol/generated/refdata/protocol/payment_frequency_protocol';
import { subjects as irOperationSubjects } from '@ores/wire-protocol/generated/synthetic/protocol/ir_curve_operations_protocol';
import {
    subjects as parameterValueSubjects,
    type IrCurveGenerationConfigProcessParameterValueWrite,
    type IrCurveGenerationConfigProcessParameterValuesFilter,
} from '@ores/wire-protocol/generated/synthetic/protocol/ir_curve_generation_config_process_parameter_value_protocol';
import {
    subjects as templateEntrySubjects,
    type IrCurveTemplateEntryWrite,
    type IrCurveTemplateEntriesFilter,
} from '@ores/wire-protocol/generated/synthetic/protocol/ir_curve_template_entry_protocol';
import {
    subjects as collectionSubjects,
    type MarketDataGenerationConfigsFilter,
} from '@ores/wire-protocol/generated/synthetic/protocol/market_data_generation_config_protocol';
import { subjects as fxConvertSubjects } from '@ores/wire-protocol/generated/synthetic/protocol/convert_fx_mixture_protocol';
import { subjects as fxSimpleSubjects } from '@ores/wire-protocol/generated/synthetic/protocol/convert_fx_simple_protocol';
import { subjects as feedOverviewSubjects } from '@ores/wire-protocol/generated/synthetic/protocol/feed_overview_protocol';
import { subjects as feedSelectionSubjects } from '@ores/wire-protocol/generated/synthetic/protocol/feed_selection_protocol';
import { subjects as irConvertSubjects } from '@ores/wire-protocol/generated/synthetic/protocol/convert_ir_parameters_protocol';
import { subjects as fxSimulateSubjects } from '@ores/wire-protocol/generated/synthetic/protocol/simulate_fx_spot_paths_protocol';
import {
    subjects as definitionSubjects,
    type YieldCurveProcessParameterDefinitionsFilter,
} from '@ores/wire-protocol/generated/synthetic/protocol/yield_curve_process_parameter_definition_protocol';
import {
    subjects as processTypeSubjects,
    type YieldCurveProcessTypeWrite,
    type YieldCurveProcessTypesFilter,
} from '@ores/wire-protocol/generated/synthetic/protocol/yield_curve_process_type_protocol';
import {
    subjects as providerSubjects,
    type MarketDataProvidersFilter,
} from '@ores/wire-protocol/generated/synthetic/protocol/market_data_provider_protocol';
import { subjects as marketdataOperationSubjects } from '@ores/wire-protocol/generated/marketdata/protocol/operations_protocol';
import { HttpFailure, invalidRequest, notFound, notPermitted } from './errors.js';
import {
    NO_ORDER,
    PAGE,
    idSchema,
    input,
    intentSchema,
    pageQuerySchema,
    paramId,
    readAll,
    refusal,
    resultSchema,
    row,
} from './refdata-calls.js';
import { TICK_CAPACITY, type FeedTickBuffers } from './feed-ticks.js';
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
 * A write schema is strict: the party, like the tenant, is the session's and
 * never the browser's, so a write that states it is refused rather than
 * having the field dropped.
 *
 * A system-owned resource is one catalogue every tenant reads. The server
 * reads and writes it in the system tenant, so a write from a tenant session
 * would change every tenant's catalogue; only a system session writes it.
 *
 * `filter` is the list filter with every member empty. The server refuses a
 * filter that leaves a member out, so every filtered read starts from it, and
 * the generated filter type makes the compiler refuse one that misses a member.
 *
 * `search` and `sortable` say what the model made searchable and sortable, so
 * a page asks the server only for what it can answer. `parentField` names the
 * filter that reads the rows of one parent, such as one feed's mixture.
 */
export interface SyntheticResource {
    readonly key: string;
    readonly entityType: string;
    readonly keyField: 'id' | 'code';
    readonly oneOf: 'id_one_of' | 'code_one_of';
    /**
     * The field the server's removal names the row by, when it is not the key field the
     * browser addresses the row by. The route reads the row and names it by this field,
     * so a deletion of a row that does not exist is a 404 and not an unconditional no-op.
     */
    readonly removalKey?: string;
    /**
     * A filter the session adds to every read of the resource, so a row of another party
     * of the same tenant is never answered. It throws when the session names no party.
     */
    readonly scope?: (session: LiveSession) => Readonly<Record<string, unknown>>;
    /**
     * Completes a checked write before it is sent: the members the browser never states,
     * such as the party. It may refuse a write the resource does not allow.
     */
    readonly complete?: (
        session: LiveSession,
        write: Record<string, unknown>,
        read: (id: string) => Promise<Record<string, unknown> | undefined>,
    ) => Promise<Record<string, unknown>>;
    readonly rows: string;
    readonly filter: Readonly<Record<string, null>>;
    readonly subjects: {
        readonly list: string;
        readonly putMany: string;
        readonly remove: string;
        readonly removeMany: string;
    };
    readonly writes?: z.ZodType<Record<string, unknown>>;
    readonly readOnlyBecause?: string;
    readonly systemOwned?: true;
    /** The permission namespace, when the rows belong to another service than the synthetic one. */
    readonly namespace?: 'refdata' | 'marketdata';
    readonly search?: true;
    /**
     * The filter members a page may be narrowed by, named in the address as `f.<member>`.
     * A member outside this list is refused, so a screen cannot reach a filter the model
     * did not make filterable.
     */
    readonly filters?: readonly string[];
    readonly sortable?: readonly string[];
    readonly parentField?: string;
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
        filter: { id_one_of: null } satisfies FoldersFilter,
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
        filter: { id_one_of: null } satisfies MarketDataGenerationConfigsFilter,
        subjects: {
            list: collectionSubjects.list_market_data_generation_configs_request,
            putMany: collectionSubjects.put_many_market_data_generation_configs_request,
            remove: collectionSubjects.delete_market_data_generation_config_request,
            removeMany: collectionSubjects.delete_many_market_data_generation_configs_request,
        },
        readOnlyBecause: 'No journey writes a collection from the web.',
    },
    {
        key: 'providers',
        systemOwned: true,
        entityType: 'ores.synthetic.market_data_provider',
        keyField: 'code',
        oneOf: 'code_one_of',
        rows: 'providers',
        filter: { code_one_of: null } satisfies MarketDataProvidersFilter,
        subjects: {
            list: providerSubjects.list_market_data_providers_request,
            putMany: providerSubjects.put_many_market_data_providers_request,
            remove: providerSubjects.delete_market_data_provider_request,
            removeMany: providerSubjects.delete_many_market_data_providers_request,
        },
        readOnlyBecause: "The providers are the installation's catalogue, seeded with it.",
    },
    {
        key: 'fx-feeds',
        entityType: 'ores.synthetic.fx_spot_generation_config',
        keyField: 'id',
        oneOf: 'id_one_of',
        rows: 'fx_spot_generation_configs',
        filter: { id_one_of: null, search: null } satisfies FxSpotGenerationConfigsFilter,
        search: true,
        sortable: [
            'base_currency_code',
            'quote_currency_code',
            'source_name',
            'oresmd_uri',
            'ticks_per_hour',
        ],
        subjects: {
            list: fxFeedSubjects.list_fx_spot_generation_configs_request,
            putMany: fxFeedSubjects.put_many_fx_spot_generation_configs_request,
            remove: fxFeedSubjects.delete_fx_spot_generation_config_request,
            removeMany: fxFeedSubjects.delete_many_fx_spot_generation_configs_request,
        },
        writes: z.strictObject({
            id: idSchema,
            config_id: idSchema,
            base_currency_code: code,
            quote_currency_code: code,
            source_name: label,
            oresmd_uri: label,
            price_source: code,
            gmm_initial_price: z.number(),
            ticks_per_hour: z.int().positive(),
            process_type: code,
            enabled: z.boolean(),
            auto_start: z.boolean(),
            arrival_mode: code.default('periodic'),
            branching_ratio: z.number().min(0).lt(1).default(0),
            decay: z.number().positive().default(1),
            sensitivity: z.number().positive().default(1),
            vintage_source: z.string().max(200),
            vintage_date: z.string().max(40),
            folder_id: idSchema.nullable(),
            asset_class_code: code.default('fx'),
        }) satisfies z.ZodType<FxSpotGenerationConfigWrite>,
    },
    {
        key: 'gmm-components',
        entityType: 'ores.synthetic.gmm_component',
        keyField: 'id',
        oneOf: 'id_one_of',
        rows: 'gmm_components',
        filter: {
            fx_spot_config_id: null,
            id_one_of: null,
            fx_spot_config_id_one_of: null,
        } satisfies GmmComponentsFilter,
        parentField: 'fx_spot_config_id',
        subjects: {
            list: gmmSubjects.list_gmm_components_request,
            putMany: gmmSubjects.put_many_gmm_components_request,
            remove: gmmSubjects.delete_gmm_component_request,
            removeMany: gmmSubjects.delete_many_gmm_components_request,
        },
        writes: z.strictObject({
            id: idSchema,
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
        filter: { id_one_of: null, search: null } satisfies IrCurveGenerationConfigsFilter,
        subjects: {
            list: irFeedSubjects.list_ir_curve_generation_configs_request,
            putMany: irFeedSubjects.put_many_ir_curve_generation_configs_request,
            remove: irFeedSubjects.delete_ir_curve_generation_config_request,
            removeMany: irFeedSubjects.delete_many_ir_curve_generation_configs_request,
        },
        writes: z.strictObject({
            id: idSchema,
            config_id: idSchema,
            currency_code: code,
            index_family: code,
            tenor: z.string().max(20),
            role: code,
            process_type: code,
            ticks_per_hour: z.int().positive(),
            enabled: z.boolean(),
            auto_start: z.boolean(),
            arrival_mode: code.default('periodic'),
            branching_ratio: z.number().min(0).lt(1).default(0),
            decay: z.number().positive().default(1),
            sensitivity: z.number().positive().default(1),
            price_source: code,
            vintage_source: z.string().max(200),
            vintage_date: z.string().max(40),
            vintage_series_uri: z.string().max(2000),
            description: text,
            fixed_leg_payment_frequency_code: z.string().max(100),
            source_name: label,
            folder_id: idSchema.nullable(),
            asset_class_code: code.default('interest_rates'),
        }) satisfies z.ZodType<IrCurveGenerationConfigWrite>,
    },
    {
        key: 'ir-parameter-values',
        entityType: 'ores.synthetic.ir_curve_generation_config_process_parameter_value',
        keyField: 'id',
        oneOf: 'id_one_of',
        rows: 'process_parameter_values',
        filter: {
            config_id: null,
            id_one_of: null,
            config_id_one_of: null,
        } satisfies IrCurveGenerationConfigProcessParameterValuesFilter,
        parentField: 'config_id',
        subjects: {
            list: parameterValueSubjects.list_ir_curve_generation_config_process_parameter_values_request,
            putMany:
                parameterValueSubjects.put_many_ir_curve_generation_config_process_parameter_values_request,
            remove: parameterValueSubjects.delete_ir_curve_generation_config_process_parameter_value_request,
            removeMany:
                parameterValueSubjects.delete_many_ir_curve_generation_config_process_parameter_values_request,
        },
        writes: z.strictObject({
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
        filter: {
            ir_curve_config_id: null,
            id_one_of: null,
            ir_curve_config_id_one_of: null,
        } satisfies IrCurveTemplateEntriesFilter,
        parentField: 'ir_curve_config_id',
        subjects: {
            list: templateEntrySubjects.list_ir_curve_template_entries_request,
            putMany: templateEntrySubjects.put_many_ir_curve_template_entries_request,
            remove: templateEntrySubjects.delete_ir_curve_template_entry_request,
            removeMany: templateEntrySubjects.delete_many_ir_curve_template_entries_request,
        },
        writes: z.strictObject({
            id: idSchema,
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
        filter: { code_one_of: null } satisfies YieldCurveProcessTypesFilter,
        subjects: {
            list: processTypeSubjects.list_yield_curve_process_types_request,
            putMany: processTypeSubjects.put_many_yield_curve_process_types_request,
            remove: processTypeSubjects.delete_yield_curve_process_type_request,
            removeMany: processTypeSubjects.delete_many_yield_curve_process_types_request,
        },
        writes: z.strictObject({
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
        filter: { id_one_of: null } satisfies YieldCurveProcessParameterDefinitionsFilter,
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
    ...[
        {
            key: 'tenors',
            entityType: 'ores.refdata.tenor',
            rows: 'tenors',
            subjects: {
                list: tenorSubjects.list_tenors_request,
                putMany: tenorSubjects.put_many_tenors_request,
                remove: tenorSubjects.delete_tenor_request,
                removeMany: tenorSubjects.delete_many_tenors_request,
            },
        },
        {
            key: 'instrument-codes',
            entityType: 'ores.refdata.instrument_code',
            rows: 'instruments',
            subjects: {
                list: instrumentCodeSubjects.list_instrument_codes_request,
                putMany: instrumentCodeSubjects.put_many_instrument_codes_request,
                remove: instrumentCodeSubjects.delete_instrument_code_request,
                removeMany: instrumentCodeSubjects.delete_many_instrument_codes_request,
            },
        },
        {
            key: 'payment-frequencies',
            entityType: 'ores.refdata.payment_frequency',
            rows: 'payment_frequencies',
            subjects: {
                list: paymentFrequencySubjects.list_payment_frequencies_request,
                putMany: paymentFrequencySubjects.put_many_payment_frequencies_request,
                remove: paymentFrequencySubjects.delete_payment_frequency_request,
                removeMany: paymentFrequencySubjects.delete_many_payment_frequencies_request,
            },
        },
    ].map((lookup): SyntheticResource => ({
        ...lookup,
        keyField: 'code',
        oneOf: 'code_one_of',
        filter: { code_one_of: null },
        namespace: 'refdata',
        readOnlyBecause:
            'These are reference data of the refdata service. A synthetic screen offers their codes and does not write them.',
    })),
];

/** The synthetic entity types whose history the history route serves. */
export const SYNTHETIC_HISTORY_TYPES: readonly string[] = SYNTHETIC_RECORDS.map(
    (resource) => resource.entityType,
);

/** The name a resource's subjects and permissions share, such as `gmm_components`. */
function resourceName(resource: SyntheticResource): string {
    return resource.subjects.list.split('.')[2] ?? '';
}

/** The resources whose rows are feeds, which the tick read reads a feed through. */
const FEED_KEYS = ['fx-feeds', 'ir-curve-feeds'] as const;

const FEED_RESOURCES = new Map(
    FEED_KEYS.map((key): [string, SyntheticResource] => {
        const found = SYNTHETIC_RECORDS.find((candidate) => candidate.key === key);
        if (found === undefined) {
            throw new Error(`The synthetic resources have no ${key}.`);
        }
        return [key, found];
    }),
);

function resourceFor(request: FastifyRequest): SyntheticResource {
    const { resource } = request.params as { resource: string };
    const found = SYNTHETIC_RECORDS.find((candidate) => candidate.key === resource);
    if (found === undefined) {
        throw notFound(`There is no synthetic resource ${resource}.`);
    }
    return found;
}

/** One row by its key, read as the session, or a 404 when the session cannot see it. */
async function readByKey(
    session: LiveSession,
    resource: SyntheticResource,
    key: string,
): Promise<Record<string, unknown>> {
    const reply = await session.client.callAuthenticated(
        resource.subjects.list,
        {
            offset: 0,
            limit: 1,
            order: NO_ORDER,
            filter: { ...resource.filter, ...resource.scope?.(session), [resource.oneOf]: [key] },
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
    return found;
}

/**
 * The provider the collection a feed sits under stands for. The feed's own
 * record names no provider, so a read of one feed takes it from its collection.
 */
async function providerOfFeed(
    session: LiveSession,
    feed: Record<string, unknown>,
): Promise<string> {
    const collections = SYNTHETIC_RECORDS.find((candidate) => candidate.key === 'collections');
    if (collections === undefined) {
        throw new Error('The synthetic resources have no collections.');
    }
    const collection = await readByKey(session, collections, z.string().parse(feed['config_id']));
    return z.string().parse(collection['provider_code']);
}

/**
 * The source names of the feeds the service runs now, demanded or manual.
 *
 * A feed row says whether it runs by this set alone: the service starts a generator for
 * a feed when an adapter demands it or when an operator starts it, and the set names
 * every generator that runs.
 */
async function runningSourceNames(session: LiveSession): Promise<ReadonlySet<string>> {
    const reply = await session.client.callAuthenticated(
        feedSubjects.list_feeds_request,
        {},
        operationReplySchema.extend({ running_source_names: z.array(z.string()).default([]) }),
    );
    refuseUnless(reply, upstreamFailure);
    return new Set(reply.running_source_names);
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

/** The filter members a page request names as `f.<member>`, refused unless the resource allows them. */
function narrowingOf(
    resource: SyntheticResource,
    query: Record<string, unknown>,
): Record<string, string> {
    const narrowed: Record<string, string> = {};
    for (const [name, value] of Object.entries(query)) {
        if (!name.startsWith('f.')) {
            continue;
        }
        const member = name.slice(2);
        if (!(resource.filters ?? []).includes(member)) {
            throw invalidRequest(`${resource.key} cannot be narrowed by ${member}.`);
        }
        narrowed[member] = input(z.string().trim().min(1).max(100), value);
    }
    return narrowed;
}

function keySchema(resource: SyntheticResource): z.ZodType<string> {
    return resource.keyField === 'id' ? idSchema : code;
}

/** A batch that names one row twice is refused, so its outcome never depends on the order. */
function refuseRepeatedKeys(keys: readonly unknown[], verb: string): void {
    if (new Set(keys).size !== keys.length) {
        throw invalidRequest(`A ${verb} names each row once.`);
    }
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

/**
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
        .max(PAGE),
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
        .max(PAGE),
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

/** The engines an FX feed runs, which the conversion reads between. */
const fxEngine = z.enum(['geometric', 'arithmetic', 'ornstein_uhlenbeck']);

const fxConvertSchema = z
    .object({
        gmm_means: z.array(z.number()).max(100),
        gmm_stdevs: z.array(z.number()).max(100),
        gmm_weights: z.array(z.number()).max(100),
        from_process_type: fxEngine,
        to_process_type: fxEngine,
        initial_price: z.number().positive(),
        ticks_per_hour: z.int().positive(),
    })
    .strict()
    .refine(
        (request) =>
            request.gmm_means.length === request.gmm_stdevs.length &&
            request.gmm_means.length === request.gmm_weights.length,
        { message: 'The mixture has one mean, one deviation and one weight per component.' },
    );

const mixtureReplySchema = operationReplySchema.extend({
    gmm_means: z.array(z.number()).default([]),
    gmm_stdevs: z.array(z.number()).default([]),
    gmm_weights: z.array(z.number()).default([]),
});

const fxSimpleSchema = z.object({
    process_type: fxEngine,
    to_simple: z.boolean(),
    initial_price: z.number().positive(),
    ticks_per_hour: z.int().positive(),
    gmm_means: z.array(z.number()).max(100).default([]),
    gmm_stdevs: z.array(z.number()).max(100).default([]),
    gmm_weights: z.array(z.number()).max(100).default([]),
    trend_pct_per_hour: z.number().default(0),
    volatility_pct_per_hour: z.number().default(0),
    jump_share_pct: z.number().default(0),
    half_life_minutes: z.number().default(0),
});

const fxSimpleReplySchema = mixtureReplySchema.extend({
    has_simple_form: z.boolean().default(false),
    trend_pct_per_hour: z.number().default(0),
    volatility_pct_per_hour: z.number().default(0),
    jump_share_pct: z.number().default(0),
    half_life_minutes: z.number().default(0),
});

const irSimulateSchema = z.object({
    process_type: code,
    parameters: parameterSpecs,
    ...runSize,
});

const irConvertSchema = z.object({
    process_type: code,
    to_simple: z.boolean(),
    values: z.record(z.string().max(100), z.number()),
});

const irConvertReplySchema = operationReplySchema.extend({
    has_simple_form: z.boolean().default(false),
    caution: z.string().default(''),
    parameter_names: z.array(z.string()).default([]),
    parameter_values: z.array(z.number()).default([]),
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

/** What narrows the feed table: all optional, and all checked again by the service. */
const feedFilterSchema = z.object({
    folder: z.string().trim().max(100).default(''),
    provider: z.string().trim().max(64).default(''),
    assetClass: z.string().trim().max(64).default(''),
    state: z.enum(['', 'all', 'started', 'stopped']).default(''),
    shouldRun: z.stringbool().default(false),
    search: z.string().trim().max(200).default(''),
});

interface FeedFilter {
    readonly folder: string;
    readonly provider: string;
    readonly assetClass: string;
    readonly state: string;
    readonly shouldRun: boolean;
    readonly search: string;
}

function wireFilter(filter: FeedFilter): Record<string, unknown> {
    return {
        folder_id: filter.folder,
        provider_code: filter.provider,
        asset_class: filter.assetClass,
        state: filter.state,
        should_run: filter.shouldRun,
        search: filter.search,
    };
}

/** The feeds to start or stop: the picked ones, or the filter's less the left out. */
const selectionSchema = feedFilterSchema
    .extend({
        shouldRun: z.boolean().default(false),
        feedIds: z.array(z.string().max(40)).max(2000).default([]),
        excludedIds: z.array(z.string().max(40)).max(2000).default([]),
    })
    .strict();

function wireSelection(
    selection: FeedFilter & { readonly feedIds: string[]; readonly excludedIds: string[] },
): Record<string, unknown> {
    return {
        ...wireFilter(selection),
        feed_ids: selection.feedIds,
        excluded_ids: selection.excludedIds,
    };
}

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
    ticks: FeedTickBuffers,
): void {
    server.get('/api/synthetic', async (request) => {
        const session = requireSession(request);
        return {
            resources: SYNTHETIC_RECORDS.map((resource) => {
                const because = readOnlyFor(resource, session);
                return {
                    key: resource.key,
                    entityType: resource.entityType,
                    keyField: resource.keyField,
                    writable: because === undefined,
                    readOnlyBecause: because ?? null,
                    search: resource.search === true,
                    sortable: resource.sortable ?? [],
                    readPermission: `${resource.namespace ?? 'synthetic'}::${resourceName(resource)}:read`,
                    writePermission: `${resource.namespace ?? 'synthetic'}::${resourceName(resource)}:write`,
                    deletePermission: `${resource.namespace ?? 'synthetic'}::${resourceName(resource)}:delete`,
                };
            }),
        };
    });

    /**
     * The children of one folder, or the roots, with the feeds under each at any
     * depth and how many of each kind. The service counts, so no screen reads
     * the feeds.
     */
    server.get('/api/synthetic/feed-folders', async (request) => {
        const session = requireSession(request);
        const query = input(
            z.object({ parent: z.string().trim().max(100).default('') }).strict(),
            request.query,
        );
        const reply = await session.client.callAuthenticated(
            feedOverviewSubjects.list_feed_folders_request,
            { parent_id: query.parent },
            operationReplySchema.extend({
                folders: z
                    .array(
                        z.object({
                            id: z.string(),
                            parent_id: z.string(),
                            name: z.string(),
                            kind: z.string(),
                            provider_code: z.string(),
                            fx_feeds: z.int(),
                            ir_feeds: z.int(),
                            has_children: z.boolean(),
                        }),
                    )
                    .default([]),
            }),
        );
        refuseUnless(reply, upstreamFailure);
        return { folders: reply.folders };
    });

    /**
     * A page of the feeds a filter matches: a folder with the folders under it, a
     * provider, an asset class, a state, some words. The service pages and counts, so
     * the feed table never reads them all.
     */
    server.get('/api/synthetic/folder-feeds', async (request) => {
        const session = requireSession(request);
        const query = input(
            feedFilterSchema
                .extend({
                    offset: z.coerce.number().int().min(0).max(1_000_000).default(0),
                    limit: z.coerce.number().int().min(1).max(200).default(60),
                })
                .strict(),
            request.query,
        );
        const reply = await session.client.callAuthenticated(
            feedOverviewSubjects.list_folder_feeds_request,
            {
                ...wireFilter(query),
                offset: query.offset,
                limit: query.limit,
            },
            operationReplySchema.extend({
                total: z.int().default(0),
                feeds: z
                    .array(
                        z.object({
                            id: z.string(),
                            kind: z.string(),
                            label: z.string(),
                            source_name: z.string(),
                            oresmd_uri: z.string(),
                            folder_id: z.string(),
                            enabled: z.boolean(),
                            provider_code: z.string(),
                            asset_class_code: z.string(),
                            underlying: z.string(),
                            engine: z.string(),
                            ticks_per_hour: z.int(),
                            auto_start: z.boolean(),
                        }),
                    )
                    .default([]),
            }),
        );
        refuseUnless(reply, invalidRequest);
        const running = await runningSourceNames(session);
        return {
            total: reply.total,
            feeds: reply.feeds.map((feed) => ({
                ...feed,
                running: running.has(feed.source_name),
            })),
        };
    });

    /**
     * Starts the feeds a person picked, or the feeds a filter matches less those left
     * out. The request carries the filter and the ids; the service finds the feeds.
     */
    server.post('/api/synthetic/feed-selection/start', async (request) => {
        const session = requireSession(request);
        const body = input(selectionSchema, request.body);
        const reply = await session.client.callAuthenticated(
            feedSelectionSubjects.start_selected_feeds_request,
            wireSelection(body),
            operationReplySchema.extend({
                started: z.int().default(0),
                already_running: z.int().default(0),
                skipped: z.int().default(0),
            }),
        );
        refuseUnless(reply, invalidRequest);
        return {
            message: reply.message,
            started: reply.started,
            alreadyRunning: reply.already_running,
            skipped: reply.skipped,
        };
    });

    server.post('/api/synthetic/feed-selection/stop', async (request) => {
        const session = requireSession(request);
        const body = input(selectionSchema, request.body);
        const reply = await session.client.callAuthenticated(
            feedSelectionSubjects.stop_selected_feeds_request,
            wireSelection(body),
            operationReplySchema.extend({ stopped: z.int().default(0) }),
        );
        refuseUnless(reply, invalidRequest);
        return { message: reply.message, stopped: reply.stopped };
    });

    /**
     * The newest ticks of one feed's current run, oldest first. The page says
     * which kind of feed it reads, so the feed is read through that kind's
     * resource alone. The feed is read as the session first, so a feed the
     * session cannot see has no ticks to show. The read states the set of feeds
     * the service publishes, so a buffer lasts one run.
     */
    server.get('/api/synthetic/feeds/:id/ticks', async (request) => {
        const session = requireSession(request);
        const { limit, resource } = input(
            z.object({
                limit: z.coerce.number().int().min(1).max(TICK_CAPACITY).default(200),
                resource: z.enum(FEED_KEYS).default('fx-feeds'),
            }),
            request.query,
        );
        const feeds = FEED_RESOURCES.get(resource);
        if (feeds === undefined) {
            throw notFound(`There is no feed resource ${resource}.`);
        }
        const feed = await readByKey(session, feeds, paramId(request));
        const sourceName = z.string().parse(feed['source_name']);
        ticks.watch(sourceName);
        return { sourceName, ticks: ticks.ticks(sourceName, limit) };
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

    /**
     * Every row of a resource, or one page of it with the server's total,
     * searched and ordered on the server, when the request names a limit.
     */
    server.get('/api/synthetic/:resource', async (request) => {
        const session = requireSession(request);
        const resource = resourceFor(request);
        const query = request.query as Record<string, unknown>;
        if (query['limit'] === undefined) {
            return {
                rows: await readAll(session, resource.subjects.list, resource.rows, {
                    filter:
                        resource.scope === undefined
                            ? null
                            : { ...resource.filter, ...resource.scope(session) },
                    as_of: null,
                }),
            };
        }
        const page = input(pageQuerySchema, query);
        if (page.sort !== '' && !(resource.sortable ?? []).includes(page.sort)) {
            throw invalidRequest(`${resource.key} cannot be ordered by ${page.sort}.`);
        }
        if (page.search !== '' && resource.search !== true) {
            throw invalidRequest(`${resource.key} cannot be searched.`);
        }
        const narrowed = narrowingOf(resource, query);
        const reply = await session.client.callAuthenticated(
            resource.subjects.list,
            {
                offset: page.offset,
                limit: page.limit,
                order: { field: page.sort, descending: page.descending },
                filter:
                    page.search === '' &&
                    resource.scope === undefined &&
                    Object.keys(narrowed).length === 0
                        ? null
                        : {
                              ...resource.filter,
                              ...resource.scope?.(session),
                              ...narrowed,
                              ...(page.search === '' ? {} : { search: page.search }),
                          },
                as_of: null,
            },
            z.looseObject({ result: resultSchema, total: z.int().nonnegative().default(0) }),
        );
        if (reply.result.outcome !== 'ok') {
            throw refusal(reply.result);
        }
        return { rows: z.array(row).default([]).parse(reply[resource.rows]), total: reply.total };
    });

    /** The rows that belong to one parent, such as one feed's mixture. */
    server.get('/api/synthetic/:resource/by/:parent', async (request) => {
        const session = requireSession(request);
        const resource = resourceFor(request);
        if (resource.parentField === undefined) {
            throw notFound(`${resource.key} is not read by a parent.`);
        }
        const parent = input(idSchema, (request.params as { parent: string }).parent);
        return {
            rows: await readAll(session, resource.subjects.list, resource.rows, {
                filter: { ...resource.filter, [resource.parentField]: parent },
                as_of: null,
            }),
        };
    });

    /**
     * One row by its key. A feed's record carries no provider, so a read of one
     * feed names the provider of the collection it sits under, as the feed table
     * names a feed's provider.
     */
    server.get('/api/synthetic/:resource/key/:key', async (request) => {
        const session = requireSession(request);
        const resource = resourceFor(request);
        const key = input(keySchema(resource), (request.params as { key: string }).key);
        const found = await readByKey(session, resource, key);
        if (!FEED_RESOURCES.has(resource.key)) {
            return { row: found };
        }
        const provider = await providerOfFeed(session, found);
        const running = await runningSourceNames(session);
        return {
            row: {
                ...found,
                provider_code: provider,
                running: running.has(z.string().parse(found['source_name'])),
            },
        };
    });

    /** Writes one or many rows in one call, and answers with the rows as written. */
    server.put('/api/synthetic/:resource', async (request) => {
        const session = requireSession(request);
        const { resource, writes } = writableFor(request, session);
        const body = input(saveBodySchema, request.body);
        const readRow = async (id: string): Promise<Record<string, unknown> | undefined> => {
            try {
                return await readByKey(session, resource, id);
            } catch (error) {
                if (error instanceof HttpFailure && error.status === 404) {
                    return undefined;
                }
                throw error;
            }
        };
        const changes = await Promise.all(
            body.changes.map(async (change) => {
                const checked = input(writes, change.write);
                return {
                    write:
                        resource.complete === undefined
                            ? checked
                            : await resource.complete(session, checked, readRow),
                    precondition:
                        change.version === null
                            ? { kind: 'must_not_exist', version: null }
                            : { kind: 'must_match_version', version: change.version },
                };
            }),
        );
        refuseRepeatedKeys(
            changes.map((change) => change.write[resource.keyField]),
            'save',
        );
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
        const removals = await Promise.all(
            body.removals.map(async (removal) => {
                const key = input(keySchema(resource), removal.key);
                const named =
                    resource.removalKey === undefined
                        ? key
                        : String((await readByKey(session, resource, key))[resource.removalKey]);
                return {
                    key: { [resource.removalKey ?? resource.keyField]: named },
                    precondition:
                        removal.version === null
                            ? { kind: 'any', version: null }
                            : { kind: 'must_match_version', version: removal.version },
                };
            }),
        );
        refuseRepeatedKeys(
            body.removals.map((removal) => removal.key),
            'removal',
        );
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

    /**
     * The mixture an engine change leaves, converted by the synthetic service,
     * which knows how each engine reads the numbers. Nothing is stored.
     */
    server.post('/api/synthetic/convert/fx', async (request) => {
        const session = requireSession(request);
        const reply = await session.client.callAuthenticated(
            fxConvertSubjects.convert_fx_mixture_request,
            input(fxConvertSchema, request.body),
            mixtureReplySchema,
        );
        refuseUnless(reply, invalidRequest);
        return {
            gmm_means: reply.gmm_means,
            gmm_stdevs: reply.gmm_stdevs,
            gmm_weights: reply.gmm_weights,
        };
    });

    /**
     * An FX mixture read as the simple view, or the simple view written as a
     * mixture, converted by the synthetic service. Nothing is stored.
     */
    server.post('/api/synthetic/convert/fx-simple', async (request) => {
        const session = requireSession(request);
        const reply = await session.client.callAuthenticated(
            fxSimpleSubjects.convert_fx_simple_request,
            input(fxSimpleSchema, request.body),
            fxSimpleReplySchema,
        );
        refuseUnless(reply, invalidRequest);
        const { success: _success, ...answer } = reply;
        return answer;
    });

    /**
     * The parameters of an IR engine in the terms of the simple view, or the
     * simple view in the terms of the engine, converted by the synthetic
     * service. Nothing is stored.
     */
    server.post('/api/synthetic/convert/ir', async (request) => {
        const session = requireSession(request);
        const body = input(irConvertSchema, request.body);
        const reply = await session.client.callAuthenticated(
            irConvertSubjects.convert_ir_parameters_request,
            {
                process_type: body.process_type,
                to_simple: body.to_simple,
                parameter_names: Object.keys(body.values),
                parameter_values: Object.values(body.values),
            },
            irConvertReplySchema,
        );
        refuseUnless(reply, invalidRequest);
        return {
            has_simple_form: reply.has_simple_form,
            message: reply.message,
            caution: reply.caution,
            values: Object.fromEntries(
                reply.parameter_names.map((name, i) => [name, reply.parameter_values[i] ?? 0]),
            ),
        };
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
