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

import { randomUUID } from 'node:crypto';
import type { FastifyInstance, FastifyRequest } from 'fastify';
import { z } from 'zod';
import { readMyParties } from '@ores/wire-protocol';
import { subjects as configSubjects } from '@ores/wire-protocol/generated/analytics/protocol/pricing_model_config_protocol';
import { subjects as productSubjects } from '@ores/wire-protocol/generated/analytics/protocol/pricing_model_product_protocol';
import { subjects as parameterSubjects } from '@ores/wire-protocol/generated/analytics/protocol/pricing_model_product_parameter_protocol';
import { subjects as typeSubjects } from '@ores/wire-protocol/generated/analytics/protocol/pricing_engine_type_protocol';
import { subjects as pairSubjects } from '@ores/wire-protocol/generated/analytics/protocol/pricing_model_engine_pair_protocol';
import { subjects as settingSubjects } from '@ores/wire-protocol/generated/analytics/protocol/pricing_pair_setting_protocol';
import { subjects as documentSubjects } from '@ores/wire-protocol/generated/analytics/protocol/configuration_document_protocol';
import { subjects as configurationSubjects } from '@ores/wire-protocol/generated/reporting/protocol/configuration_protocol';
import { HttpFailure, invalidRequest, notFound, notPermitted } from './errors.js';
import { input, intentSchema, readAll, refusal, resultSchema, text } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/*
 * The pricing engine configurations a party holds, read and written as one
 * document.
 *
 * The analytics service keeps a configuration as three tables: the header, one
 * row for each product and one row for each parameter. The screens never see
 * the split, so a read answers the whole document and a write takes it whole.
 * A write that omits a product or a parameter the earlier version held removes
 * it, which is how the document save of the analytics service behaves.
 *
 * A configuration is bound to a report through a reporting configuration of
 * type pricing_engines. A create therefore writes that header first and the
 * document second, and a failed second step removes the first.
 *
 * Every write carries a reason. The document save has no field for one, so the
 * reason travels on the rows, which the service keeps when a row names one.
 */

const CONFIGURATION_TYPE = 'pricing_engines';
const OWNING_COMPONENT = 'ores.analytics';

/** The value a row carries for an id or a time the service stamps itself. */
const NIL = '00000000-0000-0000-0000-000000000000';
const EPOCH = '1970-01-01 00:00:00.000000Z';

// The seed and the publish from the DQ staging give rows ids that carry no version
// and variant bits, so a strict UUID would refuse a row the service itself holds.
const idSchema = z.guid();

const MAX_PRODUCTS = 500;
const MAX_PARAMETERS = 5000;

const productSchema = z.object({
    id: idSchema,
    version: z.int().min(0).default(0),
    typeCode: z.string().trim().min(1).max(200),
    model: z.string().trim().max(200),
    engine: z.string().trim().max(200),
    position: z.int().min(0),
});

const parameterSchema = z.object({
    id: idSchema,
    version: z.int().min(0).default(0),
    productId: idSchema.nullable(),
    scope: z.string().trim().min(1).max(50),
    name: z.string().trim().min(1).max(200),
    value: z.string().max(2000),
    position: z.int().min(0),
});

const documentBodySchema = z.object({
    name: z.string().trim().min(1).max(200),
    description: z.string().max(2000).default(''),
    variant: z.string().trim().max(200).default(''),
    products: z.array(productSchema).max(MAX_PRODUCTS),
    parameters: z.array(parameterSchema).max(MAX_PARAMETERS),
    intent: intentSchema,
});

const updateBodySchema = documentBodySchema.extend({
    version: z.int().min(1),
});

type DocumentBody = z.infer<typeof documentBodySchema>;

/** The rows of one configuration, as the three tables hold them. */
interface Stored {
    readonly config: Readonly<Record<string, unknown>>;
    readonly products: readonly Readonly<Record<string, unknown>>[];
    readonly parameters: readonly Readonly<Record<string, unknown>>[];
}

/** A party as its own account names it. */
interface PartyName {
    readonly name: string;
    readonly shortCode: string;
}

type PartyNames = ReadonlyMap<string, PartyName>;

/**
 * The parties the session works in, by id. A session of one party holds only
 * its own configurations, but a tenant administrator holds those of every
 * party, and the same name can repeat across them, so a row names its party.
 * A read that fails leaves the names empty and the screen shows the id.
 */
async function readPartyNames(session: LiveSession): Promise<PartyNames> {
    try {
        const mine = await readMyParties(session.client);
        return new Map(
            mine.parties.map((party) => [
                party.partyId,
                { name: party.name, shortCode: party.shortCode },
            ]),
        );
    } catch {
        return new Map();
    }
}

/** The party fields of a row. */
function partyOf(
    config: Readonly<Record<string, unknown>>,
    parties: PartyNames,
): Readonly<Record<string, string>> {
    const partyId = text(config['party_id']);
    const party = parties.get(partyId);
    return { partyId, partyName: party?.name ?? '', partyCode: party?.shortCode ?? '' };
}

function num(value: unknown): number {
    return typeof value === 'number' ? value : 0;
}

/** The configuration with this id, or a not-found. */
async function readStored(session: LiveSession, id: string): Promise<Stored> {
    const configs = await readAll(
        session,
        configSubjects.list_pricing_model_configs_request,
        'configs',
        {
            filter: { configuration_id: null, id_one_of: [id], configuration_id_one_of: null },
            as_of: null,
        },
    );
    const config = configs[0];
    if (config === undefined) {
        throw notFound('No pricing engine configuration has this id.');
    }
    const [products, parameters] = await Promise.all([
        readAll(session, productSubjects.list_pricing_model_products_request, 'products', {
            filter: null,
            as_of: null,
        }),
        readAll(
            session,
            parameterSubjects.list_pricing_model_product_parameters_request,
            'parameters',
            {
                filter: null,
                as_of: null,
            },
        ),
    ]);
    return {
        config,
        products: products.filter((row) => text(row['pricing_model_config_id']) === id),
        parameters: parameters.filter((row) => text(row['pricing_model_config_id']) === id),
    };
}

/** What a screen reads of one configuration. */
function documentOf(stored: Stored, parties: PartyNames): Readonly<Record<string, unknown>> {
    const { config } = stored;
    return {
        id: text(config['id']),
        ...partyOf(config, parties),
        version: num(config['version']),
        name: text(config['name']),
        description: text(config['description']),
        variant: text(config['config_variant']),
        configurationId: text(config['configuration_id']),
        modifiedBy: text(config['modified_by']),
        recordedAt: text(config['recorded_at']),
        changeReason: text(config['change_reason_code']),
        changeCommentary: text(config['change_commentary']),
        products: [...stored.products]
            .sort((a, b) => num(a['position']) - num(b['position']))
            .map((row) => ({
                id: text(row['id']),
                version: num(row['version']),
                typeCode: text(row['pricing_engine_type_code']),
                model: text(row['model']),
                engine: text(row['engine']),
                position: num(row['position']),
            })),
        parameters: [...stored.parameters]
            .sort((a, b) => num(a['position']) - num(b['position']))
            .map((row) => ({
                id: text(row['id']),
                version: num(row['version']),
                productId:
                    typeof row['pricing_model_product_id'] === 'string'
                        ? row['pricing_model_product_id']
                        : null,
                scope: text(row['parameter_scope']),
                name: text(row['parameter_name']),
                value: text(row['parameter_value']),
                position: num(row['position']),
            })),
    };
}

/** The row a list shows for one configuration. */
function summaryOf(
    config: Readonly<Record<string, unknown>>,
    products: number,
    parameters: number,
    parties: PartyNames,
): Readonly<Record<string, unknown>> {
    return {
        id: text(config['id']),
        ...partyOf(config, parties),
        version: num(config['version']),
        name: text(config['name']),
        description: text(config['description']),
        variant: text(config['config_variant']),
        configurationId: text(config['configuration_id']),
        products,
        parameters,
        modifiedBy: text(config['modified_by']),
        recordedAt: text(config['recorded_at']),
    };
}

/** The three tables' rows for a document, in the shape the analytics service saves. */
function rowsOf(
    body: DocumentBody,
    id: string,
    version: number,
    configurationId: string,
): Readonly<Record<string, unknown>> {
    const stamp = {
        tenant_id: NIL,
        party_id: NIL,
        modified_by: '',
        performed_by: '',
        change_reason_code: body.intent.reason_code,
        change_commentary: body.intent.commentary,
        recorded_at: EPOCH,
    };
    return {
        config: {
            ...stamp,
            version,
            id,
            name: body.name,
            description: body.description,
            config_variant: body.variant,
            configuration_id: configurationId,
        },
        products: body.products.map((product) => ({
            ...stamp,
            version: product.version,
            id: product.id,
            pricing_model_config_id: id,
            pricing_engine_type_code: product.typeCode,
            model: product.model,
            engine: product.engine,
            position: product.position,
        })),
        parameters: body.parameters.map((parameter) => ({
            ...stamp,
            version: parameter.version,
            id: parameter.id,
            pricing_model_config_id: id,
            pricing_model_product_id: parameter.productId,
            parameter_scope: parameter.scope,
            parameter_name: parameter.name,
            parameter_value: parameter.value,
            position: parameter.position,
        })),
    };
}

const saveReply = z.looseObject({
    success: z.boolean().default(false),
    message: z.string().default(''),
    id: z.string().default(''),
});

/** Saves the document, or throws what the service said. */
async function save(
    session: LiveSession,
    document: Readonly<Record<string, unknown>>,
): Promise<void> {
    const reply = await session.client.callAuthenticated(
        documentSubjects.save_pricing_engines_document_request,
        { document },
        saveReply,
    );
    if (!reply.success) {
        throw saveFailure(reply.message);
    }
}

/** The status a refused save is told as. A stale version is a conflict, and the rest a bad request. */
function saveFailure(message: string): HttpFailure {
    if (/version[_ ]conflict|does not match|stale/i.test(message)) {
        return new HttpFailure(409, { code: 'conflict', message });
    }
    if (/denied|not permitted|permission/i.test(message)) {
        return notPermitted(message);
    }
    return invalidRequest(message);
}

/** The pick lists a screen draws its combo boxes from. */
async function vocabulary(session: LiveSession): Promise<Readonly<Record<string, unknown>>> {
    const rows = (subject: string, key: string) =>
        readAll(session, subject, key, { filter: null, as_of: null });
    const [types, pairs, settings] = await Promise.all([
        rows(typeSubjects.list_pricing_engine_types_request, 'types'),
        rows(pairSubjects.list_pricing_model_engine_pairs_request, 'pairs'),
        rows(settingSubjects.list_pricing_pair_settings_request, 'settings'),
    ]);
    return {
        types: types.map((row) => ({
            code: text(row['code']),
            description: text(row['description']),
            instrumentTypeCode: text(row['instrument_type_code']),
        })),
        pairs: pairs.map((row) => ({
            typeCode: text(row['pricing_engine_type_code']),
            model: text(row['model']),
            engine: text(row['engine']),
            usual: row['is_usual'] === true,
            note: text(row['note']),
        })),
        settings: settings.map((row) => ({
            typeCode: text(row['pricing_engine_type_code']),
            model: text(row['model']),
            engine: text(row['engine']),
            scope: text(row['scope']),
            name: text(row['name']),
            kind: text(row['kind']),
            usual: text(row['usual_value']),
            options: text(row['options']) === '' ? [] : text(row['options']).split(','),
            note: text(row['note']),
        })),
    };
}

/** The reporting configuration a new document fills, written first. */
async function createHeader(
    session: LiveSession,
    name: string,
    intent: DocumentBody['intent'],
): Promise<string> {
    const id = randomUUID();
    const reply = await session.client.callAuthenticated(
        configurationSubjects.put_configuration_request,
        {
            change: {
                write: {
                    id,
                    name,
                    configuration_type_code: CONFIGURATION_TYPE,
                    owning_component: OWNING_COMPONENT,
                },
                precondition: { kind: 'must_not_exist', version: null },
            },
            intent,
        },
        z.looseObject({ result: resultSchema }),
    );
    if (reply.result.outcome !== 'ok') {
        throw refusal(reply.result);
    }
    return id;
}

/** Removes a header written for a document that did not save. A failure here is not worse than the one that led to it. */
async function removeHeader(
    session: LiveSession,
    name: string,
    intent: DocumentBody['intent'],
): Promise<void> {
    await session.client
        .callAuthenticated(
            configurationSubjects.delete_configuration_request,
            {
                removal: { key: { name }, precondition: { kind: 'any', version: null } },
                intent,
            },
            z.looseObject({ result: resultSchema }),
        )
        .catch(() => undefined);
}

export function registerPricingEngineRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/reporting/pricing-engines', async (request) => {
        const session = requireSession(request);
        const [parties, configs, products, parameters] = await Promise.all([
            readPartyNames(session),
            readAll(session, configSubjects.list_pricing_model_configs_request, 'configs', {
                filter: null,
                as_of: null,
            }),
            readAll(session, productSubjects.list_pricing_model_products_request, 'products', {
                filter: null,
                as_of: null,
            }),
            readAll(
                session,
                parameterSubjects.list_pricing_model_product_parameters_request,
                'parameters',
                {
                    filter: null,
                    as_of: null,
                },
            ),
        ]);
        const count = (rows: readonly Readonly<Record<string, unknown>>[]) => {
            const counts = new Map<string, number>();
            for (const row of rows) {
                const id = text(row['pricing_model_config_id']);
                counts.set(id, (counts.get(id) ?? 0) + 1);
            }
            return counts;
        };
        const productCount = count(products);
        const parameterCount = count(parameters);
        return {
            rows: configs.map((config) =>
                summaryOf(
                    config,
                    productCount.get(text(config['id'])) ?? 0,
                    parameterCount.get(text(config['id'])) ?? 0,
                    parties,
                ),
            ),
        };
    });

    server.get('/api/reporting/pricing-engines/vocabulary', async (request) =>
        vocabulary(requireSession(request)),
    );

    server.get('/api/reporting/pricing-engines/:id', async (request) => {
        const session = requireSession(request);
        const id = input(idSchema, (request.params as { id: string }).id);
        return documentOf(await readStored(session, id), await readPartyNames(session));
    });

    server.post('/api/reporting/pricing-engines', async (request, reply) => {
        const session = requireSession(request);
        const body = input(documentBodySchema, request.body);
        const id = randomUUID();
        const configurationId = await createHeader(session, body.name, body.intent);
        try {
            await save(session, rowsOf(body, id, 0, configurationId));
        } catch (failure) {
            await removeHeader(session, body.name, body.intent);
            throw failure;
        }
        void reply.code(201);
        return documentOf(await readStored(session, id), await readPartyNames(session));
    });

    server.put('/api/reporting/pricing-engines/:id', async (request) => {
        const session = requireSession(request);
        const id = input(idSchema, (request.params as { id: string }).id);
        const body = input(updateBodySchema, request.body);
        const stored = await readStored(session, id);
        await save(
            session,
            rowsOf(body, id, body.version, text(stored.config['configuration_id'])),
        );
        return documentOf(await readStored(session, id), await readPartyNames(session));
    });

    server.delete('/api/reporting/pricing-engines/:id', async (request, reply) => {
        const session = requireSession(request);
        const id = input(idSchema, (request.params as { id: string }).id);
        const stored = await readStored(session, id);
        const configurationId = text(stored.config['configuration_id']);
        const deleted = await session.client.callAuthenticated(
            documentSubjects.delete_pricing_engines_document_request,
            { configuration_id: configurationId },
            z.looseObject({ success: z.boolean().default(false), message: z.string().default('') }),
        );
        if (!deleted.success) {
            throw invalidRequest(deleted.message);
        }
        void reply.code(204);
        return null;
    });
}
