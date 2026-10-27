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
import { readEntityHistory, readMyParties } from '@ores/wire-protocol';
import { subjects as configSubjects } from '@ores/wire-protocol/generated/analytics/protocol/pricing_model_config_protocol';
import { subjects as productSubjects } from '@ores/wire-protocol/generated/analytics/protocol/pricing_model_product_protocol';
import { subjects as parameterSubjects } from '@ores/wire-protocol/generated/analytics/protocol/pricing_model_product_parameter_protocol';
import { subjects as typeSubjects } from '@ores/wire-protocol/generated/analytics/protocol/pricing_engine_type_protocol';
import { subjects as pairSubjects } from '@ores/wire-protocol/generated/analytics/protocol/pricing_model_engine_pair_protocol';
import { subjects as settingSubjects } from '@ores/wire-protocol/generated/analytics/protocol/pricing_pair_setting_protocol';
import { subjects as modelSubjects } from '@ores/wire-protocol/generated/analytics/protocol/pricing_model_protocol';
import { subjects as methodSubjects } from '@ores/wire-protocol/generated/analytics/protocol/pricing_method_protocol';
import { subjects as templateSubjects } from '@ores/wire-protocol/generated/analytics/protocol/sensitivity_template_protocol';
import { subjects as templateMethodSubjects } from '@ores/wire-protocol/generated/analytics/protocol/sensitivity_method_protocol';
import { subjects as templateClassSubjects } from '@ores/wire-protocol/generated/analytics/protocol/sensitivity_asset_class_protocol';
import { subjects as tenorSubjects } from '@ores/wire-protocol/generated/refdata/protocol/tenor_protocol';
import { subjects as instrumentCodeSubjects } from '@ores/wire-protocol/generated/refdata/protocol/instrument_code_protocol';
import { subjects as bindingSubjects } from '@ores/wire-protocol/generated/reporting/protocol/report_configuration_protocol';
import { subjects as definitionSubjects } from '@ores/wire-protocol/generated/reporting/protocol/report_definition_protocol';
import { subjects as oreParameterSubjects } from '@ores/wire-protocol/generated/reporting/protocol/ore_parameter_protocol';
import { subjects as valueSetSubjects } from '@ores/wire-protocol/generated/reporting/protocol/ore_parameter_value_set_protocol';
import { subjects as valueSetValueSubjects } from '@ores/wire-protocol/generated/reporting/protocol/ore_parameter_value_set_value_protocol';
import { subjects as valueSetCatalogueSubjects } from '@ores/wire-protocol/generated/reporting/protocol/ore_parameter_value_set_catalogue_protocol';
import { subjects as settingValueSetSubjects } from '@ores/wire-protocol/generated/reporting/protocol/ore_parameter_setting_value_set_protocol';
import { subjects as documentSubjects } from '@ores/wire-protocol/generated/analytics/protocol/configuration_document_protocol';
import { subjects as configurationSubjects } from '@ores/wire-protocol/generated/reporting/protocol/configuration_protocol';
import { subjects as orePricingSubjects } from '@ores/wire-protocol/generated/ore/protocol/pricing_engines_protocol';
import { HttpFailure, invalidRequest, notFound, notPermitted, unexpectedReply } from './errors.js';
import { input, intentSchema, readAll, refusal, resultSchema, text } from './refdata-calls.js';
import { readValueSets, type ValueFields } from './ore-value-sets.js';
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

/** The export's answer: the files the ORE service wrote and the reasons a refusal gives. */
const oreExportSchema = z.looseObject({
    success: z.boolean().default(false),
    message: z.string().default(''),
    count: z.int().default(0),
    files: z.array(z.object({ name: z.string(), content: z.string() })).default([]),
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

/** The reports that bind a configuration, by the binding that names it. */
async function readBoundReports(
    session: LiveSession,
    configurationId: string,
): Promise<readonly Readonly<Record<string, unknown>>[]> {
    const bindings = await readAll(
        session,
        bindingSubjects.list_report_configurations_request,
        'report_configurations',
        {
            filter: {
                report_definition_id: null,
                configuration_type_code: 'pricing_engines',
                configuration_id: configurationId,
                id_one_of: null,
                report_definition_id_one_of: null,
                configuration_type_code_one_of: null,
                configuration_id_one_of: null,
            },
            as_of: null,
        },
    );
    const ids = [...new Set(bindings.map((binding) => text(binding['report_definition_id'])))];
    if (ids.length === 0) {
        return [];
    }
    const definitions = await readAll(
        session,
        definitionSubjects.list_report_definitions_request,
        'definitions',
        {
            filter: { party_id: null, id_one_of: ids, party_id_one_of: null, search: null },
            as_of: null,
        },
    );
    return definitions.map((definition) => ({
        id: text(definition['id']),
        name: text(definition['name']),
    }));
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

/**
 * The three tables' rows for a document, in the shape the analytics service saves.
 *
 * The tenant is the session's, because the service's tenant type refuses the nil
 * uuid, and the row the service stamps with the session's tenant is the same row.
 */
function rowsOf(
    body: DocumentBody,
    id: string,
    version: number,
    configurationId: string,
    tenantId: string,
): Readonly<Record<string, unknown>> {
    const stamp = {
        tenant_id: tenantId,
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

/** One reason the service refused a document, as the service states it. */
const problemReply = z.looseObject({
    code: z.string(),
    product_id: z.string().default(''),
    parameter_scope: z.string().default(''),
    parameter_name: z.string().default(''),
    message: z.string().default(''),
});

/** One reason the service refused a document, as the browser reads it. */
export interface DocumentProblem {
    readonly code: string;
    /** The product the problem is about, empty when it is about no product. */
    readonly productId: string;
    readonly parameterScope: string;
    readonly parameterName: string;
    readonly message: string;
}

const saveReply = z.looseObject({
    success: z.boolean().default(false),
    message: z.string().default(''),
    id: z.string().default(''),
    problems: z.array(problemReply).default([]),
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
        throw saveFailure(
            reply.message,
            reply.problems.map((problem) => ({
                code: problem.code,
                productId: problem.product_id,
                parameterScope: problem.parameter_scope,
                parameterName: problem.parameter_name,
                message: problem.message,
            })),
        );
    }
}

/**
 * The status a refused save is told as. A stale version is a conflict, and the
 * rest a bad request, which carries the problems the document must fix.
 */
function saveFailure(message: string, problems: readonly DocumentProblem[]): HttpFailure {
    if (/version[_ ]conflict|does not match|stale/i.test(message)) {
        return new HttpFailure(409, { code: 'conflict', message });
    }
    if (/denied|not permitted|permission/i.test(message)) {
        return notPermitted(message);
    }
    return invalidRequest(message, problems.length > 0 ? { problems } : {});
}

/** A tenor a time setting offers: a count of days, weeks, months or years. */
const PERIOD_UNITS = new Set(['DAY', 'WEEK', 'MONTH', 'YEAR']);
const PERIOD_CODE = /^\d+[DWMY]$/;

/** The pick lists a screen draws its combo boxes from. */
async function vocabulary(session: LiveSession): Promise<Readonly<Record<string, unknown>>> {
    const rows = (subject: string, key: string) =>
        readAll(session, subject, key, { filter: null, as_of: null });
    const [
        types,
        pairs,
        settings,
        models,
        methods,
        templates,
        templateMethods,
        templateClasses,
        tenors,
        instruments,
        docs,
        valueSetRows,
        valueSetValueRows,
        catalogueRows,
        linkRows,
    ] = await Promise.all([
        rows(typeSubjects.list_pricing_engine_types_request, 'types'),
        rows(pairSubjects.list_pricing_model_engine_pairs_request, 'pairs'),
        rows(settingSubjects.list_pricing_pair_settings_request, 'settings'),
        rows(modelSubjects.list_pricing_models_request, 'models'),
        rows(methodSubjects.list_pricing_methods_request, 'methods'),
        rows(templateSubjects.list_sensitivity_templates_request, 'templates'),
        rows(templateMethodSubjects.list_sensitivity_methods_request, 'methods'),
        rows(templateClassSubjects.list_sensitivity_asset_classes_request, 'asset_classes'),
        rows(tenorSubjects.list_tenors_request, 'tenors'),
        rows(instrumentCodeSubjects.list_instrument_codes_request, 'instruments'),
        rows(oreParameterSubjects.list_ore_parameters_request, 'parameters'),
        rows(valueSetSubjects.list_ore_parameter_value_sets_request, 'sets'),
        rows(valueSetValueSubjects.list_ore_parameter_value_set_values_request, 'values'),
        rows(
            valueSetCatalogueSubjects.list_ore_parameter_value_set_catalogues_request,
            'catalogues',
        ),
        rows(
            settingValueSetSubjects.list_ore_parameter_setting_value_sets_request,
            'setting_value_sets',
        ),
    ]);
    const valueSetFields = await readValueSets(session, {
        sets: valueSetRows,
        values: valueSetValueRows,
        catalogues: catalogueRows,
        links: linkRows,
    });
    const assetClassOf = new Map(
        instruments.map((row) => [text(row['code']), text(row['asset_class'])]),
    );
    const templateMethodNames = new Map(
        templateMethods.map((row) => [text(row['code']), text(row['name'])]),
    );
    const templateClassNames = new Map(
        templateClasses.map((row) => [text(row['asset_class_code']), text(row['name'])]),
    );
    const documented = new Map(
        docs
            .filter((row) => text(row['schema_name']) === 'ore.xsd')
            .map((row) => [
                `${text(row['scope'])}|${text(row['subtype'])}|${text(row['name'])}`,
                row,
            ]),
    );
    const docOf = (scope: string, subtype: string, name: string) =>
        documented.get(`${scope}|${subtype}|${name}`);
    const described = (row: Readonly<Record<string, unknown>>) => ({
        code: text(row['code']),
        description: text(row['description']),
        detail: text(row['detail']),
        source: text(row['source']),
    });
    const valueSets: Record<string, ValueFields> = {};
    for (const row of linkRows) {
        const scope = text(row['scope']);
        const name = text(row['name']);
        valueSets[`${scope}|${name}`] = valueSetFields(scope, name);
    }
    return {
        valueSets,
        types: types.map((row) => {
            const doc = docOf('Types', '', text(row['code']));
            return {
                code: text(row['code']),
                description: text(row['description']),
                instrumentTypeCode: text(row['instrument_type_code']),
                assetClass: assetClassOf.get(text(row['instrument_type_code'])) ?? '',
                documentation: text(doc?.['description']),
            };
        }),
        pairs: pairs.map((row) => {
            const typeCode = text(row['pricing_engine_type_code']);
            const model = text(row['model']);
            const engine = text(row['engine']);
            const doc = docOf('Pairs', typeCode, `${model}|${engine}`);
            return {
                typeCode,
                model,
                engine,
                usual: row['is_usual'] === true,
                label: text(doc?.['label']),
                help: text(doc?.['help']),
                description: text(doc?.['description']),
                defaultText: text(doc?.['default_text']),
                xpath: text(doc?.['xpath']),
            };
        }),
        /*
         * A setting the code reads but the guide never lists has no pair of its
         * own. Its row stands with no type, so it is never a choice for a pair
         * and only ever answers a lookup by name.
         */
        settings: [
            ...settings.map((row) => {
                const scope = text(row['scope']);
                const typeCode = text(row['pricing_engine_type_code']);
                const doc = docOf(
                    scope === 'model' ? 'ModelParameters' : 'EngineParameters',
                    typeCode,
                    text(row['name']),
                );
                return {
                    typeCode,
                    model: text(row['model']),
                    engine: text(row['engine']),
                    scope,
                    name: text(row['name']),
                    kind: text(row['kind']),
                    usual: text(row['usual_value']),
                    options: text(row['options']) === '' ? [] : text(row['options']).split(','),
                    ...valueSetFields(scope, text(row['name'])),
                    required: row['is_required'] === true,
                    documented: doc !== undefined,
                    label: text(doc?.['label']),
                    help: text(doc?.['help']),
                    description: text(doc?.['description']),
                    defaultText: text(doc?.['default_text']),
                    format: text(doc?.['format']),
                    introducedIn: text(doc?.['introduced_in']),
                    removedIn: text(doc?.['removed_in']),
                    xpath: text(doc?.['xpath']),
                };
            }),
            ...docs
                .filter(
                    (row) =>
                        text(row['schema_name']) === 'ore.xsd' &&
                        text(row['subtype']) === '' &&
                        (text(row['scope']) === 'ModelParameters' ||
                            text(row['scope']) === 'EngineParameters'),
                )
                .map((row) => ({
                    typeCode: '',
                    model: '',
                    engine: '',
                    scope: text(row['scope']) === 'ModelParameters' ? 'model' : 'engine',
                    name: text(row['name']),
                    kind: 'text',
                    usual: '',
                    options: [],
                    ...valueSetFields(text(row['scope']), text(row['name'])),
                    required: false,
                    documented: true,
                    label: text(row['label']),
                    help: text(row['help']),
                    description: text(row['description']),
                    defaultText: text(row['default_text']),
                    format: text(row['format']),
                    introducedIn: text(row['introduced_in']),
                    removedIn: text(row['removed_in']),
                    xpath: text(row['xpath']),
                })),
        ],
        globals: docs
            .filter(
                (row) =>
                    text(row['schema_name']) === 'ore.xsd' &&
                    text(row['scope']) === 'GlobalParameters',
            )
            .map((row) => ({
                name: text(row['name']),
                domain: text(row['ore_parameter_value_domain_code']),
                ...valueSetFields('GlobalParameters', text(row['name'])),
                label: text(row['label']),
                help: text(row['help']),
                description: text(row['description']),
                defaultText: text(row['default_text']),
                format: text(row['format']),
                introducedIn: text(row['introduced_in']),
                removedIn: text(row['removed_in']),
                xpath: text(row['xpath']),
            }))
            .sort((a, b) => a.name.localeCompare(b.name)),
        models: models.map((row) => {
            const doc = docOf('Models', '', text(row['code']));
            return {
                code: text(row['code']),
                description: text(doc?.['help']),
                detail: text(doc?.['description']),
                source: text(row['source']),
            };
        }),
        methods: methods.map((row) => {
            const doc = docOf('Engines', '', text(row['code']));
            return {
                code: text(row['code']),
                description: text(doc?.['help']),
                detail: text(doc?.['description']),
                source: text(row['source']),
            };
        }),
        /*
         * A template names two lookups, and the words a person reads for them
         * live in their own tables. A trade-level template names no class, so
         * its class reads empty and the screen says the method alone.
         */
        templates: templates.map((row) => ({
            code: text(row['code']),
            methodCode: text(row['method_code']),
            assetClassCode: text(row['asset_class_code']),
            assetClass: templateClassNames.get(text(row['asset_class_code'])) ?? '',
            method: templateMethodNames.get(text(row['method_code'])) ?? text(row['method_code']),
            description: text(row['description']),
        })),
        tenors: tenors
            .filter(
                (row) => PERIOD_UNITS.has(text(row['unit'])) && PERIOD_CODE.test(text(row['code'])),
            )
            .sort((a, b) => Number(a['sort_order'] ?? 0) - Number(b['sort_order'] ?? 0))
            .map((row) => ({
                code: text(row['code']),
                displayName: text(row['display_name']),
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

/**
 * One version of one entity of the document, as the History tab draws it.
 *
 * The generic history read states every field of a version and what changed
 * from the one before. A configuration's versions, its products' and its
 * parameters' are read together, because a save writes all three and a person
 * reads one timeline.
 */
interface DocumentVersion {
    readonly entityType: string;
    readonly entityId: string;
    readonly entityName: string;
    readonly version: number;
    readonly modifiedBy: string;
    readonly recordedAt: string;
    readonly fields: readonly Readonly<Record<string, string>>[];
    readonly changes: readonly Readonly<Record<string, string>>[];
}

/** How many history reads travel at once. A large document holds hundreds of entities. */
const HISTORY_BATCH = 32;

/** Every version of the configuration, its products and its parameters. */
async function documentHistory(
    session: LiveSession,
    id: string,
): Promise<readonly DocumentVersion[]> {
    const stored = await readStored(session, id);
    const typeOfProduct = new Map(
        stored.products.map((product) => [
            text(product['id']),
            text(product['pricing_engine_type_code']),
        ]),
    );
    const entities = [
        {
            entityType: 'ores.analytics.pricing_model_config',
            entityId: id,
            entityName: text(stored.config['name']),
        },
        ...stored.products.map((product) => ({
            entityType: 'ores.analytics.pricing_model_product',
            entityId: text(product['id']),
            entityName: text(product['pricing_engine_type_code']),
        })),
        ...stored.parameters.map((parameter) => ({
            entityType: 'ores.analytics.pricing_model_product_parameter',
            entityId: text(parameter['id']),
            entityName: [
                typeOfProduct.get(text(parameter['pricing_model_product_id'])) ?? '',
                text(parameter['parameter_name']),
            ]
                .filter((part) => part !== '')
                .join(' · '),
        })),
    ];
    const read = async (entity: (typeof entities)[number]): Promise<DocumentVersion[]> => {
        const versions = await readEntityHistory(
            session.client,
            entity.entityType,
            entity.entityId,
        );
        return versions.map((version) => ({
            entityType: entity.entityType,
            entityId: entity.entityId,
            entityName: entity.entityName,
            version: version.version,
            modifiedBy: version.modifiedBy,
            recordedAt: version.recordedAt,
            fields: version.fields,
            changes: version.changes,
        }));
    };
    const out: DocumentVersion[] = [];
    for (let start = 0; start < entities.length; start += HISTORY_BATCH) {
        const batch = entities.slice(start, start + HISTORY_BATCH);
        const read_ = await Promise.all(batch.map(read));
        out.push(...read_.flat());
    }
    return out;
}

/**
 * The header that offers one file to the browser, under the name the ORE
 * service stated.
 *
 * The name comes from the reporting file types table rather than from this
 * server, so it is quoted and stripped of the characters that would end the
 * value, and nothing else about it is rewritten.
 */
export function attachmentOf(name: string): string {
    const safe = name.replace(/[\u0000-\u001f\u007f"\\/]/g, '');
    const ascii = safe.replace(/[^\x20-\x7e]/g, '');
    if (safe !== '' && safe === ascii) {
        return `attachment; filename="${safe}"`;
    }
    const encoded = encodeURIComponent(name).replace(
        /['()*!]/g,
        (c) => `%${c.charCodeAt(0).toString(16).toUpperCase()}`,
    );
    return `attachment; filename="${ascii === '' ? 'document' : ascii}"; filename*=UTF-8''${encoded}`;
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
        const [stored, parties] = await Promise.all([
            readStored(session, id),
            readPartyNames(session),
        ]);
        const document = documentOf(stored, parties);
        const reports = await readBoundReports(session, text(document['configurationId']));
        return { ...document, reports };
    });

    server.get('/api/reporting/pricing-engines/:id/history', async (request) => {
        const session = requireSession(request);
        const id = input(idSchema, (request.params as { id: string }).id);
        return { versions: await documentHistory(session, id) };
    });

    /**
     * The ORE document of one configuration (RS-ORE-06): the ORE service writes
     * it, names it, and this route hands it to the browser as a file.
     *
     * The configuration is named by the id the screen shows; the service reads
     * the document by the reporting configuration the record fills, so the two
     * ids are joined here rather than sent to the screen. A reply that carries
     * more than one file is answered as it stands, because one response cannot
     * name two files.
     */
    server.get('/api/reporting/pricing-engines/:id/ore-document', async (request, reply) => {
        const session = requireSession(request);
        const id = input(idSchema, (request.params as { id: string }).id);
        const stored = await readStored(session, id);
        const exported = await session.client.callAuthenticated(
            orePricingSubjects.export_pricing_engines_request,
            { configuration_id: text(stored.config['configuration_id']) },
            oreExportSchema,
        );
        const [file, ...others] = exported.files;
        if (!exported.success || file === undefined) {
            throw invalidRequest(
                exported.message === ''
                    ? 'The pricing engines document could not be written.'
                    : exported.message,
            );
        }
        if (others.length > 0) {
            throw unexpectedReply('The export answered more than one file.');
        }
        void reply.header('content-disposition', attachmentOf(file.name)).type('application/xml');
        return file.content;
    });

    server.post('/api/reporting/pricing-engines', async (request, reply) => {
        const session = requireSession(request);
        const body = input(documentBodySchema, request.body);
        const id = randomUUID();
        const configurationId = await createHeader(session, body.name, body.intent);
        try {
            await save(session, rowsOf(body, id, 0, configurationId, session.tenantId));
        } catch (failure) {
            await removeHeader(session, body.name, body.intent);
            throw failure;
        }
        void reply.code(201);
        const created = documentOf(await readStored(session, id), await readPartyNames(session));
        return {
            ...created,
            reports: await readBoundReports(session, text(created['configurationId'])),
        };
    });

    server.put('/api/reporting/pricing-engines/:id', async (request) => {
        const session = requireSession(request);
        const id = input(idSchema, (request.params as { id: string }).id);
        const body = input(updateBodySchema, request.body);
        const stored = await readStored(session, id);
        await save(
            session,
            rowsOf(
                body,
                id,
                body.version,
                text(stored.config['configuration_id']),
                session.tenantId,
            ),
        );
        const updated = documentOf(await readStored(session, id), await readPartyNames(session));
        return {
            ...updated,
            reports: await readBoundReports(session, text(updated['configurationId'])),
        };
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
