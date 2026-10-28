/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import type { FastifyInstance, FastifyRequest } from 'fastify';
import { z } from 'zod';
import { subjects as documentSubjects } from '@ores/wire-protocol/generated/refdata/protocol/configuration_document_protocol';
import { subjects as sectionSubjects } from '@ores/wire-protocol/generated/refdata/protocol/curve_section_protocol';
import { subjects as configurationSubjects } from '@ores/wire-protocol/generated/refdata/protocol/curve_configuration_protocol';
import { subjects as definitionSubjects } from '@ores/wire-protocol/generated/refdata/protocol/curve_definition_protocol';
import { subjects as parameterSubjects } from '@ores/wire-protocol/generated/reporting/protocol/ore_parameter_protocol';
import { subjects as valueSetSubjects } from '@ores/wire-protocol/generated/reporting/protocol/ore_parameter_value_set_protocol';
import { subjects as valueSetValueSubjects } from '@ores/wire-protocol/generated/reporting/protocol/ore_parameter_value_set_value_protocol';
import { subjects as valueSetCatalogueSubjects } from '@ores/wire-protocol/generated/reporting/protocol/ore_parameter_value_set_catalogue_protocol';
import { subjects as settingValueSetSubjects } from '@ores/wire-protocol/generated/reporting/protocol/ore_parameter_setting_value_set_protocol';
import {
    CURVE_SECTIONS,
    curveSectionOf,
    type CurveSectionWiring,
} from './curve-sections.generated.js';
import { NO_VALUE_SET, readValueSets } from './ore-value-sets.js';
import { HttpFailure, invalidRequest, notFound } from './errors.js';
import { input, readAll, row, text, valueText } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/*
 * One curve configuration, as the ORE element and leaf screen standard draws it.
 *
 * The section list, the each section's entry element and the order the sections
 * are read in come from =curve_section=, so the screen holds no table that names
 * a family. What a column holds is the projection's fact, and it reaches here
 * through =curve-sections.generated.ts=, which the generator reads out of the
 * projection. What a leaf means, and the values it accepts, come from the ORE
 * parameter contract and the value set tables, so the screen holds no ORE text.
 *
 * The document is read whole, once, and the leaves of each entry are grouped by
 * the section the entry's definition names. An entry is one =curve_definition=
 * row -- the identifier and the description every family shares -- plus the rows
 * of its own family table, which are joined back to it by =curve_definition_id=.
 */

/** The schema the curve configuration's leaves are documented under. */
const SCHEMA_NAME = 'curveconfig.xsd';

/** The table every entry's identifier and description live in. */
const DEFINITION_ENTITY = 'curve_definition';

/** The value a row carries for a party the caller does not state. */
const NO_PARTY = '';

/**
 * The identifier of a configuration row.
 *
 * A uuid the application mints carries the RFC version and variant bits, and the
 * rows an ORE bundle import writes do not: =90e6d408-aa8e-3e14-206b-...= is a
 * variant the RFC does not define, so =z.uuid()= refuses it and the page cannot
 * open a curve configuration the import created. The address therefore takes any
 * well-formed guid, as the report configuration route does.
 */
const idSchema = z.guid();

const documentQuerySchema = z.object({ party: idSchema.optional() });

/** One leaf of one row, as a screen changes it: the column and the value it takes. */
const changeSchema = z.object({
    entity: z.string().min(1).max(80),
    /** The curve definition the entry is: every family row joins back to it. */
    entryId: z.string().min(1).max(80),
    column: z.string().min(1).max(80),
    /** The value to hold, or null to unset the leaf and let ORE apply its default. */
    value: z.string().nullable(),
});

/** The changes a screen asks for, and the version it read before it asks. */
const amendSchema = z.object({
    version: z.number().int().nonnegative(),
    reasonCode: z.string().min(1).max(120),
    commentary: z.string().max(4000).default(''),
    changes: z.array(changeSchema).min(1).max(200),
});

/** The value set fields of one leaf, as the contract and the set tables state them. */
type ValueSetOf = (
    scope: string,
    name: string,
) => {
    readonly valueSet: string;
    readonly sourceKind: string;
    readonly choices: readonly { readonly value: string; readonly label: string }[];
    readonly minimum: number | null;
    readonly maximum: number | null;
    readonly pattern: string;
};

/** One parameter of one entry: its value, its words, and the values it accepts. */
export interface CurveParameterView {
    readonly entity: string;
    readonly column: string;
    readonly ore: string;
    readonly xpath: string;
    readonly value: string | null;
    readonly documented: boolean;
    readonly required: boolean;
    readonly label: string;
    readonly help: string;
    readonly description: string;
    readonly defaultText: string;
    readonly format: string;
    readonly introducedIn: string;
    readonly removedIn: string;
    readonly valueSet: string;
    readonly sourceKind: string;
    readonly choices: readonly { readonly value: string; readonly label: string }[];
    readonly minimum: number | null;
    readonly maximum: number | null;
    readonly pattern: string;
}

/** One child element an entry holds, and whether the entry states it. */
export interface CurveBlockView {
    readonly element: string;
    readonly present: boolean | null;
}

/** One curve entry: the definition every family shares, and its own parameters. */
export interface CurveEntryView {
    readonly id: string;
    readonly curveId: string;
    readonly description: string;
    readonly version: number;
    readonly position: number;
    readonly parameters: readonly CurveParameterView[];
    readonly blocks: readonly CurveBlockView[];
}

/** One section of the document, its entries, and whether the columns are known. */
export interface CurveSectionView {
    readonly code: string;
    readonly entryElement: string;
    readonly description: string;
    readonly mapped: boolean;
    readonly entries: readonly CurveEntryView[];
}

/** One curve configuration as the list draws it, without reading its document. */
export interface CurveConfigurationSummary {
    readonly configurationId: string;
    readonly id: string;
    readonly name: string;
    readonly description: string;
    readonly version: number;
    readonly sections: number;
    readonly curves: number;
}

/** The curve configuration a reporting configuration is filled by. */
export interface CurveConfigurationView {
    readonly configurationId: string;
    readonly id: string;
    readonly name: string;
    readonly description: string;
    readonly version: number;
    readonly partyId: string;
    readonly modifiedBy: string;
    readonly recordedAt: string;
    readonly documented: number;
    readonly total: number;
    readonly sections: readonly CurveSectionView[];
}

/** A row as a number, defaulting to zero for a row that states none. */
function numberOf(value: unknown): number {
    return typeof value === 'number' ? value : 0;
}

/** The document vectors a curve configuration document carries, by their name. */
function vectorOf(
    document: Readonly<Record<string, unknown>>,
    name: string,
): readonly Readonly<Record<string, unknown>>[] {
    return z.array(row).default([]).parse(document[name]);
}

/**
 * The document a reporting configuration is filled by, read whole.
 *
 * The call answers =success: false= and a sentence when no document fills the
 * configuration, which is the missing case the page states, so it is a 404 and
 * not an empty page.
 */
async function readDocument(
    session: LiveSession,
    configurationId: string,
    party: string | undefined,
): Promise<Readonly<Record<string, unknown>>> {
    const reply = await session.client.callAuthenticated(
        documentSubjects.get_curve_configuration_document_request,
        { configuration_id: configurationId, party_id: party ?? NO_PARTY },
        z.looseObject({
            success: z.boolean().default(false),
            message: z.string().default(''),
            document: z.looseObject({}).default({}),
        }),
    );
    if (!reply.success) {
        throw notFound(
            reply.message === ''
                ? `No curve configuration document fills configuration ${configurationId}.`
                : reply.message,
        );
    }
    return reply.document;
}

/**
 * The sections the curve configuration may hold, in the order the schema
 * declares them.
 *
 * A section the wiring does not name is still listed, after the ones it does,
 * because a section the data holds and the projection does not yet map is a gap
 * to show and not a section to hide. =mapped= says which it is.
 */
function orderedSections(
    rows: readonly Readonly<Record<string, unknown>>[],
): readonly { readonly code: string; readonly entry: string; readonly description: string }[] {
    const stated = rows.map((section) => ({
        code: text(section['code']),
        entry: text(section['entry_element']),
        description: text(section['description']),
    }));
    const order = new Map(CURVE_SECTIONS.map((wiring, index) => [wiring.section, index]));
    return stated.sort((left, right) => {
        const leftOrder = order.get(left.code) ?? CURVE_SECTIONS.length;
        const rightOrder = order.get(right.code) ?? CURVE_SECTIONS.length;
        return leftOrder === rightOrder
            ? left.code.localeCompare(right.code)
            : leftOrder - rightOrder;
    });
}

/** One entry's row of each of the section's tables, by the table's entity name. */
function rowsOfEntry(
    document: Readonly<Record<string, unknown>>,
    wiring: CurveSectionWiring,
    entryId: string,
): ReadonlyMap<string, Readonly<Record<string, unknown>>> {
    const found = new Map<string, Readonly<Record<string, unknown>>>();
    for (const entity of wiring.entities) {
        const candidate = vectorOf(document, entity.vector).find(
            (candidate) => text(candidate[wiring.definitionKey]) === entryId,
        );
        if (candidate !== undefined) {
            found.set(entity.entity, candidate);
        }
    }
    return found;
}

/**
 * One entry's parameters: every column the projection reads for the section, in
 * the order the projection reads them, each joined to its leaf in the contract.
 */
function parametersOf(
    definition: Readonly<Record<string, unknown>>,
    rows: ReadonlyMap<string, Readonly<Record<string, unknown>>>,
    wiring: CurveSectionWiring,
    contract: ReadonlyMap<string, Readonly<Record<string, unknown>>>,
    valueSetOf: ValueSetOf,
): readonly CurveParameterView[] {
    return wiring.parameters.map((parameter) => {
        const xpath = `/${parameter.xpath}`;
        const entry = contract.get(xpath);
        const source =
            parameter.entity === DEFINITION_ENTITY ? definition : rows.get(parameter.entity);
        const scope = text(entry?.['scope']);
        const name = text(entry?.['name']);
        return {
            entity: parameter.entity,
            column: parameter.column,
            ore: parameter.ore,
            xpath,
            value: valueText(source?.[parameter.column]),
            documented: entry !== undefined,
            required: entry?.['is_required'] === true,
            label: text(entry?.['label']),
            help: text(entry?.['help']),
            description: text(entry?.['description']),
            defaultText: text(entry?.['default_text']),
            format: text(entry?.['format']),
            introducedIn: text(entry?.['introduced_in']),
            removedIn: text(entry?.['removed_in']),
            ...(entry === undefined ? NO_VALUE_SET : valueSetOf(scope, name)),
        };
    });
}

/** The child elements an entry holds, and whether the entry's rows state each one. */
function blocksOf(
    rows: ReadonlyMap<string, Readonly<Record<string, unknown>>>,
    wiring: CurveSectionWiring,
): readonly CurveBlockView[] {
    return wiring.blocks.map((block) => {
        const stated = [...rows.values()].find(
            (row) => block.presence !== '' && row[block.presence] !== undefined,
        );
        const value = stated === undefined ? null : valueText(stated[block.presence]);
        return {
            element: block.element,
            present: value === null ? null : value === 'true' || value === 'Y' || value === 'yes',
        };
    });
}

/** One entry of a section: its definition row and the rows of its own tables. */
function entryOf(
    definition: Readonly<Record<string, unknown>>,
    document: Readonly<Record<string, unknown>>,
    wiring: CurveSectionWiring | undefined,
    contract: ReadonlyMap<string, Readonly<Record<string, unknown>>>,
    valueSetOf: ValueSetOf,
): CurveEntryView {
    const id = text(definition['id']);
    if (wiring === undefined) {
        return {
            id,
            curveId: text(definition['curve_id']),
            description: text(definition['description']),
            version: numberOf(definition['version']),
            position: numberOf(definition['position']),
            parameters: [],
            blocks: [],
        };
    }
    const rows = rowsOfEntry(document, wiring, id);
    return {
        id,
        curveId: text(definition['curve_id']),
        description: text(definition['description']),
        version: numberOf(definition['version']),
        position: numberOf(definition['position']),
        parameters: parametersOf(definition, rows, wiring, contract, valueSetOf),
        blocks: blocksOf(rows, wiring),
    };
}

/**
 * Every family a curve configuration document carries.
 *
 * A save states each one, empty where the screen changed nothing: the service
 * reads the document as a whole, so a family it does not name is a document it
 * cannot read. The families left empty are not written, so their rows keep their
 * versions.
 */
const DOCUMENT_FAMILIES = [
    'sections',
    'definitions',
    'yield_curves',
    'equity_curves',
    'inflation_curves',
    'default_curves',
    'commodity_curves',
    'fx_volatilities',
    'yield_volatilities',
    'base_correlations',
    'correlations',
    'report_configurations',
    'cds_volatilities',
    'cds_volatility_terms',
    'volatility_configs',
    'inflation_cap_floor_volatilities',
    'swaption_volatilities',
    'cap_floor_volatilities',
    'parametric_smiles',
    'parametric_smile_parameters',
    'equity_volatilities',
    'commodity_volatilities',
    'bond_future_volatilities',
    'global_reports',
    'commodity_price_segments',
    'default_curve_configurations',
    'seasonality_factors',
    'securities',
    'intraday_power_curves',
    'bootstrap_configs',
    'segments',
    'segment_curves',
    'quotes',
] as const;

/** The document vector one entity's rows are read from, or the empty name. */
function vectorByEntity(): ReadonlyMap<string, string> {
    const found = new Map<string, string>();
    for (const wiring of CURVE_SECTIONS)
        for (const entity of wiring.entities) found.set(entity.entity, entity.vector);
    return found;
}

/** A leaf as a change is checked against: its words, and the values it accepts. */
interface LeafRule {
    readonly label: string;
    readonly required: boolean;
    readonly choices: readonly { readonly value: string; readonly label: string }[];
    readonly minimum: number | null;
    readonly maximum: number | null;
    readonly pattern: string;
}

/**
 * Why the leaf would not take the value, or null when it would.
 *
 * The value set tables are the ORE parser's own lists, so a value outside them
 * is one ORE refuses to read. The store's guard does not cover the curve tables
 * yet, so the boundary is where the refusal happens.
 */
function refusalOf(rule: LeafRule, value: string | null): string | null {
    if (value === null || value === '') {
        return rule.required ? `${rule.label} is required.` : null;
    }
    if (rule.choices.length > 0 && !rule.choices.some((choice) => choice.value === value)) {
        const accepted = rule.choices.map((choice) => choice.value).join(', ');
        return `${rule.label} must be one of ${accepted}, and "${value}" is not.`;
    }
    if (rule.minimum !== null || rule.maximum !== null) {
        const number = Number(value);
        if (Number.isNaN(number)) return `${rule.label} must be a number.`;
        if (rule.minimum !== null && number < rule.minimum)
            return `${rule.label} must be at least ${String(rule.minimum)}.`;
        if (rule.maximum !== null && number > rule.maximum)
            return `${rule.label} must be at most ${String(rule.maximum)}.`;
    }
    if (rule.pattern !== '' && !new RegExp(rule.pattern).test(value))
        return `${rule.label} must match ${rule.pattern}.`;
    return null;
}

/** Writes the rows the caller sends, with the reason that says why they changed. */
async function saveDocument(
    session: LiveSession,
    document: Readonly<Record<string, unknown>>,
    reasonCode: string,
    commentary: string,
): Promise<void> {
    const reply = await session.client.callAuthenticated(
        documentSubjects.save_curve_configuration_document_request,
        { document, reason_code: reasonCode, commentary },
        z.looseObject({
            success: z.boolean().default(false),
            message: z.string().default(''),
            id: z.string().default(''),
        }),
    );
    if (!reply.success) {
        const stale = /version[_ ]conflict|does not match/i.test(reply.message);
        throw new HttpFailure(stale ? 409 : 400, {
            code: stale ? 'conflict' : 'invalid-request',
            message: reply.message === '' ? 'The change was refused.' : reply.message,
        });
    }
}

/** The curve configuration a reporting configuration is filled by, as a screen reads it. */
export function registerCurveConfigurationRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /*
     * The curve configurations the session's parties hold, in name order.
     *
     * The list reads the header rows and the definitions, and counts the curves
     * of each by the section their definition names, so a person reaches a
     * configuration without a report that binds it.
     */
    server.get('/api/refdata/curve-configurations', async (request) => {
        const session = requireSession(request);
        const [configurations, definitions] = await Promise.all([
            readAll(
                session,
                configurationSubjects.list_curve_configurations_request,
                'configurations',
                {
                    filter: null,
                    as_of: null,
                },
            ),
            readAll(session, definitionSubjects.list_curve_definitions_request, 'curves', {
                filter: null,
                as_of: null,
            }),
        ]);
        const counted = new Map<string, { sections: Set<string>; curves: number }>();
        for (const definition of definitions) {
            const key = text(definition['curve_configuration_id']);
            const counts = counted.get(key) ?? { sections: new Set<string>(), curves: 0 };
            counts.sections.add(text(definition['section_code']));
            counts.curves += 1;
            counted.set(key, counts);
        }
        const rows: CurveConfigurationSummary[] = configurations
            .map((configuration) => {
                const id = text(configuration['id']);
                const counts = counted.get(id) ?? { sections: new Set<string>(), curves: 0 };
                return {
                    configurationId: text(configuration['configuration_id']),
                    id,
                    name: text(configuration['name']),
                    description: text(configuration['description']),
                    version: numberOf(configuration['version']),
                    sections: counts.sections.size,
                    curves: counts.curves,
                };
            })
            .sort((left, right) => left.name.localeCompare(right.name));
        return { rows, total: rows.length };
    });

    server.get('/api/refdata/curve-configurations/:id', async (request) => {
        const session = requireSession(request);
        const configurationId = input(idSchema, (request.params as { id: string }).id);
        const { party } = input(documentQuerySchema, request.query);
        const document = await readDocument(session, configurationId, party);
        const header = z.looseObject({}).parse(document['config']);
        const definitions = vectorOf(document, 'definitions');
        const [sectionRows, parameters, sets, values, catalogues, links] = await Promise.all([
            readAll(session, sectionSubjects.list_curve_sections_request, 'sections', {
                filter: null,
                as_of: null,
            }),
            readAll(session, parameterSubjects.list_ore_parameters_request, 'parameters', {
                filter: null,
                as_of: null,
            }),
            readAll(session, valueSetSubjects.list_ore_parameter_value_sets_request, 'sets', {
                filter: null,
                as_of: null,
            }),
            readAll(
                session,
                valueSetValueSubjects.list_ore_parameter_value_set_values_request,
                'values',
                {
                    filter: null,
                    as_of: null,
                },
            ),
            readAll(
                session,
                valueSetCatalogueSubjects.list_ore_parameter_value_set_catalogues_request,
                'catalogues',
                { filter: null, as_of: null },
            ),
            readAll(
                session,
                settingValueSetSubjects.list_ore_parameter_setting_value_sets_request,
                'setting_value_sets',
                { filter: null, as_of: null },
            ),
        ]);
        const contract = new Map(
            parameters
                .filter((entry) => text(entry['schema_name']) === SCHEMA_NAME)
                .map((entry) => [text(entry['xpath']), entry]),
        );
        const valueSetOf = await readValueSets(session, { sets, values, catalogues, links });
        const sections = orderedSections(sectionRows).map((section) => {
            const wiring = CURVE_SECTIONS.find((candidate) => candidate.section === section.code);
            const entries = definitions
                .filter((definition) => text(definition['section_code']) === section.code)
                .sort((left, right) => numberOf(left['position']) - numberOf(right['position']))
                .map((definition) => entryOf(definition, document, wiring, contract, valueSetOf));
            return {
                code: section.code,
                entryElement: section.entry === '' ? (wiring?.entryElement ?? '') : section.entry,
                description: section.description,
                mapped: wiring !== undefined,
                entries,
            };
        });
        const total = sections.reduce(
            (count, section) =>
                count + section.entries.reduce((sum, entry) => sum + entry.parameters.length, 0),
            0,
        );
        const documented = sections.reduce(
            (count, section) =>
                count +
                section.entries.reduce(
                    (sum, entry) => sum + entry.parameters.filter((leaf) => leaf.documented).length,
                    0,
                ),
            0,
        );
        const view: CurveConfigurationView = {
            configurationId,
            id: text(header['id']),
            name: text(header['name']),
            description: text(header['description']),
            version: numberOf(header['version']),
            partyId: text(header['party_id']),
            modifiedBy: text(header['modified_by']),
            recordedAt: text(header['recorded_at']),
            documented,
            total,
            sections,
        };
        return view;
    });

    /**
     * Changes leaves of one curve configuration.
     *
     * A save writes every family the document carries, and the screen draws one
     * entry at a time. So the route reads the stored document whole, changes the
     * leaves the caller names, and writes back only the families it touched: the
     * rows the screen does not draw keep their versions and their history stays
     * a record of what changed. Each row states the version it was read at, so a
     * row that moved on since the read is refused as a conflict rather than
     * overwritten.
     */
    server.put('/api/refdata/curve-configurations/:id', async (request, reply) => {
        const session = requireSession(request);
        const configurationId = input(idSchema, (request.params as { id: string }).id);
        const { party } = input(documentQuerySchema, request.query);
        const body = input(amendSchema, request.body);
        const document = await readDocument(session, configurationId, party);
        const header = z.looseObject({}).parse(document['config']);
        if (numberOf(header['version']) !== body.version) {
            throw new HttpFailure(409, {
                code: 'conflict',
                message:
                    'This curve configuration changed since you opened it. Read it again before you change it.',
            });
        }
        const [parameters, sets, values, catalogues, links] = await Promise.all([
            readAll(session, parameterSubjects.list_ore_parameters_request, 'parameters', {
                filter: null,
                as_of: null,
            }),
            readAll(session, valueSetSubjects.list_ore_parameter_value_sets_request, 'sets', {
                filter: null,
                as_of: null,
            }),
            readAll(
                session,
                valueSetValueSubjects.list_ore_parameter_value_set_values_request,
                'values',
                { filter: null, as_of: null },
            ),
            readAll(
                session,
                valueSetCatalogueSubjects.list_ore_parameter_value_set_catalogues_request,
                'catalogues',
                { filter: null, as_of: null },
            ),
            readAll(
                session,
                settingValueSetSubjects.list_ore_parameter_setting_value_sets_request,
                'setting_value_sets',
                { filter: null, as_of: null },
            ),
        ]);
        const contract = new Map(
            parameters
                .filter((entry) => text(entry['schema_name']) === SCHEMA_NAME)
                .map((entry) => [text(entry['xpath']), entry]),
        );
        const valueSetOf = await readValueSets(session, { sets, values, catalogues, links });
        const vectors = vectorByEntity();
        const touched = new Map<string, Readonly<Record<string, unknown>>[]>();
        for (const change of body.changes) {
            const parameter = CURVE_SECTIONS.flatMap((wiring) => wiring.parameters).find(
                (candidate) =>
                    candidate.entity === change.entity && candidate.column === change.column,
            );
            if (parameter === undefined) {
                throw invalidRequest(
                    `${change.entity}.${change.column} is not a leaf of a curve configuration.`,
                );
            }
            const leaf = contract.get(`/${parameter.xpath}`);
            const scope = text(leaf?.['scope']);
            const name = text(leaf?.['name']);
            const set = leaf === undefined ? NO_VALUE_SET : valueSetOf(scope, name);
            const refusal = refusalOf(
                {
                    label: text(leaf?.['label']) || parameter.ore,
                    required: leaf?.['is_required'] === true,
                    choices: set.choices,
                    minimum: set.minimum,
                    maximum: set.maximum,
                    pattern: text(leaf?.['pattern']),
                },
                change.value,
            );
            if (refusal !== null) throw invalidRequest(refusal);
            const vector = vectors.get(change.entity);
            if (vector === undefined) {
                throw invalidRequest(`${change.entity} has no table in a curve configuration.`);
            }
            const rows = touched.get(vector) ?? vectorOf(document, vector);
            const key =
                change.entity === DEFINITION_ENTITY
                    ? 'id'
                    : (CURVE_SECTIONS.find((wiring) =>
                          wiring.entities.some((entity) => entity.entity === change.entity),
                      )?.definitionKey ?? '');
            const row = rows.find((candidate) => text(candidate[key]) === change.entryId);
            if (row === undefined) {
                throw invalidRequest(
                    `No ${change.entity} row of the curve entry ${change.entryId} is stored.`,
                );
            }
            const changed = { ...row, [change.column]: change.value };
            touched.set(
                vector,
                rows.map((candidate) => (candidate === row ? changed : candidate)),
            );
        }
        const partial: Record<string, unknown> = { config: header };
        for (const family of DOCUMENT_FAMILIES) partial[family] = [];
        for (const [vector, rows] of touched) partial[vector] = rows;
        await saveDocument(session, partial, body.reasonCode, body.commentary);
        return reply.code(204).send();
    });
}
