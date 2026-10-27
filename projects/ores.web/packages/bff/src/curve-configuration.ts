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
import { notFound } from './errors.js';
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

const documentQuerySchema = z.object({ party: z.uuid().optional() });

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

/** The curve configuration a reporting configuration is filled by, as a screen reads it. */
export function registerCurveConfigurationRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/refdata/curve-configurations/:id', async (request) => {
        const session = requireSession(request);
        const configurationId = input(z.uuid(), (request.params as { id: string }).id);
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
}
