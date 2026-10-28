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
import { subjects as settingsSubjects } from '@ores/wire-protocol/generated/reporting/protocol/engine_settings_protocol';
import { subjects as fixingsSubjects } from '@ores/wire-protocol/generated/reporting/protocol/engine_settings_fixings_protocol';
import { subjects as buildingSubjects } from '@ores/wire-protocol/generated/reporting/protocol/engine_settings_building_protocol';
import { subjects as executionSubjects } from '@ores/wire-protocol/generated/reporting/protocol/engine_settings_execution_protocol';
import { subjects as groupSubjects } from '@ores/wire-protocol/generated/reporting/protocol/engine_settings_group_protocol';
import { subjects as settingSubjects } from '@ores/wire-protocol/generated/reporting/protocol/engine_settings_group_setting_protocol';
import { subjects as parameterSubjects } from '@ores/wire-protocol/generated/reporting/protocol/ore_parameter_protocol';
import { subjects as bindingSubjects } from '@ores/wire-protocol/generated/reporting/protocol/report_configuration_protocol';
import { subjects as definitionSubjects } from '@ores/wire-protocol/generated/reporting/protocol/report_definition_protocol';
import { subjects as configurationSubjects } from '@ores/wire-protocol/generated/reporting/protocol/configuration_protocol';
import { subjects as proposalSubjects } from '@ores/wire-protocol/generated/reporting/protocol/engine_settings_proposal_operations_protocol';
import { HttpFailure, invalidRequest, notFound, otherParty } from './errors.js';
import {
    input,
    intentSchema,
    readAll,
    refusal,
    resultSchema,
    row,
    text,
    write,
} from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/*
 * The engine settings a party holds: a named configuration that reports bind,
 * as they bind their pricing engines.
 *
 * The service keeps a configuration as a central row and three parts, one for
 * each group of settings. A part that has no row means the engine default for
 * every setting in it, and a setting that is null inside a row means the
 * engine default for that setting, so the screens can always say which value is
 * in force. A write to a part makes the next version of the central row, in the
 * service, so a part is written alone and the document is never saved whole.
 *
 * What a setting is, its range and its control, comes from the engine settings
 * groups tables, so a group and its settings are rows and never a change here.
 * What a setting means is documentation, and it comes from the ore parameter of
 * the same name, so the wording too is edited in the data and never in this
 * file.
 *
 * The subjects a group's part is read and written under, and the key its rows
 * travel as, stay here: they are the wiring of the service, not the model of the
 * screen.
 *
 * A configuration is bound to a report through a reporting configuration of
 * type engine_settings, so a create writes that header first and the central
 * row second, and a failed second step removes the first.
 */

const CONFIGURATION_TYPE = 'engine_settings';
const OWNING_COMPONENT = 'ores.reporting';
const SCHEMA_NAME = 'ore.xsd';
const SETUP_SCOPE = 'Setup';

/** The value a row carries for an id or a time the service stamps itself. */
const NIL = '00000000-0000-0000-0000-000000000000';

const idSchema = z.guid();

/** The kind of value a setting takes, which is the control the screen draws. */
const kindSchema = z.enum(['flag', 'whole', 'choice']);

/** One group of settings, as the reporting service states it. */
const groupRowSchema = z.looseObject({
    code: z.string().trim().min(1),
    position: z.number().default(0),
});

/** One setting of a group, as the reporting service states it. */
const settingRowSchema = z.looseObject({
    group_code: z.string().trim().min(1),
    column_name: z.string().trim().min(1),
    ore_name: z.string().trim().min(1),
    kind: kindSchema,
    changes_numbers: z.boolean().default(false),
    /** The words a choice accepts, comma separated, and empty for a setting that is not one. */
    options: z.string().default(''),
    minimum: z.number().nullable().default(null),
    maximum: z.number().nullable().default(null),
    position: z.number().default(0),
});

type GroupRow = z.infer<typeof groupRowSchema>;
type SettingRow = z.infer<typeof settingRowSchema>;

/** One setting of a group: the column its value travels under, its control, and the values it accepts. */
interface EngineSetting {
    readonly column: string;
    readonly ore: string;
    readonly kind: z.infer<typeof kindSchema>;
    /** Whether the setting changes the numbers a run computes, and is not only operational. */
    readonly changesNumbers: boolean;
    readonly options: readonly string[];
    readonly min: number | null;
    readonly max: number | null;
}

/** One group of settings, in the order the data places it in the screen. */
interface EngineGroup {
    /** The code of the group, which is the word its part travels under. */
    readonly id: string;
    /**
     * Whether a write to this group moves the numbers a run computes, which is
     * the declaration the settings of the group carry. A write to a group that
     * does is a proposal once a report binds the configuration; a write to one
     * that does not lands directly.
     */
    readonly changesNumbers: boolean;
    readonly settings: readonly EngineSetting[];
}

/** Where the rows of one group live: the subjects they travel under and the key they travel as. */
interface GroupTransport {
    readonly list: string;
    readonly rows: string;
    readonly put: string;
    readonly delete: string;
    readonly versions: string;
}

/*
 * The wiring of each group of settings. The data states the groups; this states
 * where each group's part is read and written, which is the service's shape and
 * not the screen's model. A group the data holds and this does not name has no
 * rows to reach, so it is not offered.
 */
const TRANSPORT: Readonly<Record<string, GroupTransport>> = {
    fixings: {
        list: fixingsSubjects.list_engine_settings_fixings_request,
        rows: 'engine_settings_fixings',
        put: fixingsSubjects.put_engine_settings_fixings_request,
        delete: fixingsSubjects.delete_engine_settings_fixings_request,
        versions: fixingsSubjects.list_engine_settings_fixings_versions_request,
    },
    building: {
        list: buildingSubjects.list_engine_settings_building_request,
        rows: 'engine_settings_building',
        put: buildingSubjects.put_engine_settings_building_request,
        delete: buildingSubjects.delete_engine_settings_building_request,
        versions: buildingSubjects.list_engine_settings_building_versions_request,
    },
    execution: {
        list: executionSubjects.list_engine_settings_execution_request,
        rows: 'engine_settings_execution',
        put: executionSubjects.put_engine_settings_execution_request,
        delete: executionSubjects.delete_engine_settings_execution_request,
        versions: executionSubjects.list_engine_settings_execution_versions_request,
    },
};

const groupSchema = z.enum(Object.keys(TRANSPORT) as [string, ...string[]]);

function settingOf(row: SettingRow): EngineSetting {
    return {
        column: row.column_name,
        ore: row.ore_name,
        kind: row.kind,
        changesNumbers: row.changes_numbers,
        options: row.options
            .split(',')
            .map((option) => option.trim())
            .filter((option) => option !== ''),
        min: row.minimum,
        max: row.maximum,
    };
}

/**
 * The groups of settings the data states, in the order it places them, with the
 * settings of each in the order it places those. A group the wiring does not
 * name is not offered, because its part could not be read or written.
 */
async function readGroups(session: LiveSession): Promise<readonly EngineGroup[]> {
    const [groupRows, settingRows] = await Promise.all([
        readAll(session, groupSubjects.list_engine_settings_groups_request, 'groups', {
            filter: null,
            as_of: null,
        }),
        readAll(session, settingSubjects.list_engine_settings_group_settings_request, 'settings', {
            filter: null,
            as_of: null,
        }),
    ]);
    const settings = new Map<string, EngineSetting[]>();
    for (const row of settingRows
        .map((value) => settingRowSchema.parse(value))
        .sort((left, right) => left.position - right.position)) {
        const ofGroup = settings.get(row.group_code) ?? [];
        ofGroup.push(settingOf(row));
        settings.set(row.group_code, ofGroup);
    }
    return groupRows
        .map((value) => groupRowSchema.parse(value))
        .sort((left, right) => left.position - right.position)
        .filter((group) => TRANSPORT[group.code] !== undefined)
        .map((group) => {
            const ofGroup = settings.get(group.code) ?? [];
            return {
                id: group.code,
                changesNumbers: ofGroup.some((setting) => setting.changesNumbers),
                settings: ofGroup,
            };
        });
}

/** The group with this code, or a 404 when the data does not hold it. */
function groupOf(groups: readonly EngineGroup[], id: string): EngineGroup {
    const group = groups.find((candidate) => candidate.id === id);
    if (group === undefined) {
        throw notFound('There is no such group of engine settings.');
    }
    return group;
}

/** Where the rows of this group live. A group the data holds is one this names. */
function transportOf(group: EngineGroup): GroupTransport {
    const transport = TRANSPORT[group.id];
    if (transport === undefined) {
        throw notFound('There is no such group of engine settings.');
    }
    return transport;
}

/** One setting's value: null for the engine default, else a value its kind accepts. */
function valueSchema(setting: EngineSetting): z.ZodType<boolean | number | string | null> {
    switch (setting.kind) {
        case 'flag':
            return z.boolean().nullable();
        case 'whole': {
            let whole = z.int().min(setting.min ?? 0);
            if (setting.max !== null) {
                whole = whole.max(setting.max);
            }
            return whole.nullable();
        }
        case 'choice':
            return z.enum(setting.options as [string, ...string[]]).nullable();
    }
}

function valuesSchema(group: EngineGroup): z.ZodType<Readonly<Record<string, unknown>>> {
    return z.strictObject(
        Object.fromEntries(group.settings.map((setting) => [setting.column, valueSchema(setting)])),
    );
}

const detailsSchema = z.object({
    name: z.string().trim().min(1).max(200),
    description: z.string().max(2000).default(''),
    partyId: idSchema.optional(),
    intent: intentSchema,
});

const updateSchema = detailsSchema.omit({ partyId: true }).extend({ version: z.int().min(1) });

const groupBodySchema = z.object({
    /** The version of the part the person read, or null when the group had no row. */
    version: z.int().min(1).nullable(),
    values: z.record(z.string(), z.unknown()),
    intent: intentSchema,
});

const removalSchema = z.object({ intent: intentSchema });

function num(value: unknown): number {
    return typeof value === 'number' ? value : 0;
}

interface PartyName {
    readonly name: string;
    readonly shortCode: string;
}

/** The parties the session works in, by id. A read that fails leaves the names empty. */
async function readPartyNames(session: LiveSession): Promise<ReadonlyMap<string, PartyName>> {
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

function partyOf(
    central: Readonly<Record<string, unknown>>,
    parties: ReadonlyMap<string, PartyName>,
): Readonly<Record<string, string>> {
    const partyId = text(central['party_id']);
    const party = parties.get(partyId);
    return { partyId, partyName: party?.name ?? '', partyCode: party?.shortCode ?? '' };
}

/** The values a part row holds, by column, with null for the engine default. */
function valuesOf(
    group: EngineGroup,
    part: Readonly<Record<string, unknown>>,
): Record<string, unknown> {
    return Object.fromEntries(
        group.settings.map((setting) => [setting.column, part[setting.column] ?? null]),
    );
}

/** The part of one group for one configuration, or null when the group has no row. */
function partOf(
    group: EngineGroup,
    parts: readonly Readonly<Record<string, unknown>>[],
    id: string,
): Readonly<Record<string, unknown>> | null {
    const found = parts.find((part) => text(part['engine_settings_id']) === id);
    if (found === undefined) {
        return null;
    }
    return {
        id: text(found['id']),
        version: num(found['version']),
        values: valuesOf(group, found),
        modifiedBy: text(found['modified_by']),
        recordedAt: text(found['recorded_at']),
        changeReason: text(found['change_reason_code']),
        changeCommentary: text(found['change_commentary']),
    };
}

async function readParts(
    session: LiveSession,
    ids: readonly string[] | null,
    groups: readonly EngineGroup[],
): Promise<ReadonlyMap<string, readonly Readonly<Record<string, unknown>>[]>> {
    const filter =
        ids === null
            ? null
            : { engine_settings_id: null, id_one_of: null, engine_settings_id_one_of: [...ids] };
    const all = await Promise.all(
        groups.map((group) => {
            const transport = transportOf(group);
            return readAll(session, transport.list, transport.rows, { filter, as_of: null });
        }),
    );
    return new Map(groups.map((group, index) => [group.id, all[index] ?? []]));
}

async function readCentral(
    session: LiveSession,
    id: string,
): Promise<Readonly<Record<string, unknown>>> {
    const found = await readAll(
        session,
        settingsSubjects.list_engine_settings_request,
        'settings',
        {
            filter: { configuration_id: null, id_one_of: [id], configuration_id_one_of: null },
            as_of: null,
        },
    );
    const central = found[0];
    if (central === undefined) {
        throw notFound('No engine settings have this id.');
    }
    return central;
}

/**
 * Refuses a configuration that another party owns.
 *
 * The service reads and writes a part under the party of the session, so from a
 * parent party the part is not found and a write would add a second one. The
 * session sees the configuration in a list, which spans its child parties, so the
 * refusal says whose it is and leaves the switch to the person.
 */
function requireOwningParty(
    session: LiveSession,
    central: Readonly<Record<string, unknown>>,
): void {
    const owner = text(central['party_id']);
    // A session with no party cannot be compared. A signed-in session always has one,
    // so this passes only a login still choosing its party, which cannot reach a route.
    if (session.party === undefined || session.party.id === owner) {
        return;
    }
    throw otherParty(
        session.availableParties.find((party) => party.id === owner)?.name ?? 'another party',
    );
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
                configuration_type_code: CONFIGURATION_TYPE,
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

/** What a screen reads of one configuration. */
async function documentOf(
    session: LiveSession,
    central: Readonly<Record<string, unknown>>,
    groups: readonly EngineGroup[],
): Promise<Readonly<Record<string, unknown>>> {
    const id = text(central['id']);
    const [parties, parts, reports] = await Promise.all([
        readPartyNames(session),
        readParts(session, [id], groups),
        readBoundReports(session, text(central['configuration_id'])).catch(() => null),
    ]);
    return {
        id,
        ...partyOf(central, parties),
        version: num(central['version']),
        name: text(central['name']),
        description: text(central['description']),
        configurationId: text(central['configuration_id']),
        modifiedBy: text(central['modified_by']),
        recordedAt: text(central['recorded_at']),
        changeReason: text(central['change_reason_code']),
        changeCommentary: text(central['change_commentary']),
        groups: Object.fromEntries(
            groups.map((group) => [group.id, partOf(group, parts.get(group.id) ?? [], id)]),
        ),
        reports,
    };
}

/** The settings of each group with the facts the screen needs and the documentation of each. */
async function documentation(
    session: LiveSession,
    groups: readonly EngineGroup[],
): Promise<Readonly<Record<string, unknown>>> {
    const parameters = await readAll(
        session,
        parameterSubjects.list_ore_parameters_request,
        'parameters',
        { filter: null, as_of: null },
    );
    const byName = new Map(
        parameters
            .filter(
                (parameter) =>
                    text(parameter['schema_name']) === SCHEMA_NAME &&
                    text(parameter['scope']) === SETUP_SCOPE,
            )
            .map((parameter) => [text(parameter['name']), parameter]),
    );
    return {
        groups: groups.map((group) => ({
            id: group.id,
            changesNumbers: group.settings.some((setting) => setting.changesNumbers),
            settings: group.settings.map((setting) => {
                const doc = byName.get(setting.ore);
                return {
                    column: setting.column,
                    ore: setting.ore,
                    kind: setting.kind,
                    changesNumbers: setting.changesNumbers,
                    options: setting.options,
                    min: setting.min,
                    max: setting.max,
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
        })),
    };
}

/** A refused write told as the status that says why. A stale version is a conflict. */
function writeRefusal(result: z.infer<typeof resultSchema>): HttpFailure {
    if (result.outcome === 'conflict') {
        return new HttpFailure(409, { code: 'conflict', message: result.message });
    }
    return refusal(result);
}

async function putCentral(
    session: LiveSession,
    writeBody: Readonly<Record<string, unknown>>,
    version: number | null,
    intent: z.infer<typeof intentSchema>,
): Promise<Readonly<Record<string, unknown>>> {
    const reply = await session.client.callAuthenticated(
        settingsSubjects.put_engine_settings_request,
        {
            change: {
                write: writeBody,
                precondition:
                    version === null
                        ? { kind: 'must_not_exist', version: null }
                        : { kind: 'must_match_version', version },
            },
            intent,
        },
        z.looseObject({ result: resultSchema, engine_settings: row.nullable().default(null) }),
    );
    if (reply.result.outcome !== 'ok') {
        throw writeRefusal(reply.result);
    }
    return reply.engine_settings ?? (await readCentral(session, text(writeBody['id'])));
}

/** The approval a proposal created, as the screen reads it. */
const proposalReplySchema = z.looseObject({
    result: resultSchema,
    request_id: z.string().default(''),
});

/**
 * Holds a write to a numbers-changing group in an approval request, and writes
 * no part.
 *
 * The service decides whether the group is numbers-changing and whether a
 * report binds the configuration; it refuses a proposal that is not one, so a
 * caller cannot make a governed configuration change in place by asking for a
 * proposal.
 */
async function proposeGroup(
    session: LiveSession,
    group: EngineGroup,
    id: string,
    version: number | null,
    values: Readonly<Record<string, unknown>>,
    intent: z.infer<typeof intentSchema>,
): Promise<Readonly<Record<string, unknown>>> {
    const reason =
        intent.commentary.trim() === ''
            ? intent.reason_code
            : `${intent.reason_code}: ${intent.commentary}`;
    const reply = await session.client.callAuthenticated(
        proposalSubjects.raise_engine_settings_change_request,
        {
            engine_settings_id: id,
            group_code: group.id,
            base_version: version ?? 0,
            values: group.settings.map((setting) => ({
                setting_code: `${group.id}:${setting.column}`,
                text_value: textValue(values[setting.column]),
            })),
            reason,
        },
        proposalReplySchema,
    );
    if (reply.result.outcome !== 'ok') {
        throw writeRefusal(reply.result);
    }
    return { requestId: reply.request_id };
}

/** A value as the text the proposal carries; an empty text is the engine default. */
function textValue(value: unknown): string {
    if (value === null || value === undefined) {
        return '';
    }
    if (typeof value === 'boolean') {
        return value ? 'true' : 'false';
    }
    return String(value);
}

async function createHeader(
    session: LiveSession,
    name: string,
    intent: z.infer<typeof intentSchema>,
): Promise<string> {
    const id = randomUUID();
    const result = await write(session, configurationSubjects.put_configuration_request, {
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
    });
    if (result.outcome !== 'ok') {
        throw writeRefusal(result);
    }
    return id;
}

async function removeHeader(
    session: LiveSession,
    name: string,
    intent: z.infer<typeof intentSchema>,
): Promise<void> {
    await write(session, configurationSubjects.delete_configuration_request, {
        removal: { key: { name }, precondition: { kind: 'any', version: null } },
        intent,
    }).catch(() => undefined);
}

/** The party a new configuration belongs to: the one asked for, else the session's own. */
function partyForNew(session: LiveSession, asked: string | undefined): string {
    const party = asked ?? session.party?.id;
    if (party === undefined) {
        throw invalidRequest('Choose the party that holds the engine settings.');
    }
    return party;
}

function paramId(request: FastifyRequest): string {
    return input(idSchema, (request.params as { id: string }).id);
}

export function registerEngineSettingsRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/reporting/engine-settings', async (request) => {
        const session = requireSession(request);
        const groups = await readGroups(session);
        const [parties, centrals, parts] = await Promise.all([
            readPartyNames(session),
            readAll(session, settingsSubjects.list_engine_settings_request, 'settings', {
                filter: null,
                as_of: null,
            }),
            readParts(session, null, groups),
        ]);
        const executionGroup = groups.find((group) => group.id === 'execution');
        return {
            rows: centrals.map((central) => {
                const id = text(central['id']);
                const execution =
                    executionGroup === undefined
                        ? null
                        : partOf(executionGroup, parts.get(executionGroup.id) ?? [], id);
                const values = (execution?.['values'] ?? {}) as Record<string, unknown>;
                return {
                    id,
                    ...partyOf(central, parties),
                    version: num(central['version']),
                    name: text(central['name']),
                    description: text(central['description']),
                    configurationId: text(central['configuration_id']),
                    groupsSet: groups.filter(
                        (group) => partOf(group, parts.get(group.id) ?? [], id) !== null,
                    ).length,
                    groups: groups.length,
                    threads: typeof values['n_threads'] === 'number' ? values['n_threads'] : null,
                    modifiedBy: text(central['modified_by']),
                    recordedAt: text(central['recorded_at']),
                };
            }),
        };
    });

    server.get('/api/reporting/engine-settings/documentation', async (request) => {
        const session = requireSession(request);
        return documentation(session, await readGroups(session));
    });

    server.get('/api/reporting/engine-settings/:id', async (request) => {
        const session = requireSession(request);
        const groups = await readGroups(session);
        return documentOf(session, await readCentral(session, paramId(request)), groups);
    });

    server.get('/api/reporting/engine-settings/:id/history', async (request) => {
        const session = requireSession(request);
        const id = paramId(request);
        const central = await readCentral(session, id);
        requireOwningParty(session, central);
        const centralVersions = await readAll(
            session,
            settingsSubjects.list_engine_settings_versions_request,
            'versions',
            { key: { name: text(central['name']) }, filter: null },
        );
        const groups = await readGroups(session);
        const parts = await readParts(session, [id], groups);
        const partVersions = await Promise.all(
            groups.map(async (group) => {
                const current = parts.get(group.id)?.[0];
                if (current === undefined) {
                    return [];
                }
                const rows = await readAll(session, transportOf(group).versions, 'versions', {
                    key: { id: text(current['id']) },
                    filter: null,
                });
                return rows.map((version) => ({ group: group.id, version }));
            }),
        );
        const entry = (kind: string, version: Readonly<Record<string, unknown>>) => ({
            kind,
            version: num(version['version']),
            modifiedBy: text(version['modified_by']),
            recordedAt: text(version['recorded_at']),
            changeReason: text(version['change_reason_code']),
            changeCommentary: text(version['change_commentary']),
            values:
                kind === 'central'
                    ? { name: text(version['name']), description: text(version['description']) }
                    : valuesOf(groupOf(groups, kind), version),
        });
        return {
            entries: [
                ...centralVersions
                    .filter((version) => text(version['id']) === id)
                    .map((version) => entry('central', version)),
                ...partVersions.flat().map(({ group, version }) => entry(group, version)),
            ].sort((a, b) => b.recordedAt.localeCompare(a.recordedAt)),
        };
    });

    server.post('/api/reporting/engine-settings', async (request, reply) => {
        const session = requireSession(request);
        const body = input(detailsSchema, request.body);
        const partyId = partyForNew(session, body.partyId);
        const configurationId = await createHeader(session, body.name, body.intent);
        const id = randomUUID();
        try {
            const central = await putCentral(
                session,
                {
                    id,
                    party_id: partyId,
                    name: body.name,
                    description: body.description,
                    configuration_id: configurationId,
                },
                null,
                body.intent,
            );
            void reply.code(201);
            return await documentOf(session, central, await readGroups(session));
        } catch (failure) {
            await removeHeader(session, body.name, body.intent);
            throw failure;
        }
    });

    server.put('/api/reporting/engine-settings/:id', async (request) => {
        const session = requireSession(request);
        const id = paramId(request);
        const body = input(updateSchema, request.body);
        const central = await readCentral(session, id);
        requireOwningParty(session, central);
        const updated = await putCentral(
            session,
            {
                id,
                party_id: text(central['party_id']),
                name: body.name,
                description: body.description,
                configuration_id: text(central['configuration_id']),
            },
            body.version,
            body.intent,
        );
        return documentOf(session, updated, await readGroups(session));
    });

    /**
     * Writes one group, or proposes the write.
     *
     * A group that changes the numbers a run computes and a configuration that
     * at least one report binds make the write a proposal: nothing in force
     * moves, and the answer is the approval the proposal created. A group that
     * changes no numbers, or a configuration that no report binds, changes in
     * place, with the reason the write carried kept against the version.
     */
    server.put('/api/reporting/engine-settings/:id/groups/:group', async (request, reply) => {
        const session = requireSession(request);
        const id = paramId(request);
        const code = input(groupSchema, (request.params as { group: string }).group);
        const groups = await readGroups(session);
        const group = groupOf(groups, code);
        const body = input(groupBodySchema, request.body);
        const values = input(valuesSchema(group), body.values);
        const central = await readCentral(session, id);
        requireOwningParty(session, central);
        if (group.changesNumbers) {
            // A read that fails is not a read that found no report: the
            // governed case is the one that must not be got wrong, so a
            // failure proposes rather than writes.
            const reports = await readBoundReports(
                session,
                text(central['configuration_id']),
            ).catch(() => null);
            if (reports === null || reports.length > 0) {
                const proposal = await proposeGroup(
                    session,
                    group,
                    id,
                    body.version,
                    values,
                    body.intent,
                );
                void reply.code(202);
                return { document: await documentOf(session, central, groups), proposal };
            }
        }
        const existing = (await readParts(session, [id], groups)).get(group.id)?.[0];
        const rowId = existing === undefined ? randomUUID() : text(existing['id']);
        const result = await write(session, transportOf(group).put, {
            change: {
                write: { id: rowId, engine_settings_id: id, ...values },
                precondition:
                    body.version === null
                        ? { kind: 'must_not_exist', version: null }
                        : { kind: 'must_match_version', version: body.version },
            },
            intent: body.intent,
        });
        if (result.outcome !== 'ok') {
            throw writeRefusal(result);
        }
        return {
            document: await documentOf(session, await readCentral(session, id), groups),
            proposal: null,
        };
    });

    server.delete('/api/reporting/engine-settings/:id', async (request, reply) => {
        const session = requireSession(request);
        const id = paramId(request);
        const body = input(removalSchema, request.body);
        const central = await readCentral(session, id);
        requireOwningParty(session, central);
        const reports = await readBoundReports(session, text(central['configuration_id']));
        if (reports.length > 0) {
            throw new HttpFailure(409, {
                code: 'conflict',
                message: 'A report binds these engine settings, so they cannot be deleted.',
            });
        }
        const groups = await readGroups(session);
        const parts = await readParts(session, [id], groups);
        for (const group of groups) {
            const part = parts.get(group.id)?.[0];
            if (part !== undefined) {
                const removed = await write(session, transportOf(group).delete, {
                    removal: {
                        key: { id: text(part['id']) },
                        precondition: { kind: 'any', version: null },
                    },
                    intent: body.intent,
                });
                if (removed.outcome !== 'ok') {
                    throw writeRefusal(removed);
                }
            }
        }
        const removed = await write(session, settingsSubjects.delete_engine_settings_request, {
            removal: {
                key: { name: text(central['name']) },
                precondition: { kind: 'any', version: null },
            },
            intent: body.intent,
        });
        if (removed.outcome !== 'ok') {
            throw writeRefusal(removed);
        }
        await removeHeader(session, text(central['name']), body.intent);
        void reply.code(204);
        return null;
    });
}
