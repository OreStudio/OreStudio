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

import { z } from 'zod';
import { request } from './transport.js';

/**
 * The engine settings screens' reads and writes.
 *
 * A configuration is a central row and three groups of settings. A group that has
 * no row is `null`, and a setting that is `null` inside a group means the engine
 * default, so a screen can always say which value is in force. A group is written
 * alone against the version the screen read, and a group that changed since is
 * refused as a conflict.
 */

const JSON_HEADERS = { 'Content-Type': 'application/json' } as const;

/** The groups of settings, in the order the screens draw them. */
export const GROUP_IDS = ['fixings', 'building', 'execution'] as const;

export type GroupId = (typeof GROUP_IDS)[number];

/** A value a setting holds: a flag, a whole number, a word from a short list, or null for the default. */
const valueSchema = z.union([z.boolean(), z.number(), z.string()]).nullable();

export type SettingValue = z.infer<typeof valueSchema>;

/** The values of one group, by the column of each setting. */
export type GroupValues = Readonly<Record<string, SettingValue>>;

const partSchema = z.object({
    id: z.string(),
    version: z.int().positive(),
    values: z.record(z.string(), valueSchema),
    modifiedBy: z.string(),
    recordedAt: z.string(),
    changeReason: z.string(),
    changeCommentary: z.string(),
});

const documentSchema = z.object({
    id: z.string(),
    partyId: z.string(),
    partyName: z.string(),
    partyCode: z.string(),
    version: z.int().nonnegative(),
    name: z.string(),
    description: z.string(),
    configurationId: z.string(),
    modifiedBy: z.string(),
    recordedAt: z.string(),
    changeReason: z.string(),
    changeCommentary: z.string(),
    groups: z.object({
        fixings: partSchema.nullable(),
        building: partSchema.nullable(),
        execution: partSchema.nullable(),
    }),
    /** The reports that bind the configuration, or null when the read of them failed. */
    reports: z.array(z.object({ id: z.string(), name: z.string() })).nullable(),
});

const summarySchema = z.object({
    id: z.string(),
    partyId: z.string(),
    partyName: z.string(),
    partyCode: z.string(),
    version: z.int().nonnegative(),
    name: z.string(),
    description: z.string(),
    configurationId: z.string(),
    groupsSet: z.int().nonnegative(),
    groups: z.int().nonnegative(),
    threads: z.int().nullable(),
    modifiedBy: z.string(),
    recordedAt: z.string(),
});

const settingSchema = z.object({
    column: z.string(),
    ore: z.string(),
    kind: z.enum(['flag', 'whole', 'choice']),
    /** Whether the setting changes the numbers a run computes, and is not only operational. */
    changesNumbers: z.boolean(),
    options: z.array(z.string()),
    min: z.int().nullable(),
    max: z.int().nullable(),
    /** False when the ore parameter of the setting is not in the data. */
    documented: z.boolean(),
    label: z.string(),
    help: z.string(),
    description: z.string(),
    defaultText: z.string(),
    format: z.string(),
    introducedIn: z.string(),
    removedIn: z.string(),
    xpath: z.string(),
});

const documentationSchema = z.object({
    groups: z.array(
        z.object({
            id: z.enum(GROUP_IDS),
            changesNumbers: z.boolean(),
            settings: z.array(settingSchema),
        }),
    ),
});

const historySchema = z.object({
    entries: z.array(
        z.object({
            kind: z.enum(['central', ...GROUP_IDS]),
            version: z.int().positive(),
            modifiedBy: z.string(),
            recordedAt: z.string(),
            changeReason: z.string(),
            changeCommentary: z.string(),
            values: z.record(z.string(), valueSchema).nullable(),
        }),
    ),
});

/** One group of a configuration: its row, the values in it and the version it is at. */
export type EnginePart = z.infer<typeof partSchema>;

/** A configuration with its three groups. */
export type EngineDocument = z.infer<typeof documentSchema>;

/**
 * The approval a numbers-changing write became, when it became one.
 *
 * A write to a group that moves the numbers a run computes, on a configuration
 * a report binds, is a proposal: nothing in force moves and the approval decides
 * the change. The screen names the request so a person can follow it.
 */
const proposalSchema = z.object({
    requestId: z.string(),
});

export type EngineProposal = z.infer<typeof proposalSchema>;

/** What a group write answered: the configuration as it now stands, and the approval when the write was a proposal. */
const saveResultSchema = z.object({
    document: documentSchema,
    proposal: proposalSchema.nullable().default(null),
});

export type EngineSaveResult = z.infer<typeof saveResultSchema>;

/** The row a list shows for a configuration. */
export type EngineSummary = z.infer<typeof summarySchema>;

/** One setting as the screen knows it, with the documentation of its ore parameter. */
export type EngineSetting = z.infer<typeof settingSchema>;

/** The settings of every group with their documentation. */
export type EngineDocumentation = z.infer<typeof documentationSchema>;

/** One change in the history: a version of the central row or of one group. */
export type EngineHistoryEntry = z.infer<typeof historySchema>['entries'][number];

/** Why a write is made. */
export interface EngineIntent {
    readonly reason_code: string;
    readonly commentary: string;
}

const ROOT = '/api/reporting/engine-settings';

export const engineSettings = {
    /** Every configuration the session's party holds. */
    async list(): Promise<readonly EngineSummary[]> {
        return z
            .object({ rows: z.array(summarySchema) })
            .parse(await request(ROOT, { method: 'GET' })).rows;
    },

    /** What each setting is and what it means. */
    async documentation(): Promise<EngineDocumentation> {
        return documentationSchema.parse(await request(`${ROOT}/documentation`, { method: 'GET' }));
    },

    /** One configuration with its three groups. */
    async read(id: string): Promise<EngineDocument> {
        return documentSchema.parse(
            await request(`${ROOT}/${encodeURIComponent(id)}`, { method: 'GET' }),
        );
    },

    /** Every version of the configuration and of its groups, newest first. */
    async history(id: string): Promise<readonly EngineHistoryEntry[]> {
        return historySchema.parse(
            await request(`${ROOT}/${encodeURIComponent(id)}/history`, { method: 'GET' }),
        ).entries;
    },

    /** Creates a configuration and the reporting header a report binds it through. */
    async create(write: {
        readonly name: string;
        readonly description: string;
        readonly intent: EngineIntent;
    }): Promise<EngineDocument> {
        return documentSchema.parse(
            await request(ROOT, {
                method: 'POST',
                headers: JSON_HEADERS,
                body: JSON.stringify(write),
            }),
        );
    },

    /** Changes the name and the description against the version the screen read. */
    async rename(
        id: string,
        version: number,
        write: {
            readonly name: string;
            readonly description: string;
            readonly intent: EngineIntent;
        },
    ): Promise<EngineDocument> {
        return documentSchema.parse(
            await request(`${ROOT}/${encodeURIComponent(id)}`, {
                method: 'PUT',
                headers: JSON_HEADERS,
                body: JSON.stringify({ ...write, version }),
            }),
        );
    },

    /**
     * Writes one group against the version of its row, or null when it has none.
     * The values name every setting of the group, null for the engine default.
     *
     * A group that changes the numbers a run computes, on a configuration a
     * report binds, is proposed instead: the answer then names the approval and
     * the configuration stands as it was.
     */
    async saveGroup(
        id: string,
        group: GroupId,
        version: number | null,
        values: GroupValues,
        intent: EngineIntent,
    ): Promise<EngineSaveResult> {
        return saveResultSchema.parse(
            await request(`${ROOT}/${encodeURIComponent(id)}/groups/${group}`, {
                method: 'PUT',
                headers: JSON_HEADERS,
                body: JSON.stringify({ version, values, intent }),
            }),
        );
    },

    /** Removes the configuration and its groups. */
    async remove(id: string, intent: EngineIntent): Promise<void> {
        await request(`${ROOT}/${encodeURIComponent(id)}`, {
            method: 'DELETE',
            headers: JSON_HEADERS,
            body: JSON.stringify({ intent }),
        });
    },
};
