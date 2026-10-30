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

import type {
    EngineDocumentation,
    EngineHistoryEntry,
    EnginePart,
    EngineSetting,
    GroupId,
    SettingValue,
} from '../api/engineSettings.js';
import { NO_SOURCE_KIND, type ValueSetKind } from '../api/valueSets.js';

/**
 * What the engine settings screens decide, apart from how they draw it: the
 * values a group holds and the ones a person is changing, the check of a value
 * against what its setting accepts, the difference a change makes, and the way
 * the history reads as changes.
 *
 * A setting that is `null` is not set, and the engine uses its default. That is
 * a value of its own and never an empty one, so a person can put a setting back
 * to the default as well as give it a value.
 */

export type EngineGroup = EngineDocumentation['groups'][number];

/** The values of one group, by the column of each setting, as a person is editing them. */
export type Draft = Readonly<Record<string, SettingValue>>;

/** The groups of a configuration, in the order the screens draw them. */
export const GROUP_ORDER: readonly GroupId[] = ['fixings', 'building', 'execution'];

/** A group with every setting at its default. */
export function defaultsOf(group: EngineGroup): Draft {
    return Object.fromEntries(group.settings.map((setting) => [setting.column, null]));
}

/** The values given, with the settings they leave out at their default. */
export function draftFromValues(
    group: EngineGroup,
    values: Readonly<Record<string, SettingValue>>,
): Draft {
    return Object.fromEntries(
        group.settings.map((setting) => [setting.column, values[setting.column] ?? null]),
    );
}

/** The values a group holds, with the settings the row leaves out at their default. */
export function draftOf(group: EngineGroup, part: EnginePart | null): Draft {
    return draftFromValues(group, part?.values ?? {});
}

/** A value as the text of its control: empty for the default. */
export function controlText(value: SettingValue): string {
    if (value === null) {
        return '';
    }
    if (typeof value === 'boolean') {
        return value ? 'yes' : 'no';
    }
    return String(value);
}

/**
 * The value a control's text means for a setting, or `invalid` when the text is
 * not a value the setting's value set holds. An empty text is the default.
 *
 * The set is the only source: its kind says whether the text is a switch, a
 * whole number, a number or one of the choices, and its bounds are applied by
 * problemOf.
 */
export function valueOfText(setting: EngineSetting, text: string): SettingValue | 'invalid' {
    const trimmed = text.trim();
    if (trimmed === '') {
        return null;
    }
    switch (setting.sourceKind) {
        case 'boolean':
            return trimmed === 'yes' ? true : trimmed === 'no' ? false : 'invalid';
        case 'integer':
            return /^-?\d{1,9}$/.test(trimmed) ? Number(trimmed) : 'invalid';
        case 'number':
        case 'range':
            return Number.isFinite(Number(trimmed)) ? Number(trimmed) : 'invalid';
        case 'enumerated':
        case 'catalogue':
            return setting.choices.some((choice) => choice.value === trimmed) ? trimmed : 'invalid';
        default:
            return trimmed;
    }
}

/** A problem with a value, as the message key and the words that fill it. */
export interface Problem {
    readonly key: string;
    readonly params: Readonly<Record<string, string>>;
}

/**
 * The words for a text its set does not hold, by the kind the set states.
 *
 * Every kind answers for itself, so a kind this build does not know is a
 * compile error rather than a sentence about a list of choices.
 */
function problemKeyOf(kind: ValueSetKind): string {
    switch (kind) {
        case 'integer':
            return 'engineSettings.problem.whole';
        case 'number':
        case 'range':
            return 'engineSettings.problem.number';
        case 'boolean':
            return 'engineSettings.problem.flag';
        case 'enumerated':
        case 'catalogue':
            return 'engineSettings.problem.choice';
        case 'regex':
        case 'text':
        case NO_SOURCE_KIND:
            return 'engineSettings.problem.text';
    }
}

/** What is wrong with the text a person typed for a setting, or null when it is a value the setting takes. */
export function problemOf(setting: EngineSetting, text: string, label: string): Problem | null {
    const value = valueOfText(setting, text);
    if (value === 'invalid') {
        return {
            key: problemKeyOf(setting.sourceKind),
            params: { setting: label },
        };
    }
    if (typeof value === 'number') {
        if (setting.minimum !== null && value < setting.minimum) {
            return {
                key: 'engineSettings.problem.min',
                params: { setting: label, min: String(setting.minimum) },
            };
        }
        if (setting.maximum !== null && value > setting.maximum) {
            return {
                key: 'engineSettings.problem.max',
                params: { setting: label, max: String(setting.maximum) },
            };
        }
    }
    return null;
}

/** The columns whose value a draft changes from the saved one. */
export function changedColumns(group: EngineGroup, saved: Draft, draft: Draft): readonly string[] {
    return group.settings
        .filter((setting) => (saved[setting.column] ?? null) !== (draft[setting.column] ?? null))
        .map((setting) => setting.column);
}

/** One change of a value, for the list a save shows. */
export interface Change {
    readonly column: string;
    readonly before: SettingValue;
    readonly after: SettingValue;
}

export function differenceOf(group: EngineGroup, saved: Draft, draft: Draft): readonly Change[] {
    return changedColumns(group, saved, draft).map((column) => ({
        column,
        before: saved[column] ?? null,
        after: draft[column] ?? null,
    }));
}

/** The filters a person narrows a long list of settings by. */
export interface Filters {
    /** Only the settings this configuration sets, which the engine would otherwise default. */
    readonly set: boolean;
    /** Only the settings that change the numbers a run computes. */
    readonly numbers: boolean;
}

export const NO_FILTERS: Filters = { set: false, numbers: false };

/** The settings of a group that pass the filters, judged against the values saved. */
export function visibleSettings(
    group: EngineGroup,
    saved: Draft,
    filters: Filters,
): readonly EngineSetting[] {
    return group.settings.filter(
        (setting) =>
            (!filters.set || (saved[setting.column] ?? null) !== null) &&
            (!filters.numbers || setting.changesNumbers),
    );
}

/** How many settings of a group the configuration sets, and how many the group has. */
export function setCount(group: EngineGroup, saved: Draft): number {
    return group.settings.filter((setting) => (saved[setting.column] ?? null) !== null).length;
}

/** The label of a setting, or the name ORE gives it when its ore parameter is missing. */
export function labelOf(setting: EngineSetting): string {
    return setting.label === '' ? setting.ore : setting.label;
}

/** A change in the history, with the values the group had before it and whether it is the current one. */
export interface Step {
    readonly entry: EngineHistoryEntry;
    readonly previous: Draft | null;
    readonly current: boolean;
    readonly first: boolean;
}

/**
 * The history as steps, newest first. A step of a group is compared with the step
 * of the same group before it, and the newest of each kind is the current one, so
 * a person is never offered a revert to what the group already holds.
 */
export function stepsOf(entries: readonly EngineHistoryEntry[]): readonly Step[] {
    const ordered = [...entries].sort((a, b) => b.recordedAt.localeCompare(a.recordedAt));
    return ordered.map((entry, index) => {
        const same = ordered.filter((other) => other.kind === entry.kind);
        const position = same.indexOf(entry);
        const older = same[position + 1];
        return {
            entry,
            previous: older?.values ?? null,
            current: position === 0,
            first: older === undefined,
        };
    });
}

/**
 * The values a revert to a step writes: what the group held at that version, as a
 * new version. The current version offers none, because the group already holds it.
 */
export function revertValues(group: EngineGroup, step: Step): Draft | null {
    if (step.current || step.entry.kind === 'central' || step.entry.values === null) {
        return null;
    }
    return draftFromValues(group, step.entry.values);
}
