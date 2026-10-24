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

import { describe, expect, it } from 'vitest';
import type { EngineHistoryEntry } from '../api/engineSettings.js';
import {
    changedColumns,
    controlText,
    defaultsOf,
    differenceOf,
    draftOf,
    labelOf,
    NO_FILTERS,
    problemOf,
    revertValues,
    setCount,
    shortDefault,
    stepsOf,
    valueOfText,
    visibleSettings,
    type EngineGroup,
} from './model.js';

const setting = (overrides: Partial<EngineGroup['settings'][number]>) => ({
    column: 'n_threads',
    ore: 'nThreads',
    kind: 'whole' as const,
    changesNumbers: false,
    options: [],
    min: 1,
    max: null,
    documented: true,
    label: 'Threads',
    help: '',
    description: '',
    defaultText: '1.',
    format: '',
    introducedIn: '1.8.9.0',
    removedIn: '',
    xpath: '',
    ...overrides,
});

const EXECUTION: EngineGroup = {
    id: 'execution',
    changesNumbers: false,
    settings: [
        setting({}),
        setting({ column: 'log_mask', ore: 'logMask', min: 0, label: 'Log mask' }),
        setting({
            column: 'gzip_compression_level',
            ore: 'gzipCompressionLevel',
            min: 0,
            max: 9,
            label: 'Gzip compression level',
        }),
    ],
};

const BUILDING: EngineGroup = {
    id: 'building',
    changesNumbers: true,
    settings: [
        setting({
            column: 'observation_model',
            ore: 'observationModel',
            kind: 'choice',
            options: ['None', 'Disable', 'Defer', 'Unregister'],
            min: null,
            label: 'Observation model',
        }),
        setting({
            column: 'build_failed_trades',
            ore: 'buildFailedTrades',
            kind: 'flag',
            changesNumbers: true,
            min: null,
            label: 'Build failed trades',
        }),
    ],
};

describe('the values of a group', () => {
    it('start every setting at its default', () => {
        expect(defaultsOf(EXECUTION)).toEqual({
            n_threads: null,
            log_mask: null,
            gzip_compression_level: null,
        });
    });

    it('read the settings a row leaves out as the default', () => {
        const part = {
            id: 'p',
            version: 2,
            values: { n_threads: 4 },
            modifiedBy: '',
            recordedAt: '',
            changeReason: '',
            changeCommentary: '',
        };
        expect(draftOf(EXECUTION, part)).toEqual({
            n_threads: 4,
            log_mask: null,
            gzip_compression_level: null,
        });
        expect(draftOf(EXECUTION, null)).toEqual(defaultsOf(EXECUTION));
    });

    it('tell a group with no row from a setting put back to the default', () => {
        expect(
            setCount(EXECUTION, { n_threads: 4, log_mask: null, gzip_compression_level: 0 }),
        ).toBe(2);
        expect(setCount(EXECUTION, defaultsOf(EXECUTION))).toBe(0);
    });
});

describe('the text of a control', () => {
    it('is empty for the default and spells a flag as yes or no', () => {
        expect(controlText(null)).toBe('');
        expect(controlText(true)).toBe('yes');
        expect(controlText(false)).toBe('no');
        expect(controlText(0)).toBe('0');
        expect(controlText('Defer')).toBe('Defer');
    });

    it('reads back as the value it came from, and zero is not the default', () => {
        const threads = EXECUTION.settings[0]!;
        expect(valueOfText(threads, '')).toBeNull();
        expect(valueOfText(threads, ' 8 ')).toBe(8);
        expect(valueOfText(EXECUTION.settings[1]!, '0')).toBe(0);
        expect(valueOfText(BUILDING.settings[1]!, 'no')).toBe(false);
        expect(valueOfText(BUILDING.settings[1]!, 'yes')).toBe(true);
        expect(valueOfText(BUILDING.settings[0]!, 'Defer')).toBe('Defer');
    });

    it.each([
        ['a fraction', 0, '1.5'],
        ['a negative number', 0, '-1'],
        ['words in a number', 0, 'four'],
        ['a flag that is not yes or no', 1, 'maybe'],
    ])('does not read %s', (_what, index, text) => {
        const target = index === 0 ? EXECUTION.settings[0]! : BUILDING.settings[1]!;
        expect(valueOfText(target, text)).toBe('invalid');
    });

    it('does not read a word that is not in the list', () => {
        expect(valueOfText(BUILDING.settings[0]!, 'Bogus')).toBe('invalid');
    });
});

describe('the check of a value', () => {
    it('accepts the default and a value in range', () => {
        expect(problemOf(EXECUTION.settings[0]!, '', 'Threads')).toBeNull();
        expect(problemOf(EXECUTION.settings[0]!, '1', 'Threads')).toBeNull();
        expect(problemOf(EXECUTION.settings[2]!, '9', 'Gzip')).toBeNull();
    });

    it('names the bound a number breaks', () => {
        expect(problemOf(EXECUTION.settings[0]!, '0', 'Threads')).toEqual({
            key: 'engineSettings.problem.min',
            params: { setting: 'Threads', min: '1' },
        });
        expect(problemOf(EXECUTION.settings[2]!, '10', 'Gzip')).toEqual({
            key: 'engineSettings.problem.max',
            params: { setting: 'Gzip', max: '9' },
        });
    });

    it('says a number is not a whole number, and a word is not on the list', () => {
        expect(problemOf(EXECUTION.settings[0]!, '1.5', 'Threads')?.key).toBe(
            'engineSettings.problem.whole',
        );
        expect(problemOf(BUILDING.settings[0]!, 'Bogus', 'Observation')?.key).toBe(
            'engineSettings.problem.choice',
        );
    });
});

describe('the difference a change makes', () => {
    const saved = { n_threads: 4, log_mask: null, gzip_compression_level: 6 };

    it('lists only the settings that moved, with what they were and what they become', () => {
        const draft = { n_threads: 8, log_mask: null, gzip_compression_level: null };
        expect(changedColumns(EXECUTION, saved, draft)).toEqual([
            'n_threads',
            'gzip_compression_level',
        ]);
        expect(differenceOf(EXECUTION, saved, draft)).toEqual([
            { column: 'n_threads', before: 4, after: 8 },
            { column: 'gzip_compression_level', before: 6, after: null },
        ]);
    });

    it('is empty when nothing moved, and a value typed back is not a change', () => {
        expect(changedColumns(EXECUTION, saved, { ...saved })).toEqual([]);
        expect(changedColumns(EXECUTION, saved, { ...saved, n_threads: 8 })).toHaveLength(1);
        expect(changedColumns(EXECUTION, saved, { ...saved, n_threads: 4 })).toEqual([]);
    });
});

describe('the filters', () => {
    const saved = { observation_model: 'Defer', build_failed_trades: null };

    it('show every setting without a filter', () => {
        expect(visibleSettings(BUILDING, saved, NO_FILTERS)).toHaveLength(2);
    });

    it('keep the settings the configuration sets', () => {
        expect(
            visibleSettings(BUILDING, saved, { set: true, numbers: false }).map((s) => s.ore),
        ).toEqual(['observationModel']);
    });

    it('keep the settings that change the numbers', () => {
        expect(
            visibleSettings(BUILDING, saved, { set: false, numbers: true }).map((s) => s.ore),
        ).toEqual(['buildFailedTrades']);
    });

    it('keep what passes both', () => {
        expect(visibleSettings(BUILDING, saved, { set: true, numbers: true })).toEqual([]);
    });
});

describe('the label of a setting', () => {
    it('is the label of its ore parameter, or the name ORE gives it when there is none', () => {
        expect(labelOf(EXECUTION.settings[0]!)).toBe('Threads');
        expect(labelOf(setting({ label: '', ore: 'nThreads' }))).toBe('nThreads');
    });
});

const entry = (
    kind: EngineHistoryEntry['kind'],
    version: number,
    at: string,
    values: EngineHistoryEntry['values'],
): EngineHistoryEntry => ({
    kind,
    version,
    modifiedBy: 'analyst',
    recordedAt: at,
    changeReason: 'common.correction',
    changeCommentary: '',
    values,
});

describe('the history as steps', () => {
    const entries = [
        entry('central', 1, '2026-10-18 09:00:00Z', null),
        entry('execution', 1, '2026-10-18 09:01:00Z', { n_threads: 1 }),
        entry('execution', 2, '2026-10-19 09:00:00Z', { n_threads: 4 }),
        entry('execution', 3, '2026-10-20 09:00:00Z', { n_threads: 8 }),
    ];

    it('puts the newest first and marks the newest of each kind as current', () => {
        const steps = stepsOf(entries);
        expect(steps.map((step) => `${step.entry.kind}${step.entry.version}`)).toEqual([
            'execution3',
            'execution2',
            'execution1',
            'central1',
        ]);
        expect(steps.map((step) => step.current)).toEqual([true, false, false, true]);
        expect(steps.map((step) => step.first)).toEqual([false, false, true, true]);
    });

    it('compares a step with the one of the same group before it', () => {
        const steps = stepsOf(entries);
        expect(steps[0]?.previous).toEqual({ n_threads: 4 });
        expect(steps[1]?.previous).toEqual({ n_threads: 1 });
        expect(steps[2]?.previous).toBeNull();
    });

    it('offers a revert to every earlier version of a group, with the values it held', () => {
        const steps = stepsOf(entries);
        expect(revertValues(EXECUTION, steps[0]!)).toBeNull();
        expect(revertValues(EXECUTION, steps[1]!)).toEqual({
            n_threads: 4,
            log_mask: null,
            gzip_compression_level: null,
        });
        expect(revertValues(EXECUTION, steps[2]!)).toEqual({
            n_threads: 1,
            log_mask: null,
            gzip_compression_level: null,
        });
        expect(revertValues(EXECUTION, steps[3]!)).toBeNull();
    });
});

describe('the short default', () => {
    it('is the value the default text starts with', () => {
        expect(shortDefault('No. Today’s fixing is loaded.')).toBe('No');
        expect(shortDefault('15. Alert, critical, error and warning.')).toBe('15');
        expect(shortDefault('None.')).toBe('None');
        expect(shortDefault('Yes')).toBe('Yes');
        expect(shortDefault('')).toBe('');
    });
});
