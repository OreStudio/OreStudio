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

import {
    chmodSync,
    mkdirSync,
    mkdtempSync,
    readdirSync,
    readFileSync,
    renameSync,
    rmSync,
    statSync,
    symlinkSync,
    writeFileSync,
} from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { afterEach, beforeEach, describe, expect, it } from 'vitest';
import { FileScenarioStore } from './file-scenario-store.js';
import { UnknownStepError, type RunInput, type StepOutcome } from './scenario-doc.js';
import { InvalidDocIdError, PathEscapeError } from './scenario-store.js';

const fixture = (name: string): string =>
    readFileSync(new URL(`./fixtures/${name}`, import.meta.url), 'utf8');

const SINGLE = 'A607D53A-66E3-4211-AE3B-069A603F509F';
const MULTI = 'B12A8474-32FF-45B7-BFD8-FF1B42845DAD';
const STORY = 'FE07BF4D-054D-4A69-AF3C-D70D10493370';

const storyDoc = `:PROPERTIES:
:ID: ${STORY}
:END:
#+title: Commission: party, counterparty, and party_status
#+type: story

* Goal

A story.
`;

function input(steps: StepOutcome[], state: RunInput['state'] = 'FAILED'): RunInput {
    return {
        state,
        steps,
        completedAt: '2026-10-11T09:00:00Z',
        branch: 'feature/x',
        commit: 'abc1234',
        worktree: 'eager_maxwell',
    };
}

let root: string;
let outside: string;

function put(path: string, text: string): void {
    const full = join(root, path);
    mkdirSync(join(full, '..'), { recursive: true });
    writeFileSync(full, text);
}

beforeEach(() => {
    const base = mkdtempSync(join(tmpdir(), 'ores-scenarios-'));
    root = join(base, 'doc');
    outside = join(base, 'outside');
    mkdirSync(root);
    mkdirSync(outside);
    put('sprint_23/story/scenario_single.org', fixture('single_client_scenario.org'));
    put('sprint_23/story/scenario_multi.org', fixture('multi_client_scenario.org'));
    put('sprint_23/story/story.org', storyDoc);
    put('sprint_23/story/notes.txt', 'not an org file');
});

afterEach(() => {
    rmSync(join(root, '..'), { recursive: true, force: true });
});

const store = () => new FileScenarioStore(root, { rescanAfterMs: 0 });

describe('listing scenarios', () => {
    it('lists each scenario with its state, target, story and task', async () => {
        const list = await store().list();
        expect(list.map((s) => s.id)).toEqual([MULTI, SINGLE]);
        const multi = list.find((s) => s.id === MULTI);
        expect(multi).toMatchObject({
            state: 'FAILED',
            phase: 'done',
            clients: ['blue', 'red'],
            path: 'sprint_23/story/scenario_multi.org',
            story: { id: STORY },
            completedAt: '2026-07-15T08:31:59Z',
        });
        expect(multi?.target).toContain('CounterpartyDetailDialog');
        expect(multi?.steps).toEqual({ total: 13, pending: 10, pass: 2, fail: 1, dropped: 0 });
    });

    it('tells a waiting scenario from a done one', async () => {
        put(
            'sprint_23/story/scenario_waiting.org',
            fixture('single_client_scenario.org')
                .replace(SINGLE, 'C0000000-0000-4000-8000-000000000001')
                .replace(/\| Status\s+\| PASSED \|/, '| Status        |       |'),
        );
        const list = await store().list();
        const waiting = list.find((s) => s.id === 'C0000000-0000-4000-8000-000000000001');
        expect(waiting).toMatchObject({ state: 'PENDING', phase: 'waiting' });
        expect(list.find((s) => s.id === SINGLE)?.phase).toBe('done');
    });

    it('leaves out docs that are not scenarios and files that are not org', async () => {
        const list = await store().list();
        expect(list.some((s) => s.id === STORY)).toBe(false);
    });

    it('does not follow a symbolic link out of the root', async () => {
        writeFileSync(
            join(outside, 'scenario_escape.org'),
            fixture('single_client_scenario.org').replace(
                SINGLE,
                'D0000000-0000-4000-8000-000000000002',
            ),
        );
        symlinkSync(outside, join(root, 'linked'));
        symlinkSync(
            join(outside, 'scenario_escape.org'),
            join(root, 'sprint_23/story/scenario_link.org'),
        );
        const list = await store().list();
        expect(list.map((s) => s.id)).toEqual([MULTI, SINGLE]);
    });
});

describe('reading', () => {
    it('reads a scenario by id in any case', async () => {
        const scenario = await store().read(SINGLE.toLowerCase());
        expect(scenario?.steps).toHaveLength(5);
    });

    it('finds any doc by its :ID:', async () => {
        const doc = await store().readDoc(STORY);
        expect(doc).toMatchObject({
            id: STORY,
            type: 'story',
            path: 'sprint_23/story/story.org',
        });
        expect(doc?.text).toContain('* Goal');
    });

    it('answers null for an id nothing has, and for a story read as a scenario', async () => {
        expect(await store().readDoc('E0000000-0000-4000-8000-000000000003')).toBeNull();
        expect(await store().read(STORY)).toBeNull();
    });

    it('refuses anything that is not a document id', async () => {
        for (const bad of [
            '../../etc/passwd',
            '',
            'sprint_23/story/story.org',
            `${STORY}/..`,
            '%2e%2e',
        ]) {
            await expect(store().readDoc(bad)).rejects.toBeInstanceOf(InvalidDocIdError);
            await expect(store().record(bad, input([]))).rejects.toBeInstanceOf(InvalidDocIdError);
        }
    });

    it('finds a doc added after the first scan', async () => {
        const s = store();
        await s.list();
        put(
            'sprint_24/scenario_new.org',
            fixture('single_client_scenario.org').replace(
                SINGLE,
                'F0000000-0000-4000-8000-000000000004',
            ),
        );
        expect((await s.read('F0000000-0000-4000-8000-000000000004'))?.steps).toHaveLength(5);
    });

    it('does not scan again within the interval', async () => {
        const s = new FileScenarioStore(root, { rescanAfterMs: 60_000 });
        await s.list();
        put(
            'sprint_24/scenario_new.org',
            fixture('single_client_scenario.org').replace(
                SINGLE,
                'F0000000-0000-4000-8000-000000000004',
            ),
        );
        expect(await s.read('F0000000-0000-4000-8000-000000000004')).toBeNull();
    });

    it('reads a doc that moved after the scan', async () => {
        const s = store();
        await s.list();
        renameSync(join(root, 'sprint_23/story/story.org'), join(root, 'sprint_23/moved.org'));
        expect((await s.readDoc(STORY))?.path).toBe('sprint_23/moved.org');
    });
});

describe('recording a run', () => {
    const file = () => join(root, 'sprint_23/story/scenario_multi.org');

    it('writes the run into the file and names the steps it changed', async () => {
        const result = await store().record(
            MULTI,
            input([
                { client: 'blue', title: 'Read', status: 'PASS', notes: 'fine' },
                { client: 'red', title: 'Connect as a second instance', status: 'PASS', notes: '' },
            ]),
        );
        expect(result.changed).toEqual([
            { client: 'blue', title: 'Read' },
            { client: 'red', title: 'Connect as a second instance' },
        ]);
        const back = await store().read(MULTI);
        expect(back?.steps.find((s) => s.title === 'Read')).toMatchObject({
            status: 'PASS',
            notes: 'fine',
        });
        expect(readFileSync(file(), 'utf8')).toContain('| Commit        | abc1234 |');
        expect(result.scenario.run.worktree).toBe('eager_maxwell');
    });

    it('shows a save in the next listing, even before a rescan', async () => {
        const s = new FileScenarioStore(root, { rescanAfterMs: 60_000 });
        const before = await s.list();
        expect(before.find((x) => x.id === MULTI)?.steps.pass).toBe(2);
        await s.record(
            MULTI,
            input([{ client: 'blue', title: 'Read', status: 'PASS', notes: '' }], 'PENDING'),
        );
        const after = await s.list();
        expect(after.find((x) => x.id === MULTI)?.steps.pass).toBe(3);
    });

    it('leaves the file as it was when a step is unknown', async () => {
        const before = readFileSync(file(), 'utf8');
        await expect(
            store().record(
                MULTI,
                input([
                    { client: 'blue', title: 'Read', status: 'PASS', notes: '' },
                    { client: 'blue', title: 'No such step', status: 'PASS', notes: '' },
                ]),
            ),
        ).rejects.toBeInstanceOf(UnknownStepError);
        expect(readFileSync(file(), 'utf8')).toBe(before);
    });

    it('refuses an id that is not a scenario', async () => {
        await expect(store().record(STORY, input([]))).rejects.toThrow(/No scenario/);
        await expect(
            store().record('E0000000-0000-4000-8000-000000000003', input([])),
        ).rejects.toThrow(/No scenario/);
    });

    it('keeps the mode of the file and leaves no temporary file', async () => {
        chmodSync(file(), 0o640);
        await store().record(
            MULTI,
            input([{ client: 'blue', title: 'Read', status: 'PASS', notes: '' }]),
        );
        expect(statSync(file()).mode & 0o777).toBe(0o640);
        expect(
            readdirSync(join(root, 'sprint_23/story')).filter((n) => n.endsWith('.tmp')),
        ).toEqual([]);
    });

    it('lets the last save to name a step win it, and keeps the rest', async () => {
        const s = store();
        await Promise.all([
            s.record(
                MULTI,
                input([
                    { client: 'blue', title: 'Read', status: 'PASS', notes: 'first' },
                    { client: 'blue', title: 'Update', status: 'PASS', notes: 'only first' },
                ]),
            ),
            s.record(
                MULTI,
                input([{ client: 'blue', title: 'Read', status: 'FAIL', notes: 'second' }]),
            ),
        ]);
        const back = await s.read(MULTI);
        expect(back?.steps.find((x) => x.title === 'Read')).toMatchObject({
            status: 'FAIL',
            notes: 'second',
        });
        expect(back?.steps.find((x) => x.title === 'Update')?.notes).toBe('only first');
    });

    it('does not write through a file that now points outside the root', async () => {
        const s = store();
        await s.list();
        const target = join(outside, 'victim.org');
        writeFileSync(target, fixture('multi_client_scenario.org'));
        rmSync(file());
        symlinkSync(target, file());
        const before = readFileSync(target, 'utf8');
        await expect(
            s.record(MULTI, input([{ client: 'blue', title: 'Read', status: 'PASS', notes: '' }])),
        ).rejects.toBeInstanceOf(PathEscapeError);
        expect(readFileSync(target, 'utf8')).toBe(before);
        await expect(s.readDoc(MULTI)).rejects.toBeInstanceOf(PathEscapeError);
    });
});
