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
import { FileScenarioStore, sprintOf } from './file-scenario-store.js';
import { UnknownStepError, type RunInput, type StepOutcome } from './scenario-doc.js';
import { MAX_IMAGE_BYTES } from '@ores/contracts';
import {
    InvalidDocIdError,
    InvalidImageError,
    PathEscapeError,
    ScenarioNotFoundError,
    StepNumberError,
} from './scenario-store.js';

const fixture = (name: string): string =>
    readFileSync(new URL(`../../../org/src/fixtures/${name}`, import.meta.url), 'utf8');

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

function input(steps: StepOutcome[]): RunInput {
    return {
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

    it('gives each scenario its sprint from its path, and its created date', async () => {
        put(
            'agile/versions/v0/sprint_27/story/scenario_in_a_sprint.org',
            fixture('single_client_scenario.org').replace(
                SINGLE,
                'C0000000-0000-4000-8000-0000000000AA',
            ),
        );
        const list = await store().list();
        const inSprint = list.find((s) => s.id === 'C0000000-0000-4000-8000-0000000000AA');
        expect(inSprint?.sprint).toBe('sprint_27');
        expect(list.find((s) => s.id === MULTI)?.sprint).toBe('sprint_23');
        expect(list.find((s) => s.id === MULTI)?.created).toBe('2026-07-13');
    });

    it('leaves the sprint null for a scenario outside any sprint folder', async () => {
        put(
            'recipes/scenario_loose.org',
            fixture('single_client_scenario.org').replace(
                SINGLE,
                'C0000000-0000-4000-8000-0000000000BB',
            ),
        );
        const loose = (await store().list()).find(
            (s) => s.id === 'C0000000-0000-4000-8000-0000000000BB',
        );
        expect(loose?.sprint).toBeNull();
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

describe('scanning', () => {
    it('lets calls that arrive during the first scan see the whole store', async () => {
        const s = new FileScenarioStore(root);
        const [list, doc, scenario] = await Promise.all([
            s.list(),
            s.readDoc(STORY),
            s.read(SINGLE),
        ]);
        expect(list).toHaveLength(2);
        expect(doc?.type).toBe('story');
        expect(scenario?.steps).toHaveLength(5);
    });

    it('tries again at once after a scan that failed', async () => {
        const s = new FileScenarioStore(root, { rescanAfterMs: 600_000 });
        const locked = join(root, 'sprint_23/locked');
        mkdirSync(locked);
        chmodSync(locked, 0o000);
        try {
            await expect(s.list()).rejects.toThrow();
        } finally {
            chmodSync(locked, 0o755);
        }
        expect(await s.list()).toHaveLength(2);
    });

    it('lists a scenario whose #+type: sits past the first chunk of the file', async () => {
        const id = 'A1111111-1111-4111-8111-111111111111';
        const long = fixture('single_client_scenario.org')
            .replace(SINGLE, id)
            .replace('#+description: ', `#+description: ${'long '.repeat(1500)}`);
        expect(long.indexOf('#+type:')).toBeGreaterThan(5000);
        put('sprint_24/scenario_long.org', long);
        expect((await store().list()).map((x) => x.id)).toContain(id);
    });

    it('reads the type and title of a doc from its text, not from the index', async () => {
        const s = new FileScenarioStore(root, { rescanAfterMs: 600_000 });
        await s.list();
        const file = join(root, 'sprint_23/story/story.org');
        writeFileSync(file, readFileSync(file, 'utf8').replace('#+type: story', '#+type: recipe'));
        expect((await s.readDoc(STORY))?.type).toBe('recipe');
    });

    it('skips a file that vanishes during the walk', async () => {
        const s = store();
        rmSync(join(root, 'sprint_23/story/notes.txt'));
        expect(await s.list()).toHaveLength(2);
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
            input([{ client: 'blue', title: 'Read', status: 'PASS', notes: '' }]),
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

    it('serialises saves that start on a cold index and a warm one', async () => {
        const s = store();
        const first = s.record(
            MULTI,
            input([
                { client: 'blue', title: 'Read', status: 'PASS', notes: 'first' },
                { client: 'blue', title: 'Update', status: 'PASS', notes: 'only first' },
            ]),
        );
        await s.read(MULTI);
        const second = s.record(
            MULTI,
            input([{ client: 'blue', title: 'Read', status: 'FAIL', notes: 'second' }]),
        );
        await Promise.all([first, second]);
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

const PNG = Uint8Array.from([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a, 1, 2, 3, 4]);
const JPEG = Uint8Array.from([0xff, 0xd8, 0xff, 0xe0, 1, 2]);
const GIF = Uint8Array.from([0x47, 0x49, 0x46, 0x38, 0x39, 0x61, 1, 2]);
const WEBP = Uint8Array.from([0x52, 0x49, 0x46, 0x46, 1, 2, 3, 4, 0x57, 0x45, 0x42, 0x50, 9]);
const NOON = () => new Date('2026-10-11T09:30:15.123Z');

describe('keeping a screenshot of a step', () => {
    const dir = () => join(root, 'sprint_23/story');
    const timed = () => new FileScenarioStore(root, { rescanAfterMs: 0, now: NOON });

    it('saves it beside the scenario doc, under a name from the scenario and the step', async () => {
        const saved = await timed().saveImage(MULTI, 3, PNG);
        expect(saved.name).toBe('scenario_multi_step3_20261011_093015.png');
        expect(readdirSync(dir())).toContain(saved.name);
        expect(readFileSync(join(dir(), saved.name))).toEqual(Buffer.from(PNG));
    });

    it('names the file by what the bytes are, not by what the caller says', async () => {
        const s = timed();
        expect((await s.saveImage(MULTI, 1, JPEG)).name).toMatch(/_step1_\d{8}_\d{6}\.jpg$/);
        expect((await s.saveImage(MULTI, 2, GIF)).name).toMatch(/_step2_\d{8}_\d{6}\.gif$/);
        expect((await s.saveImage(MULTI, 3, WEBP)).name).toMatch(/_step3_\d{8}_\d{6}\.webp$/);
    });

    it('gives two screenshots of one step in one second two names', async () => {
        const s = timed();
        const first = await s.saveImage(MULTI, 1, PNG);
        const second = await s.saveImage(MULTI, 1, PNG);
        const third = await s.saveImage(MULTI, 1, PNG);
        expect(new Set([first.name, second.name, third.name]).size).toBe(3);
        expect(second.name).toBe('scenario_multi_step1_20261011_093015_2.png');
    });

    it('builds a safe name from a doc whose file name is not', async () => {
        put(
            'sprint_24/scenario with spaces & marks.org',
            fixture('single_client_scenario.org').replace(
                SINGLE,
                'F0000000-0000-4000-8000-000000000009',
            ),
        );
        const saved = await timed().saveImage('F0000000-0000-4000-8000-000000000009', 1, PNG);
        expect(saved.name).toMatch(/^[A-Za-z0-9._-]+$/);
        expect(readdirSync(join(root, 'sprint_24'))).toContain(saved.name);
    });

    it('refuses bytes that are not an image, and writes nothing', async () => {
        const before = readdirSync(dir());
        for (const text of [
            'hello',
            '<svg xmlns="http://www.w3.org/2000/svg"/>',
            '<script>1</script>',
        ]) {
            await expect(
                timed().saveImage(MULTI, 1, new TextEncoder().encode(text)),
            ).rejects.toMatchObject({ reason: 'not-image' });
        }
        expect(readdirSync(dir())).toEqual(before);
    });

    it('refuses an empty file and a file over the limit, and writes nothing', async () => {
        const before = readdirSync(dir());
        await expect(timed().saveImage(MULTI, 1, new Uint8Array())).rejects.toMatchObject({
            reason: 'empty',
        });
        const big = new Uint8Array(MAX_IMAGE_BYTES + 1);
        big.set(PNG);
        await expect(timed().saveImage(MULTI, 1, big)).rejects.toBeInstanceOf(InvalidImageError);
        await expect(timed().saveImage(MULTI, 1, big)).rejects.toMatchObject({
            reason: 'too-large',
        });
        expect(readdirSync(dir())).toEqual(before);
    });

    it('takes an image of exactly the limit', async () => {
        const edge = new Uint8Array(MAX_IMAGE_BYTES);
        edge.set(PNG);
        expect((await timed().saveImage(MULTI, 1, edge)).name).toMatch(/\.png$/);
    });

    it('refuses a step that the scenario does not have', async () => {
        const before = readdirSync(dir());
        for (const step of [0, -1, 14, 1.5, Number.NaN]) {
            await expect(timed().saveImage(MULTI, step, PNG)).rejects.toBeInstanceOf(
                StepNumberError,
            );
        }
        expect(readdirSync(dir())).toEqual(before);
    });

    it('answers not found for an id no scenario has, and for a story', async () => {
        await expect(
            timed().saveImage('E0000000-0000-4000-8000-000000000003', 1, PNG),
        ).rejects.toBeInstanceOf(ScenarioNotFoundError);
        await expect(timed().saveImage(STORY, 1, PNG)).rejects.toBeInstanceOf(
            ScenarioNotFoundError,
        );
    });

    it('refuses an id that is not a document id', async () => {
        await expect(timed().saveImage('../../etc', 1, PNG)).rejects.toBeInstanceOf(
            InvalidDocIdError,
        );
    });

    it('does not write beside a doc that now points outside the root', async () => {
        const s = timed();
        await s.list();
        const target = join(outside, 'victim.org');
        writeFileSync(target, fixture('multi_client_scenario.org'));
        rmSync(join(dir(), 'scenario_multi.org'));
        symlinkSync(target, join(dir(), 'scenario_multi.org'));
        await expect(s.saveImage(MULTI, 1, PNG)).rejects.toBeInstanceOf(PathEscapeError);
        expect(readdirSync(outside)).toEqual(['victim.org']);
    });
});

describe('the sprint of a path', () => {
    it('is the sprint folder, wherever it sits in the path', () => {
        expect(sprintOf('agile/versions/v0/sprint_27/story/scenario_a.org')).toBe('sprint_27');
        expect(sprintOf('sprint_3/scenario_a.org')).toBe('sprint_3');
        expect(sprintOf('a/sprint_22/b/sprint_23/scenario_a.org')).toBe('sprint_22');
    });

    it('is null for a path with no sprint folder, and for a folder that only looks like one', () => {
        expect(sprintOf('recipes/scenario_a.org')).toBeNull();
        expect(sprintOf('sprint_/scenario_a.org')).toBeNull();
        expect(sprintOf('my_sprint_27/scenario_a.org')).toBeNull();
        expect(sprintOf('sprint_27.org')).toBeNull();
    });
});
