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

import { existsSync, readdirSync, readFileSync, statSync } from 'node:fs';
import { join } from 'node:path';
import { describe, expect, it } from 'vitest';
import {
    AmbiguousStepError,
    DuplicateOutcomeError,
    parseScenario,
    recordRun,
    ScenarioFormatError,
    UnknownStepError,
    type RunInput,
    type StepOutcome,
} from './scenario-doc.js';

const fixture = (name: string): string =>
    readFileSync(new URL(`../../../org/src/fixtures/${name}`, import.meta.url), 'utf8');

const single = fixture('single_client_scenario.org');
const multi = fixture('multi_client_scenario.org');

const repoRoot = new URL('../../../../../../', import.meta.url).pathname;
const template = join(
    repoRoot,
    'projects/ores.codegen/library/templates/doc_test_scenario.org.mustache',
);

function run(steps: StepOutcome[], overrides: Partial<RunInput> = {}): RunInput {
    return {
        steps,
        completedAt: '2026-10-11T09:00:00Z',
        branch: 'feature/x',
        commit: 'abc1234',
        worktree: 'eager_maxwell',
        ...overrides,
    };
}

function outcome(
    client: string | null,
    title: string,
    status: StepOutcome['status'],
    notes = '',
): StepOutcome {
    return { client, title, status, notes };
}

/** A scenario rendered from the template that compass fills in. */
function fromTemplate(): string {
    const values: Record<string, string> = {
        id: '11111111-2222-4333-8444-555555555555',
        title: 'Test Scenario: Check the template',
        description: 'A scenario made from the current template.',
        filetags: ':template:sprint_27:v0:',
        date: '2026-10-11',
        parent_id: '66666666-7777-4888-8999-000000000000',
        parent_title: 'The task',
        story_id: 'AAAAAAAA-BBBB-4CCC-8DDD-EEEEEEEEEEEE',
        story_title: 'The story',
        state: 'PENDING',
    };
    return readFileSync(template, 'utf8')
        .replace(/^\{\{!.*\}\}\n/, '')
        .replace(/\{\{(\w+)\}\}/g, (_, key: string) => values[key] ?? '');
}

describe('reading a multi-client scenario', () => {
    const scenario = parseScenario(multi);

    it('reads the header', () => {
        expect(scenario.id).toBe('B12A8474-32FF-45B7-BFD8-FF1B42845DAD');
        expect(scenario.title).toBe('Verify counterparty Qt UI end-to-end post-NATS');
        expect(scenario.target).toContain('CounterpartyDetailDialog');
        expect(scenario.task).toEqual({
            id: '8DC4ABA3-B053-4C40-B575-6EDFCF3C86DE',
            title: 'Verify counterparty Qt UI end-to-end post-NATS',
        });
        expect(scenario.story?.id).toBe('FE07BF4D-054D-4A69-AF3C-D70D10493370');
    });

    it('names the clients and puts every step under its client', () => {
        expect(scenario.clients).toEqual(['blue', 'red']);
        const red = scenario.steps.filter((s) => s.client === 'red');
        expect(red.map((s) => s.title)).toEqual([
            'Connect as a second instance',
            'Confirm the create event arrives',
            'Confirm the update and delete events arrive',
        ]);
        expect(scenario.steps.filter((s) => s.client === 'blue')).toHaveLength(10);
    });

    it('reads each step status and note', () => {
        const create = scenario.steps.find((s) => s.title === 'Create');
        expect(create?.status).toBe('FAIL');
        expect(create?.notes).toContain('duplicate key value');
        expect(scenario.steps.find((s) => s.title === 'Read')?.status).toBe('PENDING');
    });

    it('reads the earlier run and its state', () => {
        expect(scenario.state).toBe('FAILED');
        expect(scenario.run).toEqual({
            status: 'FAILED',
            completedAt: '2026-07-15T08:31:59Z',
            branch: 'feature/build-composite-child-table-qt-facet',
            commit: '944622678',
            worktree: 'prime_origin',
        });
    });

    it('keeps the instructions of a step', () => {
        const first = scenario.steps[0];
        expect(first?.instructions.join('\n')).toContain('tenant_admin@barclays_plc');
        expect(first?.instructions.join('\n')).not.toContain('Result');
    });
});

describe('reading a single-client scenario', () => {
    const scenario = parseScenario(single);

    it('has no clients and null client on every step', () => {
        expect(scenario.clients).toEqual([]);
        expect(scenario.steps).toHaveLength(5);
        expect(scenario.steps.every((s) => s.client === null)).toBe(true);
    });

    it('takes a closed Results status over a State row left at PENDING', () => {
        expect(single).toMatch(/\| State\s+\| PENDING/);
        expect(scenario.state).toBe('PASSED');
    });
});

describe('a scenario made by the current compass template', () => {
    it.skipIf(!existsSync(template))('parses', () => {
        const scenario = parseScenario(fromTemplate());
        expect(scenario.state).toBe('PENDING');
        expect(scenario.clients).toEqual([]);
        expect(scenario.steps.map((s) => s.title)).toEqual([
            'Log in as <persona>',
            '(Next manual step the tester should perform.)',
        ]);
        expect(scenario.steps.every((s) => s.status === 'PENDING')).toBe(true);
        expect(scenario.task?.title).toBe('The task');
        expect(scenario.story?.title).toBe('The story');
        expect(scenario.beforeYouStart.join('\n')).toContain('freshly provisioned');
        expect(scenario.beforeYouStart.join('\n')).not.toContain('belong here');
        expect(scenario.run.status).toBe('');
    });

    it.skipIf(!existsSync(template))('takes a run and reads it back', () => {
        const text = fromTemplate();
        const first = parseScenario(text).steps[0];
        expect(first).toBeDefined();
        const done = recordRun(
            text,
            run([outcome(null, first?.title ?? '', 'PASS', 'Logged in.')]),
        );
        const back = parseScenario(done.text);
        expect(back.steps[0]?.status).toBe('PASS');
        expect(back.steps[0]?.notes).toBe('Logged in.');
        expect(back.steps[1]?.status).toBe('PENDING');
        expect(back.run.branch).toBe('feature/x');
        expect(back.run.status).toBe('PENDING');
    });
});

/** Lines a save may change, so what is left must match. */
const WRITTEN =
    /^(\| (Status|Notes|State|Completed at|Branch|Commit|Worktree)\s*\|.*|\*+ Result|\| Field\s+\| Value \||\|[-+]+\||)$/;

function untouched(text: string): string[] {
    return text.split(/\r?\n/).filter((line) => !WRITTEN.test(line));
}

describe('writing a run into a scenario', () => {
    const all = (client: string | null, titles: string[], status: StepOutcome['status']) =>
        titles.map((title) => outcome(client, title, status));

    it('records the named steps, the results and the state', () => {
        const titles = parseScenario(single).steps.map((s) => s.title);
        const done = recordRun(
            single,
            run([
                ...all(null, titles.slice(0, 3), 'PASS'),
                outcome(null, titles[3] ?? '', 'FAIL', 'Broke.'),
            ]),
        );
        const back = parseScenario(done.text);
        expect(back.steps.map((s) => s.status)).toEqual(['PASS', 'PASS', 'PASS', 'FAIL', 'PASS']);
        expect(back.steps[3]?.notes).toBe('Broke.');
        expect(back.run).toEqual({
            status: 'FAILED',
            completedAt: '2026-10-11T09:00:00Z',
            branch: 'feature/x',
            commit: 'abc1234',
            worktree: 'eager_maxwell',
        });
        expect(done.text).toMatch(/\| State\s+\| FAILED/);
        expect(done.changed.map((c) => c.title)).toEqual(titles.slice(0, 4));
    });

    it('leaves the scenario open while a step is pending, with no completion time', () => {
        const done = recordRun(multi, run([outcome('blue', 'Read', 'PASS')]));
        expect(done.state).toBe('PENDING');
        const back = parseScenario(done.text);
        expect(back.state).toBe('PENDING');
        expect(back.run.status).toBe('PENDING');
        expect(back.run.completedAt).toBe('');
        expect(back.run.branch).toBe('feature/x');
        expect(done.text).toMatch(/\| State\s+\| PENDING/);
    });

    it('closes a scenario as PASSED when every step passes', () => {
        const titles = parseScenario(single).steps.map((s) => s.title);
        const done = recordRun(single, run(all(null, titles, 'PASS')));
        expect(done.state).toBe('PASSED');
        expect(parseScenario(done.text).run.completedAt).toBe('2026-10-11T09:00:00Z');
    });

    it('closes a scenario as FAILED when no step is pending and one failed', () => {
        const first = parseScenario(single).steps[0]?.title ?? '';
        expect(recordRun(single, run([outcome(null, first, 'FAIL')])).state).toBe('FAILED');
    });

    it('opens a closed scenario again when a step goes back to pending', () => {
        const first = parseScenario(single).steps[0]?.title ?? '';
        const done = recordRun(single, run([outcome(null, first, 'PENDING')]));
        const back = parseScenario(done.text);
        expect(back.state).toBe('PENDING');
        expect(back.run.completedAt).toBe('');
        expect(back.steps[0]?.status).toBe('PENDING');
    });

    it('leaves every other line as it was', () => {
        const titles = parseScenario(multi).steps.map((s) => s.title);
        const done = recordRun(multi, run([outcome('blue', titles[1] ?? '', 'PASS', 'ok')]));
        expect(untouched(done.text)).toEqual(untouched(multi));
    });

    it('is the same text when the same run is written twice', () => {
        const input = run([outcome('red', 'Connect as a second instance', 'PASS', 'seen')]);
        const once = recordRun(multi, input).text;
        expect(recordRun(once, input).text).toBe(once);
    });

    it('writes a step under its own client when two clients share a title', () => {
        const doubled = multi.replace('*** Confirm the create event arrives', '*** Create');
        const done = recordRun(doubled, run([outcome('red', 'Create', 'FAIL', 'late')]));
        const back = parseScenario(done.text);
        expect(back.steps.find((s) => s.client === 'red' && s.title === 'Create')?.status).toBe(
            'FAIL',
        );
        expect(
            back.steps.find((s) => s.client === 'blue' && s.title === 'Create')?.notes,
        ).toContain('duplicate key');
    });

    it('adds a Result to a step that has none', () => {
        const text = fromTemplate();
        const done = recordRun(text, run([outcome(null, 'Log in as <persona>', 'PASS')]));
        expect(done.text).toMatch(
            /\*\*\* Result\n\n\| Field {2}\| Value \|\n\|-+\+-+\|\n\| Status \| PASS \|\n/,
        );
    });

    it('adds a table to a Result heading that has none', () => {
        const bare = single.replace(
            /\*\*\* Result\n\n\| Field {2}\| Value \|\n\|-+\+-+\|\n\| Status \| PASS \|\n/,
            '*** Result\n',
        );
        expect(bare).not.toBe(single);
        const title = parseScenario(bare).steps[0]?.title ?? '';
        const done = recordRun(bare, run([outcome(null, title, 'FAIL', 'x')]));
        const first = parseScenario(done.text).steps[0];
        expect(first?.status).toBe('FAIL');
        expect(first?.notes).toBe('x');
    });

    it('keeps a row of a Result table it does not own', () => {
        const extra = single.replace(
            '| Status | PASS |',
            '| Status | PASS |\n| Screenshot | [[file:shot_01.png]] |',
        );
        const title = parseScenario(extra).steps[0]?.title ?? '';
        const done = recordRun(extra, run([outcome(null, title, 'FAIL', 'n')]));
        expect(done.text).toContain('| Screenshot | [[file:shot_01.png]] |');
    });

    it('removes the Notes row when the note is cleared', () => {
        const done = recordRun(multi, run([outcome('blue', 'Create', 'PASS', '')]));
        const create = parseScenario(done.text).steps.find((s) => s.title === 'Create');
        expect(create?.status).toBe('PASS');
        expect(create?.notes).toBe('');
        expect(done.text).not.toContain('duplicate key');
    });

    it('keeps a note in one table cell', () => {
        const title = parseScenario(single).steps[0]?.title ?? '';
        const done = recordRun(single, run([outcome(null, title, 'FAIL', 'a | b\nsecond line')]));
        expect(parseScenario(done.text).steps[0]?.notes).toBe('a / b; second line');
    });

    it('keeps the line end of the doc', () => {
        const crlf = single.replace(/\n/g, '\r\n');
        const title = parseScenario(crlf).steps[0]?.title ?? '';
        const done = recordRun(crlf, run([outcome(null, title, 'FAIL')]));
        expect(done.text.replace(/\r\n/g, '')).not.toContain('\n');
    });

    it('refuses a step the doc does not have, and writes nothing', () => {
        const input = run([
            outcome(null, 'Connect', 'PASS'),
            outcome(null, 'No such step', 'PASS'),
        ]);
        expect(() => recordRun(single, input)).toThrow(UnknownStepError);
        expect(() => recordRun(multi, run([outcome(null, 'Create', 'PASS')]))).toThrow(
            UnknownStepError,
        );
        expect(() => recordRun(multi, run([outcome('green', 'Create', 'PASS')]))).toThrow(
            UnknownStepError,
        );
    });

    it('refuses a step whose title the doc repeats', () => {
        const twice = single.replace(/^\*\* .*$/m, (line, offset: number, whole: string) => {
            const titles = [...whole.matchAll(/^\*\* (.*)$/gm)].map((m) => m[1]);
            return offset >= 0 ? `** ${titles[1] ?? ''}` : line;
        });
        const title = parseScenario(twice).steps[0]?.title ?? '';
        expect(() => recordRun(twice, run([outcome(null, title, 'PASS')]))).toThrow(
            AmbiguousStepError,
        );
    });

    it('refuses a save that names one step twice', () => {
        const title = parseScenario(single).steps[0]?.title ?? '';
        const twice = run([outcome(null, title, 'PASS'), outcome(null, title, 'FAIL')]);
        expect(() => recordRun(single, twice)).toThrow(DuplicateOutcomeError);
    });

    it('ends a Results table it creates with a blank line before the next heading', () => {
        const noTable = single.replace(/\* Results\n\n\| Field[\s\S]*?\n\n/, '* Results\n\n');
        expect(noTable).not.toBe(single);
        const title = parseScenario(noTable).steps[0]?.title ?? '';
        const done = recordRun(noTable, run([outcome(null, title, 'PASS')]));
        expect(done.text).toMatch(/\| Worktree\s+\| eager_maxwell \|\n\n/);
    });

    it('refuses text that is not a scenario', () => {
        expect(() => recordRun('* Steps\n', run([]))).toThrow(ScenarioFormatError);
        expect(() => parseScenario('#+type: task\n')).toThrow(ScenarioFormatError);
    });
});

function walk(dir: string, found: string[] = []): string[] {
    for (const name of readdirSync(dir)) {
        const path = join(dir, name);
        if (statSync(path).isDirectory()) walk(path, found);
        else if (name.startsWith('scenario_') && name.endsWith('.org')) found.push(path);
    }
    return found;
}

describe('every scenario doc in the repository', () => {
    const root = join(repoRoot, 'doc/agile');

    it.skipIf(!existsSync(root))('parses, and takes a full run back without losing a step', () => {
        const files = walk(root);
        expect(files.length).toBeGreaterThan(50);
        for (const file of files) {
            const text = readFileSync(file, 'utf8');
            const scenario = parseScenario(text);
            expect(scenario.steps.length, file).toBeGreaterThan(0);
            const input = run(
                scenario.steps.map((s) => outcome(s.client, s.title, 'PASS', 'again')),
            );
            let done;
            try {
                done = recordRun(text, input);
            } catch (error) {
                if (error instanceof AmbiguousStepError) continue;
                throw error;
            }
            const back = parseScenario(done.text);
            expect(
                back.steps.map((s) => [s.client, s.title]),
                file,
            ).toEqual(scenario.steps.map((s) => [s.client, s.title]));
            expect(
                back.steps.every((s) => s.status === 'PASS' && s.notes === 'again'),
                file,
            ).toBe(true);
            expect(back.state, file).toBe('PASSED');
        }
    });
});
