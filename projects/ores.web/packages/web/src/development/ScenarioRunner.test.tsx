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

import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { describe, expect, it } from 'vitest';
import type { Scenario, StepStatus } from '@ores/contracts';
import { TranslationProvider } from '../i18n/Provider.js';
import { openRunner, reduce, type Action, type Outcome, type Runner } from './runner.js';
import { ScenarioRunner } from './ScenarioRunner.js';

function scenario(statuses: StepStatus[], overrides: Partial<Scenario> = {}): Scenario {
    return {
        id: '11111111-1111-4111-8111-111111111111',
        title: 'Retake the currency screenshots',
        description: '',
        state: 'PENDING',
        target: 'CurrencyDetailDialog',
        story: { id: 'S', title: 'Refresh the manual' },
        task: { id: 'T', title: 'Retake the shots' },
        clients: [],
        beforeYouStart: [],
        steps: statuses.map((status, i) => ({
            client: null,
            title: `Open tab ${i + 1}`,
            instructions: [`Click the *General* tab number =${i + 1}=.`],
            status,
            notes: '',
        })),
        run: { status: '', completedAt: '', branch: '', commit: '', worktree: '' },
        ...overrides,
    };
}

function screen(
    runner: Runner,
    options: {
        readonly scenario?: Scenario;
        readonly outcome?: Outcome | null;
        readonly confirmingRetest?: boolean;
        readonly saving?: boolean;
        readonly imageBusy?: boolean;
        readonly imageError?: string | null;
    } = {},
): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <MemoryRouter>
                <ScenarioRunner
                    scenario={options.scenario ?? scenario(runner.steps.map((s) => s.status))}
                    runner={runner}
                    dispatch={() => undefined}
                    onSave={() => undefined}
                    saving={options.saving ?? false}
                    outcome={options.outcome ?? null}
                    onImages={() => undefined}
                    imageBusy={options.imageBusy ?? false}
                    imageError={options.imageError ?? null}
                    confirmingRetest={options.confirmingRetest ?? false}
                    onAskRetest={() => undefined}
                    onConfirmRetest={() => undefined}
                    onCancelRetest={() => undefined}
                />
            </MemoryRouter>
        </TranslationProvider>,
    );
}

const run = (runner: Runner, ...actions: Action[]): Runner => actions.reduce(reduce, runner);
const fresh = (n = 3): Runner => openRunner(scenario(Array<StepStatus>(n).fill('PENDING')));

describe('a scenario being run', () => {
    it('shows the header, the progress, the step and its instructions', () => {
        const html = screen(fresh());
        expect(html).toContain('Retake the currency screenshots');
        expect(html).toContain('Refresh the manual · Retake the shots');
        expect(html).toContain('CurrencyDetailDialog');
        expect(html).toContain('role="progressbar"');
        expect(html).toContain('aria-valuenow="0"');
        expect(html).toContain('0 of 3 steps marked');
        expect(html).toContain('Step 1 of 3');
        expect(html).toContain('Open tab 1');
        expect(html).toContain('<strong>General</strong>');
        expect(html).toMatch(/<code[^>]*>1<\/code>/);
    });

    it('offers Pass, Fail, Clear, a note and Save, and no Retest', () => {
        const html = screen(fresh());
        for (const label of ['Pass', 'Fail', 'Clear', 'Save']) {
            expect(html).toContain(`>${label}</button>`);
        }
        expect(html).toContain('<textarea');
        expect(html).not.toContain('Retest');
        expect(html).not.toMatch(/<button[^>]*disabled=""[^>]*>(Pass|Fail|Clear)</);
    });

    it('names the step count once', () => {
        expect(screen(fresh()).match(/Step 1 of 3/g)?.length).toBe(1);
    });

    it('shows no scenario heading twice and names the story and task once', () => {
        const html = screen(fresh());
        expect(html.match(/Refresh the manual/g)?.length).toBe(1);
    });

    it('shows the note of a failed step, and stays on that step', () => {
        const runner = run(
            fresh(),
            { type: 'mark', mark: 'FAIL' },
            { type: 'note', notes: 'The tab stayed blank.' },
        );
        const html = screen(runner);
        expect(html).toContain('Step 1 of 3');
        expect(html).toContain('aria-pressed="true"');
        expect(html).toContain('The tab stayed blank.');
        expect(html).toContain('Failed');
    });

    it('moves to the next step after a Pass', () => {
        const html = screen(run(fresh(), { type: 'mark', mark: 'PASS' }));
        expect(html).toContain('Step 2 of 3');
        expect(html).toContain('Open tab 2');
        expect(html).toContain('1 of 3 steps marked');
        expect(html).toContain('aria-valuenow="33"');
    });

    it('stays on the last step after the last Pass and keeps Save, because marking never closes', () => {
        const runner = run(
            fresh(2),
            { type: 'mark', mark: 'PASS' },
            { type: 'mark', mark: 'PASS' },
        );
        const html = screen(runner);
        expect(html).toContain('Step 2 of 2');
        expect(html).toContain('>Save</button>');
        expect(html).not.toContain('Retest');
        expect(html).toContain('2 of 2 steps marked');
    });

    it('shows the client of a step in a multi-client scenario', () => {
        const multi = scenario(['PENDING', 'PENDING'], { clients: ['blue', 'red'] });
        const withClients: Scenario = {
            ...multi,
            steps: multi.steps.map((step, i) => ({ ...step, client: i === 0 ? 'blue' : 'red' })),
        };
        const html = screen(openRunner(withClients), { scenario: withClients });
        expect(html).toContain('>blue<');
        expect(html).toContain('>red<');
        expect(html).toContain('Clients');
    });

    it('shows instructions as text when they hold markup', () => {
        const hostile = scenario(['PENDING'], {
            steps: [
                {
                    client: null,
                    title: 'Hostile',
                    instructions: ['<script>alert(1)</script>', '[[javascript:alert(1)][click]]'],
                    status: 'PENDING',
                    notes: '',
                },
            ],
        });
        const html = screen(openRunner(hostile), { scenario: hostile });
        expect(html).not.toContain('<script>');
        expect(html).not.toContain('href="javascript:');
    });
});

describe('a screenshot for a step', () => {
    it('invites a paste or a drop on a step that can be edited', () => {
        expect(screen(fresh())).toContain('Paste or drop a screenshot here');
    });

    it('says the image is being saved', () => {
        const html = screen(fresh(), { imageBusy: true });
        expect(html).toContain('Saving the image...');
        expect(html).not.toContain('Paste or drop a screenshot here');
    });

    it('holds Save while an image is uploading, so the link is in the note when it goes', () => {
        const busy = screen(fresh(), { imageBusy: true });
        expect(busy).toMatch(/<button[^>]*disabled=""[^>]*>Save</);
        const idle = screen(fresh());
        expect(idle).not.toMatch(/<button[^>]*disabled=""[^>]*>Save</);
    });

    it('shows why an image was refused, as an alert', () => {
        const html = screen(fresh(), {
            imageError: 'The file is not a PNG, JPEG, GIF or WebP image.',
        });
        expect(html).toContain('role="alert"');
        expect(html).toContain('not a PNG, JPEG, GIF or WebP');
    });

    it('shows the link in the note after an attach', () => {
        const runner = run(fresh(), { type: 'attach', index: 0, name: 'a_step1.png' });
        expect(screen(runner)).toContain('[[file:a_step1.png]]');
    });

    it('draws no invitation on a closed scenario, which cannot take an image', () => {
        const closed = openRunner(scenario(['PASS'], { state: 'PASSED' }));
        expect(screen(closed)).not.toContain('Paste or drop a screenshot here');
    });
});

describe('after a save', () => {
    it('says what is left when the save leaves the scenario open', () => {
        const html = screen(run(fresh(), { type: 'mark', mark: 'PASS' }), {
            outcome: { kind: 'open', pending: 2 },
        });
        expect(html).toContain('Saved. 2 steps are still to mark. The scenario stays open.');
        expect(html).toContain('>Save</button>');
    });

    it('says the scenario closed, and with which state', () => {
        const closed = openRunner(scenario(['PASS', 'PASS'], { state: 'PASSED' }));
        const html = screen(closed, { outcome: { kind: 'closed', state: 'PASSED' } });
        expect(html).toContain('Saved and closed as Passed.');
    });

    it('shows the reason when the save is refused', () => {
        const html = screen(fresh(), {
            outcome: { kind: 'failed', message: 'The scenario has no step "Frobnicate".' },
        });
        expect(html).toContain('role="alert"');
        expect(html).toContain('Frobnicate');
    });

    it('shows a save in progress', () => {
        expect(screen(fresh(), { saving: true })).toContain('animate-spin');
    });

    it('holds marks and notes while the save is in flight, and leaves moving between steps', () => {
        const html = screen(fresh(), { saving: true });
        for (const label of ['Pass', 'Fail', 'Clear']) {
            expect(html).toMatch(new RegExp(`<button[^>]*disabled=""[^>]*>${label}<`));
        }
        expect(html).toMatch(/<textarea[^>]*disabled=""/);
        expect(html).not.toMatch(/<button[^>]*disabled=""[^>]*>Skip</);
    });
});

describe('a closed scenario', () => {
    const closedScenario = scenario(['PASS', 'FAIL'], { state: 'FAILED' });
    const closed = openRunner(closedScenario);

    it('opens read-only, with Retest and no Save', () => {
        const html = screen(closed, { scenario: closedScenario });
        expect(html).toContain('>Retest</button>');
        expect(html).not.toContain('>Save</button>');
        expect(html).toContain('This scenario is closed.');
        for (const label of ['Pass', 'Fail', 'Clear']) {
            expect(html).toMatch(new RegExp(`<button[^>]*disabled=""[^>]*>${label}<`));
        }
        expect(html).toMatch(/<textarea[^>]*disabled=""/);
    });

    it('asks before a retest clears anything', () => {
        const html = screen(closed, { scenario: closedScenario, confirmingRetest: true });
        expect(html).toContain('role="dialog"');
        expect(html).toContain('Start a retest?');
        expect(html).toContain('clears every step');
        expect(html).toContain('>Cancel</button>');
    });

    it('opens the scenario again after a retest, with every step pending', () => {
        const again = run(closed, { type: 'retest' });
        const html = screen(again, { scenario: closedScenario });
        expect(html).toContain('>Save</button>');
        expect(html).not.toContain('>Retest</button>');
        expect(html).toContain('0 of 2 steps marked');
        expect(html).toContain('Step 1 of 2');
    });
});
