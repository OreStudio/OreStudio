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
import type { Scenario } from '@ores/contracts';
import { TranslationProvider } from '../i18n/Provider.js';
import { ScenarioReport, countStatuses } from './ScenarioReport.js';

const ID = '11111111-1111-4111-8111-111111111111';

function scenario(overrides: Partial<Scenario> = {}): Scenario {
    return {
        id: ID,
        title: 'A reviewed scenario',
        description: '',
        created: '2026-10-11',
        state: 'FAILED',
        target: 'CurrencyDetailDialog',
        story: null,
        task: null,
        clients: [],
        beforeYouStart: [],
        steps: [
            {
                client: null,
                title: 'Open the dialog',
                instructions: ['Click the *General* tab.'],
                status: 'PASS',
                notes: '',
            },
            {
                client: null,
                title: 'Check the rounding',
                instructions: ['Look at =field 2=.'],
                status: 'FAIL',
                notes: 'The field stayed blank.; [[file:shot_step2_20261011_014118.png]]; [[file:shot_step2_20261011_014118_2.png]]',
            },
            {
                client: 'red',
                title: 'Confirm on the second client',
                instructions: [],
                status: 'PENDING',
                notes: '',
            },
            {
                client: null,
                title: 'A step left out',
                instructions: [],
                status: 'DROPPED',
                notes: 'Not needed now.',
            },
        ],
        run: {
            status: 'FAILED',
            completedAt: '2026-10-11T01:50:09Z',
            branch: 'feature/x',
            commit: 'abc1234',
            worktree: 'eager_maxwell',
        },
        ...overrides,
    };
}

const report = (s: Scenario): string =>
    renderToStaticMarkup(
        <TranslationProvider>
            <MemoryRouter>
                <ScenarioReport scenario={s} />
            </MemoryRouter>
        </TranslationProvider>,
    );

describe('the count of each status', () => {
    it('counts the steps by status', () => {
        expect(countStatuses(scenario())).toEqual({ PASS: 1, FAIL: 1, PENDING: 1, DROPPED: 1 });
    });

    it('counts nothing for a scenario with no steps', () => {
        expect(countStatuses(scenario({ steps: [] }))).toEqual({
            PASS: 0,
            FAIL: 0,
            PENDING: 0,
            DROPPED: 0,
        });
    });
});

describe('a scenario read as a report', () => {
    it('summarises the state, the counts, the target, the dates and the build', () => {
        const html = report(scenario());
        expect(html).toContain('>Failed</span>');
        expect(html).toContain('1 Passed');
        expect(html).toContain('1 Failed');
        expect(html).toContain('1 Pending');
        expect(html).toContain('1 Dropped');
        expect(html).toContain('CurrencyDetailDialog');
        expect(html).toContain('2026-10-11');
        expect(html).toContain('2026-10-11T01:50:09Z');
        expect(html).toContain('feature/x · abc1234 · eager_maxwell');
    });

    it('shows every step on the one page, in order, each with its number and its status', () => {
        const html = report(scenario());
        const at = (text: string) => html.indexOf(text);
        for (const title of [
            'Open the dialog',
            'Check the rounding',
            'Confirm on the second client',
            'A step left out',
        ]) {
            expect(at(title), title).toBeGreaterThan(-1);
        }
        expect(at('Open the dialog')).toBeLessThan(at('Check the rounding'));
        expect(at('Check the rounding')).toBeLessThan(at('Confirm on the second client'));
        expect(at('Confirm on the second client')).toBeLessThan(at('A step left out'));
        for (const n of [1, 2, 3, 4]) expect(html).toContain(`id="step-${n}"`);
    });

    it('shows what the tester wrote, without the image links in the words', () => {
        const html = report(scenario());
        expect(html).toContain('The field stayed blank.');
        expect(html).not.toContain('[[file:');
        expect(html).toContain('Not needed now.');
    });

    it('shows the screenshots as large pictures with their file names under them', () => {
        const html = report(scenario());
        const base = `/api/qa/scenarios/${ID}/files/`;
        expect(html).toContain(`<img src="${base}shot_step2_20261011_014118.png"`);
        expect(html).toContain(`<img src="${base}shot_step2_20261011_014118_2.png"`);
        expect(html).toContain('max-h-[28rem]');
        expect(html).toContain('>shot_step2_20261011_014118.png</figcaption>');
        expect(html.match(/<img /g)?.length).toBe(2);
    });

    it('opens a picture in a new tab, safely', () => {
        const html = report(scenario());
        expect(html).toContain('target="_blank"');
        expect(html).toContain('rel="noopener noreferrer"');
    });

    it('says a step was not marked, and not for a step that has a mark', () => {
        const html = report(scenario());
        expect(html.match(/Not marked\./g)?.length).toBe(1);
    });

    it('always shows the instructions of a step, with nothing folded away', () => {
        const html = report(scenario());
        expect(html).toContain('>Instructions</h3>');
        expect(html).toContain('<strong>General</strong>');
        expect(html).toContain('field 2');
        expect(html).not.toContain('<details');
        expect(html).not.toContain('<summary');
    });

    it('shows no Instructions label for a step that has none', () => {
        const html = report(scenario());
        expect(html.match(/>Instructions<\/h3>/g)?.length).toBe(2);
    });

    it('shows the client of a step', () => {
        expect(report(scenario())).toContain('>red<');
    });

    it('draws a numbered marker for each step, coloured by its status, that jumps to the step', () => {
        const html = report(scenario());
        expect(html).toContain('aria-label="Steps at a glance"');
        const markers = html.match(/<button[^>]*title="\d\.[^"]*"[^>]*>\d<\/button>/g) ?? [];
        expect(markers).toHaveLength(4);
        expect(markers[0]).toContain('text-up');
        expect(markers[1]).toContain('text-down');
        expect(markers[2]).toContain('text-ink-faint');
    });

    it('shows a dash for what the doc does not say', () => {
        const bare = scenario({
            target: '',
            created: '',
            run: { status: '', completedAt: '', branch: '', commit: '', worktree: '' },
        });
        const html = report(bare);
        expect(html.match(/<dd class="break-words">-<\/dd>/g)?.length).toBe(4);
    });

    it('says so when a scenario has no steps', () => {
        const html = report(scenario({ steps: [] }));
        expect(html).toContain('This scenario has no steps.');
        expect(html).not.toContain('aria-label="Steps at a glance"');
    });

    it('shows markup in the instructions as text', () => {
        const hostile = scenario({
            steps: [
                {
                    client: null,
                    title: 'Hostile',
                    instructions: ['<script>alert(1)</script>'],
                    status: 'PASS',
                    notes: '<img src=x onerror=alert(1)>',
                },
            ],
        });
        const html = report(hostile);
        expect(html).not.toContain('<script>');
        expect(html).not.toContain('<img src="x"');
        expect(html).toContain('&lt;img src=x onerror=alert(1)&gt;');
    });
});
