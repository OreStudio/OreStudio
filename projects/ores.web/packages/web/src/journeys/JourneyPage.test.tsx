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

import { describe, expect, it } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import type { ReactNode } from 'react';
import { TranslationProvider } from '../i18n/Provider.js';
import { JourneyPage } from './JourneyPage.js';
import { defineJourney, type JourneyStep } from './runtime.js';

function step(
    id: string,
    title: string,
    extra: Partial<JourneyStep<ReactNode>> = {},
): JourneyStep<ReactNode> {
    return { id, title, lead: `${title} lead`, body: `${title} body`, ...extra };
}

const welcome = step('welcome', 'Welcome', { next: { label: 'Start', enabled: true } });
const administrator = step('administrator', 'Create the administrator', {
    next: { label: 'Create', enabled: true },
    final: true,
});
const ready = step('ready', 'Ready');

function render(steps: readonly JourneyStep<ReactNode>[], at: number): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <JourneyPage steps={steps} at={at} onMove={() => undefined} />
        </TranslationProvider>,
    );
}

/** The whole button element whose label ends at `label`, attributes included. */
function buttonLabelled(html: string, label: string): string {
    const end = html.indexOf(label);
    const start = html.lastIndexOf('<button', end);
    const close = html.indexOf('</button>', end);
    return html.slice(start, close + '</button>'.length);
}

describe('the journey page', () => {
    it('renders one rail entry per step, in the order the steps declare', () => {
        const steps = defineJourney([welcome, administrator, ready]);

        const html = render(steps, 0);
        const nav = html.slice(html.indexOf('<nav'), html.indexOf('</nav>'));

        expect(nav).toContain('Welcome');
        expect(nav).toContain('Create the administrator');
        expect(nav).toContain('Ready');
        expect(nav.indexOf('Welcome')).toBeLessThan(nav.indexOf('Create the administrator'));
        expect(nav.indexOf('Create the administrator')).toBeLessThan(nav.indexOf('Ready'));
    });

    it('marks the current step, and marks the steps behind it as done', () => {
        const steps = defineJourney([welcome, administrator, ready]);

        const html = render(steps, 1);

        expect(html).toContain('aria-current="step"');
        expect(html).toContain('✓');
        expect(html).toContain('1');
        expect(html).toContain('3');
    });

    it('renders the current step and no other step body', () => {
        const steps = defineJourney([welcome, administrator, ready]);

        const html = render(steps, 1);

        expect(html).toContain('Create the administrator body');
        expect(html).not.toContain('Welcome body');
        expect(html).not.toContain('Ready body');
    });

    it('offers the action the step declares, by its label', () => {
        const steps = defineJourney([welcome, ready]);

        const html = render(steps, 0);

        expect(html).toContain('Start');
    });

    it('offers Back on a step that declares no action of its own', () => {
        const review = step('review', 'Review');
        const steps = defineJourney([welcome, review]);

        const html = render(steps, 1);

        expect(buttonLabelled(html, 'Back')).not.toContain('disabled=""');
    });

    it('omits the footer when the step offers nothing to do', () => {
        const steps = defineJourney([welcome, administrator, ready]);

        const html = render(steps, 2);

        expect(html).not.toContain('Back');
        expect(html).not.toContain('Start');
    });

    it('disables Back at the first step', () => {
        const steps = defineJourney([welcome, ready]);

        const html = render(steps, 0);

        expect(buttonLabelled(html, 'Back')).toContain('disabled=""');
    });

    it('disables Back when the step before it changed server state', () => {
        const review = step('review', 'Review', { next: { label: 'Finish', enabled: true } });
        const steps = defineJourney([welcome, administrator, review]);

        const html = render(steps, 2);

        expect(buttonLabelled(html, 'Back')).toContain('disabled=""');
    });

    it('refuses a position the journey does not have', () => {
        const steps = defineJourney([welcome, ready]);

        expect(() => render(steps, 2)).toThrow(RangeError);
        expect(() => render(steps, -1)).toThrow(RangeError);
    });
});
