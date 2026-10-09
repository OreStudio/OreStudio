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
import type { Timeline as Stream, TimelineEvent } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { Timeline } from './Timeline.js';

function event(over: Partial<TimelineEvent>): TimelineEvent {
    return {
        entityType: 'ores.iam.account',
        entityId: '7c1f0e5a-0000-0000-0000-000000000000',
        kind: 'changed',
        at: '2026-10-04 10:00:00Z',
        actor: 'pyra.natarajan',
        version: 2,
        reasonCode: '',
        commentary: '',
        fields: [],
        ...over,
    };
}

function render(timeline: Stream): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <Timeline timeline={timeline} />
        </TranslationProvider>,
    );
}

const EMPTY_GAPS: Stream['gaps'] = [];

/**
 * What one entry draws, and what the stream says it cannot draw. The stream is
 * general, so what is asserted here is the shape rather than any one subject:
 * a field that changed carries the product's own diff widget, an act that
 * changed nothing is drawn as facts, and the omissions are stated.
 */
describe('the timeline', () => {
    it('says so when nothing has happened', () => {
        const html = render({ subject: 'person', id: 'ana', events: [], gaps: [] });

        expect(html).toContain('Nothing has happened to this yet.');
    });

    it('draws a changed field as a difference, and the version it came from', () => {
        const html = render({
            subject: 'person',
            id: 'ana',
            events: [
                event({
                    version: 2,
                    reasonCode: 'system.update',
                    fields: [
                        { name: 'Job Title', value: 'Head of Rates' },
                        { name: 'Change Reason Code', value: 'system.update' },
                    ],
                }),
                event({
                    version: 1,
                    kind: 'raised',
                    fields: [{ name: 'Job Title', value: 'Rates Analyst' }],
                }),
            ],
            gaps: EMPTY_GAPS,
        });

        expect(html).toContain('v2');
        expect(html).toContain('Job Title');
        expect(html).toContain('Rates Analyst');
        expect(html).toContain('Head of Rates');
        expect(html).toContain('mark');
        expect(html).toContain('system.update');
    });

    it('draws an act that changed no field quietly, as the facts it carries', () => {
        const html = render({
            subject: 'person',
            id: 'ana',
            events: [
                event({
                    kind: 'signed_in',
                    entityType: 'ores.iam.auth_event',
                    entityId: 'e1',
                    version: 1,
                    fields: [{ name: 'Event', value: 'login_success' }],
                }),
            ],
            gaps: EMPTY_GAPS,
        });

        expect(html).toContain('login_success');
        expect(html).toContain('Signed in');
        expect(html).toContain('opacity-90');
        expect(html).not.toContain('<mark');
    });

    it('never names an internal entity or an unread source to the reader', () => {
        const html = render({
            subject: 'person',
            id: 'ana',
            events: [event({ version: 1, kind: 'raised' })],
            gaps: [
                { entity: 'ores.iam.account_role', reason: 'the roles read refused this caller' },
                { entity: 'ores.iam.auth_event', reason: 'the sign-in read refused this caller' },
            ],
        });

        /*
         * A gap is the server's honest record of what the stream could not
         * draw, and it is not the reader's business: it names tables, reads and
         * refusals. The screen tells the reader what happened, not which of the
         * deployment's own reads were tried.
         */
        expect(html).not.toContain('ores.iam.');
        expect(html).not.toContain('refused this caller');
    });

    it('separates the days the entries fall on', () => {
        const html = render({
            subject: 'person',
            id: 'ana',
            events: [
                event({ at: '2026-10-04 10:00:00Z', version: 2 }),
                event({ at: '2026-10-02 10:00:00Z', version: 1, kind: 'raised' }),
            ],
            gaps: EMPTY_GAPS,
        });

        expect(html.match(/uppercase/g)?.length).toBeGreaterThanOrEqual(2);
    });
});
