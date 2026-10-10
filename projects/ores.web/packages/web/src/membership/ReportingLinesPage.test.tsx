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
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import type { ReportingTreeNode, TimelineEvent } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { managerCandidates } from './managers.js';
import { reportingHistory, ReportingLinesPage } from './ReportingLinesPage.js';

/**
 * The Reporting lines screen, rendered from a seeded query cache.
 *
 * What is checked is what the screen promises: the shape is drawn from the
 * read, the people who reach no root are stated rather than drawn as roots,
 * the gaps the journey still records are stated, and the manager picker cannot
 * offer anybody who reports to the person being changed.
 */

const ADA = '11111111-1111-1111-1111-111111111111';
const GRACE = '22222222-2222-2222-2222-222222222222';
const ALAN = '33333333-3333-3333-3333-333333333333';
const EDSGER = '44444444-4444-4444-4444-444444444444';

function person(
    accountId: string,
    fullName: string,
    reportsToAccountId: string | null,
    depth: number,
    directReports: number,
): ReportingTreeNode {
    return {
        accountId,
        username: fullName.toLowerCase().replace(/ /g, '.'),
        fullName,
        jobTitle: `${fullName} title`,
        reportsToAccountId,
        depth,
        directReports,
    };
}

/** Ada at the root, Grace under her, Alan under Grace, Edsger under Ada. */
const TREE = [
    person(ADA, 'Ada Lovelace', null, 0, 2),
    person(GRACE, 'Grace Hopper', ADA, 1, 1),
    person(EDSGER, 'Edsger Dijkstra', ADA, 1, 0),
    person(ALAN, 'Alan Turing', GRACE, 2, 0),
];

function render(nodes: readonly ReportingTreeNode[], unrooted = 0, me = ''): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['reporting-tree'], { unrooted, nodes });
    client.setQueryData(
        ['amend-reasons'],
        [{ code: 'common.non_material_update', description: 'Non material update' }],
    );
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter>
                    <ReportingLinesPage me={me} />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('Reporting lines', () => {
    it('draws the tree with each person and how many report to them', () => {
        const html = render(TREE);

        expect(html).toContain('Ada Lovelace');
        expect(html).toContain('Grace Hopper');
        expect(html).toContain('Edsger Dijkstra');
        expect(html).toContain('Alan Turing');
        expect(html).toContain('2 reporting');
        expect(html).toContain('4 people');
        expect(html).toContain('Pick a person in the tree to read their line.');
    });

    it('draws each person with their picture', () => {
        const html = render(TREE);

        expect(html).toContain('/api/accounts/ada.lovelace/picture');
        expect(html).toContain('/api/accounts/alan.turing/picture');
    });

    it('opens on the signed-in person and marks them as the reader', () => {
        const html = render(TREE, 0, 'grace.hopper');

        // The card names the person the reader is, and the tree marks the row.
        expect(html).toContain('>You<');
        expect(html).not.toContain('Pick a person in the tree to read their line.');
        expect(html).toContain('Grace Hopper title');
        // Grace reports to Ada, drawn with her picture.
        expect(html).toContain('Reports to');
    });

    it('offers one Refresh, and no longer lists what it cannot do', () => {
        const html = render(TREE);

        expect(html).toContain('>Refresh<');
        expect(html).not.toContain('What this screen cannot do');
        expect(html).not.toContain('Save line');
        expect(html).not.toContain('Clear line');
    });

    it('states the people who reach no root rather than drawing them as roots', () => {
        const stranded = person(ALAN, 'Alan Turing', EDSGER, -1, 0);
        const html = render([...TREE.filter((node) => node.accountId !== ALAN), stranded], 1);

        expect(html).toContain('reach no root');
        expect(html).toContain('1 people reach no root');
    });
});

describe('the manager picker', () => {
    it('leaves out the person and everybody who reports to them', () => {
        const selected = TREE[0] as ReportingTreeNode;
        const offered = managerCandidates(TREE, selected).map((node) => node.fullName);

        // Ada has no manager to change to: every other person in the tree
        // reports to her, directly or through Grace.
        expect(offered).toEqual([]);

        const grace = TREE[1] as ReportingTreeNode;
        const forGrace = managerCandidates(TREE, grace).map((node) => node.fullName);
        // Grace may report to Ada, and to Edsger, who is not in her branch.
        expect(forGrace).toEqual(['Ada Lovelace', 'Edsger Dijkstra']);
    });
});

function version(
    version: number,
    manager: string,
    over: Partial<TimelineEvent> = {},
): TimelineEvent {
    return {
        entityType: 'ores.iam.account',
        entityId: 'a',
        kind: version === 1 ? 'raised' : 'changed',
        at: `2026-10-0${String(version)} 10:00:00Z`,
        actor: 'tenant_admin',
        version,
        reasonCode: 'system.update',
        commentary: '',
        fields: [{ name: 'Reports To Account ID', value: manager }],
        ...over,
    };
}

describe('the history of a line', () => {
    it('lists only the versions where the manager changed, newest first', () => {
        const changes = reportingHistory([
            version(1, ''),
            version(2, GRACE),
            // A version that left the manager alone is not a change of the line.
            version(3, GRACE, { fields: [{ name: 'Reports To Account ID', value: GRACE }] }),
            version(4, ADA),
            version(5, ''),
        ]);

        expect(changes.map((change) => [change.version, change.from, change.to])).toEqual([
            [5, ADA, null],
            [4, GRACE, ADA],
            [2, null, GRACE],
        ]);
    });

    it('keeps the reason and the note a change was made with', () => {
        const [change] = reportingHistory([
            version(1, ''),
            version(2, ADA, { reasonCode: 'common.rectification', commentary: 'Wrong manager' }),
        ]);

        expect(change?.reasonCode).toBe('common.rectification');
        expect(change?.commentary).toBe('Wrong manager');
    });

    it('ignores the entries that are not versions of the account', () => {
        const changes = reportingHistory([
            version(1, ADA),
            version(2, GRACE, { entityType: 'ores.iam.account_contact_information' }),
        ]);

        expect(changes).toHaveLength(1);
    });
});
