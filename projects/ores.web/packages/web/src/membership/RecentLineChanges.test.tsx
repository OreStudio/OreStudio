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
import { renderToStaticMarkup } from 'react-dom/server';
import type { ReportingTreeNode, TimelineEvent } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { RecentLineChanges } from './RecentLineChanges.js';

function node(accountId: string, username: string, fullName: string): ReportingTreeNode {
    return {
        accountId,
        username,
        fullName,
        jobTitle: '',
        accountType: 'user',
        imageId: null,
        reportsToAccountId: null,
        reportsOutsideScope: false,
        partyIds: [],
        depth: 0,
        directReports: 0,
    };
}

function account(id: string, username: string, version: number, recordedAt: string) {
    return { id, username, fullName: username, version, recordedAt };
}

function version(n: number, manager: string): TimelineEvent {
    return {
        entityType: 'ores.iam.account',
        entityId: 'a',
        kind: n === 1 ? 'raised' : 'changed',
        at: `2026-10-0${String(n)} 09:00:00Z`,
        actor: 'grace',
        version: n,
        reasonCode: '',
        commentary: '',
        fields: [{ name: 'Reports To Account ID', value: manager }],
    };
}

function render(): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['accounts'], {
        accounts: [
            account('a1', 'ada', 3, '2026-10-03 09:00:00Z'),
            account('a2', 'alan', 2, '2026-10-05 09:00:00Z'),
            account('a3', 'new', 1, '2026-10-06 09:00:00Z'),
        ],
        totalCount: 3,
    });
    // Ada's line moved; Alan's account changed but his line did not.
    client.setQueryData(['timeline', 'person', 'ada'], {
        subject: 'person',
        id: 'ada',
        gaps: [],
        events: [version(1, ''), version(2, 'g'), version(3, 'h')],
    });
    client.setQueryData(['timeline', 'person', 'alan'], {
        subject: 'person',
        id: 'alan',
        gaps: [],
        events: [version(1, 'g'), version(2, 'g')],
    });
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <RecentLineChanges
                    nodes={[
                        node('a1', 'ada', 'Ada Lovelace'),
                        node('a2', 'alan', 'Alan Turing'),
                        node('a3', 'new', 'New Person'),
                    ]}
                    nameFor={(id) => (id === null ? 'nobody' : id.toUpperCase())}
                    selectedId="a1"
                    onSelect={() => undefined}
                />
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('RecentLineChanges', () => {
    it('lists a person whose manager moved, with from and to', () => {
        const html = render();

        expect(html).toContain('Ada Lovelace');
        expect(html).toContain('G → H');
    });

    it('leaves out a person whose account changed but whose line did not', () => {
        expect(render()).not.toContain('Alan Turing');
    });

    it('leaves out an account that was only created', () => {
        expect(render()).not.toContain('New Person');
    });
});
