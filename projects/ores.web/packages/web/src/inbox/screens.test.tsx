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
import { MemoryRouter, Route, Routes } from 'react-router';
import type { InboxNotificationView, InboxRequestView } from '@ores/wire-protocol/browser';
import { TranslationProvider } from '../i18n/Provider.js';
import { MyAccessPage } from '../access/MyAccessPage.js';
import { AppShell } from '../components/AppShell.js';
import { AskForRoleDialog } from './AskForRoleDialog.js';
import { NotificationBell, notificationRoute } from './NotificationBell.js';
import { RequestDetailPage } from './RequestDetailPage.js';
import { RequestsPage } from './RequestsPage.js';

/**
 * The inbox screens, rendered from a seeded query cache.
 *
 * What is checked is what each screen promises: a person reads the reason they
 * wrote and can take a waiting request back, an administrator reads who asked
 * and for what, a refusal needs a reason before it is offered, and the bell
 * says how much is unread.
 */

const REQUEST = 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa';
const ROLE = 'bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb';

const CATALOGUE = [
    { code: '*', description: 'Full access' },
    { code: 'refdata::currencies:read', description: 'View currencies' },
];

/** A member's own request: no roles, because the join behind them is unreadable. */
function mine(overrides: Partial<InboxRequestView> = {}): InboxRequestView {
    return {
        id: REQUEST,
        version: 3,
        kindCode: 'iam.role_grant',
        stateCode: 'waiting',
        requestedBy: 'daniel',
        requestedAt: '2026-10-04 09:00:00Z',
        reason: 'I price the FX book and cannot read currencies.',
        expiresAt: '',
        roles: [],
        decision: null,
        ...overrides,
    } as unknown as InboxRequestView;
}

/** The administrator's queue row: the same request, with the role resolved. */
function queued(overrides: Partial<InboxRequestView> = {}): InboxRequestView {
    return mine({
        requestedBy: 'daniel',
        roles: [{ roleId: ROLE, name: 'Trading', description: 'Trading desk access' }],
        ...overrides,
    });
}

function notification(overrides: Partial<InboxNotificationView> = {}): InboxNotificationView {
    return {
        id: 'cccccccc-cccc-4ccc-8ccc-cccccccccccc',
        kindCode: 'inbox.approval_waiting',
        messageKey: 'notification.inbox.approval_waiting',
        raisedBy: 'priya',
        raisedAt: '2026-10-04 09:00:00Z',
        linkRoute: 'requests',
        linkId: REQUEST,
        arguments: [
            { name: 'kind', value: 'Role request' },
            { name: 'requester', value: 'daniel' },
            { name: 'reason', value: 'I price the FX book.' },
        ],
        readAt: '',
        ...overrides,
    } as unknown as InboxNotificationView;
}

function render(
    seed: (client: QueryClient) => void,
    element: ReactNode,
    path: string,
    pattern = path,
): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['permissions'], CATALOGUE);
    seed(client);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={[path]}>
                    <Routes>
                        <Route path={pattern} element={element} />
                    </Routes>
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('a person’s own requests', () => {
    it('names what they asked for, shows the reason they wrote and offers a way back', () => {
        const html = render(
            (client) => {
                client.setQueryData(['my-requests'], { items: [mine()], total: 1 });
                client.setQueryData(['my-access'], { roles: [] });
            },
            <MyAccessPage tenantName="Acme" />,
            '/access',
        );

        expect(html).toContain('Your requests');
        // The member's own row carries no roles, so the kind names it.
        expect(html).toContain('Role request');
        expect(html).toContain('Waiting');
        expect(html).toContain('asked 2026-10-04');
        expect(html).toContain('You wrote: I price the FX book and cannot read currencies.');
        expect(html).toMatch(/<button[^>]*>Withdraw<\/button>/);
    });

    it('draws no panel at all when the person has asked for nothing', () => {
        const html = render(
            (client) => {
                client.setQueryData(['my-requests'], { items: [], total: 0 });
                client.setQueryData(['my-access'], { roles: [] });
            },
            <MyAccessPage tenantName="Acme" />,
            '/access',
        );

        expect(html).not.toContain('Your requests');
    });

    it('shows the decider and their comment once the request is answered', () => {
        const html = render(
            (client) => {
                client.setQueryData(['my-requests'], {
                    items: [
                        mine({
                            stateCode: 'refused',
                            decision: {
                                decisionCode: 'refuse',
                                decidedBy: 'priya',
                                decidedAt: '2026-10-05 09:00:00Z',
                                comment: 'Read access comes with the desk induction.',
                            },
                        }),
                    ],
                    total: 1,
                });
                client.setQueryData(['my-access'], { roles: [] });
            },
            <MyAccessPage tenantName="Acme" />,
            '/access',
        );

        expect(html).toContain('Refused');
        expect(html).toContain('priya');
        expect(html).toContain('Read access comes with the desk induction.');
        expect(html).not.toMatch(/<button[^>]*>Withdraw<\/button>/);
    });

    it('offers the ask dialog from the roles panel', () => {
        const html = render(
            (client) => {
                client.setQueryData(['my-requests'], { items: [], total: 0 });
                client.setQueryData(['my-access'], { roles: [] });
            },
            <MyAccessPage tenantName="Acme" />,
            '/access',
        );

        expect(html).toContain('Ask for a role');
    });

    it('offers only the roles the tenant lets a member ask for', () => {
        const html = render(
            (client) => {
                client.setQueryData(
                    ['roles'],
                    [
                        {
                            id: 'role-trading',
                            version: 1,
                            name: 'Trading',
                            description: 'Trading desk access',
                            service: false,
                            registrationDefault: false,
                            requestable: true,
                            permissionCodes: ['refdata::currencies:read'],
                        },
                        {
                            id: 'role-admin',
                            version: 1,
                            name: 'TenantAdmin',
                            description: 'Runs the tenant',
                            service: false,
                            registrationDefault: false,
                            requestable: false,
                            permissionCodes: ['iam::accounts:create'],
                        },
                        {
                            id: 'role-service',
                            version: 1,
                            name: 'IamService',
                            description: 'IAM domain service',
                            service: true,
                            registrationDefault: false,
                            requestable: false,
                            permissionCodes: [],
                        },
                        {
                            id: 'role-held',
                            version: 1,
                            name: 'Member',
                            description: 'What everyone starts with',
                            service: false,
                            registrationDefault: false,
                            requestable: true,
                            permissionCodes: [],
                        },
                    ],
                );
                client.setQueryData(['my-access'], { roles: [{ roleId: 'role-held' }] });
            },
            <AskForRoleDialog onClose={() => {}} />,
            '/access',
        );

        expect(html).toContain('Trading desk access');
        expect(html).not.toContain('Runs the tenant');
        expect(html).not.toContain('IAM domain service');
        expect(html).not.toContain('What everyone starts with');
    });
});

describe('the request queue', () => {
    it('names who asked, what for and why, and opens the request from the row', () => {
        const html = render(
            (client) => {
                client.setQueryData(['request-queue'], { items: [queued()], total: 1 });
            },
            <RequestsPage />,
            '/requests',
        );

        expect(html).toContain('Who');
        expect(html).toContain('Asked for');
        expect(html).toContain('daniel');
        expect(html).toContain('Trading');
        expect(html).toContain('I price the FX book and cannot read currencies.');
        expect(html).toContain('src="/api/accounts/daniel/picture"');
    });

    it('says nothing is waiting rather than drawing an empty table', () => {
        const html = render(
            (client) => {
                client.setQueryData(['request-queue'], { items: [], total: 0 });
            },
            <RequestsPage />,
            '/requests',
        );

        expect(html).toContain('Nothing is waiting.');
    });

    it('lists what has been answered below the queue', () => {
        const html = render(
            (client) => {
                client.setQueryData(['request-queue'], {
                    items: [
                        queued({
                            stateCode: 'approved',
                            decision: {
                                decisionCode: 'approve',
                                decidedBy: 'priya',
                                decidedAt: '2026-10-05 09:00:00Z',
                                comment: 'Induction done.',
                            },
                        }),
                    ],
                    total: 1,
                });
            },
            <RequestsPage />,
            '/requests',
        );

        expect(html).toContain('Answered');
        expect(html).toContain('Given');
        expect(html).toContain('by priya');
        expect(html).toContain('Induction done.');
    });
});

describe('answering one request', () => {
    function detail(me: string): string {
        return render(
            (client) => {
                client.setQueryData(['request-queue'], { items: [queued()], total: 1 });
                client.setQueryData(
                    ['roles'],
                    [
                        {
                            id: ROLE,
                            version: 1,
                            name: 'Trading',
                            description: 'Trading desk access',
                            service: false,
                            registrationDefault: false,
                            requestable: true,
                            permissionCodes: ['refdata::currencies:read'],
                        },
                    ],
                );
            },
            <RequestDetailPage me={me} />,
            `/requests/${REQUEST}`,
            '/requests/:id',
        );
    }

    it('shows the requester, the role and what the role would let them do', () => {
        const html = detail('priya');

        expect(html).toContain('daniel asks for Trading');
        expect(html).toContain('Trading desk access');
        expect(html).toContain('What Trading would let them do');
        expect(html).toContain('I price the FX book and cannot read currencies.');
        expect(html).toContain('href="/requests"');
    });

    it('will not refuse without a reason, and offers the reason control', () => {
        const html = detail('priya');

        expect(html).toContain('Reason');
        expect(html).toMatch(/<button[^>]*disabled=""[^>]*>Refuse<\/button>/);
        expect(html).toMatch(/<button[^>]*>Give Trading<\/button>/);
    });

    it('tells an administrator who asked themselves that somebody else answers it', () => {
        const html = detail('daniel');

        expect(html).toContain('You asked for this yourself, so another administrator answers it.');
        expect(html).toMatch(/<button[^>]*disabled=""[^>]*>Give Trading<\/button>/);
    });
});

describe('the notification bell', () => {
    it('says how many are unread and lists what happened', () => {
        const html = render(
            (client) => {
                client.setQueryData(['unread-notifications'], 3);
                client.setQueryData(['notifications'], { items: [notification()], total: 1 });
            },
            <NotificationBell />,
            '/',
        );

        expect(html).toContain('aria-label="Notifications, 3 unread"');
        expect(html).toContain('>3</span>');
        expect(html).toContain('Notifications');
        expect(html).toContain('Mark all read');
        // The message is the key the server sent, rendered with its own values.
        expect(html).toContain('daniel asks for Role request. They wrote: I price the FX book.');
    });

    it('says there is nothing to read rather than drawing an empty list', () => {
        const html = render(
            (client) => {
                client.setQueryData(['unread-notifications'], 0);
                client.setQueryData(['notifications'], { items: [], total: 0 });
            },
            <NotificationBell />,
            '/',
        );

        expect(html).toContain(
            '<p class="px-2 py-3 text-sm text-ink-muted">Nothing to tell you.</p>',
        );
    });

    it('points a notification at the request it names', () => {
        expect(notificationRoute(notification())).toBe(`/requests/${REQUEST}`);
        expect(notificationRoute(notification({ linkId: '' }))).toBe('/requests');
    });
});

/*
 * The queue is the administrator's, so the menu offers its door only to
 * somebody who holds the permission the request kind names to decide it. The
 * server checks the same permission again on every call, because the menu is
 * the client's structure and not a decision.
 */
describe('the requests menu item', () => {
    function shell(permissionCodes: readonly string[]): string {
        return render(
            (client) => {
                client.setQueryData(['my-access'], {
                    roles: [
                        {
                            roleId: ROLE,
                            name: 'Viewer',
                            description: '',
                            permissionCodes: [...permissionCodes],
                            givenBy: 'system',
                            givenAt: '2026-10-05 09:30:00Z',
                            reasonCode: 'access.initial',
                            commentary: '',
                        },
                    ],
                });
                client.setQueryData(['unread-notifications'], 0);
                client.setQueryData(['notifications'], { items: [], total: 0 });
            },
            <AppShell
                username="priya"
                tenantName="Acme"
                partyName="Acme Operations"
                mode="application"
                onSignOut={() => undefined}
            >
                <p>the screen</p>
            </AppShell>,
            '/',
        );
    }

    it('appears for somebody who may assign roles', () => {
        const html = shell(['iam::roles:assign']);

        expect(html).toContain('href="/requests"');
        expect(html).toContain('>Requests</a>');
    });

    it('is left out of the menu for somebody who may not', () => {
        expect(shell(['refdata::currencies:read'])).not.toContain('href="/requests"');
    });
});
