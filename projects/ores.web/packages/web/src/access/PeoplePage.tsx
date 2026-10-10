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

import { useQuery } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import type { Account, SessionMode } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { RecordList, type ListSource } from '../refdata/RecordList.js';
import { Avatar, imageUrl } from '../ui/Images.js';
import { Tag } from '../ui/Primitives.js';
import { displayName } from './names.js';
import { roleLabel } from './words.js';
import { useHolds } from './holds.js';
import { partyLabel } from '../membership/organisation.js';
import { StaffOfMyParties } from '../membership/StaffOfMyParties.js';

/** The address of one person's page. */
export function personPath(username: string): string {
    return `/people/${encodeURIComponent(username)}`;
}

/**
 * Where an entry's author is opened, for a reader who may open people.
 *
 * A service writes entries too, and a service has no person page, so only a
 * name that is a person is linked.
 */
export function actorPathFor(mayOpen: boolean): (actor: string) => string | undefined {
    return (actor) =>
        mayOpen && !/(_service|^system$|^ores_)/.test(actor) ? personPath(actor) : undefined;
}

/** The tenant's people, one page at a time, searched and ordered on the server. */
const PEOPLE: ListSource<Account> = {
    key: 'people',
    read: (page) => api.accountsPage(page),
    search: true,
    sortable: ['username', 'full_name'],
    mayAdd: false,
};

/**
 * Who can sign in, and the roles each one holds: the shared record list, so it
 * pages, searches and sorts like every other list. Opening a person is where
 * their details, contact, roles and sign-ins are kept.
 *
 * The session's own tenant decides the framing. For a system administrator
 * that tenant is the system tenant, so the same list is the deployment's own
 * accounts and the screen is named for them.
 */
export function PeoplePage({ mode }: { readonly mode: SessionMode }): ReactNode {
    const { t } = useTranslation();
    const holds = useHolds();
    const system = mode === 'system-administration';
    const title = system ? t('accounts.title') : t('access.people.title');
    /*
     * The roles column reads one person's grants a row at a time, and that read
     * is its own permission. A caller who does not hold it is not shown the
     * column at all: a member who opens this screen is asking who is here, not
     * what each of them may do, and a refusal is a worse answer than an absent
     * column.
     */
    const mayReadRoles = holds('iam::roles:read');
    // The parties each person works in come with the organisation read.
    const tree = useQuery({
        queryKey: ['reporting-tree'],
        queryFn: () => api.reportingTree(),
        enabled: !system,
        meta: { quiet: true },
    });
    const partiesOf = (accountId: string): readonly string[] => {
        const node = tree.data?.nodes.find((candidate) => candidate.accountId === accountId);
        const named = new Map((tree.data?.parties ?? []).map((party) => [party.partyId, party]));
        return (node?.partyIds ?? []).flatMap((id) => {
            const party = named.get(id);
            return party === undefined ? [] : [partyLabel(party)];
        });
    };
    // A member reads the people of their parties from the organisation, not the accounts.
    if (!system && !holds('iam::accounts:read') && holds('iam::organisation:read')) {
        return <StaffOfMyParties />;
    }
    const lead = system
        ? t('accounts.description')
        : mayReadRoles
          ? t('access.people.lead')
          : t('access.people.leadNoRoles');
    return (
        <RecordList
            source={PEOPLE}
            title={title}
            lead={lead}
            crumbs={
                system
                    ? [{ label: t('shell.menu.home'), to: '/' }, { label: title }]
                    : [
                          { label: t('shell.menu.home'), to: '/' },
                          { label: t('access.hub.title'), to: '/organisation' },
                          { label: title },
                      ]
            }
            pathOf={(account) => personPath(account.username)}
            columns={[
                {
                    id: 'person',
                    header: t('access.people.person'),
                    sort: 'full_name',
                    cell: (account) => (
                        <span className="flex items-center gap-3">
                            <Avatar
                                name={displayName(account, account.username)}
                                size="sm"
                                src={account.imageId === null ? null : imageUrl(account.imageId)}
                            />
                            {displayName(account, account.username)}
                        </span>
                    ),
                },
                {
                    id: 'username',
                    header: t('access.people.username'),
                    sort: 'username',
                    mono: true,
                    cell: (account) => account.username,
                },
                {
                    id: 'kind',
                    header: t('signIns.kind'),
                    cell: (account) =>
                        account.accountType === 'user' ? (
                            <span className="text-ink-muted">{t('signIns.person')}</span>
                        ) : (
                            <Tag tone="accent">{t('signIns.service')}</Tag>
                        ),
                },
                {
                    id: 'job_title',
                    header: t('access.people.jobTitle'),
                    cell: (account) => account.jobTitle,
                },
                ...(system
                    ? []
                    : [
                          {
                              id: 'parties',
                              header: t('access.people.partiesColumn'),
                              cell: (account: Account) => (
                                  <span className="flex flex-wrap gap-1">
                                      {partiesOf(account.id).map((name) => (
                                          <Tag key={name}>{name}</Tag>
                                      ))}
                                  </span>
                              ),
                          },
                      ]),
                ...(mayReadRoles
                    ? [
                          {
                              id: 'roles',
                              header: t('access.people.roles'),
                              cell: (account: Account) => <HeldRoles accountId={account.id} />,
                          },
                      ]
                    : []),
            ]}
        />
    );
}

/**
 * The roles one person holds. Read one person at a time, because the server
 * has no joined read yet; a page shows fifteen people, so fifteen reads.
 */
function HeldRoles({ accountId }: { readonly accountId: string }): ReactNode {
    const { t } = useTranslation();
    const access = useQuery({
        queryKey: ['account-access', accountId],
        queryFn: () => api.accountAccess(accountId),
    });
    const held = access.data?.roles;
    if (held === undefined) {
        return <span className="text-ink-faint">…</span>;
    }
    if (held.length === 0) {
        return <span className="text-ink-faint">{t('access.people.noRole')}</span>;
    }
    return (
        <span className="flex flex-wrap gap-1">
            {held.map((role) => (
                <Tag key={role.roleId}>{roleLabel(t, role.name)}</Tag>
            ))}
        </span>
    );
}
