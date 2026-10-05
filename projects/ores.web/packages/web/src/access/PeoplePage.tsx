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
import type { Account } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { RecordList, type ListSource } from '../refdata/RecordList.js';
import { Avatar, imageUrl } from '../ui/Images.js';
import { Tag } from '../ui/Primitives.js';
import { roleLabel } from './words.js';

/** The address of one person's page. */
export function personPath(username: string): string {
    return `/people/${encodeURIComponent(username)}`;
}

/** What a person is called: their name, or their username when they have none. */
export function nameOf(account: Account): string {
    return account.fullName === '' ? account.username : account.fullName;
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
 * Who can sign in to the tenant, and the roles each one holds: the shared
 * record list, so it pages, searches and sorts like every other list. Opening
 * a person is where their details, contact, roles and sign-ins are kept.
 */
export function PeoplePage(): ReactNode {
    const { t } = useTranslation();
    return (
        <RecordList
            source={PEOPLE}
            title={t('access.people.title')}
            lead={t('access.people.lead')}
            crumbs={[{ label: t('shell.menu.home'), to: '/' }, { label: t('access.people.title') }]}
            pathOf={(account) => personPath(account.username)}
            columns={[
                {
                    id: 'person',
                    header: t('access.people.person'),
                    sort: 'full_name',
                    cell: (account) => (
                        <span className="flex items-center gap-3">
                            <Avatar
                                name={nameOf(account)}
                                size="sm"
                                src={account.imageId === null ? null : imageUrl(account.imageId)}
                            />
                            {nameOf(account)}
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
                    id: 'job_title',
                    header: t('access.people.jobTitle'),
                    cell: (account) => account.jobTitle,
                },
                {
                    id: 'roles',
                    header: t('access.people.roles'),
                    cell: (account) => <HeldRoles accountId={account.id} />,
                },
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
