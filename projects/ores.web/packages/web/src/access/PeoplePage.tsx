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

import { useQueries, useQuery } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { useNavigate } from 'react-router';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { Avatar, imageUrl } from '../ui/Images.js';
import { Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { roleLabel } from './words.js';

/**
 * Who can sign in to the tenant, and the roles each one holds.
 *
 * Each row is a person with their picture; opening one is where roles are
 * given and taken away. The roles are read one account at a time, because the
 * server has no joined read yet, and a row whose roles cannot be read still
 * names the person.
 */
export function PeoplePage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const people = useQuery({ queryKey: ['accounts'], queryFn: api.accounts });
    const accounts = people.data?.accounts ?? [];
    const access = useQueries({
        queries: accounts.map((account) => ({
            queryKey: ['account-access', account.id],
            queryFn: () => api.accountAccess(account.id),
        })),
    });

    if (people.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (people.isError) {
        return <Notice tone="error">{people.error.message}</Notice>;
    }

    return (
        <div>
            <PageHeader title={t('access.people.title')} description={t('access.people.lead')} />
            <div className="overflow-x-auto rounded-md border border-line">
                <table className="w-full text-left text-sm">
                    <thead>
                        <tr className="border-b border-line text-xs text-ink-muted">
                            <th className="px-4 py-2 font-medium">{t('access.people.person')}</th>
                            <th className="px-4 py-2 font-medium">{t('access.people.roles')}</th>
                        </tr>
                    </thead>
                    <tbody>
                        {accounts.map((account, index) => {
                            const name =
                                account.fullName === '' ? account.username : account.fullName;
                            const held = access[index]?.data?.roles;
                            return (
                                <tr
                                    key={account.id}
                                    className="cursor-pointer border-b border-line-subtle last:border-b-0 hover:bg-surface-hover"
                                    onClick={() =>
                                        void navigate(
                                            `/people/${encodeURIComponent(account.username)}`,
                                        )
                                    }
                                >
                                    <td className="px-4 py-2">
                                        <span className="flex items-center gap-3">
                                            <Avatar
                                                name={name}
                                                src={
                                                    account.imageId === null
                                                        ? null
                                                        : imageUrl(account.imageId)
                                                }
                                            />
                                            <span>
                                                <span className="block text-ink">{name}</span>
                                                <span className="block text-xs text-ink-faint">
                                                    {account.username}
                                                    {account.jobTitle !== '' &&
                                                        ` · ${account.jobTitle}`}
                                                </span>
                                            </span>
                                        </span>
                                    </td>
                                    <td className="px-4 py-2">
                                        {held === undefined ? (
                                            <span className="text-ink-faint">…</span>
                                        ) : held.length === 0 ? (
                                            <span className="text-ink-faint">
                                                {t('access.people.noRole')}
                                            </span>
                                        ) : (
                                            <span className="flex flex-wrap gap-1">
                                                {held.map((role) => (
                                                    <Tag key={role.roleId}>
                                                        {roleLabel(t, role.name)}
                                                    </Tag>
                                                ))}
                                            </span>
                                        )}
                                    </td>
                                </tr>
                            );
                        })}
                    </tbody>
                </table>
            </div>
        </div>
    );
}
