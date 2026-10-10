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
import { useState, type ReactNode } from 'react';
import { api } from '../api/client.js';
import { Crumbs } from '../refdata/shared.js';
import { useTranslation } from '../i18n/Provider.js';
import { Input, PageHeader, Tag } from '../ui/Primitives.js';
import { RefreshButton } from '../ui/RefreshButton.js';
import { NodeAvatar, nameOf } from './NodeParts.js';
import { partyLabel } from './organisation.js';

/**
 * The staff a member may see: the people who work in the parties their account
 * works in, and everyone who reports to them.
 *
 * It reads the organisation tree, which the organisation read answers, and not
 * the tenant's accounts, which a member does not read. So the list holds a name,
 * a title, a picture and the parties, and it opens no one's page.
 */
export function StaffOfMyParties(): ReactNode {
    const { t } = useTranslation();
    const [text, setText] = useState('');
    const tree = useQuery({ queryKey: ['reporting-tree'], queryFn: () => api.reportingTree() });
    const parties = new Map((tree.data?.parties ?? []).map((entry) => [entry.partyId, entry]));
    const wanted = text.trim().toLowerCase();
    const people = [...(tree.data?.nodes ?? [])]
        .filter(
            (node) =>
                wanted === '' ||
                nameOf(node).toLowerCase().includes(wanted) ||
                node.username.toLowerCase().includes(wanted) ||
                node.jobTitle.toLowerCase().includes(wanted),
        )
        .sort((a, b) => nameOf(a).localeCompare(nameOf(b)));
    return (
        <div className="space-y-6">
            <div>
                <Crumbs
                    parts={[
                        { label: t('shell.menu.home'), to: '/' },
                        { label: t('access.hub.title'), to: '/organisation' },
                        { label: t('access.hub.staff') },
                    ]}
                />
                <PageHeader
                    title={t('access.hub.staff')}
                    description={t('access.people.leadParties')}
                    actions={
                        <RefreshButton
                            onClick={() => void tree.refetch()}
                            pending={tree.isFetching}
                        />
                    }
                />
            </div>
            <Input
                type="search"
                className="max-w-sm"
                aria-label={t('access.people.findStaff')}
                placeholder={t('access.people.findStaff')}
                value={text}
                onChange={(event) => setText(event.target.value)}
            />
            <section className="card overflow-x-auto">
                <table className="w-full text-sm">
                    <thead>
                        <tr className="border-b border-line text-left text-xs text-ink-muted">
                            <th className="px-4 py-2 font-medium">{t('access.people.person')}</th>
                            <th className="px-4 py-2 font-medium">{t('access.people.username')}</th>
                            <th className="px-4 py-2 font-medium">{t('signIns.kind')}</th>
                            <th className="px-4 py-2 font-medium">{t('access.people.jobTitle')}</th>
                            <th className="px-4 py-2 font-medium">
                                {t('access.people.partiesColumn')}
                            </th>
                        </tr>
                    </thead>
                    <tbody>
                        {people.map((node) => (
                            <tr key={node.accountId} className="border-b border-line-subtle">
                                <td className="px-4 py-2">
                                    <span className="flex items-center gap-3">
                                        <NodeAvatar node={node} size="sm" />
                                        {nameOf(node)}
                                    </span>
                                </td>
                                <td className="px-4 py-2 font-mono">{node.username}</td>
                                <td className="px-4 py-2">
                                    {node.accountType === 'user' ? (
                                        <span className="text-ink-muted">
                                            {t('signIns.person')}
                                        </span>
                                    ) : (
                                        <Tag tone="accent">{t('signIns.service')}</Tag>
                                    )}
                                </td>
                                <td className="px-4 py-2">{node.jobTitle}</td>
                                <td className="px-4 py-2">
                                    <span className="flex flex-wrap gap-1">
                                        {node.partyIds.map((id) => {
                                            const entry = parties.get(id);
                                            return entry === undefined ? null : (
                                                <Tag key={id}>{partyLabel(entry)}</Tag>
                                            );
                                        })}
                                    </span>
                                </td>
                            </tr>
                        ))}
                        {people.length === 0 && (
                            <tr>
                                <td colSpan={5} className="px-4 py-3 text-ink-muted">
                                    {t('access.nothingMatches')}
                                </td>
                            </tr>
                        )}
                    </tbody>
                </table>
            </section>
        </div>
    );
}
