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
 */

import { useQuery } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { keyFingerprint, listConsumers, type VaultConsumer } from '../api/vault.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { DEFAULT_PAGE_SIZE, Pager } from '../ui/Pager.js';
import { Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { RefreshQueries } from '../ui/RefreshButton.js';

/**
 * The services the vault may seal a value to.
 *
 * A consumer is a workload with a key pair it registered, and the vault seals a
 * value only to the consumer its own token names. The list answers that record:
 * the name the secret's owner field carries, the subject the workload proves,
 * the key a person can tell apart, and whether the vault will use it.
 *
 * There is no Add. The vault registers a consumer when the workload asks, and
 * the gateway has no door that registers one on somebody's behalf.
 */
export function ConsumersPage(): ReactNode {
    const { t } = useTranslation();
    const [offset, setOffset] = useState(0);
    const consumers = useQuery({
        queryKey: ['vault-consumers', offset],
        queryFn: () => listConsumers(offset, DEFAULT_PAGE_SIZE),
    });

    const trail = <AreaTrail area="vault" screen={t('vault.consumers.title')} />;

    if (consumers.isError) {
        return (
            <div className="space-y-4">
                {trail}
                <PageHeader
                    title={t('vault.consumers.title')}
                    description={t('vault.consumers.lead')}
                />
                <Notice tone="error">{consumers.error.message}</Notice>
            </div>
        );
    }

    const rows = consumers.data?.consumers ?? [];
    const total = consumers.data?.total ?? 0;

    return (
        <div className="space-y-4">
            <div>
                {trail}
                <PageHeader
                    title={t('vault.consumers.title')}
                    description={t('vault.consumers.lead')}
                    actions={<RefreshQueries keys={[['vault-consumers', offset]]} />}
                />
            </div>

            {consumers.isPending ? (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            ) : rows.length === 0 ? (
                <Notice tone="info">{t('vault.consumers.none')}</Notice>
            ) : (
                <section className="overflow-hidden rounded-md border border-line">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                <th className="px-4 py-2 font-medium">
                                    {t('vault.consumers.name')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('vault.consumers.key')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('vault.consumers.description')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('vault.consumers.state')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {rows.map((row: VaultConsumer) => (
                                <tr
                                    key={row.id}
                                    className="border-b border-line-subtle last:border-b-0"
                                >
                                    <td className="px-4 py-2">
                                        <span className="block font-mono text-xs">{row.name}</span>
                                        <span className="block text-xs text-ink-faint">
                                            {row.actor}
                                        </span>
                                    </td>
                                    <td className="px-4 py-2 font-mono text-xs text-ink-dim">
                                        {keyFingerprint(row.public_key)}
                                    </td>
                                    <td className="px-4 py-2 text-xs text-ink-dim">
                                        {row.description}
                                    </td>
                                    <td className="px-4 py-2">
                                        <Tag tone={row.is_enabled ? 'up' : 'muted'}>
                                            {row.is_enabled
                                                ? t('vault.consumers.enabled')
                                                : t('vault.consumers.disabled')}
                                        </Tag>
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                    <Pager
                        offset={offset}
                        shown={rows.length}
                        total={total}
                        pageSize={DEFAULT_PAGE_SIZE}
                        showing={t('vault.consumers.showing', {
                            first: String(offset + 1),
                            last: String(offset + rows.length),
                            total: String(total),
                        })}
                        onMove={setOffset}
                    />
                </section>
            )}

            <Notice tone="info">{t('vault.consumers.noRegister')}</Notice>
        </div>
    );
}
