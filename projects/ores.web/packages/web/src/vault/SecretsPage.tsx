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
import { Link, useNavigate } from 'react-router';
import { listSecrets, type VaultSecret } from '../api/vault.js';
import { PERMISSION } from '../access/permissions.js';
import { useHeld } from '../access/holds.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { DEFAULT_PAGE_SIZE, Pager } from '../ui/Pager.js';
import { LinkButton, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { RefreshQueries } from '../ui/RefreshButton.js';
import { formatDateTime } from '../ui/Time.js';
import { VAULT, secretPath } from '../shell/paths.js';
import { WriteSecretDialog } from './WriteSecretDialog.js';

/**
 * The values this installation keeps.
 *
 * The list is what the vault holds and not a value: a secret's value leaves the
 * vault only by an ask it records, and that ask has a screen of its own. Every
 * row opens the secret's record, which is where the owner, the protecting key
 * and the ask live.
 *
 * Add is drawn for a person who holds the write, and not drawn disabled for one
 * who does not: an action somebody cannot take is not an action of theirs.
 */
export function SecretsPage(): ReactNode {
    const { t, language } = useTranslation();
    const navigate = useNavigate();
    const holds = useHeld();
    const [offset, setOffset] = useState(0);
    const [writing, setWriting] = useState(false);
    const secrets = useQuery({
        queryKey: ['vault-secrets', offset],
        queryFn: () => listSecrets(offset, DEFAULT_PAGE_SIZE),
    });

    // The list is the area's own page, so its trail is Home and the area: the
    // title states the screen, and a step that repeats the area says nothing.
    const trail = <AreaTrail area="vault" />;
    const actions = (
        <div className="flex items-center gap-2">
            <LinkButton to={VAULT.consumers} size="sm">
                {t('vault.consumers.title')}
            </LinkButton>
            {holds(PERMISSION.vaultSecretsWrite) && (
                <button
                    type="button"
                    onClick={() => setWriting(true)}
                    className="rounded-md border border-accent/50 bg-accent/10 px-3 py-2 text-sm text-accent-bright hover:bg-accent/20"
                >
                    {t('vault.secrets.add')}
                </button>
            )}
            <RefreshQueries keys={[['vault-secrets', offset]]} />
        </div>
    );

    if (secrets.isError) {
        return (
            <div className="space-y-4">
                {trail}
                <PageHeader
                    title={t('vault.secrets.title')}
                    description={t('vault.secrets.lead')}
                />
                <Notice tone="error">{secrets.error.message}</Notice>
            </div>
        );
    }

    const rows = secrets.data?.secrets ?? [];
    const total = secrets.data?.total ?? 0;

    return (
        <div className="space-y-4">
            <div>
                {trail}
                <PageHeader
                    title={t('vault.secrets.title')}
                    description={t('vault.secrets.lead')}
                    actions={actions}
                />
            </div>

            <Notice tone="info">{t('vault.secrets.sealed')}</Notice>

            {secrets.isPending ? (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            ) : rows.length === 0 ? (
                <Notice tone="info">{t('vault.secrets.none')}</Notice>
            ) : (
                <section className="overflow-hidden rounded-md border border-line">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                <th className="px-4 py-2 font-medium">{t('vault.secrets.name')}</th>
                                <th className="px-4 py-2 font-medium">
                                    {t('vault.secrets.owner')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('vault.secrets.description')}
                                </th>
                                <th className="px-4 py-2 text-right font-medium">
                                    {t('vault.secrets.keyVersion')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('vault.secrets.written')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {rows.map((row: VaultSecret) => (
                                <tr
                                    key={row.id}
                                    tabIndex={0}
                                    onClick={() => navigate(secretPath(row.name))}
                                    onKeyDown={(event) => {
                                        if (event.key === 'Enter' || event.key === ' ') {
                                            navigate(secretPath(row.name));
                                        }
                                    }}
                                    className="cursor-pointer border-b border-line-subtle last:border-b-0 hover:bg-surface-hover"
                                >
                                    <td className="px-4 py-2">
                                        <Link
                                            to={secretPath(row.name)}
                                            className="font-mono text-xs underline decoration-line-strong underline-offset-2 hover:decoration-accent"
                                            onClick={(event) => event.stopPropagation()}
                                        >
                                            {row.name}
                                        </Link>
                                    </td>
                                    <td className="px-4 py-2">
                                        <span className="font-mono text-xs">{row.owner}</span>
                                    </td>
                                    <td className="px-4 py-2 text-xs text-ink-dim">
                                        {row.description}
                                    </td>
                                    <td className="px-4 py-2 text-right">
                                        <Tag tone="muted">{`key ${row.key_version}`}</Tag>
                                    </td>
                                    <td className="px-4 py-2 text-xs text-ink-dim">
                                        {formatDateTime(row.recorded_at, language)}
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
                        showing={t('vault.secrets.showing', {
                            first: String(offset + 1),
                            last: String(offset + rows.length),
                            total: String(total),
                        })}
                        onMove={setOffset}
                    />
                </section>
            )}

            {writing && (
                <WriteSecretDialog
                    onClose={() => setWriting(false)}
                    onWritten={(name) => {
                        setWriting(false);
                        navigate(secretPath(name));
                    }}
                />
            )}
        </div>
    );
}
