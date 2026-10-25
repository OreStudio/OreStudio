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
import { Link, useParams } from 'react-router';
import { counterparties } from '../api/counterparties.js';
import { useTranslation } from '../i18n/Provider.js';
import { Crumbs } from './shared.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { useAreaParts } from '../shell/AreaTrail.js';
import { PATHS } from '../shell/paths.js';
import { Fact, Notice, Tag } from '../ui/Primitives.js';

/** The things a counterparty is called, in the order a reader looks for them. */
const IDENTIFIER_COLUMNS = ['id_scheme', 'id_value'] as const;

/**
 * One counterparty: who it is, how it stands, and the identifiers it is known
 * by. A counterparty has no page of its own until now, so this is also where a
 * trade's counterparty link lands.
 */
export function CounterpartyPage(): ReactNode {
    const { t } = useTranslation();
    const { id = '' } = useParams();
    const open = useQuery({
        queryKey: ['counterparties', id],
        queryFn: () => counterparties.one(id),
        enabled: id !== '',
    });
    const trail = useAreaParts('refdata', t('refdata.counterparties.title'));

    if (open.isPending) {
        return (
            <ScreenFrame
                trail={<Crumbs parts={trail} />}
                title={t('refdata.counterparties.title')}
                description={t('refdata.counterparties.lead')}
            >
                <p className="text-sm text-ink-muted" role="status">
                    {t('common.loading')}
                </p>
            </ScreenFrame>
        );
    }

    if (open.isError) {
        return (
            <ScreenFrame
                trail={<Crumbs parts={[...trail, { label: id }]} />}
                title={t('refdata.counterparties.title')}
                description={t('refdata.counterparties.lead')}
            >
                <div className="space-y-3">
                    <Notice tone="info">{t('refdata.counterparties.notFound')}</Notice>
                    <Link
                        to={PATHS.refdata}
                        className="text-sm text-accent hover:text-accent-bright"
                    >
                        {t('refdata.counterparties.backToRefdata')}
                    </Link>
                </div>
            </ScreenFrame>
        );
    }

    const { counterparty, identifiers } = open.data;
    const name = counterparty.full_name;

    return (
        <ScreenFrame
            trail={<Crumbs parts={[...trail, { label: name }]} />}
            title={name}
            description={counterparty.short_code}
        >
            <div className="space-y-6">
                <dl className="grid grid-cols-2 gap-4 md:grid-cols-4">
                    <Fact
                        label={t('refdata.counterparties.shortCode')}
                        value={counterparty.short_code}
                    />
                    <Fact
                        label={t('refdata.counterparties.fullName')}
                        value={counterparty.full_name}
                    />
                    <Fact
                        label={t('refdata.counterparties.partyType')}
                        value={counterparty.party_type}
                    />
                    <Fact label={t('refdata.counterparties.status')}>
                        <Tag tone={counterparty.status === 'Active' ? 'up' : 'muted'}>
                            {counterparty.status}
                        </Tag>
                    </Fact>
                </dl>
                <section className="overflow-hidden rounded-md border border-line">
                    <h2 className="border-b border-line bg-surface-2 px-3 py-2 text-sm font-medium text-ink">
                        {t('refdata.counterparties.identifiers')}
                    </h2>
                    {identifiers.length === 0 ? (
                        <p className="px-3 py-3 text-sm text-ink-muted">
                            {t('refdata.counterparties.noIdentifiers')}
                        </p>
                    ) : (
                        <table className="w-full text-left text-sm">
                            <thead>
                                <tr className="border-b border-line text-xs uppercase tracking-wide text-ink-faint">
                                    {IDENTIFIER_COLUMNS.map((column) => (
                                        <th key={column} className="px-3 py-2 font-medium">
                                            {t(`refdata.counterparties.${column}`)}
                                        </th>
                                    ))}
                                </tr>
                            </thead>
                            <tbody>
                                {identifiers.map((entry, index) => (
                                    <tr key={index} className="border-b border-line last:border-0">
                                        {IDENTIFIER_COLUMNS.map((column) => (
                                            <td key={column} className="px-3 py-2 text-ink">
                                                {String(entry[column] ?? '—')}
                                            </td>
                                        ))}
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    )}
                </section>
            </div>
        </ScreenFrame>
    );
}
