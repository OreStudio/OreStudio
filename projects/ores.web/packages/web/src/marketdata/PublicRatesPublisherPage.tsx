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

/** One public publisher on its own screen: its terms and the rates it publishes. */

import { useParams } from 'react-router';
import { useQuery } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { PATHS } from '../shell/paths.js';
import { LinkButton, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { ProviderDetail } from './PublicRatesDetail.js';

export function PublicRatesPublisherPage(): ReactNode {
    const { t } = useTranslation();
    const { publisher = '' } = useParams();
    const providers = useQuery({ queryKey: ['public-rates'], queryFn: api.publicRates });
    const coverage = useQuery({
        queryKey: ['public-rates-coverage'],
        queryFn: api.publicRatesCoverage,
    });
    const provider = providers.data?.find((candidate) => candidate.code === publisher);
    const last =
        provider?.series
            .map((series) => coverage.data?.[series.id]?.last ?? '')
            .filter((date) => date !== '')
            .sort()
            .at(-1) ?? '';
    const fresh =
        last !== '' && last >= new Date(Date.now() - 86_400_000).toISOString().slice(0, 10);
    const trail = [
        { label: t('marketdata.publicRates.title'), to: PATHS.publicRates },
        { label: t('marketdata.publicRates.detail.crumb') },
    ];
    return (
        <div className="space-y-6">
            <div>
                <AreaTrail area="marketdata" steps={trail} screen={publisher} />
                <PageHeader
                    title={provider?.name ?? publisher}
                    description={t('marketdata.publicRates.detail.lead')}
                    actions={
                        <LinkButton to={PATHS.publicRates} variant="secondary">
                            {t('marketdata.publicRates.detail.back')}
                        </LinkButton>
                    }
                />
            </div>
            {providers.isError ? (
                <Notice tone="error">{providers.error.message}</Notice>
            ) : providers.isPending ? (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            ) : provider === undefined ? (
                <Notice tone="warn">
                    {t('marketdata.publicRates.detail.missing', { publisher })}
                </Notice>
            ) : (
                <>
                    <section className="card space-y-2 p-6">
                        <div className="flex flex-wrap items-center gap-2">
                            <Tag tone={provider.terms_accepted ? 'up' : 'warn'} small>
                                {provider.terms_accepted
                                    ? t('marketdata.publicRates.terms.accepted')
                                    : t('marketdata.publicRates.terms.notAccepted')}
                            </Tag>
                            <span className="text-xs text-ink-faint">
                                {provider.kind} · {provider.terms_credit}
                            </span>
                        </div>
                        <a
                            href={provider.terms_url}
                            target="_blank"
                            rel="noreferrer"
                            className="text-xs text-accent hover:underline"
                        >
                            {t('marketdata.publicRates.terms.read')}
                        </a>
                    </section>
                    <section className="card space-y-3 p-6">
                        <h2 className="text-sm font-medium">
                            {t('marketdata.publicRates.reads.title')}
                        </h2>
                        <p className="text-xs text-ink-faint">
                            {t('marketdata.publicRates.reads.lead')}
                        </p>
                        <table className="w-full text-sm">
                            <thead className="text-left text-xs text-ink-faint">
                                <tr>
                                    <th className="py-1 font-normal">
                                        {t('marketdata.publicRates.columns.class')}
                                    </th>
                                    <th className="py-1 font-normal">
                                        {t('marketdata.publicRates.columns.feed')}
                                    </th>
                                    <th className="py-1 font-normal">
                                        {t('marketdata.publicRates.columns.supplier')}
                                    </th>
                                    <th className="py-1 font-normal">
                                        {t('marketdata.publicRates.columns.service')}
                                    </th>
                                    <th className="py-1 font-normal">
                                        {t('marketdata.publicRates.columns.session')}
                                    </th>
                                    <th className="py-1 font-normal">
                                        {t('marketdata.publicRates.columns.last')}
                                    </th>
                                    <th className="py-1 font-normal">
                                        {t('marketdata.publicRates.columns.freshness')}
                                    </th>
                                </tr>
                            </thead>
                            <tbody>
                                {provider.feeds.map((feed) => (
                                    <tr key={feed.id} className="border-t border-line-subtle">
                                        <td className="py-1 font-mono text-xs">
                                            {feed.asset_classes.join(', ') || '—'}
                                        </td>
                                        <td className="py-1 font-mono text-xs">{feed.name}</td>
                                        <td className="py-1 text-xs">{feed.supplier_name}</td>
                                        <td className="py-1 text-xs">{feed.service}</td>
                                        <td className="py-1 font-mono text-xs">
                                            {feed.session_code}
                                        </td>
                                        <td className="py-1 font-mono text-xs">{last || '—'}</td>
                                        <td className="py-1">
                                            <Tag
                                                tone={last === '' ? 'muted' : fresh ? 'up' : 'warn'}
                                                small
                                            >
                                                {last === ''
                                                    ? t('marketdata.publicRates.freshness.no-data')
                                                    : fresh
                                                      ? t('marketdata.publicRates.freshness.fresh')
                                                      : t('marketdata.publicRates.freshness.late')}
                                            </Tag>
                                        </td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    </section>
                    <ProviderDetail provider={provider} />
                </>
            )}
        </div>
    );
}
