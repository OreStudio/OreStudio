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
import type { ReactNode } from 'react';
import { Link, useSearchParams } from 'react-router';
import { api, type MarketInstrument } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { pairPath } from '../refdata/currencyPairs.js';
import { Fact, Field, Input, Notice, PageHeader, Select, Tag, cx } from '../ui/Primitives.js';
import { relativeTime } from '../ui/Time.js';

/**
 * Every instrument the party holds, as one list, from
 * doc/prototypes/market_data/find-an-instrument/index.html.
 *
 * The catalogue is the server's grouping: a curve is one row even though it has
 * a pillar per tenor, a ladder is one row across its term axis, and a surface is
 * one row with two axes. The search, the kind filter and the freshness verdict
 * are the server's as well, so this screen states what came back and derives
 * nothing.
 */

const KINDS = ['curve', 'ladder', 'surface', 'pair', 'fixing'] as const;

function freshness(seconds: number, language: string): string {
    const now = new Date();
    return relativeTime(new Date(now.getTime() - seconds * 1000), now, language);
}

function valuesOf(instrument: MarketInstrument): string {
    const values = instrument.axes.flatMap((axis) => axis.values);
    return values.length === 0 ? '—' : values.join(', ');
}

function sourcesOf(instrument: MarketInstrument): string {
    const codes = [...new Set(instrument.sources.map((source) => source.provider_code))];
    return codes.length === 0 ? '—' : codes.join(', ');
}

/** The page a row opens, for the kinds the product has a page for. */
function destination(instrument: MarketInstrument): string | undefined {
    return instrument.kind === 'pair' && instrument.name.includes('/')
        ? pairPath(instrument.name)
        : undefined;
}

function Row({ instrument }: { readonly instrument: MarketInstrument }): ReactNode {
    const { t, language } = useTranslation();
    const to = destination(instrument);
    const newest = [...instrument.sources].sort((a, b) => a.age_seconds - b.age_seconds)[0];
    return (
        <tr className="border-b border-line-subtle">
            <td className="px-2 py-1">
                <Tag tone="accent" small>
                    {t(`marketdata.instruments.kinds.${instrument.kind}`)}
                </Tag>
            </td>
            <td className="whitespace-nowrap px-2 py-1 font-semibold">
                {to === undefined ? (
                    instrument.name
                ) : (
                    <Link className="text-accent" to={to}>
                        {instrument.name}
                    </Link>
                )}
            </td>
            <td className="px-2 py-1 text-ink-muted">{sourcesOf(instrument)}</td>
            <td className="px-2 py-1 font-mono text-xs">{valuesOf(instrument)}</td>
            <td className={cx('px-2 py-1 text-right', instrument.stale && 'text-danger')}>
                {newest === undefined ? '—' : freshness(newest.age_seconds, language)}
            </td>
        </tr>
    );
}

export function InstrumentFinderPage(): ReactNode {
    const { t } = useTranslation();
    const [search, setSearch] = useSearchParams();
    const set = (values: Readonly<Record<string, string>>): void => {
        const next = new URLSearchParams(search);
        Object.entries(values).forEach(([key, value]) => {
            next.set(key, value);
        });
        setSearch(next, { replace: true });
    };

    const query = search.get('q') ?? '';
    const kind = search.get('kind') ?? '';
    const provider = search.get('provider') ?? '';
    const key = ['market-instruments', query, kind, provider] as const;

    const catalogue = useQuery({
        queryKey: key,
        queryFn: () =>
            api.marketInstruments({
                query,
                kinds: kind === '' ? [] : [kind],
                providerCode: provider,
            }),
        retry: false,
    });
    const instruments = catalogue.data?.instruments ?? [];
    const providers = [
        ...new Set(instruments.flatMap((row) => row.sources.map((s) => s.provider_code))),
    ];

    return (
        <div className="space-y-4">
            <AreaTrail area="marketdata" screen={t('marketdata.instruments.title')} />
            <PageHeader
                title={t('marketdata.instruments.title')}
                description={t('marketdata.instruments.lead')}
            />
            <section className="flex flex-wrap items-end gap-4 rounded-md border border-line p-4">
                <Field label={t('marketdata.instruments.search')}>
                    <Input
                        value={query}
                        placeholder={t('marketdata.instruments.searchHint')}
                        onChange={(event) => {
                            set({ q: event.target.value });
                        }}
                    />
                </Field>
                <Field label={t('marketdata.instruments.kind')}>
                    <Select
                        value={kind}
                        onChange={(event) => {
                            set({ kind: event.target.value });
                        }}
                    >
                        <option value="">{t('marketdata.instruments.all')}</option>
                        {KINDS.map((option) => (
                            <option key={option} value={option}>
                                {t(`marketdata.instruments.kinds.${option}`)}
                            </option>
                        ))}
                    </Select>
                </Field>
                <Field label={t('marketdata.instruments.provider')}>
                    <Select
                        value={provider}
                        onChange={(event) => {
                            set({ provider: event.target.value });
                        }}
                    >
                        <option value="">{t('marketdata.instruments.all')}</option>
                        {providers.map((option) => (
                            <option key={option}>{option}</option>
                        ))}
                    </Select>
                </Field>
                {catalogue.data !== undefined && (
                    <Fact label={t('marketdata.instruments.found')}>
                        {catalogue.data.truncated
                            ? t('marketdata.instruments.pageOf', {
                                  shown: instruments.length,
                                  total: catalogue.data.total,
                              })
                            : t('marketdata.instruments.count', {
                                  total: catalogue.data.total,
                              })}
                    </Fact>
                )}
            </section>
            {catalogue.isError && <Notice tone="error">{catalogue.error.message}</Notice>}
            {catalogue.isSuccess && instruments.length === 0 && (
                <Notice tone="info">{t('marketdata.instruments.noMatches')}</Notice>
            )}
            {instruments.length > 0 && (
                <section className="space-y-2 rounded-md border border-line p-4">
                    <div className="overflow-x-auto">
                        <table className="w-full text-sm">
                            <thead>
                                <tr className="border-b border-line">
                                    <th className="px-2 py-1 text-left text-xs font-semibold text-ink-faint">
                                        {t('marketdata.instruments.columns.kind')}
                                    </th>
                                    <th className="px-2 py-1 text-left text-xs font-semibold text-ink-faint">
                                        {t('marketdata.instruments.columns.instrument')}
                                    </th>
                                    <th className="px-2 py-1 text-left text-xs font-semibold text-ink-faint">
                                        {t('marketdata.instruments.columns.source')}
                                    </th>
                                    <th className="px-2 py-1 text-left text-xs font-semibold text-ink-faint">
                                        {t('marketdata.instruments.columns.values')}
                                    </th>
                                    <th className="px-2 py-1 text-right text-xs font-semibold text-ink-faint">
                                        {t('marketdata.instruments.columns.freshness')}
                                    </th>
                                </tr>
                            </thead>
                            <tbody>
                                {instruments.map((instrument) => (
                                    <Row key={instrument.instrument_id} instrument={instrument} />
                                ))}
                            </tbody>
                        </table>
                    </div>
                </section>
            )}
            <Notice tone="info">{t('marketdata.instruments.whatOpens')}</Notice>
        </div>
    );
}
