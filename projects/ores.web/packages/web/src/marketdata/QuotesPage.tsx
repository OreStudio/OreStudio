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

import { useQueries, useQuery } from '@tanstack/react-query';
import { useEffect, useRef, useState, type ReactNode } from 'react';
import { quotes, type Quote, type QuoteFeed } from '../api/quotes.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { Field, Input, Notice, PageHeader, Select, Tag, cx } from '../ui/Primitives.js';
import { Pager } from '../ui/Pager.js';
import { LineChart, SERIES_COLOURS } from './LineChart.js';
import {
    ALL_VENDORS,
    allQuotesOf,
    bySeries,
    changeOf,
    feedKey,
    instantOf,
    instrumentTypes,
    isStale,
    merged,
    shownQuotes,
} from './quotesView.js';
import { DATA_CELL, HEAD_CELL } from './readScreen.js';

/**
 * The market as the vendors quote it, from
 * doc/prototypes/market_data/watch-the-vendors-quotes/index.html, variant C, which
 * the product owner signed off: a rail of the sources the session may watch, the
 * quotes of the chosen one updated in place, and every vendor's quote of the
 * picked datum.
 *
 * The server holds the board and the entitlement; the screen polls the board for
 * what arrived after the last read and keeps the latest quote of each datum.
 */

/** A quote's value as the screen shows it: seven significant digits, no grouping. */
const NUMBER = new Intl.NumberFormat('en-GB', { maximumSignificantDigits: 7, useGrouping: false });

function shown(value: number | string): string {
    const n = typeof value === 'number' ? value : Number(value);
    return Number.isFinite(n) ? NUMBER.format(n) : String(value);
}

/** How often the screen asks the board for what arrived. */
const POLL_MS = 1000;

/** The quotes the board has given, polled while the screen is open and not paused. */
function useBoard(paused: boolean): {
    readonly held: ReadonlyMap<string, Quote>;
    readonly failed: boolean;
} {
    const [held, setHeld] = useState<ReadonlyMap<string, Quote>>(new Map());
    const [failed, setFailed] = useState(false);
    const sequence = useRef(0);
    useEffect(() => {
        if (paused) return undefined;
        let live = true;
        const read = async (): Promise<void> => {
            try {
                const board = await quotes.board(sequence.current);
                if (!live) return;
                sequence.current = board.sequence;
                if (board.quotes.length > 0) setHeld((now) => merged(now, board.quotes));
                setFailed(false);
            } catch {
                if (live) setFailed(true);
            }
        };
        void read();
        const timer = setInterval(() => void read(), POLL_MS);
        return () => {
            live = false;
            clearInterval(timer);
        };
    }, [paused]);
    return { held, failed };
}

/** The ages the screen shows keep counting while it is paused, so the clock ticks apart. */
function useNow(): number {
    const [now, setNow] = useState(() => Date.now());
    useEffect(() => {
        const timer = setInterval(() => setNow(Date.now()), POLL_MS);
        return () => clearInterval(timer);
    }, []);
    return now;
}

function age(at: string, now: number): string {
    const seconds = Math.max(0, (now - instantOf(at)) / 1000);
    return seconds < 60 ? `${seconds.toFixed(1)}s` : `${Math.round(seconds / 60)}m`;
}

function time(at: string): string {
    return at.slice(11, 23);
}

function Change({ q }: { readonly q: Quote }): ReactNode {
    const c = changeOf(q);
    if (c === 0) return <span className="text-ink-faint">0</span>;
    return (
        <span
            className={c > 0 ? 'text-up' : 'text-down'}
        >{`${c > 0 ? '+' : ''}${c.toPrecision(3)}`}</span>
    );
}

function Rail({
    feeds,
    source,
    now,
    onPick,
}: {
    readonly feeds: readonly QuoteFeed[];
    readonly source: string;
    readonly now: number;
    readonly onPick: (source: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const item = (key: string, label: ReactNode, aside: string, on: boolean): ReactNode => (
        <button
            key={key}
            type="button"
            onClick={() => onPick(key)}
            className={cx(
                'flex w-full items-center justify-between rounded px-2 py-1.5 text-left text-sm',
                on ? 'bg-accent/15 text-ink' : 'hover:bg-panel-2',
            )}
        >
            <span>{label}</span>
            <span className="text-xs text-ink-faint">{aside}</span>
        </button>
    );
    return (
        <nav
            aria-label={t('marketdata.quotes.sources')}
            className="space-y-0.5 rounded-md border border-line p-2"
        >
            <p className="px-2 py-1 text-xs uppercase tracking-wide text-ink-faint">
                {t('marketdata.quotes.sources')}
            </p>
            {item(
                ALL_VENDORS,
                t('marketdata.quotes.allVendors'),
                String(feeds.length),
                source === ALL_VENDORS,
            )}
            {feeds.map((f) => {
                const stale = isStale(f.last_at, now);
                return item(
                    feedKey(f),
                    <span>
                        <span
                            aria-hidden="true"
                            className={cx(
                                'mr-1.5 inline-block h-2 w-2 rounded-full',
                                stale ? 'bg-warn' : 'bg-up',
                            )}
                        />
                        {f.provider}
                    </span>,
                    f.last_at === null ? '—' : age(f.last_at, now),
                    source === feedKey(f),
                );
            })}
        </nav>
    );
}

function AllQuotesPanel({
    held,
    feeds,
    picked,
    now,
}: {
    readonly held: ReadonlyMap<string, Quote>;
    readonly feeds: readonly QuoteFeed[];
    readonly picked: Quote;
    readonly now: number;
}): ReactNode {
    const { t, plural } = useTranslation();
    const all = allQuotesOf(held, feeds, picked.uri);
    const from = useRef(new Date(Date.now() - 3_600_000).toISOString()).current;
    const histories = useQueries({
        queries: all.quotes.map((q) => ({
            queryKey: ['quote-history', q.provider, picked.uri, from] as const,
            queryFn: () => quotes.history(q.provider, picked.series_uri, picked.uri, from),
            retry: false,
            staleTime: Infinity,
        })),
    });
    const series = all.quotes.map((q, i) => {
        const past = histories[i]?.data?.ticks.map((tick) => tick.value) ?? [];
        return {
            points: [...past, Number(q.value)],
            colour: SERIES_COLOURS[i % SERIES_COLOURS.length] ?? '#58a6ff',
        };
    });
    return (
        <section className="space-y-2 rounded-md border border-line p-4">
            <h2 className="text-xs uppercase tracking-wide text-ink-faint">
                {t('marketdata.quotes.allQuotesOf', { series: picked.series, point: picked.point })}
            </h2>
            <table className="w-full text-sm">
                <thead>
                    <tr>
                        <th className={cx(HEAD_CELL, 'text-left')}>
                            {t('marketdata.quotes.vendor')}
                        </th>
                        <th className={HEAD_CELL}>{t('marketdata.quotes.value')}</th>
                        <th className={HEAD_CELL}>{t('marketdata.quotes.toMid')}</th>
                        <th className={HEAD_CELL}>{t('marketdata.quotes.change')}</th>
                        <th className={HEAD_CELL}>{t('marketdata.quotes.age')}</th>
                    </tr>
                </thead>
                <tbody>
                    {all.quotes.map((q, i) => (
                        <tr key={feedKey(q)} className={isStale(q.at, now) ? 'text-ink-faint' : ''}>
                            <td className="px-2 py-1">
                                <span
                                    aria-hidden="true"
                                    className="mr-2 inline-block h-0.5 w-3 align-middle"
                                    style={{ background: series[i]?.colour }}
                                />
                                {q.provider} <span className="text-ink-faint">{q.source}</span>
                            </td>
                            <td className={DATA_CELL}>{shown(q.value)}</td>
                            <td className={DATA_CELL}>
                                {all.mid === null ? '—' : shown(Number(q.value) - all.mid)}
                            </td>
                            <td className={DATA_CELL}>
                                <Change q={q} />
                            </td>
                            <td className={DATA_CELL}>{age(q.at, now)}</td>
                        </tr>
                    ))}
                    <tr className="border-t border-line">
                        <td className="px-2 py-1">
                            {plural('marketdata.quotes.midOf', all.quotes.length)}
                        </td>
                        <td className={DATA_CELL}>{all.mid === null ? '—' : shown(all.mid)}</td>
                        <td className={DATA_CELL}>
                            {all.spread === null
                                ? ''
                                : t('marketdata.quotes.spread', {
                                      spread: shown(all.spread),
                                  })}
                        </td>
                        <td colSpan={2} />
                    </tr>
                </tbody>
            </table>
            <LineChart
                series={series}
                format={(v) => shown(v)}
                label={t('marketdata.quotes.lastHour', {
                    series: picked.series,
                    point: picked.point,
                })}
            />
        </section>
    );
}

export function QuotesPage(): ReactNode {
    const { t, plural } = useTranslation();
    const [paused, setPaused] = useState(false);
    const [source, setSource] = useState(ALL_VENDORS);
    const [assetClass, setAssetClass] = useState('');
    const [instrumentType, setInstrumentType] = useState('');
    const [find, setFind] = useState('');
    const [layout, setLayout] = useState<'datum' | 'series'>('datum');
    const [open, setOpen] = useState<ReadonlySet<string>>(new Set());
    const [pickedUri, setPickedUri] = useState('');
    const [offset, setOffset] = useState(0);
    const [pageSize, setPageSize] = useState(50);
    // A new choice of what to show starts again at its first page.
    useEffect(() => {
        setOffset(0);
    }, [source, assetClass, instrumentType, find, layout]);
    const now = useNow();
    const feedsQuery = useQuery({
        queryKey: ['quote-feeds'],
        queryFn: () => quotes.feeds(),
        refetchInterval: 5000,
        retry: false,
    });
    const classesQuery = useQuery({
        queryKey: ['asset-class-codes'],
        queryFn: () => quotes.assetClasses(),
        staleTime: Infinity,
        retry: false,
    });
    const { held, failed } = useBoard(paused);
    const feeds = feedsQuery.data ?? [];
    const classes = classesQuery.data ?? [];
    const nameOf = (code: string): string => classes.find((c) => c.code === code)?.name ?? code;
    const one = source !== ALL_VENDORS;
    const rows = shownQuotes(held, feeds, { source, assetClass, instrumentType, find });
    const groups = layout === 'series' ? bySeries(rows) : [];
    const total = layout === 'datum' ? rows.length : groups.length;
    const picked = [...held.values()].find((q) => q.uri === pickedUri);

    const quoteRow = (q: Quote, nested = false): ReactNode => (
        <tr
            key={`${feedKey(q)}|${q.uri}`}
            onClick={() => setPickedUri(q.uri)}
            className={cx(
                'cursor-pointer border-t border-line/50 hover:bg-panel-2',
                q.uri === pickedUri && 'bg-panel-2',
                isStale(q.at, now) && 'text-ink-faint',
            )}
        >
            <td className="px-2 py-1">{nameOf(q.asset_class)}</td>
            <td className="px-2 py-1">{q.instrument_type}</td>
            <td className={cx('px-2 py-1', nested && 'pl-6')}>{q.series}</td>
            <td className="px-2 py-1">{q.point === '' ? '—' : q.point}</td>
            {!one && <td className="px-2 py-1">{q.provider}</td>}
            <td className={DATA_CELL}>{shown(q.value)}</td>
            <td className={DATA_CELL}>
                <Change q={q} />
            </td>
            <td className={DATA_CELL}>{time(q.at)}</td>
            <td className={DATA_CELL}>{age(q.at, now)}</td>
        </tr>
    );

    const body = (): ReactNode => {
        if (rows.length === 0)
            return (
                <tr>
                    <td colSpan={one ? 8 : 9} className="px-2 py-3 text-ink-faint">
                        {t('marketdata.quotes.noQuotes')}
                    </td>
                </tr>
            );
        if (layout === 'datum')
            return rows.slice(offset, offset + pageSize).map((q) => quoteRow(q));
        return groups.slice(offset, offset + pageSize).map((group) => {
            const points = new Set(group.quotes.map((q) => q.uri)).size;
            if (points === 1) return group.quotes.map((q) => quoteRow(q));
            const isOpen = open.has(group.key);
            const first = group.quotes[0];
            if (first === undefined) return null;
            return [
                <tr
                    key={group.key}
                    onClick={() => {
                        const next = new Set(open);
                        if (isOpen) next.delete(group.key);
                        else next.add(group.key);
                        setOpen(next);
                    }}
                    className="cursor-pointer border-t border-line/50 hover:bg-panel-2"
                >
                    <td className="px-2 py-1">{nameOf(first.asset_class)}</td>
                    <td className="px-2 py-1">{first.instrument_type}</td>
                    <td className="px-2 py-1">{`${isOpen ? '▾' : '▸'} ${first.series}`}</td>
                    <td className="px-2 py-1 text-ink-faint">
                        {plural('marketdata.quotes.points', points)}
                    </td>
                    {!one && <td />}
                    <td
                        className={cx(DATA_CELL, 'text-ink-faint')}
                    >{`${group.newest.point}: ${shown(group.newest.value)}`}</td>
                    <td className={DATA_CELL}>
                        <Change q={group.newest} />
                    </td>
                    <td className={DATA_CELL}>{time(group.newest.at)}</td>
                    <td className={DATA_CELL}>{age(group.newest.at, now)}</td>
                </tr>,
                ...(isOpen ? group.quotes.map((q) => quoteRow(q, true)) : []),
            ];
        });
    };

    return (
        <div className="space-y-4">
            <AreaTrail area="marketdata" screen={t('marketdata.quotes.title')} />
            <div className="flex items-start justify-between gap-3">
                <PageHeader
                    title={t('marketdata.quotes.title')}
                    description={t('marketdata.quotes.lead')}
                />
                <div className="flex items-center gap-2">
                    <Tag tone={paused ? 'warn' : 'up'}>
                        {paused ? t('marketdata.quotes.paused') : t('marketdata.quotes.live')}
                    </Tag>
                    <button
                        type="button"
                        className="rounded border border-line px-3 py-1 text-sm"
                        onClick={() => setPaused(!paused)}
                    >
                        {paused ? t('marketdata.quotes.resume') : t('marketdata.quotes.pause')}
                    </button>
                </div>
            </div>
            {failed && <Notice tone="warn">{t('marketdata.quotes.unreachable')}</Notice>}
            {feedsQuery.isSuccess && feeds.length === 0 ? (
                <Notice tone="info">{t('marketdata.quotes.noFeeds')}</Notice>
            ) : (
                <div className="grid grid-cols-1 gap-4 md:grid-cols-[13rem_1fr]">
                    <Rail feeds={feeds} source={source} now={now} onPick={setSource} />
                    <div className="space-y-4">
                        <section className="flex flex-wrap items-end gap-4 rounded-md border border-line p-4">
                            <Field label={t('marketdata.quotes.assetClass')}>
                                <Select
                                    value={assetClass}
                                    onChange={(event) => {
                                        setAssetClass(event.target.value);
                                        setInstrumentType('');
                                    }}
                                >
                                    <option value="">{t('marketdata.quotes.all')}</option>
                                    {classes.map((c) => (
                                        <option key={c.code} value={c.code}>
                                            {c.name}
                                        </option>
                                    ))}
                                </Select>
                            </Field>
                            <Field label={t('marketdata.quotes.instrumentType')}>
                                <Select
                                    value={instrumentType}
                                    onChange={(event) => setInstrumentType(event.target.value)}
                                >
                                    <option value="">{t('marketdata.quotes.all')}</option>
                                    {instrumentTypes(held, assetClass).map((type) => (
                                        <option key={type} value={type}>
                                            {type}
                                        </option>
                                    ))}
                                </Select>
                            </Field>
                            <Field label={t('marketdata.quotes.find')}>
                                <Input
                                    value={find}
                                    placeholder={t('marketdata.quotes.findHint')}
                                    onChange={(event) => setFind(event.target.value)}
                                />
                            </Field>
                            <Field label={t('marketdata.quotes.layout')}>
                                <div className="inline-flex overflow-hidden rounded border border-line">
                                    {(['datum', 'series'] as const).map((l) => (
                                        <button
                                            key={l}
                                            type="button"
                                            aria-pressed={layout === l}
                                            onClick={() => setLayout(l)}
                                            className={cx(
                                                'px-3 py-1 text-sm',
                                                layout === l && 'bg-accent/15',
                                            )}
                                        >
                                            {l === 'datum'
                                                ? t('marketdata.quotes.everyDatum')
                                                : t('marketdata.quotes.bySeries')}
                                        </button>
                                    ))}
                                </div>
                            </Field>
                        </section>
                        {picked === undefined ? (
                            <p className="text-sm text-ink-faint">
                                {t('marketdata.quotes.pickRow')}
                            </p>
                        ) : (
                            <AllQuotesPanel held={held} feeds={feeds} picked={picked} now={now} />
                        )}
                        <section className="overflow-x-auto rounded-md border border-line p-2">
                            <table className="w-full text-sm">
                                <thead>
                                    <tr>
                                        <th className={cx(HEAD_CELL, 'text-left')}>
                                            {t('marketdata.quotes.assetClass')}
                                        </th>
                                        <th className={cx(HEAD_CELL, 'text-left')}>
                                            {t('marketdata.quotes.type')}
                                        </th>
                                        <th className={cx(HEAD_CELL, 'text-left')}>
                                            {t('marketdata.quotes.series')}
                                        </th>
                                        <th className={cx(HEAD_CELL, 'text-left')}>
                                            {t('marketdata.quotes.point')}
                                        </th>
                                        {!one && (
                                            <th className={cx(HEAD_CELL, 'text-left')}>
                                                {t('marketdata.quotes.vendor')}
                                            </th>
                                        )}
                                        <th className={HEAD_CELL}>
                                            {t('marketdata.quotes.value')}
                                        </th>
                                        <th className={HEAD_CELL}>
                                            {t('marketdata.quotes.change')}
                                        </th>
                                        <th className={HEAD_CELL}>{t('marketdata.quotes.time')}</th>
                                        <th className={HEAD_CELL}>{t('marketdata.quotes.age')}</th>
                                    </tr>
                                </thead>
                                <tbody>{body()}</tbody>
                            </table>
                            {total > 0 && (
                                <Pager
                                    offset={offset}
                                    shown={Math.min(pageSize, total - offset)}
                                    total={total}
                                    pageSize={pageSize}
                                    showing={t('marketdata.quotes.showing', {
                                        from: offset + 1,
                                        to: Math.min(offset + pageSize, total),
                                        total,
                                    })}
                                    onMove={setOffset}
                                    onPageSize={(size) => {
                                        setPageSize(size);
                                        setOffset(0);
                                    }}
                                />
                            )}
                        </section>
                    </div>
                </div>
            )}
        </div>
    );
}
