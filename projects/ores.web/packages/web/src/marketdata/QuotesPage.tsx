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
import { useEffect, useRef, useState, type ReactNode } from 'react';
import { quotes, type Quote } from '../api/quotes.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { Field, Input, Notice, PageHeader, Tag, cx } from '../ui/Primitives.js';
import { useTabs } from '../ui/Tabs.js';
import { SERIES_COLOURS } from './LineChart.js';
import {
    COMPARE,
    CurveBoard,
    DatumBoard,
    MID,
    QuoteBoard,
    SurfaceBoard,
    type Comparison,
    type Watch,
} from './QuoteBoards.js';
import {
    boardsOf,
    CONSENSUS,
    feedKey,
    instantOf,
    isStale,
    merged,
    shownSeries,
    vendorsOf,
    type Board,
    type Order,
    type Vendor,
} from './quotesView.js';

/**
 * The market as the vendors quote it, from
 * doc/prototypes/market_data/read-the-quotes-by-series/, variant C, which the
 * product owner chose: the sources on a rail, each one included or left out, and
 * a board for each asset class and shape. A vendor is a column, never a row.
 *
 * The server holds the board and the entitlement; the screen polls the board for
 * what arrived after the last read and keeps the latest quote of each datum.
 */

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
    return seconds < 60 ? `${seconds.toFixed(0)}s` : `${Math.round(seconds / 60)}m`;
}

/** The sources, in the order they are trusted, each one ticked to include it. */
function Rail({
    vendors,
    left,
    now,
    colourOf,
    onToggle,
}: {
    readonly vendors: readonly Vendor[];
    readonly left: ReadonlySet<string>;
    readonly now: number;
    readonly colourOf: (key: string) => string;
    readonly onToggle: (key: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <nav
            aria-label={t('marketdata.quotes.sources')}
            className="space-y-0.5 rounded-md border border-line p-2"
        >
            <p className="px-2 py-1 text-xs uppercase tracking-wide text-ink-faint">
                {t('marketdata.quotes.sources')}
            </p>
            {vendors.map((v) => {
                const stale = isStale(v.lastAt, now);
                return (
                    <label
                        key={v.key}
                        className="flex cursor-pointer items-center justify-between gap-2 rounded px-2 py-1.5 text-sm hover:bg-panel-2"
                    >
                        <span className="flex items-center gap-2">
                            <input
                                type="checkbox"
                                checked={!left.has(v.key)}
                                onChange={() => onToggle(v.key)}
                            />
                            <span
                                aria-hidden="true"
                                className="inline-block h-2 w-2 rounded-full"
                                style={{ background: colourOf(v.key) }}
                            />
                            {v.provider}
                        </span>
                        <span className={cx('text-xs', stale ? 'text-warn' : 'text-ink-faint')}>
                            {v.lastAt === null ? '—' : age(v.lastAt, now)}
                        </span>
                    </label>
                );
            })}
            <p className="px-2 pt-1 text-xs text-ink-faint">{t('marketdata.quotes.sourcesHint')}</p>
        </nav>
    );
}

/**
 * The vendors with the newest tick the screen holds of each, which is ahead of
 * the feeds read: that is asked every few seconds, and the board every second.
 */
function withLastTicks(vendors: readonly Vendor[], held: ReadonlyMap<string, Quote>): Vendor[] {
    const newest = new Map<string, string>();
    for (const q of held.values()) {
        const key = feedKey(q);
        const seen = newest.get(key);
        if (seen === undefined || instantOf(q.at) > instantOf(seen)) newest.set(key, q.at);
    }
    return vendors.map((v) => {
        const at = newest.get(v.key);
        return at !== undefined && (v.lastAt === null || instantOf(at) > instantOf(v.lastAt))
            ? { ...v, lastAt: at }
            : v;
    });
}

export function QuotesPage(): ReactNode {
    const { t } = useTranslation();
    const [paused, setPaused] = useState(false);
    const [left, setLeft] = useState<ReadonlySet<string>>(new Set());
    const [find, setFind] = useState('');
    const [order, setOrder] = useState<Order>('live');
    const [picked, setPicked] = useState<Readonly<Record<string, string>>>({});
    const [curveSource, setCurveSource] = useState(CONSENSUS);
    const [surfaceMode, setSurfaceMode] = useState(MID);
    const [point, setPoint] = useState('');
    const [comparison, setComparison] = useState<Comparison>({
        a: '',
        b: CONSENSUS,
        row: '',
        column: '',
    });
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
    const vendors = withLastTicks(vendorsOf(feedsQuery.data ?? []), held);
    const colourOf = (key: string): string => {
        const i = vendors.findIndex((v) => v.key === key);
        return SERIES_COLOURS[Math.max(0, i) % SERIES_COLOURS.length] ?? 'var(--color-accent)';
    };
    const shown = new Set(vendors.filter((v) => !left.has(v.key)).map((v) => v.key));
    const watch: Watch = { vendors, shown, now, colourOf };
    const boards = boardsOf(held, vendors);
    const className = (code: string): string =>
        (classesQuery.data ?? []).find((c) => c.code === code)?.name ?? code;
    const titleOf = (b: Board): string =>
        t(`marketdata.quotes.board.${b.shape}`, { class: className(b.assetClass) });
    const listed = new Map(boards.map((b) => [b.key, shownSeries(b, shown, find, order, now)]));
    const { tab, bar } = useTabs({
        label: t('marketdata.quotes.boards'),
        tabs: boards.map((b) => b.key),
        titleOf: (key) => {
            const b = boards.find((x) => x.key === key);
            return b === undefined ? key : titleOf(b);
        },
        renderTitle: (key) => {
            const b = boards.find((x) => x.key === key);
            return (
                <>
                    {b === undefined ? key : titleOf(b)}
                    <span className="ml-1.5 text-xs text-ink-faint">
                        {listed.get(key)?.length ?? 0}
                    </span>
                </>
            );
        },
    });
    const board = boards.find((b) => b.key === tab);
    const series = listed.get(tab) ?? [];
    const pick = (key: string): void => {
        setPicked({ ...picked, [tab]: key });
        setPoint('');
    };

    const view = (): ReactNode => {
        if (board === undefined)
            return <p className="text-sm text-ink-faint">{t('marketdata.quotes.waiting')}</p>;
        if (series.length === 0)
            return <p className="text-sm text-ink-faint">{t('marketdata.quotes.noSeries')}</p>;
        const chosen = picked[tab] ?? '';
        if (board.shape === 'quote')
            return <QuoteBoard series={series} watch={watch} picked={chosen} onPick={pick} />;
        if (board.shape === 'curve')
            return (
                <CurveBoard
                    series={series}
                    watch={watch}
                    picked={chosen}
                    onPick={pick}
                    source={curveSource}
                    onSource={setCurveSource}
                />
            );
        if (board.shape === 'surface')
            return (
                <SurfaceBoard
                    series={series}
                    watch={watch}
                    picked={chosen}
                    onPick={pick}
                    mode={surfaceMode}
                    onMode={(m) => {
                        setSurfaceMode(m);
                        if (m === COMPARE) setPoint('');
                    }}
                    point={point}
                    onPoint={setPoint}
                    comparison={comparison}
                    onCompare={setComparison}
                />
            );
        return <DatumBoard series={series} watch={watch} />;
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
            {feedsQuery.isSuccess && vendors.length === 0 ? (
                <Notice tone="info">{t('marketdata.quotes.noFeeds')}</Notice>
            ) : (
                <div className="grid grid-cols-1 gap-4 md:grid-cols-[13rem_minmax(0,1fr)]">
                    <Rail
                        vendors={vendors}
                        left={left}
                        now={now}
                        colourOf={colourOf}
                        onToggle={(key) => {
                            const next = new Set(left);
                            if (next.has(key)) next.delete(key);
                            else next.add(key);
                            setLeft(next);
                        }}
                    />
                    <div className="min-w-0 space-y-4">
                        <section className="flex flex-wrap items-end gap-4 rounded-md border border-line p-4">
                            <Field label={t('marketdata.quotes.find')}>
                                <Input
                                    value={find}
                                    placeholder={t('marketdata.quotes.findHint')}
                                    onChange={(event) => setFind(event.target.value)}
                                />
                            </Field>
                            <div>
                                <span className="mb-1.5 block text-sm font-medium text-ink-muted">
                                    {t('marketdata.quotes.order')}
                                </span>
                                <div
                                    role="group"
                                    aria-label={t('marketdata.quotes.order')}
                                    className="inline-flex overflow-hidden rounded border border-line"
                                >
                                    {(['live', 'name'] as const).map((o) => (
                                        <button
                                            key={o}
                                            type="button"
                                            aria-pressed={order === o}
                                            onClick={() => setOrder(o)}
                                            className={cx(
                                                'px-3 py-2 text-sm',
                                                order === o && 'bg-accent/15',
                                            )}
                                        >
                                            {o === 'live'
                                                ? t('marketdata.quotes.liveFirst')
                                                : t('marketdata.quotes.byName')}
                                        </button>
                                    ))}
                                </div>
                            </div>
                        </section>
                        {bar}
                        {view()}
                    </div>
                </div>
            )}
        </div>
    );
}
