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

import { keepPreviousData, useQuery } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { api, type RecordRow } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { show } from '../refdata/records.js';
import { Button, Notice, Select, Tag } from '../ui/Primitives.js';
import { LineChart, SERIES_COLOURS } from './LineChart.js';
import { describedAs } from './mixture.js';
import { processTypeOf } from './processHelp.js';
import { EMA_PERIOD, emaOf, instantOf, statsOf, ticksCsv, type Tick } from './ticks.js';

/** How many of the newest ticks the plot can show. The BFF keeps 500 for each feed. */
export const TICK_COUNTS = [50, 200, 500] as const;

/** How often a running feed's ticks are read again. */
const TICKS_EVERY_MS = 2_000;

function clock(ms: number): string {
    return new Date(ms).toISOString().slice(11, 19);
}

function price(value: number): string {
    return value.toFixed(5);
}

/** Hands the ticks shown to the browser as a CSV file. */
function download(sourceName: string, ticks: readonly Tick[]): void {
    const url = URL.createObjectURL(new Blob([ticksCsv(sourceName, ticks)], { type: 'text/csv' }));
    const link = document.createElement('a');
    link.href = url;
    link.download = `${sourceName}-ticks.csv`;
    link.click();
    URL.revokeObjectURL(url);
}

/** One labelled figure of the stats strip. */
function Stat({
    label,
    children,
    tone,
}: {
    readonly label: string;
    readonly children: ReactNode;
    readonly tone?: 'up' | 'down';
}): ReactNode {
    return (
        <div>
            <div className="text-[10px] tracking-wide text-ink-faint uppercase">{label}</div>
            <div
                className={`font-mono text-sm ${tone === 'up' ? 'text-up' : tone === 'down' ? 'text-down' : 'text-ink'}`}
            >
                {children}
            </div>
        </div>
    );
}

/**
 * The last ticks of one feed as a price chart, with a strip of what they say
 * and, beside them, the mixture that made them. A running feed is read again
 * every two seconds; a stopped one keeps the ticks of its last run.
 */
export function TicksPanel({
    feed,
    running,
    mixture,
}: {
    readonly feed: RecordRow;
    readonly running: boolean | undefined;
    readonly mixture: readonly RecordRow[];
}): ReactNode {
    const { t } = useTranslation();
    const feedId = show(feed['id']);
    const [count, setCount] = useState<number>(200);
    const [average, setAverage] = useState(false);
    const ticks = useQuery({
        queryKey: ['feed-ticks', feedId, count],
        queryFn: () => api.feedTicks(feedId, count),
        refetchInterval: running === true ? TICKS_EVERY_MS : false,
        placeholderData: keepPreviousData,
    });
    const rows = ticks.data?.ticks ?? [];
    const stats = statsOf(rows);
    const values = rows.map((tick) => tick.value);

    return (
        <div className="grid items-start gap-4 xl:grid-cols-4">
            <section className="space-y-3 xl:col-span-3">
                <p className="text-sm text-ink-muted">{t('marketdata.ticks.lead')}</p>
                <div className="flex flex-wrap items-end gap-x-6 gap-y-3 rounded-md border border-line p-3">
                    <label className="block">
                        <span className="block text-[10px] tracking-wide text-ink-faint uppercase">
                            {t('marketdata.ticks.count')}
                        </span>
                        <Select
                            className="w-24"
                            value={String(count)}
                            onChange={(event) => setCount(Number(event.target.value))}
                        >
                            {TICK_COUNTS.map((one) => (
                                <option key={one} value={one}>
                                    {one}
                                </option>
                            ))}
                        </Select>
                    </label>
                    {stats !== undefined && (
                        <>
                            <Stat label={t('marketdata.ticks.last')}>
                                {price(stats.last.value)}
                            </Stat>
                            <Stat label={t('marketdata.ticks.at')}>
                                {clock(instantOf(stats.last.at))}
                            </Stat>
                            <Stat
                                label={t('marketdata.ticks.move', { count: String(stats.count) })}
                                tone={stats.move >= 0 ? 'up' : 'down'}
                            >
                                {`${stats.move >= 0 ? '+' : ''}${price(stats.move)}`}
                            </Stat>
                        </>
                    )}
                    <div className="ml-auto flex items-center gap-3">
                        <label className="flex items-center gap-2 text-sm">
                            <input
                                type="checkbox"
                                checked={average}
                                onChange={(event) => setAverage(event.target.checked)}
                            />
                            {t('marketdata.ticks.ema', { period: String(EMA_PERIOD) })}
                        </label>
                        <Button
                            size="sm"
                            disabled={rows.length === 0}
                            onClick={() => download(ticks.data?.sourceName ?? feedId, rows)}
                        >
                            {t('marketdata.ticks.export')}
                        </Button>
                    </div>
                </div>
                {rows.length < 2 ? (
                    <Notice tone="info">
                        {running === true
                            ? t('marketdata.ticks.waiting')
                            : t('marketdata.ticks.notRunning')}
                    </Notice>
                ) : (
                    <div className="space-y-2 rounded-md border border-line p-3">
                        <div className="flex flex-wrap items-center justify-between gap-3 font-mono text-xs text-ink-muted">
                            <span className="font-semibold text-ink">
                                {t('marketdata.ticks.title', {
                                    pair: `${show(feed['base_currency_code'])}/${show(feed['quote_currency_code'])}`,
                                })}
                            </span>
                            {stats !== undefined && (
                                <span className="flex gap-4">
                                    <span>
                                        {t('marketdata.ticks.high')}{' '}
                                        <strong className="text-ink">{price(stats.high)}</strong>
                                    </span>
                                    <span>
                                        {t('marketdata.ticks.low')}{' '}
                                        <strong className="text-ink">{price(stats.low)}</strong>
                                    </span>
                                </span>
                            )}
                        </div>
                        <LineChart
                            price
                            label={t('marketdata.ticks.chart')}
                            series={[
                                {
                                    points: values,
                                    colour: SERIES_COLOURS[0],
                                    strong: true,
                                    fill: true,
                                },
                                ...(average
                                    ? [
                                          {
                                              points: emaOf(values),
                                              colour: SERIES_COLOURS[2],
                                              dashed: true,
                                          },
                                      ]
                                    : []),
                            ]}
                            xs={rows.map((tick) => instantOf(tick.at))}
                            format={price}
                            xFormat={clock}
                        />
                    </div>
                )}
            </section>
            <MixtureSummary feed={feed} mixture={mixture} />
        </div>
    );
}

/** The process that made the ticks, read beside them: the engine and each component. */
function MixtureSummary({
    feed,
    mixture,
}: {
    readonly feed: RecordRow;
    readonly mixture: readonly RecordRow[];
}): ReactNode {
    const { t } = useTranslation();
    const processType = processTypeOf(feed['process_type']);
    const ou = processType === 'ornstein_uhlenbeck';
    const components = [...mixture].sort(
        (a, b) => Number(a['component_index']) - Number(b['component_index']),
    );
    const sum = components.reduce((total, row) => total + Number(row['weight']), 0);
    return (
        <aside className="space-y-3 rounded-md border border-line p-3">
            <div className="flex items-center justify-between gap-2 border-b border-line pb-2">
                <h3 className="text-xs font-semibold tracking-wide uppercase">
                    {t('marketdata.ticks.process')}
                </h3>
                <Tag tone="accent" small>
                    {t(`refdata.choices.process_type.${processType}`)}
                </Tag>
            </div>
            <p className="text-xs text-ink-muted">
                {t('marketdata.ticks.rate', { rate: show(feed['ticks_per_hour']) })}
            </p>
            {components.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('marketdata.mixture.none')}</p>
            ) : (
                <ul className="space-y-2">
                    {components.map((row, index) => {
                        const { name, note } = describedAs(show(row['description']));
                        return (
                            <li
                                key={show(row['id'])}
                                className="space-y-1 rounded-md border border-line-subtle bg-surface-base p-2"
                            >
                                <div className="flex items-start justify-between gap-2">
                                    <span className="text-sm font-medium text-ink">
                                        <span className="font-mono text-ink-faint">#{index}</span>{' '}
                                        {name === '' ? t('marketdata.ticks.unnamed') : name}
                                    </span>
                                    <span
                                        className="font-mono text-sm font-semibold"
                                        style={{
                                            color: SERIES_COLOURS[
                                                (index + 1) % SERIES_COLOURS.length
                                            ],
                                        }}
                                        title={t(
                                            `marketdata.process.columns.${processType}.weight`,
                                        )}
                                    >
                                        {ou ? 'κ' : 'w'} {show(row['weight'])}
                                    </span>
                                </div>
                                {note !== '' && <p className="text-xs text-ink-faint">{note}</p>}
                                <div className="flex justify-between gap-2 font-mono text-xs text-ink-muted">
                                    <span>μ {show(row['mean'])}</span>
                                    <span>σ {show(row['stdev'])}</span>
                                </div>
                            </li>
                        );
                    })}
                </ul>
            )}
            {!ou && components.length > 0 && (
                <div className="flex justify-between rounded-md border border-line-subtle px-2 py-1.5 font-mono text-xs">
                    <span className="text-ink-muted">{t('marketdata.ticks.weightSum')}</span>
                    <span>{sum.toFixed(4)}</span>
                </div>
            )}
        </aside>
    );
}
