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
import { parameterLines, type ParameterLine } from './irCurve.js';
import { curvesCsv, curvesOf, pillarsOf, slopeOf } from './irTicks.js';
import { LineChart, SERIES_COLOURS } from './LineChart.js';
import { instantOf } from './ticks.js';
import { Stat, clock } from './TicksPanel.js';

/** How many whole curves the plot can overlay. The BFF keeps 500 ticks for each feed. */
export const CURVE_COUNTS = [1, 5, 10, 20] as const;

/** The newest ticks the BFF keeps for a feed. */
const TICKS_KEPT = 500;

/** How often a running feed's ticks are read again. */
const TICKS_EVERY_MS = 2_000;

function rate(value: number): string {
    return `${(value * 100).toFixed(3)}%`;
}

/** Hands the curves shown to the browser as a CSV file. */
function download(name: string, csv: string): void {
    const url = URL.createObjectURL(new Blob([csv], { type: 'text/csv' }));
    const link = document.createElement('a');
    link.href = url;
    link.download = `${name}-curves.csv`;
    link.click();
    URL.revokeObjectURL(url);
}

/**
 * The last curves of one IR curve feed, drawn as the Qt market simulator draws
 * them: the newest curve over the ones before it, rate against tenor, and the
 * rate of one tenor over time. Beside them is the process that made the curves.
 * A running feed is read again every two seconds; a stopped one has none,
 * because a run's ticks go with it.
 *
 * A tick of a curve feed is one pillar of a curve, so the ticks are grouped by
 * the time they share and each whole group is one curve.
 */
export function IrTicksPanel({
    feed,
    running,
    entries,
    parameters,
}: {
    readonly feed: RecordRow;
    readonly running: boolean | undefined;
    readonly entries: readonly RecordRow[];
    readonly parameters: readonly ParameterLine[];
}): ReactNode {
    const { t } = useTranslation();
    const feedId = show(feed['id']);
    const pillars = pillarsOf(entries);
    const [count, setCount] = useState<number>(5);
    const [chosen, setChosen] = useState(0);
    const limit = Math.min(TICKS_KEPT, Math.max(1, (count + 1) * pillars.length));
    const ticks = useQuery({
        queryKey: ['feed-ticks', feedId, limit],
        queryFn: () => api.feedTicks(feedId, limit),
        refetchInterval: running === true ? TICKS_EVERY_MS : false,
        placeholderData: keepPreviousData,
        enabled: pillars.length > 0,
    });
    const curves = curvesOf(ticks.data?.ticks ?? [], pillars.length).slice(-count);
    const newest = curves[curves.length - 1];
    const pillar = Math.min(chosen, Math.max(0, pillars.length - 1));
    const label = (index: number): string => pillars[Math.round(index)]?.end ?? '';

    return (
        <div className="grid items-start gap-4 xl:grid-cols-4">
            <section className="space-y-3 xl:col-span-3">
                <p className="text-sm text-ink-muted">{t('marketdata.irTicks.lead')}</p>
                <div className="flex flex-wrap items-end gap-x-6 gap-y-3 rounded-md border border-line p-3">
                    <label className="block">
                        <span className="block text-[10px] tracking-wide text-ink-faint uppercase">
                            {t('marketdata.irTicks.curves')}
                        </span>
                        <Select
                            className="w-24"
                            value={String(count)}
                            onChange={(event) => setCount(Number(event.target.value))}
                        >
                            {CURVE_COUNTS.map((one) => (
                                <option key={one} value={one}>
                                    {one}
                                </option>
                            ))}
                        </Select>
                    </label>
                    <label className="block">
                        <span className="block text-[10px] tracking-wide text-ink-faint uppercase">
                            {t('marketdata.irTicks.tenor')}
                        </span>
                        <Select
                            className="w-28"
                            value={String(pillar)}
                            onChange={(event) => setChosen(Number(event.target.value))}
                        >
                            {pillars.map((one, index) => (
                                <option
                                    key={`${one.start}-${one.end}-${String(index)}`}
                                    value={index}
                                >
                                    {one.end}
                                </option>
                            ))}
                        </Select>
                    </label>
                    {newest !== undefined && (
                        <>
                            <Stat label={t('marketdata.irTicks.at')}>
                                {clock(instantOf(newest.at))}
                            </Stat>
                            <Stat label={t('marketdata.irTicks.short', { tenor: label(0) })}>
                                {rate(newest.rates[0] ?? 0)}
                            </Stat>
                            <Stat
                                label={t('marketdata.irTicks.long', {
                                    tenor: label(pillars.length - 1),
                                })}
                            >
                                {rate(newest.rates[newest.rates.length - 1] ?? 0)}
                            </Stat>
                            <Stat
                                label={t('marketdata.irTicks.slope')}
                                tone={slopeOf(newest) >= 0 ? 'up' : 'down'}
                            >
                                {`${slopeOf(newest) >= 0 ? '+' : ''}${slopeOf(newest).toFixed(1)}`}
                            </Stat>
                        </>
                    )}
                    <div className="ml-auto">
                        <Button
                            size="sm"
                            disabled={curves.length === 0}
                            onClick={() =>
                                download(
                                    ticks.data?.sourceName ?? feedId,
                                    curvesCsv(ticks.data?.sourceName ?? feedId, pillars, curves),
                                )
                            }
                        >
                            {t('marketdata.ticks.export')}
                        </Button>
                    </div>
                </div>
                {pillars.length === 0 ? (
                    <Notice tone="info">{t('marketdata.irTicks.noTemplate')}</Notice>
                ) : curves.length === 0 ? (
                    <Notice tone="info">
                        {running === true
                            ? t('marketdata.irTicks.waiting')
                            : t('marketdata.ticks.notRunning')}
                    </Notice>
                ) : (
                    <div className="grid gap-4 lg:grid-cols-2">
                        <div className="space-y-2 rounded-md border border-line p-3">
                            <h3 className="text-sm font-medium">
                                {t('marketdata.irTicks.curveChart')}
                            </h3>
                            <LineChart
                                label={t('marketdata.irTicks.curveChart')}
                                format={rate}
                                xFormat={label}
                                series={curves.map((curve, index) => ({
                                    points: curve.rates,
                                    colour:
                                        index === curves.length - 1
                                            ? (SERIES_COLOURS[0] ?? '')
                                            : 'var(--color-ink-faint)',
                                    strong: index === curves.length - 1,
                                    dashed: index !== curves.length - 1,
                                }))}
                            />
                        </div>
                        <div className="space-y-2 rounded-md border border-line p-3">
                            <h3 className="text-sm font-medium">
                                {t('marketdata.irTicks.pillarChart', { tenor: label(pillar) })}
                            </h3>
                            <LineChart
                                label={t('marketdata.irTicks.pillarChart', {
                                    tenor: label(pillar),
                                })}
                                xs={curves.map((curve) => instantOf(curve.at))}
                                format={rate}
                                xFormat={clock}
                                series={[
                                    {
                                        points: curves.map((curve) => curve.rates[pillar] ?? 0),
                                        colour: SERIES_COLOURS[1] ?? '',
                                        strong: true,
                                        fill: true,
                                    },
                                ]}
                            />
                        </div>
                    </div>
                )}
            </section>
            <ProcessSummary feed={feed} parameters={parameters} />
        </div>
    );
}

/** The process that made the curves, read beside them: the engine, the rate and each parameter. */
function ProcessSummary({
    feed,
    parameters,
}: {
    readonly feed: RecordRow;
    readonly parameters: readonly ParameterLine[];
}): ReactNode {
    const { t } = useTranslation();
    return (
        <aside className="space-y-3 rounded-md border border-line p-3">
            <div className="flex items-center justify-between gap-2 border-b border-line pb-2">
                <h3 className="text-xs font-semibold tracking-wide uppercase">
                    {t('marketdata.ticks.process')}
                </h3>
                <Tag tone="accent" small>
                    {t(`marketdata.irFeeds.engine.${show(feed['process_type'])}`)}
                </Tag>
            </div>
            <p className="text-xs text-ink-muted">
                {t('marketdata.ticks.rate', { rate: show(feed['ticks_per_hour']) })}
            </p>
            {parameters.length === 0 ? (
                <p className="text-sm text-ink-muted">
                    {t('marketdata.irProcess.none', {
                        engine: t(`marketdata.irFeeds.engine.${show(feed['process_type'])}`),
                    })}
                </p>
            ) : (
                <ul className="space-y-2">
                    {parameters.map((line) => (
                        <li
                            key={show(line.definition['id'])}
                            className="flex items-start justify-between gap-2 rounded-md border border-line-subtle bg-surface-base p-2"
                            title={show(line.definition['description'])}
                        >
                            <span className="text-sm text-ink">
                                {show(line.definition['display_name'])}
                                {show(line.definition['symbol']) !== '' && (
                                    <span className="ml-1 text-ink-muted">
                                        ({show(line.definition['symbol'])})
                                    </span>
                                )}
                            </span>
                            <span className="font-mono text-sm font-semibold">{line.value}</span>
                        </li>
                    ))}
                </ul>
            )}
        </aside>
    );
}

/** The saved parameters of a feed's engine, for the side column of the ticks tab. */
export function savedParameters(
    definitions: readonly RecordRow[],
    values: readonly RecordRow[],
    engine: string,
): readonly ParameterLine[] {
    return parameterLines(definitions, values, engine);
}
