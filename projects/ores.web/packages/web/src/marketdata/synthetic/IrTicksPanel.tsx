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
import { api, type RecordRow } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { show } from '../../refdata/records.js';
import { Button, Input, Notice, Tag } from '../../ui/Primitives.js';
import { parameterLines, type ParameterLine } from './irCurve.js';
import { curvesCsv, curvesOf, pillarsOf, slopeOf } from './irTicks.js';
import { LineChart } from '../LineChart.js';
import { instantOf } from './ticks.js';
import { Stat, clock } from './TicksPanel.js';

/** The most curves the plot overlays, as the Qt market simulator allows. */
const MAX_HISTORY = 20;

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
 * How a curve of the history is drawn, as the Qt market simulator draws it. The
 * newest curve is bold and green. The older ones are grey-blue, thinner, and
 * fade with age: the oldest of n is the faintest, and none is fainter than 40
 * of 255.
 */
export function curveStyle(
    index: number,
    count: number,
): { readonly colour: string; readonly width: number; readonly opacity: number } {
    const newest = index === count - 1;
    return {
        colour: newest ? '#22C55E' : '#8A9BAE',
        width: newest ? 2.5 : 1,
        opacity: newest ? 1 : Math.max(40, (255 * (index + 1)) / count - 60) / 255,
    };
}

/**
 * The last curves of one IR curve feed, drawn as the Qt market simulator draws
 * them: one chart with the tenors of the curve along the bottom, short end to
 * long end, and the rate up the side. The newest curve is bold and the earlier
 * ones are overlaid, fading with age. Beside it is the process that made the
 * curves. A running feed is read again every two seconds; a stopped one has
 * none, because a run's ticks go with it.
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
    const [history, setHistory] = useState('5');
    const count = Math.min(MAX_HISTORY, Math.max(1, Math.round(Number(history)) || 1));
    const limit = Math.min(TICKS_KEPT, Math.max(1, (count + 1) * pillars.length));
    const ticks = useQuery({
        queryKey: ['feed-ticks', feedId, limit],
        queryFn: () => api.feedTicks(feedId, limit, 'ir-curve-feeds'),
        refetchInterval: running === true ? TICKS_EVERY_MS : false,
        placeholderData: keepPreviousData,
        enabled: pillars.length > 0,
    });
    const curves = curvesOf(ticks.data?.ticks ?? [], pillars.length).slice(-count);
    const newest = curves[curves.length - 1];
    const tenors = pillars.map((pillar) => pillar.end);

    return (
        <div className="grid items-start gap-4 xl:grid-cols-4">
            <section className="space-y-3 xl:col-span-3">
                <p className="text-sm text-ink-muted">{t('marketdata.irTicks.lead')}</p>
                <div className="flex flex-wrap items-end gap-x-6 gap-y-3 rounded-md border border-line p-3">
                    <label className="block">
                        <span className="block text-[10px] tracking-wide text-ink-faint uppercase">
                            {t('marketdata.irTicks.history')}
                        </span>
                        <Input
                            type="number"
                            min={1}
                            max={MAX_HISTORY}
                            className="w-20"
                            value={history}
                            title={t('marketdata.irTicks.historyHelp')}
                            onChange={(event) => setHistory(event.target.value)}
                        />
                    </label>
                    {newest !== undefined && (
                        <>
                            <Stat label={t('marketdata.irTicks.at')}>
                                {clock(instantOf(newest.at))}
                            </Stat>
                            <Stat label={t('marketdata.irTicks.short', { tenor: tenors[0] ?? '' })}>
                                {rate(newest.rates[0] ?? 0)}
                            </Stat>
                            <Stat
                                label={t('marketdata.irTicks.long', {
                                    tenor: tenors[tenors.length - 1] ?? '',
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
                    <div className="space-y-2 rounded-md border border-line p-3">
                        <h3 className="font-mono text-xs font-semibold text-ink">
                            {t('marketdata.irTicks.curveChart')}
                        </h3>
                        <LineChart
                            label={t('marketdata.irTicks.curveChart')}
                            format={rate}
                            categories={tenors}
                            padding={0.1}
                            series={curves.map((curve, index) => ({
                                points: curve.rates,
                                dots: true,
                                strong: index === curves.length - 1,
                                ...curveStyle(index, curves.length),
                            }))}
                        />
                        <p className="text-xs text-ink-faint italic">
                            {t('marketdata.irTicks.legend')}
                        </p>
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
