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
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { Notice, Select } from '../ui/Primitives.js';
import { LineChart, SERIES_COLOURS } from './LineChart.js';

/** How many of the newest ticks the plot can show. The BFF keeps 500 for each feed. */
export const TICK_COUNTS = [50, 200, 500] as const;

/** How often a running feed's ticks are read again. */
const TICKS_EVERY_MS = 2_000;

/** A wire timestamp, such as 2026-10-11 09:00:00Z, as milliseconds. */
export function instantOf(at: string): number {
    return Date.parse(at.replace(' ', 'T'));
}

function clock(ms: number): string {
    return new Date(ms).toISOString().slice(11, 19);
}

/**
 * The last ticks of one feed as a plot, with the last price and the move over
 * the ticks shown. A running feed is read again every two seconds; a stopped
 * one keeps the ticks of its last run.
 */
export function TicksPanel({
    feedId,
    running,
}: {
    readonly feedId: string;
    readonly running: boolean | undefined;
}): ReactNode {
    const { t } = useTranslation();
    const [count, setCount] = useState<number>(200);
    const ticks = useQuery({
        queryKey: ['feed-ticks', feedId, count],
        queryFn: () => api.feedTicks(feedId, count),
        refetchInterval: running === true ? TICKS_EVERY_MS : false,
        placeholderData: keepPreviousData,
    });
    const rows = ticks.data?.ticks ?? [];
    const first = rows[0];
    const last = rows[rows.length - 1];
    const move = first !== undefined && last !== undefined ? last.value - first.value : 0;

    return (
        <div className="space-y-3">
            <p className="text-ink-muted text-sm">{t('marketdata.ticks.lead')}</p>
            <div className="flex flex-wrap items-center gap-4 text-sm">
                <label className="flex items-center gap-2">
                    {t('marketdata.ticks.count')}
                    <Select
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
                {last !== undefined && first !== undefined && (
                    <span className="font-mono">
                        {t('marketdata.ticks.last', {
                            price: last.value.toFixed(5),
                            at: clock(instantOf(last.at)),
                        })}{' '}
                        <span className={move >= 0 ? 'text-up' : 'text-down'}>
                            {t('marketdata.ticks.move', {
                                move: `${move >= 0 ? '+' : ''}${move.toFixed(5)}`,
                                count: String(rows.length),
                            })}
                        </span>
                    </span>
                )}
            </div>
            {rows.length < 2 ? (
                <Notice tone="info">
                    {running === true
                        ? t('marketdata.ticks.waiting')
                        : t('marketdata.ticks.notRunning')}
                </Notice>
            ) : (
                <LineChart
                    label={t('marketdata.ticks.chart')}
                    series={[
                        {
                            points: rows.map((tick) => tick.value),
                            colour: SERIES_COLOURS[0],
                            strong: true,
                        },
                    ]}
                    xs={rows.map((tick) => instantOf(tick.at))}
                    format={(value) => value.toFixed(5)}
                    xFormat={clock}
                />
            )}
        </div>
    );
}
