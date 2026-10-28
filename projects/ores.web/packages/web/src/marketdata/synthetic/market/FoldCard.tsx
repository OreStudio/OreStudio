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

import { useState, type ReactNode } from 'react';
import type { HistoryResolution, MarketHistory } from '../../../api/client.js';
import { useTranslation } from '../../../i18n/Provider.js';
import { ramp, RAMP } from './ArrivalsView.js';
import { FOLDS, foldHistory, type FoldSeries } from './fold.js';
import { MarketCard, MicroLabel, Segmented } from './MarketCard.js';
import { clean } from './numbers.js';

const WIDTH = 600;
const HEIGHT = 300;

/** The labels down the side of a fold: the seconds of a minute, or the seconds of ten. */
const ROW_LABELS: Readonly<Record<HistoryResolution, readonly string[]>> = {
    second: [':00', ':15', ':30', ':45', ':59'],
    tenth: ['0.0 s', '2.5 s', '5.0 s', '7.5 s', '9.9 s'],
};

function clock(at: number, resolution: HistoryResolution): string {
    return new Date(at).toLocaleTimeString([], {
        hour: '2-digit',
        minute: '2-digit',
        ...(resolution === 'tenth' ? { second: '2-digit' } : {}),
    });
}

/**
 * The last hour folded by the minute, or the last ten minutes folded by ten seconds: each
 * column a stretch, each cell a slot of it, coloured by how many ticks came. A spell lasts
 * minutes, so a busy market shows as a band of columns; a cell is a count, so the picture
 * holds at any tick rate.
 */
export function FoldCard({
    history,
    resolution,
    onResolution,
    now,
}: {
    readonly history: MarketHistory | undefined;
    readonly resolution: HistoryResolution;
    readonly onResolution: (resolution: HistoryResolution) => void;
    readonly now: number;
}): ReactNode {
    const { t } = useTranslation();
    const [choice, setChoice] = useState<string>('all');
    const fold = FOLDS[resolution];
    const groups = history?.groups ?? [];
    const series: FoldSeries = choice === 'all' || choice === 'pace' ? choice : { group: choice };
    const picture =
        history === undefined ? undefined : foldHistory(history, resolution, series, now);
    const cellWidth = WIDTH / fold.columns;
    const cellHeight = HEIGHT / fold.rows;
    const columnMs = fold.rows * fold.slotSeconds * 1000;
    const unit =
        choice === 'pace'
            ? (value: number) => `×${clean(value, 3)}`
            : (value: number) => `${clean(value, 3)}/s`;
    return (
        <MarketCard
            glyph="arrivals"
            title={t(`marketdata.market.fold.titles.${resolution}`)}
            lead={t('marketdata.market.fold.lead')}
        >
            <div className="flex flex-wrap items-center gap-2.5">
                <MicroLabel>{t('marketdata.market.fold.colourBy')}</MicroLabel>
                <Segmented
                    label={t('marketdata.market.fold.colourBy')}
                    options={['all', ...groups, 'pace']}
                    value={choice}
                    onChange={setChoice}
                    titleOf={(option) =>
                        option === 'all' || option === 'pace'
                            ? t(`marketdata.market.fold.series.${option}`)
                            : t(`marketdata.market.classes.${option}`)
                    }
                />
                <MicroLabel>{t('marketdata.market.fold.fold')}</MicroLabel>
                <Segmented
                    label={t('marketdata.market.fold.fold')}
                    options={['second', 'tenth'] as const}
                    value={resolution}
                    onChange={onResolution}
                    titleOf={(option) => t(`marketdata.market.fold.folds.${option}`)}
                />
            </div>
            {picture === undefined ? (
                <p className="text-[13px] text-ink-muted">{t('common.loading')}</p>
            ) : (
                <div className="grid grid-cols-[3rem_minmax(0,1fr)_3.5rem] gap-x-2">
                    <span />
                    <svg
                        aria-hidden
                        viewBox={`0 0 ${String(WIDTH)} 10`}
                        preserveAspectRatio="none"
                        className="mb-1 h-2.5 w-full"
                    >
                        {picture.activeShares.map((share, column) =>
                            share > 0 ? (
                                <rect
                                    key={column}
                                    x={column * cellWidth}
                                    width={cellWidth + 0.3}
                                    y={0}
                                    height={10}
                                    fill={`rgba(210,153,34,${(0.15 + 0.7 * share).toFixed(2)})`}
                                />
                            ) : null,
                        )}
                    </svg>
                    <span />
                    <span className="flex flex-col justify-between py-0.5 text-right font-mono text-[10px] text-ink-faint">
                        {ROW_LABELS[resolution].map((label) => (
                            <span key={label}>{label}</span>
                        ))}
                    </span>
                    <svg
                        role="img"
                        aria-label={t(`marketdata.market.fold.titles.${resolution}`)}
                        viewBox={`0 0 ${String(WIDTH)} ${String(HEIGHT)}`}
                        preserveAspectRatio="none"
                        className="h-[300px] w-full rounded border border-line bg-bg-primary"
                    >
                        {picture.cells.map((cell, index) => (
                            <rect
                                key={index}
                                data-cell
                                x={cell.column * cellWidth}
                                y={cell.row * cellHeight}
                                width={cellWidth + 0.3}
                                height={cellHeight + 0.3}
                                fill={ramp(cell.value / picture.top)}
                            >
                                <title>
                                    {`${new Date(cell.at).toLocaleTimeString()} · ${unit(cell.value)} · ${t(
                                        `marketdata.marketRegime.states.${cell.active ? 'active' : 'calm'}`,
                                    )}`}
                                </title>
                            </rect>
                        ))}
                    </svg>
                    <span className="grid grid-rows-[auto_1fr_auto] gap-1 py-0.5 font-mono text-[10px] text-ink-faint">
                        <span>{unit(picture.top)}</span>
                        <span
                            className="rounded-sm border border-line"
                            style={{ background: `linear-gradient(to top, ${RAMP.join(',')})` }}
                        />
                        <span>0</span>
                    </span>
                    <span />
                    <span className="flex justify-between font-mono text-[10px] text-ink-faint">
                        {[0, 0.25, 0.5, 0.75].map((share) => (
                            <span key={share}>
                                {clock(
                                    picture.firstColumnAt +
                                        Math.round(share * (fold.columns - 1)) * columnMs,
                                    resolution,
                                )}
                            </span>
                        ))}
                        <span>{t('marketdata.market.now.axisNow')}</span>
                    </span>
                    <span />
                </div>
            )}
        </MarketCard>
    );
}
