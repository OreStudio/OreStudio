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

import { useState, type ReactNode } from 'react';
import type { CurveSnapshot } from '@ores/contracts';
import { useTranslation } from '../i18n/Provider.js';
import { SERIES_COLOURS } from '../marketdata/LineChart.js';
import { Field, Notice, Select } from '../ui/Primitives.js';
import { slicesOf, surfaceOf, type Surface, type SurfaceScale } from './surfaceView.js';

/**
 * The surface of a snapshot with two axes: a heatmap of the first axis against the second,
 * and one line for each value of the first axis across the second.
 *
 * The plot reads the snapshot the screen already holds. A hole is drawn empty in the
 * heatmap and breaks a line, and is never drawn as the lowest value. The lines are slices,
 * not smiles: the columns can hold quotes in market convention, such as ATM, 25RR and 25BF,
 * which are not points on a smile.
 */

const WIDTH = 600;
const HEIGHT = 220;
const LEFT = 56;
const RIGHT = 12;
const TOP = 10;
const BOTTOM = 26;

/** The lowest and highest share of the accent a cell takes, so every value stays readable. */
const LOW_SHARE = 8;
const HIGH_SHARE = 78;

/** The background of a cell: the accent, in a share that grows with its shade. */
function shadeStyle(shade: number | undefined): { background?: string } {
    if (shade === undefined) {
        return {};
    }
    const share = Math.round(LOW_SHARE + shade * (HIGH_SHARE - LOW_SHARE));
    return { background: `color-mix(in srgb, var(--color-accent) ${share}%, transparent)` };
}

function Heatmap({ surface }: { readonly surface: Surface }): ReactNode {
    const { t } = useTranslation();
    return (
        <div className="overflow-x-auto">
            <table className="w-full text-sm">
                <thead className="text-left text-xs text-ink-faint">
                    <tr>
                        <th className="py-1 pr-4 font-normal">
                            {surface.rowAxis} / {surface.columnAxis}
                        </th>
                        {surface.columns.map((column) => (
                            <th key={column} className="px-2 py-1 text-right font-normal">
                                {column}
                            </th>
                        ))}
                    </tr>
                </thead>
                <tbody>
                    {surface.rows.map((row, r) => (
                        <tr key={row}>
                            <th className="py-1 pr-4 text-left font-mono font-normal">{row}</th>
                            {surface.cells[r]!.map((cell, c) => (
                                <td
                                    key={surface.columns[c]}
                                    className="px-2 py-1 text-right font-mono"
                                    style={shadeStyle(cell.shade)}
                                >
                                    {cell.number === undefined ? (
                                        <span
                                            className="text-ink-faint"
                                            title={t('market.evolution.matrix.hole')}
                                        >
                                            —
                                        </span>
                                    ) : (
                                        cell.text
                                    )}
                                </td>
                            ))}
                        </tr>
                    ))}
                </tbody>
            </table>
        </div>
    );
}

function Slices({ surface }: { readonly surface: Surface }): ReactNode {
    const { t } = useTranslation();
    const slices = slicesOf(surface);
    const values = slices.flatMap((slice) =>
        slice.runs.flatMap((run) => run.map((point) => point.value)),
    );
    if (values.length === 0 || surface.columns.length < 2) {
        return null;
    }
    const low = Math.min(...values);
    const high = Math.max(...values);
    const x = (column: number): number =>
        LEFT + (column / (surface.columns.length - 1)) * (WIDTH - LEFT - RIGHT);
    // A flat surface sits in the middle of the chart.
    const y = (value: number): number =>
        high === low
            ? (TOP + HEIGHT - BOTTOM) / 2
            : HEIGHT - BOTTOM - ((value - low) / (high - low)) * (HEIGHT - BOTTOM - TOP);
    const colour = (index: number): string => SERIES_COLOURS[index % SERIES_COLOURS.length]!;
    return (
        <div className="space-y-2">
            <h3 className="text-sm font-medium">{t('market.evolution.surface.slices')}</h3>
            <svg
                viewBox={`0 0 ${WIDTH} ${HEIGHT}`}
                className="w-full"
                role="img"
                aria-label={t('market.evolution.surface.slicesLabel')}
            >
                <text
                    x={LEFT - 6}
                    y={TOP + 4}
                    textAnchor="end"
                    className="fill-ink-faint text-[10px]"
                >
                    {high.toPrecision(3)}
                </text>
                <text
                    x={LEFT - 6}
                    y={HEIGHT - BOTTOM}
                    textAnchor="end"
                    className="fill-ink-faint text-[10px]"
                >
                    {low.toPrecision(3)}
                </text>
                {surface.columns.map((column, c) => (
                    <text
                        key={column}
                        x={x(c)}
                        y={HEIGHT - 8}
                        textAnchor="middle"
                        className="fill-ink-faint text-[10px]"
                    >
                        {column}
                    </text>
                ))}
                {slices.map((slice, index) =>
                    slice.runs.map((run) =>
                        run.length === 1 ? (
                            <circle
                                key={`${slice.label}-${run[0]!.column}`}
                                cx={x(run[0]!.column)}
                                cy={y(run[0]!.value)}
                                r={2.5}
                                fill={colour(index)}
                            />
                        ) : (
                            <polyline
                                key={`${slice.label}-${run[0]!.column}`}
                                points={run.map((p) => `${x(p.column)},${y(p.value)}`).join(' ')}
                                fill="none"
                                stroke={colour(index)}
                                strokeWidth={1.5}
                            />
                        ),
                    ),
                )}
            </svg>
            <ul className="flex flex-wrap gap-x-4 gap-y-1 text-xs text-ink-muted">
                {slices.map((slice, index) => (
                    <li key={slice.label} className="flex items-center gap-1.5">
                        <span
                            aria-hidden
                            className="inline-block size-2.5 rounded-sm"
                            style={{ background: colour(index) }}
                        />
                        {slice.label}
                    </li>
                ))}
            </ul>
        </div>
    );
}

export function SurfacePanel({ snapshot }: { readonly snapshot: CurveSnapshot }): ReactNode {
    const { t } = useTranslation();
    const [scale, setScale] = useState<SurfaceScale>('surface');
    const axes = snapshot.coordinate_fields.length;
    // A curve has its own chart and a shape with no axis has no grid.
    if (axes < 2) {
        return null;
    }
    const result = surfaceOf(snapshot, scale);
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('market.evolution.surface.title')}</h2>
            </header>
            {!result.ok ? (
                <Notice tone="info">
                    {t(`market.evolution.surface.${result.reason === 'axes' ? 'axes' : 'empty'}`, {
                        count: axes,
                    })}
                </Notice>
            ) : (
                <>
                    <p className="text-xs text-ink-faint">
                        {t('market.evolution.surface.note', {
                            rows: result.surface.rowAxis,
                            columns: result.surface.columnAxis,
                        })}
                    </p>
                    <div className="max-w-xs">
                        <Field label={t('market.evolution.surface.scale')}>
                            <Select
                                value={scale}
                                onChange={(event) =>
                                    setScale(event.target.value === 'column' ? 'column' : 'surface')
                                }
                            >
                                <option value="surface">
                                    {t('market.evolution.surface.scaleSurface')}
                                </option>
                                <option value="column">
                                    {t('market.evolution.surface.scaleColumn')}
                                </option>
                            </Select>
                        </Field>
                    </div>
                    <Heatmap surface={result.surface} />
                    <Slices surface={result.surface} />
                </>
            )}
        </section>
    );
}
