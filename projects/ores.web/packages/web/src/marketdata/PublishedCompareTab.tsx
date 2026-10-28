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
import { useSearchParams } from 'react-router';
import type { PublishedCurveSet } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { LineChart } from './LineChart.js';
import { formatDateTime } from '../ui/Time.js';
import { Button, Notice, Select, Tag } from '../ui/Primitives.js';
import { PersonCell } from '../access/PersonRef.js';
import {
    alignOnPillars,
    baselineAt,
    baselineChoicesOf,
    candidatesOf,
    comparePoints,
    deltaText,
    hasEnded,
    stateOf,
    tallyOf,
    type MarkKind,
} from './publishedCurves.js';

/**
 * The published set against the candidate, point by point.
 *
 * The tab answers one question: what does the candidate change? The baseline is
 * the set published at the chosen as-of, or the one in use; the candidate is the
 * newest set nobody has approved. Both are drawn on one x axis so a moved pillar
 * is visible where it moved, and the table marks each pillar as changed, added,
 * dropped or unchanged.
 *
 * A dropped pillar is drawn and not hidden: a comparison that hides a removal is
 * not a comparison. A pillar only the candidate holds ends the published line and
 * stands as a ring, and a pillar only the baseline holds is a gap in the
 * candidate's line and never a zero.
 */

/** The tone a mark carries, so a scan of the table reads the changes first. */
function toneOf(mark: MarkKind): 'warn' | 'up' | 'down' | 'muted' {
    if (mark === 'changed') return 'warn';
    if (mark === 'added') return 'up';
    if (mark === 'dropped') return 'down';
    return 'muted';
}

/** The word a mark reads as. */
function markKey(mark: MarkKind): string {
    return `marketdata.published.compare.marks.${mark}`;
}

export function PublishedCompareTab({
    sets,
}: {
    /** The sets of this curve, oldest first, with the points each holds. */
    readonly sets: readonly PublishedCurveSet[];
}): ReactNode {
    const { t, language } = useTranslation();
    const [search, setSearch] = useSearchParams();
    const [showUnchanged, setShowUnchanged] = useState(
        search.get('unchanged') === '1',
    );
    const choices = baselineChoicesOf(sets);
    const chosen = search.get('asof') ?? '';
    const baseline = baselineAt(sets, chosen);
    const candidate = candidatesOf(sets)[0];

    if (baseline === undefined) {
        return <Notice tone="info">{t('marketdata.published.compare.noBaseline')}</Notice>;
    }
    if (candidate === undefined) {
        return (
            <div className="space-y-3">
                <BaselinePanel set={baseline} />
                <Notice tone="info">
                    {t('marketdata.published.compare.noCandidate', { curve: baseline.curve })}
                </Notice>
            </div>
        );
    }

    const rows = comparePoints(baseline.points, candidate.points);
    const tally = tallyOf(rows);
    const nothingChanged = tally.changed + tally.added + tally.dropped === 0;
    const shown = showUnchanged ? rows : rows.filter((row) => row.mark !== 'unchanged');
    const { pillars, values } = alignOnPillars([baseline, candidate]);

    const choose = (asOf: string): void => {
        const parameters = new URLSearchParams(search);
        if (asOf === '') parameters.delete('asof');
        else parameters.set('asof', asOf);
        setSearch(parameters, { replace: true });
    };

    const toggleUnchanged = (): void => {
        const next = !showUnchanged;
        setShowUnchanged(next);
        const parameters = new URLSearchParams(search);
        if (next) parameters.set('unchanged', '1');
        else parameters.delete('unchanged');
        setSearch(parameters, { replace: true });
    };

    return (
        <div className="space-y-4">
            <label className="flex items-center gap-2 text-sm text-ink-muted">
                {t('marketdata.published.compare.asOf')}
                <Select
                    value={baseline.as_of}
                    onChange={(event) => choose(event.target.value)}
                    className="w-64"
                >
                    {choices.map((instant) => (
                        <option key={instant} value={instant}>
                            {formatDateTime(instant, language)}
                        </option>
                    ))}
                </Select>
            </label>

            <div className="grid gap-3 lg:grid-cols-2">
                <BaselinePanel set={baseline} />
                <CandidatePanel set={candidate} />
            </div>

            {hasEnded(baseline) && (
                <Notice tone="warn">{t('marketdata.published.compare.endedBaseline')}</Notice>
            )}

            {nothingChanged ? (
                <Notice tone="success">{t('marketdata.published.compare.nothingChanged')}</Notice>
            ) : (
                <p className="text-sm text-ink-muted">
                    {t('marketdata.published.compare.summary', {
                        changed: String(tally.changed),
                        added: String(tally.added),
                        dropped: String(tally.dropped),
                        unchanged: String(tally.unchanged),
                    })}
                </p>
            )}

            {pillars.length < 2 ? (
                <Notice tone="info">{t('marketdata.published.chart.onePoint')}</Notice>
            ) : (
                <div className="space-y-2 rounded-md border border-line p-3">
                    <h3 className="font-mono text-xs font-semibold text-ink">
                        {t('marketdata.published.compare.chart')}
                    </h3>
                    <LineChart
                        label={t('marketdata.published.compare.chart')}
                        format={(value) => (value < 1 ? `${(value * 100).toFixed(3)}%` : value.toFixed(4))}
                        categories={pillars}
                        padding={0.1}
                        series={[
                            {
                                points: values[0] ?? [],
                                colour: 'var(--color-ink-muted)',
                                dashed: true,
                                dots: true,
                                opacity: 0.8,
                            },
                            {
                                points: values[1] ?? [],
                                colour: 'var(--color-accent)',
                                strong: true,
                                dots: true,
                            },
                        ]}
                    />
                    <p className="text-xs text-ink-faint italic">
                        {t('marketdata.published.compare.chartCaption')}
                    </p>
                </div>
            )}

            <section className="space-y-2">
                <div className="flex flex-wrap items-center justify-between gap-2">
                    <h3 className="text-xs font-semibold tracking-wide uppercase">
                        {t('marketdata.published.compare.table')}
                    </h3>
                    {tally.unchanged > 0 && (
                        <Button size="sm" variant="ghost" onClick={toggleUnchanged}>
                            {showUnchanged
                                ? t('marketdata.published.compare.hideUnchanged')
                                : t('marketdata.published.compare.showUnchanged', {
                                      count: String(tally.unchanged),
                                  })}
                        </Button>
                    )}
                </div>
                <div className="overflow-x-auto rounded-md border border-line">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                <th className="px-3 py-2 font-medium">
                                    {t('marketdata.published.compare.pillar')}
                                </th>
                                <th className="px-3 py-2 font-medium">
                                    {t('marketdata.published.compare.publishedValue')}
                                </th>
                                <th className="px-3 py-2 font-medium">
                                    {t('marketdata.published.compare.candidateValue')}
                                </th>
                                <th className="px-3 py-2 font-medium">
                                    {t('marketdata.published.compare.change')}
                                </th>
                                <th className="px-3 py-2 font-medium">
                                    {t('marketdata.published.compare.mark')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {shown.map((row) => (
                                <tr
                                    key={row.pillar}
                                    className={
                                        row.mark === 'unchanged'
                                            ? 'border-b border-line-subtle text-ink-muted last:border-b-0'
                                            : 'border-b border-line-subtle last:border-b-0'
                                    }
                                >
                                    <td className="px-3 py-1 font-mono">{row.pillar}</td>
                                    <td className="px-3 py-1 font-mono">
                                        {row.mark === 'dropped' ? (
                                            row.publishedText
                                        ) : row.mark === 'added' ? (
                                            <span className="text-ink-faint">—</span>
                                        ) : row.mark === 'changed' ? (
                                            <span className="line-through text-ink-muted">
                                                {row.publishedText}
                                            </span>
                                        ) : (
                                            row.publishedText
                                        )}
                                    </td>
                                    <td className="px-3 py-1 font-mono">
                                        {row.mark === 'dropped' ? (
                                            <span className="text-ink-faint">—</span>
                                        ) : (
                                            row.candidateText
                                        )}
                                    </td>
                                    <td className="px-3 py-1 font-mono">
                                        {row.mark === 'added'
                                            ? t('marketdata.published.compare.newPillar')
                                            : row.mark === 'dropped'
                                              ? t('marketdata.published.compare.droppedPillar')
                                              : deltaText(row) === ''
                                                ? '—'
                                                : deltaText(row)}
                                    </td>
                                    <td className="px-3 py-1">
                                        <Tag tone={toneOf(row.mark)}>{t(markKey(row.mark))}</Tag>
                                    </td>
                                </tr>
                            ))}
                            {shown.length === 0 && (
                                <tr>
                                    <td colSpan={5} className="px-3 py-2 text-ink-muted">
                                        {t('marketdata.published.compare.onlyUnchanged')}
                                    </td>
                                </tr>
                            )}
                        </tbody>
                    </table>
                </div>
            </section>
        </div>
    );
}

/** The baseline: the set published at the chosen as-of, and who applied it. */
function BaselinePanel({ set }: { readonly set: PublishedCurveSet }): ReactNode {
    const { t, language } = useTranslation();
    const state = stateOf(set);
    return (
        <section className="space-y-2 rounded-md border border-line p-3">
            <div className="flex items-center justify-between gap-2 border-b border-line pb-2">
                <h3 className="text-xs font-semibold tracking-wide uppercase">
                    {t('marketdata.published.compare.baseline')}
                </h3>
                <Tag tone={state === 'published' ? 'up' : 'muted'}>
                    {t(`marketdata.published.state.${state}`)}
                </Tag>
            </div>
            <dl className="grid grid-cols-2 gap-2 text-sm">
                <Fact label={t('marketdata.published.points.asOf')}>
                    {formatDateTime(set.as_of, language)}
                </Fact>
                <Fact label={t('marketdata.published.points.publisher')}>
                    <PersonCell who={set.published_by} />
                </Fact>
                <Fact label={t('marketdata.published.points.published')}>
                    {set.published_at === null ? '—' : formatDateTime(set.published_at, language)}
                </Fact>
                <Fact label={t('marketdata.published.points.request')}>
                    {set.request_id === '' ? '—' : set.request_id.slice(0, 8)}
                </Fact>
            </dl>
        </section>
    );
}

/** The candidate: the set nobody has approved, and the instant it was built at. */
function CandidatePanel({ set }: { readonly set: PublishedCurveSet }): ReactNode {
    const { t, language } = useTranslation();
    return (
        <section className="space-y-2 rounded-md border border-accent/40 bg-accent/5 p-3">
            <div className="flex items-center justify-between gap-2 border-b border-line pb-2">
                <h3 className="text-xs font-semibold tracking-wide uppercase">
                    {t('marketdata.published.compare.candidate')}
                </h3>
                <Tag tone="accent">{t('marketdata.published.state.candidate')}</Tag>
            </div>
            <dl className="grid grid-cols-2 gap-2 text-sm">
                <Fact label={t('marketdata.published.points.asOf')}>
                    {formatDateTime(set.as_of, language)}
                </Fact>
                <Fact label={t('marketdata.published.compare.builtBy')}>{t('marketdata.published.compare.nobody')}</Fact>
                <Fact label={t('marketdata.published.points.points')}>
                    {String(set.point_count)}
                </Fact>
                <Fact label={t('marketdata.published.points.request')}>
                    {set.request_id === '' ? '—' : set.request_id.slice(0, 8)}
                </Fact>
            </dl>
        </section>
    );
}

function Fact({ label, children }: { readonly label: string; readonly children: ReactNode }): ReactNode {
    return (
        <div>
            <dt className="text-[10px] tracking-wide text-ink-faint uppercase">{label}</dt>
            <dd className="font-mono text-sm text-ink">{children}</dd>
        </div>
    );
}
