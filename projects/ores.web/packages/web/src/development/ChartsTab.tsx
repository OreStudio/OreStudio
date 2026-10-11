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

import { useQuery } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { useSearchParams } from 'react-router';
import type { Charts } from '@ores/contracts';
import { useTranslation } from '../i18n/Provider.js';
import { Notice } from '../ui/Primitives.js';
import { SprintPicker } from './BoardTab.js';
import { CHART, barsOf, burnupLine, recentVelocity, sprintLabel } from './charts.js';
import { agileQueries } from './queries.js';

/**
 * The sprint charts: stories done in each sprint, the burn-up of tasks against
 * the scope, tasks done on each day of the sprint, and tasks by state. The
 * sprint is held in the address, as the Board tab holds it.
 *
 * The tab names this screen, so it carries no heading of its own.
 */
export function ChartsTab(): ReactNode {
    const { t } = useTranslation();
    const [search, setSearch] = useSearchParams();
    const sprint = search.get('sprint') ?? undefined;
    const sprints = useQuery(agileQueries.sprints());
    const charts = useQuery(agileQueries.charts(sprint));
    const names = sprints.data?.map((ref) => ref.name) ?? [];
    const picker = (
        <SprintPicker
            names={names}
            value={sprint}
            onPick={(name) =>
                setSearch(
                    (previous) => {
                        const next = new URLSearchParams(previous);
                        next.set('sprint', name);
                        return next;
                    },
                    { replace: true },
                )
            }
        />
    );
    if (charts.isError) {
        return (
            <div className="space-y-4">
                {picker}
                <Notice tone="error">{charts.error.message}</Notice>
            </div>
        );
    }
    if (charts.isPending) return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    return (
        <div className="space-y-4">
            {picker}
            <ChartGrid charts={charts.data} />
        </div>
    );
}

export function ChartGrid({ charts }: { readonly charts: Charts }): ReactNode {
    const { t } = useTranslation();
    return (
        <div className="grid gap-4 md:grid-cols-2">
            <Panel
                title={t('development.charts.velocity')}
                note={t('development.charts.velocityNote')}
            >
                <Columns
                    label={t('development.charts.velocity')}
                    data={recentVelocity(charts).map((point) => ({
                        label: sprintLabel(point.sprint),
                        value: point.done,
                    }))}
                />
            </Panel>
            <Panel title={t('development.charts.burnup')} note={t('development.charts.burnupNote')}>
                <Burnup charts={charts} />
            </Panel>
            <Panel title={t('development.charts.perDay')} note={t('development.charts.perDayNote')}>
                {charts.perDay.length === 0 ? (
                    <p className="text-sm text-ink-muted">{t('development.charts.noDates')}</p>
                ) : (
                    <Columns
                        label={t('development.charts.perDay')}
                        data={charts.perDay.map((day) => ({
                            label: `D${day.day}`,
                            value: day.done,
                            dim: day.future,
                        }))}
                    />
                )}
            </Panel>
            <Panel
                title={t('development.charts.byState')}
                note={t('development.charts.byStateNote')}
            >
                <Columns
                    label={t('development.charts.byState')}
                    data={charts.byState.map((entry) => ({
                        label: t(`development.board.states.${entry.state}`),
                        value: entry.count,
                    }))}
                />
            </Panel>
        </div>
    );
}

function Panel({
    title,
    note,
    children,
}: {
    readonly title: string;
    readonly note: string;
    readonly children: ReactNode;
}): ReactNode {
    return (
        <section className="card space-y-2 p-4">
            <h2 className="text-base font-semibold text-ink">{title}</h2>
            {children}
            <p className="text-xs text-ink-muted">{note}</p>
        </section>
    );
}

function Columns({
    label,
    data,
}: {
    readonly label: string;
    readonly data: readonly { label: string; value: number; dim?: boolean }[];
}): ReactNode {
    const bars = barsOf(data);
    const dim = new Map(data.map((entry) => [entry.label, entry.dim === true]));
    return (
        <svg
            viewBox={`0 0 ${CHART.width} ${CHART.height}`}
            role="img"
            aria-label={label}
            className="w-full text-accent"
        >
            {bars.map((bar) => (
                <g key={bar.label}>
                    <rect
                        x={bar.x}
                        y={bar.y}
                        width={bar.width}
                        height={bar.height}
                        rx={2}
                        fill="currentColor"
                        opacity={dim.get(bar.label) === true ? 0.35 : 1}
                    >
                        <title>{`${bar.label}: ${bar.value}`}</title>
                    </rect>
                    <text
                        x={bar.x + bar.width / 2}
                        y={CHART.height - CHART.bottom + 14}
                        textAnchor="middle"
                        className="fill-ink-faint text-[9px]"
                    >
                        {bar.label}
                    </text>
                    {bar.value > 0 && (
                        <text
                            x={bar.x + bar.width / 2}
                            y={bar.y - 4}
                            textAnchor="middle"
                            className="fill-ink text-[9px]"
                        >
                            {bar.value}
                        </text>
                    )}
                </g>
            ))}
        </svg>
    );
}

function Burnup({ charts }: { readonly charts: Charts }): ReactNode {
    const { t } = useTranslation();
    const line = burnupLine(charts);
    if (line.path === '') {
        return <p className="text-sm text-ink-muted">{t('development.charts.noDone')}</p>;
    }
    return (
        <svg
            viewBox={`0 0 ${CHART.width} ${CHART.height}`}
            role="img"
            aria-label={t('development.charts.burnup')}
            className="w-full"
        >
            {line.scopeY !== null && (
                <line
                    x1={CHART.left}
                    x2={CHART.width - CHART.right}
                    y1={line.scopeY}
                    y2={line.scopeY}
                    className="stroke-ink-faint"
                    strokeDasharray="4"
                />
            )}
            <path d={line.path} fill="none" className="stroke-up" strokeWidth={2} />
            <text
                x={CHART.left}
                y={CHART.height - CHART.bottom + 14}
                className="fill-ink-faint text-[9px]"
            >
                {line.first.slice(5)}
            </text>
            <text
                x={CHART.width - CHART.right}
                y={CHART.height - CHART.bottom + 14}
                textAnchor="end"
                className="fill-ink-faint text-[9px]"
            >
                {line.last.slice(5)}
            </text>
            <text
                x={CHART.left - 6}
                y={CHART.top + 8}
                textAnchor="end"
                className="fill-ink text-[9px]"
            >
                {line.max}
            </text>
        </svg>
    );
}
