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
import { Link, useNavigate } from 'react-router';
import type { ScenarioSummary } from '@ores/contracts';
import { useTranslation } from '../i18n/Provider.js';
import { useSite } from '../session/SiteProvider.js';
import { Notice, Tag } from '../ui/Primitives.js';
import { QA_SCENARIOS_KEY, qaQueries } from './queries.js';
import {
    openFromRow,
    percentDone,
    sprintBuckets,
    stateOf,
    stepsDone,
    type QueueState,
    type SprintBuckets,
} from './queue.js';

export { QA_SCENARIOS_KEY };

const TONES = {
    pending: 'neutral',
    inProgress: 'accent',
    passed: 'up',
    failed: 'down',
    abandoned: 'muted',
} as const satisfies Record<QueueState, 'neutral' | 'accent' | 'up' | 'down' | 'muted'>;

/**
 * The width of each column, so the tables of the buckets line up under one
 * another. A date and a time stay on one line.
 */
const WIDTHS = ['w-[26%]', 'w-[20%]', 'w-[14%]', 'w-[11%]', 'w-[8%]', 'w-[9%]', 'w-[12%]'] as const;

/**
 * The scenarios, grouped by sprint, newest sprint first. In a sprint the live
 * scenarios, which wait for a tester, are at the top and the done ones are
 * below them. The abandoned ones are in a bucket of their own that is closed,
 * at the bottom. A sprint is a section that opens and closes. It is open when
 * something in it is active, and the newest sprint is always open. A row opens
 * the scenario's runner.
 *
 * The tab names this screen, so it carries no heading of its own.
 */
export function TestsQueue(): ReactNode {
    const { t } = useTranslation();
    const scenarios = useQuery(qaQueries.scenarios());

    if (scenarios.isError) {
        return <Notice tone="error">{scenarios.error.message}</Notice>;
    }
    if (scenarios.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    const sprints = sprintBuckets(scenarios.data);
    if (sprints.length === 0) {
        return (
            <p className="rounded-md border border-line bg-surface-raised px-4 py-3 text-sm text-ink-muted">
                {t('development.tests.empty')}
            </p>
        );
    }
    return (
        <div className="space-y-4">
            {sprints.map((sprint, index) => (
                <Sprint
                    key={sprint.sprint ?? 'none'}
                    sprint={sprint}
                    open={sprint.active.length > 0 || index === 0}
                />
            ))}
        </div>
    );
}

function Sprint({
    sprint,
    open,
}: {
    readonly sprint: SprintBuckets;
    readonly open: boolean;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <details open={open} className="group rounded-md border border-line bg-surface-raised">
            <summary className="flex cursor-pointer flex-wrap items-center gap-2 px-4 py-3 text-sm font-medium text-ink">
                {sprint.number === null
                    ? t('development.tests.noSprint')
                    : t('development.tests.sprint', { number: sprint.number })}
                {sprint.active.length > 0 && (
                    <Tag small tone="accent">
                        {t('development.tests.activeCount', { count: sprint.active.length })}
                    </Tag>
                )}
                {sprint.done.length > 0 && (
                    <Tag small tone="muted">
                        {t('development.tests.doneCount', { count: sprint.done.length })}
                    </Tag>
                )}
                {sprint.abandoned.length > 0 && (
                    <Tag small tone="muted">
                        {t('development.tests.abandonedCount', { count: sprint.abandoned.length })}
                    </Tag>
                )}
            </summary>
            {sprint.active.length > 0 && (
                <Bucket title={t('development.tests.active')} rows={sprint.active} />
            )}
            {sprint.done.length > 0 && (
                <Bucket title={t('development.tests.done')} rows={sprint.done} showCompleted />
            )}
            {sprint.abandoned.length > 0 && (
                <details className="border-t border-line">
                    <summary className="cursor-pointer px-4 py-2.5 text-xs font-medium tracking-wide text-ink-faint uppercase">
                        {t('development.tests.abandoned')}
                        <span className="ml-2">
                            <Tag small tone="muted">
                                {sprint.abandoned.length}
                            </Tag>
                        </span>
                    </summary>
                    <Table rows={sprint.abandoned} showCompleted={false} />
                </details>
            )}
        </details>
    );
}

function Bucket({
    title,
    rows,
    showCompleted = false,
}: {
    readonly title: string;
    readonly rows: readonly ScenarioSummary[];
    readonly showCompleted?: boolean;
}): ReactNode {
    return (
        <section className="border-t border-line">
            <h3 className="flex items-center gap-2 px-4 pt-3 pb-1 text-xs font-medium tracking-wide text-ink-faint uppercase">
                {title}
                <Tag small>{rows.length}</Tag>
            </h3>
            <Table rows={rows} showCompleted={showCompleted} />
        </section>
    );
}

function Table({
    rows,
    showCompleted,
}: {
    readonly rows: readonly ScenarioSummary[];
    readonly showCompleted: boolean;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <div className="overflow-x-auto">
            <table className="w-full table-fixed text-left text-sm">
                <colgroup>
                    {WIDTHS.map((width) => (
                        <col key={width} className={width} />
                    ))}
                </colgroup>
                <thead>
                    <tr className="border-b border-line text-xs text-ink-muted">
                        <th className="px-4 py-2 font-medium">{t('development.tests.scenario')}</th>
                        <th className="px-4 py-2 font-medium">{t('development.tests.target')}</th>
                        <th className="px-4 py-2 font-medium">
                            {t('development.tests.environment')}
                        </th>
                        <th className="px-4 py-2 font-medium">{t('development.tests.progress')}</th>
                        <th className="px-4 py-2 font-medium">{t('development.tests.state')}</th>
                        <th className="px-4 py-2 font-medium">{t('development.tests.created')}</th>
                        <th className="px-4 py-2 font-medium">
                            {showCompleted ? t('development.tests.completed') : null}
                        </th>
                    </tr>
                </thead>
                <tbody>
                    {rows.map((scenario) => (
                        <Row key={scenario.id} scenario={scenario} showCompleted={showCompleted} />
                    ))}
                </tbody>
            </table>
        </div>
    );
}

function Row({
    scenario,
    showCompleted,
}: {
    readonly scenario: ScenarioSummary;
    readonly showCompleted: boolean;
}): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const state = stateOf(scenario);
    const open = `/development/tests/${scenario.id}`;
    const { environment } = useSite();
    const here = scenario.environment !== '' && scenario.environment === environment?.id;
    const context = [scenario.story?.title, scenario.task?.title].filter(Boolean).join(' · ');
    return (
        <tr
            className="cursor-pointer border-b border-line-subtle last:border-b-0 hover:bg-overlay"
            onClick={(event) => openFromRow(event, () => void navigate(open))}
        >
            <td className="px-4 py-2.5">
                <Link to={open} className="font-medium text-accent hover:text-accent-bright">
                    {scenario.title}
                </Link>
                {context !== '' && <div className="text-xs text-ink-faint">{context}</div>}
            </td>
            <td className="px-4 py-2.5">
                <span className="font-mono text-xs">{scenario.target}</span>
                {scenario.clients.map((client) => (
                    <span key={client} className="ml-1.5">
                        <Tag small tone="muted">
                            {client}
                        </Tag>
                    </span>
                ))}
            </td>
            <td className="px-4 py-2.5">
                {scenario.environment !== '' && (
                    <span className="font-mono text-xs">{scenario.environment}</span>
                )}
                {here && (
                    <span className="ml-1.5">
                        <Tag small tone="accent">
                            {t('development.tests.thisEnvironment')}
                        </Tag>
                    </span>
                )}
            </td>
            <td className="px-4 py-2.5">
                <div
                    role="progressbar"
                    aria-valuemin={0}
                    aria-valuemax={100}
                    aria-valuenow={percentDone(scenario)}
                    className="flex h-1.5 w-24 gap-px overflow-hidden rounded-full"
                >
                    {(
                        [
                            ['pass', 'bg-up'],
                            ['fail', 'bg-down'],
                            ['dropped', 'bg-ink-faint'],
                            ['pending', 'bg-line-subtle'],
                        ] as const
                    ).map(([status, colour]) =>
                        scenario.steps[status] > 0 ? (
                            <div
                                key={status}
                                data-status={status}
                                className={`h-full ${colour}`}
                                style={{ flexGrow: scenario.steps[status] }}
                            />
                        ) : null,
                    )}
                </div>
                <div className="text-xs text-ink-faint">
                    {t('development.tests.stepsDone', {
                        done: stepsDone(scenario),
                        total: scenario.steps.total,
                    })}
                </div>
            </td>
            <td className="px-4 py-2.5">
                <Tag tone={TONES[state]}>{t(`development.tests.states.${state}`)}</Tag>
            </td>
            <td className="px-4 py-2.5 text-xs whitespace-nowrap text-ink-muted">
                {scenario.created}
            </td>
            <td className="px-4 py-2.5 text-xs whitespace-nowrap text-ink-muted">
                {showCompleted ? scenario.completedAt : ''}
            </td>
        </tr>
    );
}
