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
import { qa } from '../api/qa.js';
import { useTranslation } from '../i18n/Provider.js';
import { Notice, Tag } from '../ui/Primitives.js';
import { openFromRow, percentDone, queueOf, stateOf, stepsDone, type QueueState } from './queue.js';

export const QA_SCENARIOS_KEY = ['qa-scenarios'] as const;

const TONES = {
    pending: 'neutral',
    inProgress: 'accent',
    passed: 'up',
    failed: 'down',
} as const satisfies Record<QueueState, 'neutral' | 'accent' | 'up' | 'down'>;

/**
 * The scenarios a tester can run: those waiting for a tester first, and those
 * that are done second. A row opens the scenario's runner.
 *
 * The tab names this screen, so it carries no heading of its own.
 */
export function TestsQueue(): ReactNode {
    const { t } = useTranslation();
    const scenarios = useQuery({ queryKey: QA_SCENARIOS_KEY, queryFn: () => qa.scenarios() });

    if (scenarios.isError) {
        return <Notice tone="error">{scenarios.error.message}</Notice>;
    }
    if (scenarios.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    const { waiting, done } = queueOf(scenarios.data);
    return (
        <div className="space-y-6">
            <Group
                title={t('development.tests.waiting')}
                rows={waiting}
                empty={t('development.tests.nothingWaiting')}
            />
            <Group
                title={t('development.tests.done')}
                rows={done}
                empty={t('development.tests.nothingDone')}
            />
        </div>
    );
}

function Group({
    title,
    rows,
    empty,
}: {
    readonly title: string;
    readonly rows: readonly ScenarioSummary[];
    readonly empty: string;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="rounded-md border border-line bg-surface-raised">
            <h2 className="flex items-center gap-2 px-4 pt-3 pb-2 text-sm font-medium text-ink">
                {title}
                <Tag small>{rows.length}</Tag>
            </h2>
            <div className="overflow-x-auto">
                <table className="w-full text-left text-sm">
                    <thead>
                        <tr className="border-b border-line text-xs text-ink-muted">
                            <th className="px-4 py-2 font-medium">
                                {t('development.tests.scenario')}
                            </th>
                            <th className="px-4 py-2 font-medium">
                                {t('development.tests.target')}
                            </th>
                            <th className="px-4 py-2 font-medium">
                                {t('development.tests.progress')}
                            </th>
                            <th className="px-4 py-2 font-medium">
                                {t('development.tests.state')}
                            </th>
                            <th className="px-4 py-2 font-medium">
                                {t('development.tests.completed')}
                            </th>
                        </tr>
                    </thead>
                    <tbody>
                        {rows.map((scenario) => (
                            <Row key={scenario.id} scenario={scenario} />
                        ))}
                        {rows.length === 0 && (
                            <tr>
                                <td colSpan={5} className="px-4 py-3 text-ink-muted">
                                    {empty}
                                </td>
                            </tr>
                        )}
                    </tbody>
                </table>
            </div>
        </section>
    );
}

function Row({ scenario }: { readonly scenario: ScenarioSummary }): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const state = stateOf(scenario);
    const open = `/development/tests/${scenario.id}`;
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
                <div
                    role="progressbar"
                    aria-valuemin={0}
                    aria-valuemax={100}
                    aria-valuenow={percentDone(scenario)}
                    className="h-1.5 w-24 overflow-hidden rounded-full bg-line-subtle"
                >
                    <div
                        className={state === 'failed' ? 'h-full bg-down' : 'h-full bg-up'}
                        style={{ width: `${percentDone(scenario)}%` }}
                    />
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
            <td className="px-4 py-2.5 text-xs text-ink-muted">{scenario.completedAt}</td>
        </tr>
    );
}
