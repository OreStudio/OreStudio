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
import { Notice, Tag } from '../ui/Primitives.js';
import { QA_SCENARIOS_KEY, qaQueries } from './queries.js';
import {
    openFromRow,
    percentDone,
    sprintGroups,
    stateOf,
    stepsDone,
    type Phase,
    type QueueState,
    type SprintGroup,
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
 * The width of each column, so the tables of the sprints line up under one
 * another. A date and a time stay on one line.
 */
const WAITING_WIDTHS = ['w-[38%]', 'w-[30%]', 'w-[14%]', 'w-[10%]', 'w-[8%]'] as const;
const DONE_WIDTHS = ['w-[30%]', 'w-[25%]', 'w-[12%]', 'w-[9%]', 'w-[9%]', 'w-[15%]'] as const;

/**
 * The scenarios of one phase, grouped by sprint. The Tests tab shows the
 * scenarios that wait for a tester, which is what a tester comes for. The Done
 * tab shows the finished ones, and the Abandoned tab the ones that will not be
 * run. They are rarely wanted, so only the newest sprint is open there. A sprint is a section that opens and closes, with the
 * number of its scenarios in its heading. A row opens the scenario's runner.
 *
 * The tab names this screen, so it carries no heading of its own.
 */
export function TestsQueue({ phase }: { readonly phase: Phase }): ReactNode {
    const { t } = useTranslation();
    const scenarios = useQuery(qaQueries.scenarios());

    if (scenarios.isError) {
        return <Notice tone="error">{scenarios.error.message}</Notice>;
    }
    if (scenarios.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    const groups = sprintGroups(scenarios.data, phase);
    if (groups.length === 0) {
        return (
            <p className="rounded-md border border-line bg-surface-raised px-4 py-3 text-sm text-ink-muted">
                {phase === 'waiting'
                    ? t('development.tests.nothingWaiting')
                    : phase === 'done'
                      ? t('development.tests.nothingDone')
                      : t('development.tests.nothingAbandoned')}
            </p>
        );
    }
    return (
        <div className="space-y-4">
            {groups.map((group, index) => (
                <Sprint
                    key={group.sprint ?? 'none'}
                    group={group}
                    phase={phase}
                    open={phase === 'waiting' || index === 0}
                />
            ))}
        </div>
    );
}

function Sprint({
    group,
    phase,
    open,
}: {
    readonly group: SprintGroup;
    readonly phase: Phase;
    readonly open: boolean;
}): ReactNode {
    const { t } = useTranslation();
    const done = phase === 'done';
    return (
        <details open={open} className="group rounded-md border border-line bg-surface-raised">
            <summary className="flex cursor-pointer items-center gap-2 px-4 py-3 text-sm font-medium text-ink">
                {group.number === null
                    ? t('development.tests.noSprint')
                    : t('development.tests.sprint', { number: group.number })}
                <Tag small>{group.scenarios.length}</Tag>
            </summary>
            <div className="overflow-x-auto">
                <table className="w-full table-fixed text-left text-sm">
                    <colgroup>
                        {(done ? DONE_WIDTHS : WAITING_WIDTHS).map((width) => (
                            <col key={width} className={width} />
                        ))}
                    </colgroup>
                    <thead>
                        <tr className="border-y border-line text-xs text-ink-muted">
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
                                {t('development.tests.created')}
                            </th>
                            {done && (
                                <th className="px-4 py-2 font-medium">
                                    {t('development.tests.completed')}
                                </th>
                            )}
                        </tr>
                    </thead>
                    <tbody>
                        {group.scenarios.map((scenario) => (
                            <Row key={scenario.id} scenario={scenario} done={done} />
                        ))}
                    </tbody>
                </table>
            </div>
        </details>
    );
}

function Row({
    scenario,
    done,
}: {
    readonly scenario: ScenarioSummary;
    readonly done: boolean;
}): ReactNode {
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
            <td className="px-4 py-2.5 text-xs whitespace-nowrap text-ink-muted">
                {scenario.created}
            </td>
            {done && (
                <td className="px-4 py-2.5 text-xs whitespace-nowrap text-ink-muted">
                    {scenario.completedAt}
                </td>
            )}
        </tr>
    );
}
