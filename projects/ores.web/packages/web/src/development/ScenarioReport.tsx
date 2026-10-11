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

import type { ReactNode } from 'react';
import type { Scenario, ScenarioState, StepStatus } from '@ores/contracts';
import { useTranslation } from '../i18n/Provider.js';
import { Tag } from '../ui/Primitives.js';
import { AttachedImages } from './AttachedImages.js';
import { plainNote } from './images.js';
import { OrgText } from './OrgText.js';

const STATE_TONES = {
    PENDING: 'neutral',
    PASSED: 'up',
    FAILED: 'down',
    ABANDONED: 'muted',
} as const satisfies Record<ScenarioState, 'neutral' | 'up' | 'down' | 'muted'>;

const STEP_TONES = {
    PENDING: 'muted',
    PASS: 'up',
    FAIL: 'down',
    DROPPED: 'muted',
} as const satisfies Record<StepStatus, 'muted' | 'up' | 'down'>;

const MARKER_TONES = {
    PENDING: 'border-line text-ink-faint',
    PASS: 'border-up/60 bg-up/15 text-up',
    FAIL: 'border-down/60 bg-down/15 text-down',
    DROPPED: 'border-line-subtle text-ink-faint',
} as const satisfies Record<StepStatus, string>;

/** How many of the steps have each status. */
export function countStatuses(scenario: Scenario): Readonly<Record<StepStatus, number>> {
    const counts: Record<StepStatus, number> = { PENDING: 0, PASS: 0, FAIL: 0, DROPPED: 0 };
    for (const step of scenario.steps) counts[step.status] += 1;
    return counts;
}

function jumpTo(index: number): void {
    document
        .getElementById(`step-${index + 1}`)
        ?.scrollIntoView({ behavior: 'smooth', block: 'start' });
}

/**
 * A scenario as a report, for the person who reviews a run. Every step is on
 * the page, one after the other, with the instructions, what the tester marked,
 * what the tester wrote and the screenshots as pictures. Nothing is folded away,
 * because a reviewer reads it all by scrolling. It shows the scenario as the server
 * has it, so what a reviewer reads is what was saved.
 */
export function ScenarioReport({ scenario }: { readonly scenario: Scenario }): ReactNode {
    const { t } = useTranslation();
    const counts = countStatuses(scenario);
    const build = [scenario.run.branch, scenario.run.commit, scenario.run.worktree].filter(
        (part) => part !== '',
    );
    return (
        <div className="space-y-4">
            <section className="card space-y-3 p-4">
                <div className="flex flex-wrap items-center gap-2">
                    <Tag tone={STATE_TONES[scenario.state]}>
                        {t(`development.runner.states.${scenario.state}`)}
                    </Tag>
                    {(['PASS', 'FAIL', 'PENDING', 'DROPPED'] as const).map(
                        (status) =>
                            (counts[status] > 0 || status === 'PASS' || status === 'FAIL') && (
                                <Tag key={status} small tone={STEP_TONES[status]}>
                                    {t('development.report.count', {
                                        count: counts[status],
                                        status: t(`development.runner.stepStates.${status}`),
                                    })}
                                </Tag>
                            ),
                    )}
                </div>
                <dl className="grid grid-cols-[repeat(auto-fit,minmax(12rem,1fr))] gap-x-5 gap-y-2 text-sm">
                    <Fact label={t('development.tests.target')}>{scenario.target || '-'}</Fact>
                    <Fact label={t('development.tests.created')}>{scenario.created || '-'}</Fact>
                    <Fact label={t('development.tests.completed')}>
                        {scenario.run.completedAt || '-'}
                    </Fact>
                    <Fact label={t('development.report.build')}>
                        {build.length === 0 ? '-' : build.join(' · ')}
                    </Fact>
                </dl>
                {scenario.steps.length > 0 && (
                    <nav
                        aria-label={t('development.report.glance')}
                        className="flex flex-wrap gap-1.5"
                    >
                        {scenario.steps.map((step, index) => (
                            <button
                                key={`${index}/${step.client ?? ''}/${step.title}`}
                                type="button"
                                title={`${index + 1}. ${step.title}`}
                                onClick={() => jumpTo(index)}
                                className={`h-7 min-w-7 rounded-md border px-1.5 text-xs ${MARKER_TONES[step.status]}`}
                            >
                                {index + 1}
                            </button>
                        ))}
                    </nav>
                )}
            </section>

            {scenario.steps.length === 0 && (
                <p className="text-sm text-ink-muted">{t('development.report.noSteps')}</p>
            )}

            {scenario.steps.map((step, index) => {
                const words = plainNote(step.notes);
                return (
                    <section
                        key={`${index}/${step.client ?? ''}/${step.title}`}
                        id={`step-${index + 1}`}
                        className="card space-y-3 p-4"
                    >
                        <div className="flex items-start justify-between gap-3">
                            <h2 className="text-base font-medium text-ink">
                                <span className="mr-2 text-ink-faint">{index + 1}.</span>
                                {step.client !== null && (
                                    <span className="mr-2">
                                        <Tag small tone="accent">
                                            {step.client}
                                        </Tag>
                                    </span>
                                )}
                                {step.title}
                            </h2>
                            <Tag tone={STEP_TONES[step.status]}>
                                {t(`development.runner.stepStates.${step.status}`)}
                            </Tag>
                        </div>
                        {step.status === 'PENDING' && words === '' && (
                            <p className="text-sm text-ink-faint">
                                {t('development.report.notMarked')}
                            </p>
                        )}
                        {words !== '' && (
                            <div>
                                <h3 className="mb-0.5 text-[11px] tracking-wide text-ink-faint uppercase">
                                    {t('development.runner.note')}
                                </h3>
                                <p className="text-sm whitespace-pre-wrap text-ink">{words}</p>
                            </div>
                        )}
                        <AttachedImages scenarioId={scenario.id} notes={step.notes} large />
                        {step.instructions.length > 0 && (
                            <div>
                                <h3 className="mb-0.5 text-[11px] tracking-wide text-ink-faint uppercase">
                                    {t('development.report.instructions')}
                                </h3>
                                <OrgText source={step.instructions.join('\n')} />
                            </div>
                        )}
                    </section>
                );
            })}
        </div>
    );
}

function Fact({
    label,
    children,
}: {
    readonly label: string;
    readonly children: ReactNode;
}): ReactNode {
    return (
        <div className="space-y-0.5">
            <dt className="text-[11px] tracking-wide text-ink-faint uppercase">{label}</dt>
            <dd className="break-words">{children}</dd>
        </div>
    );
}
