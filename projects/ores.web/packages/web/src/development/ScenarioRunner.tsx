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
import { Link } from 'react-router';
import type { Scenario, ScenarioState } from '@ores/contracts';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Dialog, Field, Notice, Tag } from '../ui/Primitives.js';
import { AttachedImages } from './AttachedImages.js';
import { carriesFiles, filesOf } from './images.js';
import { OrgText } from './OrgText.js';
import {
    markedCount,
    percentMarked,
    stateNow,
    type Action,
    type DraftStep,
    type Outcome,
    type Runner,
} from './runner.js';

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
} as const;

export interface ScenarioRunnerProps {
    readonly scenario: Scenario;
    readonly runner: Runner;
    readonly dispatch: (action: Action) => void;
    readonly onSave: () => void;
    readonly saving: boolean;
    readonly outcome: Outcome | null;
    /** The files pasted or dropped on the step on show. The page checks each one. */
    readonly onImages: (files: File[]) => void;
    readonly imageBusy: boolean;
    readonly imageError: string | null;
    /** Reads an image from the clipboard and sends it to the step on show. */
    readonly onClipboard: () => void;
    readonly confirmingRetest: boolean;
    readonly onAskRetest: () => void;
    readonly onConfirmRetest: () => void;
    readonly onCancelRetest: () => void;
}

/**
 * The runner, one step at a time. The tester sees the scenario's header, a
 * progress bar, the step on show with its instructions, Pass, Fail and Clear,
 * and a note. A closed scenario is read-only, and a retest starts it again
 * after the tester confirms.
 */
export function ScenarioRunner(props: ScenarioRunnerProps): ReactNode {
    const { scenario, runner } = props;
    const { t } = useTranslation();
    // An abandoned scenario shows as that until a retest brings it back, and the
    // steps say nothing about it.
    const abandoned = runner.closed && scenario.state === 'ABANDONED';
    const state: ScenarioState = abandoned ? 'ABANDONED' : stateNow(runner);
    const context = [scenario.story?.title, scenario.task?.title].filter(Boolean).join(' · ');
    return (
        <div className="space-y-4">
            <section className="card space-y-3 p-4">
                <div className="flex flex-wrap items-start justify-between gap-3">
                    <div>
                        <h1 className="text-lg font-semibold text-ink">{scenario.title}</h1>
                        {context !== '' && <p className="text-sm text-ink-muted">{context}</p>}
                    </div>
                    <div className="text-right">
                        <Tag tone={STATE_TONES[state]}>
                            {t(`development.runner.states.${state}`)}
                        </Tag>
                        <p className="mt-1 text-xs text-ink-muted">
                            {t('development.runner.marked', {
                                done: markedCount(runner),
                                total: runner.steps.length,
                            })}
                        </p>
                    </div>
                </div>
                <dl className="grid grid-cols-[repeat(auto-fit,minmax(11rem,1fr))] gap-x-5 gap-y-2 text-sm">
                    <Fact label={t('development.tests.target')}>
                        <span className="font-mono text-xs">{scenario.target}</span>
                    </Fact>
                    <Fact label={t('development.runner.clients')}>
                        {scenario.clients.length === 0
                            ? t('development.runner.oneClient')
                            : scenario.clients.map((client) => (
                                  <span key={client} className="mr-1.5">
                                      <Tag small tone="muted">
                                          {client}
                                      </Tag>
                                  </span>
                              ))}
                    </Fact>
                    <Fact label={t('development.runner.lastSaved')}>
                        {scenario.run.completedAt === '' ? '-' : scenario.run.completedAt}
                    </Fact>
                </dl>
            </section>

            <div className="mx-auto w-full max-w-xl space-y-4">
                {runner.steps.length > 0 ? (
                    <Progress {...props} abandoned={abandoned} />
                ) : (
                    <section className="card p-4">
                        <Actions {...props} abandoned={abandoned} />
                    </section>
                )}
                {props.outcome !== null && <OutcomeNotice outcome={props.outcome} />}
                {runner.steps.length > 0 && <StepCard {...props} />}
            </div>

            {props.confirmingRetest && (
                <Dialog
                    title={t('development.runner.retestTitle')}
                    onClose={props.onCancelRetest}
                    footer={
                        <>
                            <Button type="button" variant="ghost" onClick={props.onCancelRetest}>
                                {t('development.runner.cancel')}
                            </Button>
                            <Button type="button" variant="danger" onClick={props.onConfirmRetest}>
                                {t('development.runner.retestConfirm')}
                            </Button>
                        </>
                    }
                >
                    <p className="text-sm text-ink-muted">{t('development.runner.retestBody')}</p>
                </Dialog>
            )}
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
            <dd>{children}</dd>
        </div>
    );
}

/**
 * Save, or Retest for a closed scenario, with what it does and the way back to
 * the tests. It sits above the step, beside the step navigation, so a tester
 * sees it without scrolling to the end of a long step.
 */
function Actions(props: ScenarioRunnerProps & { readonly abandoned: boolean }): ReactNode {
    const { t } = useTranslation();
    return (
        <div className="flex flex-wrap items-center gap-3">
            {props.runner.closed ? (
                <>
                    <Button type="button" onClick={props.onAskRetest}>
                        {t('development.runner.retest')}
                    </Button>
                    <span className="min-w-0 flex-1 text-xs text-ink-muted">
                        {props.abandoned
                            ? t('development.runner.abandonedHelp')
                            : t('development.runner.closedHelp')}
                    </span>
                </>
            ) : (
                <>
                    <Button
                        type="button"
                        variant="primary"
                        pending={props.saving}
                        // A save while a file is uploading would replace the working
                        // copy before the link for that file is in it.
                        disabled={props.imageBusy}
                        onClick={props.onSave}
                    >
                        {t('development.runner.save')}
                    </Button>
                    <span className="min-w-0 flex-1 text-xs text-ink-muted">
                        {t('development.runner.saveHelp')}
                    </span>
                </>
            )}
            <Link to="/development" className="text-sm text-accent hover:text-accent-bright">
                {t('development.runner.back')}
            </Link>
        </div>
    );
}

/** The colour of a step's part of the progress bar: a failed step shows red and the bar goes on. */
const SEGMENT: Record<string, string> = {
    PENDING: 'bg-line-subtle',
    PASS: 'bg-up',
    FAIL: 'bg-down',
    DROPPED: 'bg-ink-faint',
};

function Progress(props: ScenarioRunnerProps & { readonly abandoned: boolean }): ReactNode {
    const { runner, dispatch } = props;
    const { t } = useTranslation();
    return (
        <section className="card space-y-3 p-4">
            <div
                role="progressbar"
                aria-valuemin={0}
                aria-valuemax={100}
                aria-valuenow={percentMarked(runner)}
                className="flex h-1.5 gap-px overflow-hidden rounded-full"
            >
                {runner.steps.map((step, index) => (
                    <div
                        key={index}
                        data-status={step.status}
                        className={`h-full flex-1 ${SEGMENT[step.status]}`}
                    />
                ))}
            </div>
            <div className="flex items-center justify-between">
                <Button
                    type="button"
                    size="sm"
                    disabled={runner.index === 0}
                    onClick={() => dispatch({ type: 'previous' })}
                >
                    {t('development.runner.previous')}
                </Button>
                <span className="text-xs text-ink-muted">
                    {t('development.runner.stepOf', {
                        step: runner.index + 1,
                        total: runner.steps.length,
                    })}
                </span>
                <Button
                    type="button"
                    size="sm"
                    disabled={runner.index === runner.steps.length - 1}
                    onClick={() => dispatch({ type: 'next' })}
                >
                    {t('development.runner.skip')}
                </Button>
            </div>
            <div className="border-t border-line pt-3">
                <Actions {...props} />
            </div>
        </section>
    );
}

function StepCard({
    runner,
    dispatch,
    saving,
    onImages,
    imageBusy,
    imageError,
    onClipboard,
    scenario,
}: ScenarioRunnerProps): ReactNode {
    const { t } = useTranslation();
    const step: DraftStep | undefined = runner.steps[runner.index];
    if (step === undefined) return null;
    // Marks and notes wait while a save is in flight, so none is lost when the
    // server's answer replaces the working copy.
    const readOnly = runner.closed || saving;
    // A paste or a drop of a file anywhere on the card sends it to the step on
    // show, and the page says if it is not an image. Text pasted into the note is
    // left to the note.
    const attach = (transfer: DataTransfer | null): boolean => {
        const files = filesOf(transfer);
        if (files.length === 0 || readOnly) return false;
        onImages(files);
        return true;
    };
    return (
        <section
            className="card space-y-4 p-4"
            onPaste={(event) => {
                if (attach(event.clipboardData)) event.preventDefault();
            }}
            onDragOver={(event) => {
                if (!readOnly && carriesFiles(event.dataTransfer)) event.preventDefault();
            }}
            onDrop={(event) => {
                if (readOnly || !carriesFiles(event.dataTransfer)) return;
                event.preventDefault();
                attach(event.dataTransfer);
            }}
        >
            <div className="flex items-start justify-between gap-3">
                <div>
                    <p className="text-base font-medium text-ink">
                        {step.client !== null && (
                            <span className="mr-2">
                                <Tag small tone="accent">
                                    {step.client}
                                </Tag>
                            </span>
                        )}
                        {step.title}
                    </p>
                </div>
                <Tag tone={STEP_TONES[step.status]}>
                    {t(`development.runner.stepStates.${step.status}`)}
                </Tag>
            </div>
            <OrgText source={step.instructions.join('\n')} />
            <div className="flex gap-2">
                <Button
                    type="button"
                    variant={step.status === 'PASS' ? 'primary' : 'secondary'}
                    aria-pressed={step.status === 'PASS'}
                    disabled={readOnly}
                    onClick={() => dispatch({ type: 'mark', mark: 'PASS' })}
                >
                    {t('development.runner.pass')}
                </Button>
                <Button
                    type="button"
                    variant={step.status === 'FAIL' ? 'danger' : 'secondary'}
                    aria-pressed={step.status === 'FAIL'}
                    disabled={readOnly}
                    onClick={() => dispatch({ type: 'mark', mark: 'FAIL' })}
                >
                    {t('development.runner.fail')}
                </Button>
                <Button
                    type="button"
                    variant="ghost"
                    disabled={readOnly}
                    onClick={() => dispatch({ type: 'mark', mark: 'PENDING' })}
                >
                    {t('development.runner.clear')}
                </Button>
            </div>
            <Field label={t('development.runner.note')}>
                <textarea
                    className="w-full rounded-md border border-line bg-surface-base px-3 py-2 text-sm text-ink placeholder:text-ink-faint hover:border-line-strong focus:border-accent focus:outline-none focus:ring-3 focus:ring-accent/20 disabled:opacity-60"
                    rows={3}
                    value={step.notes}
                    disabled={readOnly}
                    placeholder={t('development.runner.notePlaceholder')}
                    onChange={(event) => dispatch({ type: 'note', notes: event.target.value })}
                />
            </Field>
            {!readOnly && (
                <div className="flex flex-wrap items-center gap-3 rounded-md border border-dashed border-line px-3 py-2">
                    <Button type="button" size="sm" disabled={imageBusy} onClick={onClipboard}>
                        {t('development.runner.pasteButton')}
                    </Button>
                    <p className="flex-1 text-xs text-ink-muted">
                        {imageBusy
                            ? t('development.runner.imageSaving')
                            : t('development.runner.imageHint')}
                    </p>
                </div>
            )}
            {imageError !== null && <Notice tone="error">{imageError}</Notice>}
            <AttachedImages scenarioId={scenario.id} notes={step.notes} />
        </section>
    );
}

function OutcomeNotice({ outcome }: { readonly outcome: Outcome }): ReactNode {
    const { t } = useTranslation();
    switch (outcome.kind) {
        case 'failed':
            return <Notice tone="error">{outcome.message}</Notice>;
        case 'closed':
            return (
                <Notice tone="success">
                    {t('development.runner.savedClosed', {
                        state: t(`development.runner.states.${outcome.state}`),
                    })}
                </Notice>
            );
        case 'open':
            return (
                <Notice>{t('development.runner.savedOpen', { pending: outcome.pending })}</Notice>
            );
    }
}
