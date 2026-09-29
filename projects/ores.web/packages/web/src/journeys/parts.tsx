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

/**
 * The parts a journey step is made of.
 *
 * They are presentational: the state a step works from arrives as props and the
 * work it does goes back as callbacks. The two steps that read the server while
 * they are open, the starting points and the run, keep what they read here and
 * nowhere else, because a rail step that unmounts should stop polling.
 */

import { Fragment, useEffect, useRef, useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { profileLogo } from '../assets/profiles.js';
import { Button, Field, Input, Notice, Select, cx } from '../ui/Primitives.js';
import { NewPasswordField } from '../ui/PasswordField.js';
import type { JourneyServer } from './server.js';
import { administratorPassword, type TenantDetails } from './state.js';
import type {
    PasswordPolicy,
    SeedProfileChoice,
    WorkflowProgress,
    WorkflowStepSummary,
} from '@ores/wire-protocol/browser';

/** How often an open run is asked about again. */
const POLL_INTERVAL_MS = 1500;

function reasonOf(error: unknown): string {
    return error instanceof Error ? error.message : String(error);
}

/**
 * The starting points, as cards.
 *
 * Each card states what the server said about its profile — its bullets, how
 * many settings it declares and how many steps it orders — so the screen
 * describes the deployment's rows rather than a shape written here.
 */
export function ProfileCards({
    profiles,
    selected,
    onSelect,
}: {
    readonly profiles: readonly SeedProfileChoice[];
    readonly selected: string | undefined;
    readonly onSelect: (profile: SeedProfileChoice) => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <div role="radiogroup" className="grid gap-3 sm:grid-cols-2">
            {profiles.map((profile) => {
                const logo = profileLogo(profile.code);
                return (
                    <button
                        key={profile.code}
                        type="button"
                        role="radio"
                        aria-checked={selected === profile.code}
                        onClick={() => onSelect(profile)}
                        className={cx(
                            'card p-4 text-left transition-colors',
                            selected === profile.code
                                ? 'border-accent ring-3 ring-accent/20'
                                : 'hover:border-line-strong',
                        )}
                    >
                        {logo !== undefined && (
                            <img
                                src={logo}
                                alt=""
                                className="mb-3 h-10 w-auto rounded-md bg-white p-1"
                            />
                        )}
                        <div className="flex items-baseline justify-between gap-2">
                            <span className="font-semibold">{profile.name}</span>
                            <span className="text-xs text-ink-faint">{profile.audience}</span>
                        </div>
                        <p className="mt-1 text-sm text-ink-muted">{profile.summary}</p>
                        <ul className="mt-3 space-y-1 text-sm">
                            {profile.bullets.map((bullet) => (
                                <li key={bullet} className="flex gap-2">
                                    <span aria-hidden className="text-ink-faint">
                                        •
                                    </span>
                                    {bullet}
                                </li>
                            ))}
                        </ul>
                        <p className="mt-3 text-xs text-ink-faint">
                            {t('journey.profile.counts', {
                                settings: profile.parameters.length,
                                steps: profile.steps.length,
                            })}
                        </p>
                    </button>
                );
            })}
        </div>
    );
}

/**
 * The tenant's details, and its administrator's password.
 *
 * A profile that states its own details opens as a summary with the form
 * closed, because a demo card asks for nothing; the person who wants to change
 * one opens the form. The password field is the server's policy, applied.
 */
export function TenantForm({
    profile,
    details,
    policy,
    creatingPassword,
    onChange,
    onPasswordAcceptable,
}: {
    readonly profile: SeedProfileChoice;
    readonly details: TenantDetails;
    readonly policy: PasswordPolicy;
    readonly creatingPassword: string;
    readonly onChange: (details: TenantDetails) => void;
    readonly onPasswordAcceptable: (acceptable: boolean) => void;
}): ReactNode {
    const { t } = useTranslation();
    const [open, setOpen] = useState(profile.tenant.name === '');

    const set = (
        key: 'name' | 'code' | 'hostname' | 'adminUsername' | 'adminEmail',
        value: string,
    ): void => onChange({ ...details, [key]: value });
    const setParameter = (name: string, value: string): void =>
        onChange({ ...details, parameters: { ...details.parameters, [name]: value } });

    const passwordHint = profile.forcePasswordChange
        ? t('journey.details.passwordForced')
        : undefined;

    if (!open) {
        const rows: readonly (readonly [string, string])[] = [
            [t('journey.details.rows.tenant'), `${details.name} (${details.code})`],
            [t('journey.details.rows.hostname'), details.hostname],
            [t('journey.details.rows.administrator'), details.adminUsername],
            [
                t('journey.details.rows.password'),
                details.useMyPassword
                    ? t('journey.details.passwordMine')
                    : t('journey.details.passwordTyped'),
            ],
        ];
        return (
            <div className="space-y-5">
                <div className="rounded-md border border-line bg-surface-overlay p-4">
                    <p className="text-sm">
                        {t('journey.details.standard', { profile: profile.name })}
                    </p>
                    <dl className="mt-3 grid grid-cols-[10rem_1fr] gap-y-1 text-sm">
                        {rows.map(([label, value]) => (
                            <Fragment key={label}>
                                <dt className="text-ink-faint">{label}</dt>
                                <dd>{value}</dd>
                            </Fragment>
                        ))}
                    </dl>
                    <Button
                        variant="ghost"
                        size="sm"
                        className="-ml-3 mt-3"
                        onClick={() => setOpen(true)}
                    >
                        {t('journey.details.changeSettings')}
                    </Button>
                </div>
                {!details.useMyPassword && (
                    <NewPasswordField
                        policy={policy}
                        label={t('journey.details.adminPassword')}
                        {...(passwordHint !== undefined && { hint: passwordHint })}
                        value={details.adminPassword}
                        onChange={(password, acceptable) => {
                            onChange({ ...details, adminPassword: password });
                            onPasswordAcceptable(acceptable);
                        }}
                    />
                )}
            </div>
        );
    }

    return (
        <div className="space-y-6">
            <fieldset className="grid gap-4 sm:grid-cols-2">
                <legend className="mb-2 text-sm font-semibold">
                    {t('journey.details.tenant')}
                </legend>
                <Field label={t('journey.details.name')}>
                    <Input
                        value={details.name}
                        onChange={(event) => set('name', event.target.value)}
                    />
                </Field>
                <Field label={t('journey.details.code')} hint={t('journey.details.codeHint')}>
                    <Input
                        value={details.code}
                        onChange={(event) => set('code', event.target.value)}
                    />
                </Field>
                <Field label={t('journey.details.hostname')} className="sm:col-span-2">
                    <Input
                        value={details.hostname}
                        onChange={(event) => set('hostname', event.target.value)}
                    />
                </Field>
            </fieldset>

            {profile.parameters.length > 0 && (
                <fieldset className="grid gap-4 sm:grid-cols-2">
                    <legend className="mb-2 text-sm font-semibold">
                        {t('journey.details.settings', { profile: profile.name })}
                    </legend>
                    {profile.parameters.map((parameter) => (
                        <Field
                            key={parameter.name}
                            label={parameter.label}
                            {...(parameter.hint !== '' && { hint: parameter.hint })}
                        >
                            {parameter.choices.length > 0 ? (
                                <Select
                                    value={details.parameters[parameter.name] ?? ''}
                                    onChange={(event) =>
                                        setParameter(parameter.name, event.target.value)
                                    }
                                >
                                    {parameter.choices.map((choice) => (
                                        <option key={choice}>{choice}</option>
                                    ))}
                                </Select>
                            ) : (
                                <Input
                                    value={details.parameters[parameter.name] ?? ''}
                                    onChange={(event) =>
                                        setParameter(parameter.name, event.target.value)
                                    }
                                />
                            )}
                        </Field>
                    ))}
                </fieldset>
            )}

            <fieldset className="grid gap-4 sm:grid-cols-2">
                <legend className="mb-2 text-sm font-semibold">
                    {t('journey.details.administrator')}
                </legend>
                <Field label={t('journey.details.username')}>
                    <Input
                        value={details.adminUsername}
                        onChange={(event) => set('adminUsername', event.target.value)}
                    />
                </Field>
                <Field label={t('journey.details.email')}>
                    <Input
                        type="email"
                        value={details.adminEmail}
                        onChange={(event) => set('adminEmail', event.target.value)}
                    />
                </Field>
                {profile.inheritsAdminPassword && (
                    <label className="flex items-center gap-2 text-sm sm:col-span-2">
                        <input
                            type="checkbox"
                            checked={details.useMyPassword}
                            onChange={(event) =>
                                onChange({ ...details, useMyPassword: event.target.checked })
                            }
                        />
                        {t('journey.details.useMyPassword')}
                    </label>
                )}
                {!details.useMyPassword && (
                    <div className="sm:col-span-2">
                        <NewPasswordField
                            policy={policy}
                            label={t('journey.details.adminPassword')}
                            {...(passwordHint !== undefined && { hint: passwordHint })}
                            value={details.adminPassword}
                            onChange={(password, acceptable) => {
                                onChange({ ...details, adminPassword: password });
                                onPasswordAcceptable(acceptable);
                            }}
                        />
                    </div>
                )}
            </fieldset>

            {details.useMyPassword && creatingPassword === '' && (
                <Notice tone="warn">{t('journey.details.noCreatingPassword')}</Notice>
            )}
        </div>
    );
}

/** Everything the person is about to create, before it is created. */
export function TenantSummary({
    profile,
    details,
    creatingPassword,
}: {
    readonly profile: SeedProfileChoice;
    readonly details: TenantDetails;
    readonly creatingPassword: string;
}): ReactNode {
    const { t } = useTranslation();
    const password = administratorPassword(details, creatingPassword);
    return (
        <>
            <dl className="grid gap-2 text-sm sm:grid-cols-2">
                <dt className="text-ink-faint">{t('journey.review.startingPoint')}</dt>
                <dd>{profile.name}</dd>
                <dt className="text-ink-faint">{t('journey.review.tenant')}</dt>
                <dd>
                    {details.name} ({details.code})
                </dd>
                <dt className="text-ink-faint">{t('journey.review.hostname')}</dt>
                <dd>{details.hostname}</dd>
                <dt className="text-ink-faint">{t('journey.review.administrator')}</dt>
                <dd>{details.adminUsername}</dd>
                <dt className="text-ink-faint">{t('journey.review.password')}</dt>
                <dd>
                    {details.useMyPassword
                        ? t('journey.review.passwordMine')
                        : t('journey.review.passwordSet')}
                </dd>
                {profile.parameters.map((parameter) => (
                    <Fragment key={parameter.name}>
                        <dt className="text-ink-faint">{parameter.label}</dt>
                        <dd>{details.parameters[parameter.name] || '—'}</dd>
                    </Fragment>
                ))}
            </dl>
            <p className="mt-4 text-sm text-ink-muted">
                {t('journey.review.steps', { steps: profile.steps.length })}
            </p>
            {profile.forcePasswordChange && (
                <p className="mt-2 text-sm text-ink-muted">
                    {t('journey.review.forcedChange', { principal: details.adminUsername })}
                </p>
            )}
            {password === '' && (
                <div className="mt-4">
                    <Notice tone="warn">{t('journey.review.noPassword')}</Notice>
                </div>
            )}
        </>
    );
}

const STEP_MARK: Record<string, string> = {
    pending: '○',
    in_progress: '◐',
    completed: '●',
    completed_with_warnings: '●',
    failed: '✕',
    compensating: '◐',
    compensated: '○',
};

const STEP_TONE: Record<string, string> = {
    pending: 'text-ink-faint',
    in_progress: 'text-accent-bright',
    completed: 'text-up',
    completed_with_warnings: 'text-warn',
    failed: 'text-down',
    compensating: 'text-accent-bright',
    compensated: 'text-ink-faint',
};

/** The statuses a run is still working through. */
const OPEN_RUN = new Set(['', 'pending', 'in_progress', 'compensating']);

function stepLine(step: WorkflowStepSummary): ReactNode {
    const tone = STEP_TONE[step.status] ?? 'text-ink';
    return (
        <li key={step.id} className={cx('flex items-start gap-3 text-sm', tone)}>
            <span
                aria-hidden
                className={cx('w-4 text-center', step.status === 'in_progress' && 'animate-pulse')}
            >
                {STEP_MARK[step.status] ?? '○'}
            </span>
            <span className={step.status === 'pending' ? 'text-ink-faint' : 'text-ink'}>
                {step.name}
            </span>
            {step.error !== '' && <span className="text-xs text-down">{step.error}</span>}
        </li>
    );
}

/**
 * A provisioning run, followed by asking about it again.
 *
 * The read is the progress contract, so the page asks rather than subscribing:
 * every answer describes the run as it is now, and the interval stops when the
 * step unmounts. The retry is offered only where the run stopped, because that
 * is the only place the engine will resume from.
 */
export function RunProgress({
    server,
    instanceId,
    onComplete,
}: {
    readonly server: JourneyServer;
    readonly instanceId: string;
    readonly onComplete: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const [progress, setProgress] = useState<WorkflowProgress>();
    const [failure, setFailure] = useState<string>();
    const [retryNote, setRetryNote] = useState<string>();
    const [attempt, setAttempt] = useState(0);
    const advanced = useRef(false);

    const status = progress?.status ?? '';
    const open = OPEN_RUN.has(status);

    useEffect(() => {
        let cancelled = false;
        const ask = async (): Promise<void> => {
            try {
                const answer = await server.progress(instanceId);
                if (!cancelled) {
                    setProgress(answer);
                    setFailure(undefined);
                }
            } catch (error) {
                if (!cancelled) {
                    setFailure(reasonOf(error));
                }
            }
        };
        void ask();
        if (!open) {
            return () => {
                cancelled = true;
            };
        }
        const timer = setInterval(() => void ask(), POLL_INTERVAL_MS);
        return () => {
            cancelled = true;
            clearInterval(timer);
        };
    }, [server, instanceId, open, attempt]);

    useEffect(() => {
        if (status === 'completed' && !advanced.current) {
            advanced.current = true;
            onComplete();
        }
    }, [status, onComplete]);

    const retry = async (): Promise<void> => {
        setRetryNote(undefined);
        try {
            const answer = await server.retry(instanceId);
            setRetryNote(
                answer.success
                    ? t('journey.provisioning.retrying', { step: answer.stepName })
                    : answer.message,
            );
            // The interval stopped when the run stopped, so the run is asked
            // about once more and the interval comes back if it is moving.
            setAttempt((value) => value + 1);
        } catch (error) {
            setRetryNote(reasonOf(error));
        }
    };

    const steps = [...(progress?.steps ?? [])].sort(
        (left, right) => left.step_index - right.step_index,
    );

    return (
        <div>
            <ol className="space-y-2">{steps.map(stepLine)}</ol>
            {progress?.error !== undefined && progress.error !== '' && (
                <div className="mt-4">
                    <Notice tone="error">{progress.error}</Notice>
                </div>
            )}
            {failure !== undefined && (
                <div className="mt-4">
                    <Notice tone="error">
                        {t('journey.provisioning.readFailed', { message: failure })}
                    </Notice>
                </div>
            )}
            {status === 'failed' && (
                <div className="mt-4 flex flex-wrap items-center gap-3">
                    <Button variant="primary" size="sm" onClick={() => void retry()}>
                        {t('journey.provisioning.retry')}
                    </Button>
                    <span className="text-xs text-ink-faint">
                        {t('journey.provisioning.retryKeeps')}
                    </span>
                </div>
            )}
            {status === 'compensated' && (
                <div className="mt-4">
                    <Notice tone="warn">{t('journey.provisioning.rolledBack')}</Notice>
                </div>
            )}
            {retryNote !== undefined && (
                <div className="mt-4">
                    <Notice tone="info">{retryNote}</Notice>
                </div>
            )}
        </div>
    );
}
