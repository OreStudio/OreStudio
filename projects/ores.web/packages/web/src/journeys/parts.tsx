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
import { heroSplash } from '../assets/brand.js';
import { profileLogo } from '../assets/profiles.js';
import { codeAsTyped, codeFromName, emailFromPrincipal, hostnameFromName } from './derive.js';
import { LegalEntitySearch } from './LegalEntitySearch.js';
import { Button, Field, Input, Notice, Select, cx } from '../ui/Primitives.js';
import { NewPasswordField } from '../ui/PasswordField.js';
import type { JourneyServer } from './server.js';
import { administratorPassword, type NewTenant, type TenantDetails } from './state.js';
import type {
    PasswordPolicy,
    SeedProfileChoice,
    WorkflowProgress,
    WorkflowStepSummary,
} from '@ores/wire-protocol/browser';

/** How often an open run is asked about again. */
const POLL_INTERVAL_MS = 1500;

/**
 * The project's banner.
 *
 * One element rather than one per screen: the setup journeys carry it as
 * `JourneyPage`'s header, and every Entry dialog carries it above its work, so
 * a person sees the same artwork at every door into the deployment. A screen
 * that draws its own banner is a defect this exists to prevent.
 *
 * The artwork's own dimensions are stated, so the panel reserves the room
 * before the image arrives and the width it has scales it by the ratio
 * 964:323. Nothing here states a height: a height that is not the ratio's is
 * what cut the top and the foot off the wordmark.
 */
export function JourneySplash(): ReactNode {
    return (
        <img
            src={heroSplash}
            alt=""
            width={964}
            height={323}
            className="h-auto w-full rounded-md border border-line"
        />
    );
}

/**
 * What the panel says above every step of a tenant journey.
 *
 * The splash frames the panel, and once a starting point is chosen the tenant
 * being described stands beside it: the form, the review, the run and the
 * hand-over are all about that tenant, and a person who looked away should not
 * have to walk back up the rail to remember which one it is. Both tenant
 * journeys show it, so it lives with the parts they share.
 */
export function JourneyHeader({ tenant }: { readonly tenant: NewTenant }): ReactNode {
    const tenantName = tenant.details?.name ?? '';
    const profileName = tenant.profile?.name ?? '';
    /*
     * The tenant's name is what the person typed or the profile proposed, so it
     * is the heading when there is one and the starting point is only what it
     * was built from. Before that there is no tenant to name, and the heading
     * is the starting point itself: one line, never the same line twice.
     */
    const heading = tenantName !== '' ? tenantName : profileName;
    const builtFrom = tenantName !== '' ? profileName : '';
    return (
        <div>
            <JourneySplash />
            {tenant.profile !== undefined && (
                <div className="mt-3 flex items-baseline justify-between gap-3 text-sm">
                    <span className="truncate font-medium">{heading}</span>
                    {builtFrom !== '' && (
                        <span className="truncate text-xs text-ink-faint">{builtFrom}</span>
                    )}
                </div>
            )}
        </div>
    );
}

function reasonOf(error: unknown): string {
    return error instanceof Error ? error.message : String(error);
}

/** Whether a profile creates a tenant whose code the deployment already holds. */
export function isProfileTaken(
    profile: SeedProfileChoice,
    takenCodes: ReadonlySet<string>,
): boolean {
    return profile.tenant.code !== '' && takenCodes.has(profile.tenant.code);
}

/**
 * The starting points, as cards.
 *
 * Each card states what the server said about its profile — its bullets, how
 * many settings it declares and how many steps it orders — so the screen
 * describes the deployment's rows rather than a shape written here.
 *
 * A profile that names its own tenant code creates that tenant, and a code is
 * unique, so a card whose tenant already exists is shown but cannot be chosen.
 * It carries a green tick instead of a sentence: the person only needs to see
 * that this one is already installed.
 */
export function ProfileCards({
    profiles,
    selected,
    onSelect,
    takenCodes = new Set<string>(),
}: {
    readonly profiles: readonly SeedProfileChoice[];
    readonly selected: string | undefined;
    readonly onSelect: (profile: SeedProfileChoice) => void;
    /** The tenant codes the deployment already holds. */
    readonly takenCodes?: ReadonlySet<string>;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <div role="radiogroup" className="grid gap-3 sm:grid-cols-2">
            {profiles.map((profile) => {
                const logo = profileLogo(profile.code);
                const taken = isProfileTaken(profile, takenCodes);
                return (
                    <button
                        key={profile.code}
                        type="button"
                        role="radio"
                        aria-checked={selected === profile.code}
                        disabled={taken}
                        onClick={() => onSelect(profile)}
                        className={cx(
                            'card relative p-4 text-left transition-colors',
                            taken
                                ? 'cursor-not-allowed'
                                : selected === profile.code
                                  ? 'border-accent ring-3 ring-accent/20'
                                  : 'hover:border-line-strong',
                        )}
                    >
                        {/*
                         * The mark sits outside the dimmed content, so the
                         * green reads at full strength on a card that is
                         * otherwise faded.
                         */}
                        {taken && (
                            <span className="absolute right-3 top-3 inline-flex items-center gap-1 rounded-full border border-up/40 bg-up/10 px-2 py-0.5 text-[11px] text-up">
                                <span aria-hidden>✓</span>
                                {t('journey.profile.installed')}
                            </span>
                        )}
                        <div className={cx(taken && 'opacity-50')}>
                            {logo !== undefined && (
                                <img
                                    src={logo}
                                    alt=""
                                    className="mb-3 h-10 w-auto rounded-md bg-white p-1"
                                />
                            )}
                            <div className="flex items-baseline justify-between gap-2">
                                <span className="font-semibold">{profile.name}</span>
                                {!taken && (
                                    <span className="text-xs text-ink-faint">
                                        {profile.audience}
                                    </span>
                                )}
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
                        </div>
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
    server,
    profile,
    details,
    policy,
    creatingPassword,
    onChange,
    onPasswordAcceptable,
}: {
    readonly server: JourneyServer;
    readonly profile: SeedProfileChoice;
    readonly details: TenantDetails;
    readonly policy: PasswordPolicy;
    readonly creatingPassword: string;
    readonly onChange: (details: TenantDetails) => void;
    readonly onPasswordAcceptable: (acceptable: boolean) => void;
}): ReactNode {
    const { t } = useTranslation();
    const [open, setOpen] = useState(profile.tenant.name === '');
    /*
     * The fields a person has typed in themselves. A proposal stops the moment
     * somebody edits the field it proposes, which is what the old client did:
     * the derivation helps while a form is being filled, and it never
     * overwrites what somebody wrote.
     */
    const [touched, setTouched] = useState<ReadonlySet<string>>(new Set());

    const set = (
        key: 'name' | 'code' | 'hostname' | 'adminUsername' | 'adminEmail',
        value: string,
    ): void => {
        setTouched((current) => new Set(current).add(key));
        onChange({ ...details, [key]: value });
    };

    /**
     * What choosing a legal entity proposes: the tenant is named after it, the
     * code follows the name, the hostname follows the code, the administrator
     * is addressed at the tenant, and the entity's LEI is what the run imports.
     */
    const chooseEntity = (parameter: string, legalName: string, lei: string): void => {
        const code = touched.has('code') ? details.code : codeFromName(legalName);
        const hostname = touched.has('hostname') ? details.hostname : hostnameFromName(legalName);
        onChange({
            ...details,
            name: touched.has('name') ? details.name : legalName,
            code,
            hostname,
            adminEmail: touched.has('adminEmail')
                ? details.adminEmail
                : emailFromPrincipal(details.adminUsername, hostname),
            parameters: { ...details.parameters, [parameter]: lei },
        });
    };
    const setParameter = (name: string, value: string): void =>
        onChange({ ...details, parameters: { ...details.parameters, [name]: value } });

    /*
     * The section a profile's settings are stated in. It leads when one of them
     * chooses the entity the tenant is built around, because everything else on
     * the form follows from that choice; the manual card states the tenant
     * first and its settings after, because there the tenant is the person's own.
     */
    const settings = profile.parameters.length > 0 && (
        <fieldset className="grid gap-4 sm:grid-cols-2">
            <legend className="mb-2 text-sm font-semibold">
                {t('journey.details.settings', { profile: profile.name })}
            </legend>
            {profile.parameters.map((parameter) => (
                <Field
                    key={parameter.name}
                    className={parameter.dataType === 'legal_entity' ? 'sm:col-span-2' : ''}
                    label={parameter.dataType === 'legal_entity' ? '' : parameter.label}
                    {...(parameter.dataType !== 'legal_entity' &&
                        parameter.hint !== '' && { hint: parameter.hint })}
                >
                    {parameter.dataType === 'legal_entity' ? (
                        <LegalEntitySearch
                            server={server}
                            value={details.parameters[parameter.name] ?? ''}
                            label={parameter.label}
                            hint={[parameter.hint, t('journey.details.leiHowItWorks')]
                                .filter((sentence) => sentence !== '')
                                .join(' ')}
                            onChoose={(entity) =>
                                chooseEntity(parameter.name, entity.legalName, entity.lei)
                            }
                        />
                    ) : parameter.choices.length > 0 ? (
                        <Select
                            value={details.parameters[parameter.name] ?? ''}
                            onChange={(event) => setParameter(parameter.name, event.target.value)}
                        >
                            {parameter.choices.map((choice) => (
                                <option key={choice}>{choice}</option>
                            ))}
                        </Select>
                    ) : (
                        <Input
                            value={details.parameters[parameter.name] ?? ''}
                            onChange={(event) => setParameter(parameter.name, event.target.value)}
                        />
                    )}
                </Field>
            ))}
        </fieldset>
    );

    const settingsLead = profile.parameters.some(
        (parameter) => parameter.dataType === 'legal_entity',
    );

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
            {settingsLead && settings}

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
                        onChange={(event) => set('code', codeAsTyped(event.target.value))}
                    />
                </Field>
                <Field label={t('journey.details.hostname')} className="sm:col-span-2">
                    <Input
                        value={details.hostname}
                        onChange={(event) => set('hostname', event.target.value)}
                    />
                </Field>
            </fieldset>

            {!settingsLead && settings}

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
                {/*
                 * The offer stands only while there is a password to hand over.
                 * A journey that holds none cannot keep it, and offering a
                 * choice that leaves the step unable to continue is worse than
                 * not offering it.
                 */}
                {profile.inheritsAdminPassword && creatingPassword !== '' && (
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

/**
 * One line of the rail.
 *
 * The run states each step's name and description in a person's words, and the
 * catalogue translates them where somebody has; a run started before a step
 * had words shows the step's identity instead, which is at least the name the
 * server logs.
 */
export function RunStep({
    step,
    words,
}: {
    readonly step: WorkflowStepSummary;
    /** Translates a sentence the server sent, or returns it unchanged. */
    readonly words: (sentence: string) => string;
}): ReactNode {
    const tone = STEP_TONE[step.status] ?? 'text-ink';
    const label = step.label !== '' ? words(step.label) : step.name;
    /*
     * Why a step finished with a warning. The step's own sentence is the last
     * thing it logged, which is how a step says it had nothing to do rather
     * than failing: a person who sees a warning on the rail is owed the reason
     * beside it.
     */
    const lastLogged = step.log.length > 0 ? step.log[step.log.length - 1] : undefined;
    const warning =
        step.status === 'completed_with_warnings' && lastLogged !== undefined
            ? words(lastLogged.message)
            : '';
    return (
        <li key={step.id} className={cx('flex items-start gap-3 text-sm', tone)}>
            <span
                aria-hidden
                className={cx('w-4 text-center', step.status === 'in_progress' && 'animate-pulse')}
            >
                {STEP_MARK[step.status] ?? '○'}
            </span>
            <span className={step.status === 'pending' ? 'text-ink-faint' : 'text-ink'}>
                <span className="block">{label}</span>
                {step.description !== '' && (
                    <span className="mt-0.5 block text-xs text-ink-faint">
                        {words(step.description)}
                    </span>
                )}
                {warning !== '' && (
                    <span className="mt-0.5 block text-xs text-warn">{warning}</span>
                )}
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
    onCompleted,
}: {
    readonly server: JourneyServer;
    readonly instanceId: string;
    /**
     * Called once the run has completed.
     *
     * The step does not move the rail itself: the person has just watched work
     * happen and may want to read what it did, so the step reports the outcome
     * and the panel's own action is what carries them on.
     */
    readonly onCompleted: () => void;
}): ReactNode {
    const { t, text } = useTranslation();
    const [progress, setProgress] = useState<WorkflowProgress>();
    const [failure, setFailure] = useState<string>();
    const [retryNote, setRetryNote] = useState<string>();
    const [attempt, setAttempt] = useState(0);
    const reported = useRef(false);

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
        if (status === 'completed' && !reported.current) {
            reported.current = true;
            onCompleted();
        }
    }, [status, onCompleted]);

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
            <ol className="space-y-2">
                {steps.map((step) => (
                    <RunStep key={step.id} step={step} words={text} />
                ))}
            </ol>
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
