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
 * The steps of the registration door.
 *
 * Three steps, and the write is deferred to the second: the person describes
 * the account, reads back what will be created and where it will land, and only
 * then asks for it. The destination is read back because it is not a question:
 * the tenant is resolved from the address and the party and role are the
 * deployment's choice, so the review step is where a person learns what they
 * will actually receive.
 *
 * The confirmation is one step with two endings. A tenant that nominates a
 * default party produces an account that signs in at once; one that does not
 * produces an account that waits for an administrator, and the person is told
 * which of the two they have rather than discovering it at the door.
 */

import { Fragment, type ReactNode } from 'react';
import { Field, Input, Notice } from '../ui/Primitives.js';
import { NewPasswordField, PasswordInput } from '../ui/PasswordField.js';
import { defineJourney, type JourneyStep } from './runtime.js';
import type {
    PasswordPolicy,
    RegistrationPolicyView,
    SignupRequest,
    SignupResult,
} from '@ores/wire-protocol/browser';

/** What the person has typed so far. */
export interface SignUpDraft {
    readonly principal: string;
    readonly email: string;
    readonly password: string;
    readonly confirmation: string;
}

export const EMPTY_DRAFT: SignUpDraft = {
    principal: '',
    email: '',
    password: '',
    confirmation: '',
};

const STEP_DETAILS = 'details';
const STEP_REVIEW = 'review';
const STEP_CONFIRMATION = 'confirmation';

/** Whether the details are complete enough to read back. */
export function detailsComplete(draft: SignUpDraft, passwordAcceptable: boolean): boolean {
    return (
        draft.principal.trim() !== '' &&
        draft.email.includes('@') &&
        passwordAcceptable &&
        draft.password !== '' &&
        draft.password === draft.confirmation
    );
}

export interface SignUpSteps {
    readonly t: (key: string, params?: Readonly<Record<string, string | number>>) => string;
    /** What the deployment said it offers, which is what the review reads back. */
    readonly policy: RegistrationPolicyView;
    readonly draft: SignUpDraft;
    readonly passwordPolicy: PasswordPolicy;
    readonly passwordAcceptable: boolean;
    readonly onChange: (patch: Partial<SignUpDraft>) => void;
    readonly onPasswordAcceptable: (acceptable: boolean) => void;
    /** Performs the registration. A refusal throws, and the page shows it. */
    readonly onCreate: () => Promise<void>;
    /** What the registration produced, once it has happened. */
    readonly outcome: SignupResult | undefined;
}

/** The request a registration makes, from what the person typed. */
export function signUpRequest(draft: SignUpDraft): SignupRequest {
    return {
        principal: draft.principal.trim(),
        email: draft.email.trim(),
        password: draft.password,
    };
}

function detailsStep(parts: SignUpSteps): JourneyStep<ReactNode> {
    const { t, draft, passwordPolicy, passwordAcceptable, onChange, onPasswordAcceptable } = parts;
    const mismatch = draft.confirmation !== '' && draft.password !== draft.confirmation;
    return {
        id: STEP_DETAILS,
        title: t('signUp.detailsTitle'),
        lead: t('signUp.detailsLead'),
        body: (
            <div className="space-y-4">
                <Field label={t('signUp.principal')}>
                    <Input
                        value={draft.principal}
                        autoComplete="username"
                        onChange={(event) => onChange({ principal: event.target.value })}
                    />
                </Field>
                <Field label={t('signUp.email')}>
                    <Input
                        type="email"
                        value={draft.email}
                        autoComplete="email"
                        onChange={(event) => onChange({ email: event.target.value })}
                    />
                </Field>
                <NewPasswordField
                    policy={passwordPolicy}
                    label={t('signUp.password')}
                    value={draft.password}
                    onChange={(password, acceptable) => {
                        onChange({ password });
                        onPasswordAcceptable(acceptable);
                    }}
                />
                <Field label={t('signUp.confirmation')}>
                    <PasswordInput
                        autoComplete="new-password"
                        value={draft.confirmation}
                        onChange={(event) => onChange({ confirmation: event.target.value })}
                    />
                </Field>
                {mismatch && <Notice tone="warn">{t('signUp.mismatch')}</Notice>}
            </div>
        ),
        next: {
            label: t('signUp.review'),
            enabled: detailsComplete(draft, passwordAcceptable),
        },
    };
}

/**
 * What will be created, and where it lands.
 *
 * The party is the one row that may be absent: a tenant that nominates no
 * default party still admits the registration, and the account it produces
 * waits. Saying so here is the whole reason the step exists, because the
 * alternative is a person who registers and then fails at the door with a
 * message about a party they have never heard of.
 */
function reviewStep(parts: SignUpSteps): JourneyStep<ReactNode> {
    const { t, policy, draft, onCreate } = parts;
    const rows: readonly (readonly [string, string])[] = [
        [t('signUp.principal'), draft.principal.trim()],
        [t('signUp.email'), draft.email.trim()],
        [t('signUp.tenant'), policy.tenantName],
        [t('signUp.party'), policy.partyName !== '' ? policy.partyName : t('signUp.noParty')],
        [t('signUp.role'), policy.roleName],
    ];
    return {
        id: STEP_REVIEW,
        title: t('signUp.reviewTitle'),
        lead: t('signUp.reviewLead'),
        body: (
            <div className="space-y-4">
                <dl className="grid gap-2 text-sm sm:grid-cols-2">
                    {rows.map(([label, value]) => (
                        <Fragment key={label}>
                            <dt className="text-ink-faint">{label}</dt>
                            <dd>{value}</dd>
                        </Fragment>
                    ))}
                </dl>
                <Notice tone={policy.usableNow ? 'info' : 'warn'}>
                    {policy.usableNow ? t('signUp.usableNow') : t('signUp.pending')}
                </Notice>
            </div>
        ),
        next: { label: t('signUp.create'), enabled: true, run: onCreate },
        final: true,
    };
}

function confirmationStep(parts: SignUpSteps): JourneyStep<ReactNode> {
    const { t, policy, outcome } = parts;
    const waiting = outcome?.accountStatus === 'pending';
    return {
        id: STEP_CONFIRMATION,
        title: waiting ? t('signUp.waitingTitle') : t('signUp.createdTitle'),
        lead: waiting ? t('signUp.waitingLead') : t('signUp.createdLead'),
        body: (
            <div className="space-y-3 text-sm">
                {outcome !== undefined && outcome.roleId !== '' && (
                    <p>
                        {t('signUp.received', { role: policy.roleName, tenant: policy.tenantName })}
                    </p>
                )}
                {waiting && <Notice tone="warn">{t('signUp.waitingFor')}</Notice>}
            </div>
        ),
    };
}

/** The rail, in the order the person walks it. */
export function signUpSteps(parts: SignUpSteps): readonly JourneyStep<ReactNode>[] {
    return defineJourney([detailsStep(parts), reviewStep(parts), confirmationStep(parts)]);
}
