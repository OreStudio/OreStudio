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
 * The registration door.
 *
 * The door asks the deployment one question before it offers anything, and the
 * answer decides which screen a person gets: a closed notice that names the
 * closure, or the rail the setup journeys use. The decision is a function of
 * what was read, so the branch is stated once, in one place, rather than
 * spelled out at each state's markup.
 *
 * The person is never signed in by a registration. An account that received a
 * party can sign in at once and is asked to; one that did not waits for an
 * administrator, and this screen is where it says so. That sentence is why the
 * door reads the policy at all: without it a person registers, and then fails
 * at the door with a message about a party they have never heard of.
 */

import { useCallback, useEffect, useState, type ReactNode } from 'react';
import { Link } from 'react-router';
import { useTranslation } from '../i18n/Provider.js';
import { Notice } from '../ui/Primitives.js';
import { JourneyPage } from './JourneyPage.js';
import { JourneySplash } from './parts.js';
import { EMPTY_DRAFT, signUpRequest, signUpSteps, type SignUpDraft } from './signUpSteps.js';
import type { JourneyStep } from './runtime.js';
import type {
    PasswordPolicy,
    RegistrationPolicyView,
    SignupRequest,
    SignupResult,
} from '@ores/wire-protocol/browser';

/** The door reaches the server through these three calls and nothing else. */
export interface SignUpServer {
    readonly registrationPolicy: () => Promise<RegistrationPolicyView>;
    readonly passwordPolicy: () => Promise<PasswordPolicy>;
    readonly signup: (request: SignupRequest) => Promise<SignupResult>;
}

/** What the door read, before any of it is rendered. */
export interface SignUpReading {
    readonly policy?: RegistrationPolicyView | undefined;
    readonly passwordPolicy?: PasswordPolicy | undefined;
    readonly failure?: string | undefined;
}

/** Which of the door's screens the reading calls for. */
export type DoorState =
    | { readonly kind: 'loading' }
    | { readonly kind: 'failed'; readonly reason: string }
    | { readonly kind: 'closed'; readonly code: string; readonly message: string }
    | {
          readonly kind: 'open';
          readonly policy: RegistrationPolicyView;
          readonly passwordPolicy: PasswordPolicy;
      };

/**
 * The branch, stated once.
 *
 * A refusal from the policy read is a closed door rather than a failure: the
 * deployment said what it offers, and the screen states it. A reading that did
 * not arrive is a failure, because nothing was said at all.
 */
export function doorState(reading: SignUpReading): DoorState {
    if (reading.failure !== undefined) {
        return { kind: 'failed', reason: reading.failure };
    }
    const { policy, passwordPolicy } = reading;
    if (policy === undefined || passwordPolicy === undefined) {
        return { kind: 'loading' };
    }
    if (!policy.success) {
        return { kind: 'closed', code: policy.errorCode, message: policy.message };
    }
    return { kind: 'open', policy, passwordPolicy };
}

/** The banner above the work, the same element the setup journeys carry. */
function SplashHeader(): ReactNode {
    return (
        <div className="mb-5 border-b border-line pb-5">
            <JourneySplash />
        </div>
    );
}

export interface SignUpDoorProps {
    readonly state: DoorState;
    /** The rail the open door renders; built by the caller that holds the draft. */
    readonly steps: readonly JourneyStep<ReactNode>[];
    readonly at: number;
    readonly onMove: (index: number) => void;
    readonly onRetry: () => void;
}

/**
 * The door's screens, as a function of the state.
 *
 * Presentational on purpose: the reading that decides the state and the draft
 * the rail edits both arrive as props, so every state can be rendered and
 * asserted without a server and without waiting for one.
 */
export function SignUpDoor({ state, steps, at, onMove, onRetry }: SignUpDoorProps): ReactNode {
    const { t } = useTranslation();

    if (state.kind === 'open') {
        return <JourneyPage steps={steps} at={at} onMove={onMove} header={<JourneySplash />} />;
    }

    return (
        <div className="card p-6">
            <SplashHeader />
            {state.kind === 'loading' && (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            )}
            {state.kind === 'failed' && (
                <>
                    <Notice tone="error">{t('signUp.unavailable')}</Notice>
                    <p className="mt-2 text-sm text-ink-faint">{state.reason}</p>
                </>
            )}
            {state.kind === 'closed' && (
                <>
                    <h1 className="text-lg font-semibold text-ink">{t('signUp.closedTitle')}</h1>
                    <div className="mt-4">
                        <Notice tone="warn">
                            {state.message !== '' ? state.message : t('signUp.closedFallback')}
                        </Notice>
                    </div>
                </>
            )}
            {state.kind !== 'loading' && (
                <p className="mt-6 text-sm text-ink-muted">
                    {state.kind === 'failed' && (
                        <button
                            type="button"
                            className="font-medium text-accent-bright hover:underline"
                            onClick={onRetry}
                        >
                            {t('signUp.tryAgain')}
                        </button>
                    )}
                    {state.kind === 'closed' && (
                        <>
                            {t('signUp.haveAccount')}{' '}
                            <Link
                                to="/login"
                                className="font-medium text-accent-bright hover:underline"
                            >
                                {t('signUp.goToSignIn')}
                            </Link>
                        </>
                    )}
                </p>
            )}
        </div>
    );
}

export interface SignUpJourneyProps {
    readonly server: SignUpServer;
}

function reasonOf(error: unknown): string {
    return error instanceof Error ? error.message : String(error);
}

export function SignUpJourney({ server }: SignUpJourneyProps): ReactNode {
    const { t } = useTranslation();
    const [reading, setReading] = useState<SignUpReading>({});
    const [draft, setDraft] = useState<SignUpDraft>(EMPTY_DRAFT);
    const [passwordAcceptable, setPasswordAcceptable] = useState(false);
    const [outcome, setOutcome] = useState<SignupResult>();
    const [at, setAt] = useState(0);

    const read = useCallback(async (): Promise<void> => {
        setReading({});
        try {
            const [policy, passwordPolicy] = await Promise.all([
                server.registrationPolicy(),
                server.passwordPolicy(),
            ]);
            setReading({ policy, passwordPolicy });
        } catch (error) {
            setReading({ failure: reasonOf(error) });
        }
    }, [server]);

    useEffect(() => {
        void read();
    }, [read]);

    const onChange = useCallback((patch: Partial<SignUpDraft>): void => {
        setDraft((current) => ({ ...current, ...patch }));
    }, []);

    const onCreate = useCallback(async (): Promise<void> => {
        const result = await server.signup(signUpRequest(draft));
        if (!result.success) {
            throw new Error(result.message !== '' ? result.message : t('signUp.closedFallback'));
        }
        setOutcome(result);
    }, [server, draft, t]);

    const state = doorState(reading);
    const steps =
        state.kind === 'open'
            ? signUpSteps({
                  t,
                  policy: state.policy,
                  draft,
                  passwordPolicy: state.passwordPolicy,
                  passwordAcceptable,
                  onChange,
                  onPasswordAcceptable: setPasswordAcceptable,
                  onCreate,
                  outcome,
              })
            : [];

    return (
        <SignUpDoor
            state={state}
            steps={steps}
            at={at}
            onMove={setAt}
            onRetry={() => void read()}
        />
    );
}
