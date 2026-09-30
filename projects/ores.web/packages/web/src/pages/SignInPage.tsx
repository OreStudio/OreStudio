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

import { useState, type FormEvent, type ReactNode } from 'react';
import { Link } from 'react-router';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Field, Input, Notice } from '../ui/Primitives.js';
import { PasswordInput } from '../ui/PasswordField.js';
import { JourneySplash } from '../journeys/parts.js';
import type { SignInOutcome } from '../session/SessionProvider.js';
import type { PartySummary } from '@ores/wire-protocol/browser';

/**
 * Signing in.
 *
 * The screen the Entry story finishes. What it does here is the path that
 * exists: an identity, a password, and the choice of party when the account
 * works in more than one. Self registration, the recovery paths and the words
 * for each refusal belong to that story.
 *
 * The server refuses a sign-in in bootstrap mode, and this page is not reached
 * then: the gate renders the setup page instead. Both are deliberate, and the
 * server's refusal is what makes the rule hold for a caller that ignores this
 * one.
 *
 * The banner stands above the work and the work stays below it, because the
 * party picker replaces the form rather than following it: a screen that drew
 * the banner inside one branch would lose it the moment the branch changed.
 */
export interface SignInPageProps {
    readonly onSignIn: (credentials: {
        readonly username: string;
        readonly password: string;
    }) => Promise<SignInOutcome>;
    readonly onChooseParty: (partyId: string, parties: readonly PartySummary[]) => Promise<void>;
}

export function SignInPage({ onSignIn, onChooseParty }: SignInPageProps): ReactNode {
    const { t } = useTranslation();
    const [username, setUsername] = useState('');
    const [password, setPassword] = useState('');
    const [parties, setParties] = useState<readonly PartySummary[] | undefined>(undefined);
    const [failure, setFailure] = useState<string | undefined>(undefined);
    const [busy, setBusy] = useState(false);

    const submit = async (event: FormEvent): Promise<void> => {
        event.preventDefault();
        setFailure(undefined);
        setBusy(true);
        try {
            const outcome = await onSignIn({ username, password });
            if (outcome.outcome === 'party-required') {
                setParties(outcome.parties);
            }
        } catch (error) {
            setFailure(error instanceof Error ? error.message : String(error));
        } finally {
            setBusy(false);
        }
    };

    const picker = (
        <>
            <h1 className="text-lg font-semibold text-ink">{t('signIn.chooseParty')}</h1>
            <p className="mt-2 text-sm text-ink-muted">{t('signIn.choosePartyHint')}</p>
            <ul className="mt-4 space-y-2">
                {(parties ?? []).map((party) => (
                    <li key={party.id}>
                        <Button
                            variant="secondary"
                            className="w-full justify-start"
                            onClick={() => {
                                void onChooseParty(party.id, parties ?? []);
                            }}
                        >
                            {party.name}
                        </Button>
                    </li>
                ))}
            </ul>
        </>
    );

    const form = (
        <form onSubmit={(event) => void submit(event)}>
            <h1 className="text-lg font-semibold text-ink">{t('signIn.title')}</h1>
            {failure !== undefined && (
                <div className="mt-4">
                    <Notice tone="error">{failure}</Notice>
                </div>
            )}
            <div className="mt-4 space-y-4">
                <Field label={t('signIn.username')}>
                    <Input
                        value={username}
                        autoComplete="username"
                        onChange={(event) => setUsername(event.target.value)}
                    />
                </Field>
                <Field label={t('signIn.password')}>
                    <PasswordInput
                        autoComplete="current-password"
                        value={password}
                        onChange={(event) => setPassword(event.target.value)}
                    />
                </Field>
            </div>
            <div className="mt-6 flex items-center justify-between gap-3">
                <p className="text-sm text-ink-muted">
                    {t('signIn.noAccount')}{' '}
                    <Link to="/signup" className="font-medium text-accent-bright hover:underline">
                        {t('signIn.createOne')}
                    </Link>
                </p>
                <Button
                    type="submit"
                    variant="primary"
                    pending={busy}
                    pendingLabel={t('signIn.submitting')}
                >
                    {t('signIn.submit')}
                </Button>
            </div>
        </form>
    );

    return (
        <div className="card p-6">
            <div className="mb-5 border-b border-line pb-5">
                <JourneySplash />
            </div>
            {parties !== undefined ? picker : form}
        </div>
    );
}
