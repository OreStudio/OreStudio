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
 * The tenant administrator's first use of their account.
 *
 * It is one step because it is one person's arrival: they sign in — or their
 * browser already did, when they took over from the person who provisioned the
 * tenant — choose the party to work in when their account works in more than
 * one, and set a password of their own when the starting point asked for one.
 *
 * The change comes before anything else the account does, and the rules it
 * shows are the server's: the field is handed the policy the deployment
 * answered with, so a rule that changes on the server changes here.
 */

import { useState, type FormEvent, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Field, Input, Notice } from '../ui/Primitives.js';
import { NewPasswordField, PasswordInput } from '../ui/PasswordField.js';
import type { JourneyServer } from './server.js';
import type { PasswordPolicy, PartySummary } from '@ores/wire-protocol/browser';

/**
 * What the journey knows about the tenant administrator's session.
 *
 * `sign-in` is the person who was handed the account: nobody has signed in as
 * it yet in this browser. `party` and `active` are the browser that provisioned
 * the tenant and took the account over.
 */
export type TenantEntry =
    | { readonly kind: 'sign-in'; readonly principal: string; readonly password: string }
    | {
          readonly kind: 'party';
          readonly principal: string;
          readonly password: string;
          readonly parties: readonly PartySummary[];
          readonly resetRequired: boolean;
      }
    | {
          readonly kind: 'active';
          readonly principal: string;
          readonly password: string;
          readonly resetRequired: boolean;
      };

function reasonOf(error: unknown): string {
    return error instanceof Error ? error.message : String(error);
}

export function FirstSignIn({
    server,
    policy,
    entry,
    onDone,
}: {
    readonly server: JourneyServer;
    readonly policy: PasswordPolicy;
    readonly entry: TenantEntry;
    readonly onDone: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const [current, setCurrent] = useState<TenantEntry>(entry);
    const [username, setUsername] = useState(entry.principal);
    const [password, setPassword] = useState(entry.password);
    const [changed, setChanged] = useState(false);
    const [chosen, setChosen] = useState('');
    const [acceptable, setAcceptable] = useState(false);
    const [failure, setFailure] = useState<string>();
    const [busy, setBusy] = useState(false);

    const run = async (action: () => Promise<void>): Promise<void> => {
        setFailure(undefined);
        setBusy(true);
        try {
            await action();
        } catch (error) {
            setFailure(reasonOf(error));
        } finally {
            setBusy(false);
        }
    };

    const signIn = (event: FormEvent): void => {
        event.preventDefault();
        void run(async () => {
            const outcome = await server.signIn({ username, password });
            setCurrent(
                outcome.outcome === 'party-required'
                    ? {
                          kind: 'party',
                          principal: username,
                          password,
                          parties: outcome.parties,
                          resetRequired: outcome.passwordResetRequired,
                      }
                    : {
                          kind: 'active',
                          principal: username,
                          password,
                          resetRequired: outcome.passwordResetRequired,
                      },
            );
        });
    };

    const chooseParty = (party: PartySummary): void => {
        if (current.kind !== 'party') {
            return;
        }
        void run(async () => {
            await server.chooseParty(party.id, current.parties);
            setCurrent({
                kind: 'active',
                principal: current.principal,
                password: current.password,
                resetRequired: current.resetRequired,
            });
        });
    };

    const changePassword = (event: FormEvent): void => {
        event.preventDefault();
        if (current.kind !== 'active') {
            return;
        }
        void run(async () => {
            await server.changePassword(current.password, chosen);
            setChanged(true);
        });
    };

    if (current.kind === 'sign-in') {
        return (
            <form className="space-y-4" onSubmit={signIn}>
                {failure !== undefined && <Notice tone="error">{failure}</Notice>}
                <Field label={t('journey.signIn.username')}>
                    <Input
                        value={username}
                        autoComplete="username"
                        onChange={(event) => setUsername(event.target.value)}
                    />
                </Field>
                <Field label={t('journey.signIn.password')}>
                    <PasswordInput
                        value={password}
                        autoComplete="current-password"
                        onChange={(event) => setPassword(event.target.value)}
                    />
                </Field>
                <div className="flex justify-end">
                    <Button
                        type="submit"
                        variant="primary"
                        disabled={username === '' || password === ''}
                        pending={busy}
                        pendingLabel={t('journey.signIn.submitting')}
                    >
                        {t('journey.signIn.submit')}
                    </Button>
                </div>
            </form>
        );
    }

    if (current.kind === 'party') {
        return (
            <div className="space-y-4">
                <p className="text-sm text-ink-muted">{t('journey.signIn.choosePartyHint')}</p>
                {failure !== undefined && <Notice tone="error">{failure}</Notice>}
                <ul className="space-y-2">
                    {current.parties.map((party) => (
                        <li key={party.id}>
                            <Button
                                variant="secondary"
                                className="w-full justify-start"
                                disabled={busy}
                                onClick={() => chooseParty(party)}
                            >
                                {party.name}
                            </Button>
                        </li>
                    ))}
                </ul>
            </div>
        );
    }

    if (current.resetRequired && !changed) {
        return (
            <form className="space-y-4" onSubmit={changePassword}>
                <Notice tone="info">{t('journey.change.required')}</Notice>
                {failure !== undefined && <Notice tone="error">{failure}</Notice>}
                <NewPasswordField
                    policy={policy}
                    label={t('journey.change.new')}
                    value={chosen}
                    onChange={(value, ok) => {
                        setChosen(value);
                        setAcceptable(ok);
                    }}
                />
                <div className="flex justify-end">
                    <Button
                        type="submit"
                        variant="primary"
                        disabled={!acceptable}
                        pending={busy}
                        pendingLabel={t('journey.change.submitting')}
                    >
                        {t('journey.change.submit')}
                    </Button>
                </div>
            </form>
        );
    }

    return (
        <div className="space-y-4">
            <p className="text-sm text-ink-muted">
                {changed
                    ? t('journey.change.done', { principal: current.principal })
                    : t('journey.signIn.done', { principal: current.principal })}
            </p>
            {failure !== undefined && <Notice tone="error">{failure}</Notice>}
            <div className="flex justify-end">
                <Button variant="primary" onClick={onDone}>
                    {t('common.continue')}
                </Button>
            </div>
        </div>
    );
}
