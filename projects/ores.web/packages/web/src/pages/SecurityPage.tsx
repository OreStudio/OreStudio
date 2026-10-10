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

import { useCallback, useEffect, useState, type ReactNode } from 'react';
import { api } from '../api/client.js';
import { SignInFacts } from '../access/SignInFacts.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Detail, Field, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { NewPasswordField, PasswordInput } from '../ui/PasswordField.js';
import { RefreshButton } from '../ui/RefreshButton.js';
import { RelativeTime } from '../ui/Time.js';
import type { LoginInfo, PasswordPolicy, Session, SessionView } from '@ores/wire-protocol/browser';

/**
 * Protect my account, the member's own screen.
 *
 * The journey is
 * `doc/knowledge/journeys/credentials/journey_protect_my_account.org`, and the
 * screen is the accepted prototype's variant B: the places the account is
 * signed in lead, because that is what a member opens *Security* to read, and
 * the password form and the sign-in state share the row under them.
 */

interface Loaded {
    readonly policy: PasswordPolicy;
    readonly loginInfo: LoginInfo | null;
    readonly sessions: readonly Session[];
}

type State =
    | { readonly kind: 'loading' }
    | { readonly kind: 'ready'; readonly loaded: Loaded }
    | { readonly kind: 'failed'; readonly reason: string };

export function SecurityPage({ session }: { readonly session: SessionView }): ReactNode {
    const { t } = useTranslation();
    const [state, setState] = useState<State>({ kind: 'loading' });

    const load = useCallback(async (): Promise<void> => {
        try {
            const [policy, loginInfo, sessions] = await Promise.all([
                api.passwordPolicy(),
                api.loginInfo(session.accountId),
                api.mySessions(),
            ]);
            setState({ kind: 'ready', loaded: { policy, loginInfo, sessions } });
        } catch (error) {
            setState({
                kind: 'failed',
                reason: error instanceof Error ? error.message : t('security.readFailed'),
            });
        }
    }, [session.accountId, t]);

    useEffect(() => {
        void load();
    }, [load]);

    return (
        <div className="mx-auto max-w-[1100px] space-y-6">
            <PageHeader title={t('security.title')} description={t('security.lead')} />
            {state.kind === 'loading' && <Notice tone="info">{t('security.reading')}</Notice>}
            {state.kind === 'failed' && <Notice tone="error">{state.reason}</Notice>}
            {state.kind === 'ready' && (
                <Loaded session={session} loaded={state.loaded} onReload={load} />
            )}
        </div>
    );
}

function Loaded({
    session,
    loaded,
    onReload,
}: {
    readonly session: SessionView;
    readonly loaded: Loaded;
    readonly onReload: () => Promise<void>;
}): ReactNode {
    return (
        <div className="space-y-6">
            <SessionsPanel sessions={loaded.sessions} onReload={onReload} />
            <div className="grid gap-6 lg:grid-cols-2">
                <PasswordPanel policy={loaded.policy} onChanged={onReload} />
                <SignInStatePanel session={session} state={loaded.loginInfo} />
            </div>
        </div>
    );
}

/**
 * The places the account is signed in, as the server lists them. The list leads
 * the screen because it is the reason a member opens *Security*.
 */
function SessionsPanel({
    sessions,
    onReload,
}: {
    readonly sessions: readonly Session[];
    readonly onReload: () => Promise<void>;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('security.sessions.title')}</h2>
                <RefreshButton onClick={() => void onReload()} />
            </header>

            <Notice tone="warn">{t('security.sessions.caveat')}</Notice>

            {sessions.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('security.sessions.empty')}</p>
            ) : (
                <div className="overflow-x-auto">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                <th className="py-2 pr-4 font-medium">
                                    {t('security.sessions.started')}
                                </th>
                                <th className="py-2 pr-4 font-medium">
                                    {t('security.sessions.client')}
                                </th>
                                <th className="py-2 pr-4 font-medium">
                                    {t('security.sessions.address')}
                                </th>
                                <th className="py-2 pr-4 font-medium">
                                    {t('security.sessions.country')}
                                </th>
                                <th className="py-2 text-right font-medium">
                                    {t('security.sessions.traffic')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {sessions.map((row) => (
                                <tr
                                    key={row.id}
                                    className="border-b border-line-subtle last:border-b-0"
                                >
                                    <td className="py-2 pr-4 whitespace-nowrap">
                                        <RelativeTime at={row.startTime} />
                                    </td>
                                    <td className="py-2 pr-4 font-mono text-xs">
                                        {row.clientIdentifier === ''
                                            ? t('security.sessions.unknownClient')
                                            : row.clientIdentifier}
                                    </td>
                                    <td className="py-2 pr-4 font-mono text-xs">
                                        {row.clientIp === ''
                                            ? t('security.sessions.noAddress')
                                            : row.clientIp}
                                    </td>
                                    <td className="py-2 pr-4 text-ink-muted">
                                        {row.countryCode === ''
                                            ? t('security.sessions.unknownCountry')
                                            : row.countryCode}
                                    </td>
                                    <td className="py-2 text-right font-mono text-xs text-ink-faint whitespace-nowrap">
                                        {String(row.bytesSent)} / {String(row.bytesReceived)}
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            )}
        </section>
    );
}

/**
 * The password form.
 *
 * The rules come from the server, so the screen states the policy the server
 * applies rather than keeping a copy of it. The current password travels with
 * the change because the server verifies it before it writes anything.
 */
function PasswordPanel({
    policy,
    onChanged,
}: {
    readonly policy: PasswordPolicy;
    readonly onChanged: () => Promise<void>;
}): ReactNode {
    const { t } = useTranslation();
    const [current, setCurrent] = useState('');
    const [chosen, setChosen] = useState('');
    const [acceptable, setAcceptable] = useState(false);
    const [busy, setBusy] = useState(false);
    const [outcome, setOutcome] = useState<
        { readonly ok: boolean; readonly text: string } | undefined
    >(undefined);

    const submit = async (): Promise<void> => {
        setBusy(true);
        setOutcome(undefined);
        try {
            await api.changePassword(current, chosen);
            setCurrent('');
            setChosen('');
            setAcceptable(false);
            setOutcome({ ok: true, text: t('security.password.changed') });
            await onChanged();
        } catch (error) {
            setOutcome({
                ok: false,
                text: error instanceof Error ? error.message : t('security.password.refused'),
            });
        } finally {
            setBusy(false);
        }
    };

    return (
        <section className="card space-y-4 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">{t('security.password.title')}</h2>
                <p className="text-sm text-ink-muted">{t('security.password.lead')}</p>
            </header>
            <Field label={t('security.password.current')} hint={t('security.password.currentHint')}>
                <PasswordInput
                    value={current}
                    autoComplete="current-password"
                    onChange={(event) => setCurrent(event.target.value)}
                />
            </Field>
            <NewPasswordField
                policy={policy}
                label={t('security.password.new')}
                value={chosen}
                onChange={(value, ok) => {
                    setChosen(value);
                    setAcceptable(ok);
                }}
            />
            {outcome !== undefined && (
                <Notice tone={outcome.ok ? 'success' : 'error'}>{outcome.text}</Notice>
            )}
            <div className="flex justify-end">
                <Button
                    variant="primary"
                    disabled={current.length === 0 || !acceptable}
                    pending={busy}
                    onClick={() => void submit()}
                >
                    {t('security.password.submit')}
                </Button>
            </div>
        </section>
    );
}

/**
 * What the server says about the account's sign-ins. Read only, because the
 * server writes this state as a side effect of signing in.
 */
function SignInStatePanel({
    session,
    state,
}: {
    readonly session: SessionView;
    readonly state: LoginInfo | null;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card space-y-4 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">{t('security.state.title')}</h2>
                <p className="text-sm text-ink-muted">{t('security.state.lead')}</p>
            </header>
            <div className="flex flex-wrap gap-2">
                <Tag tone="neutral">{session.username}</Tag>
                {session.passwordResetRequired && (
                    <Tag tone="warn">{t('security.state.resetRequired')}</Tag>
                )}
            </div>
            <SignInFacts state={state}>
                {state !== null && (
                    <Detail
                        label={t('security.state.account')}
                        value={state.locked ? t('signInFacts.locked') : t('signInFacts.notLocked')}
                    />
                )}
            </SignInFacts>
        </section>
    );
}
