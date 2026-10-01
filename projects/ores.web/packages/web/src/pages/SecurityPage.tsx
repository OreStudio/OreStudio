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
import { Button, Detail, Field, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { NewPasswordField, PasswordInput } from '../ui/PasswordField.js';
import type { LoginInfo, PasswordPolicy, Session, SessionView } from '@ores/wire-protocol/browser';

/**
 * Protect my account, the member's own screen.
 *
 * The journey is
 * `doc/knowledge/journeys/credentials/journey_protect_my_account.org`, and the
 * screen is the accepted prototype's variant B: the places the account is
 * signed in lead, because that is what a member opens *Security* to read, and
 * the password form and the sign-in state share the row under them.
 *
 * Two things the server cannot do yet are stated rather than hidden. Ending one
 * other sign-in has no operation, so the control is drawn unavailable with its
 * reason. And nothing writes a session's end time, so the list is every session
 * the deployment has created, not the open ones.
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
    const [state, setState] = useState<State>({ kind: 'loading' });

    const load = useCallback(async (): Promise<void> => {
        try {
            const [policy, loginInfo, sessions] = await Promise.all([
                api.passwordPolicy(),
                api.loginInfo(session.accountId),
                api.activeSessions(),
            ]);
            setState({ kind: 'ready', loaded: { policy, loginInfo, sessions } });
        } catch (error) {
            setState({
                kind: 'failed',
                reason: error instanceof Error ? error.message : 'The read failed.',
            });
        }
    }, [session.accountId]);

    useEffect(() => {
        void load();
    }, [load]);

    return (
        <div className="mx-auto max-w-[1100px] space-y-6">
            <PageHeader
                title="Security"
                description="Your password, and the places your account is signed in."
            />
            {state.kind === 'loading' && <Notice tone="info">Reading your account…</Notice>}
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
 * The places the account is signed in.
 *
 * The list is the server's, and it leads the screen because it is the reason a
 * member opens *Security*. Every row carries what the read gives: the client,
 * the address, the country and when it started.
 */
function SessionsPanel({
    sessions,
    onReload,
}: {
    readonly sessions: readonly Session[];
    readonly onReload: () => Promise<void>;
}): ReactNode {
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">Where you are signed in</h2>
                <Button size="sm" variant="ghost" onClick={() => void onReload()}>
                    Refresh
                </Button>
            </header>

            <Notice tone="warn">
                Nothing ends a session yet: no sign-out writes an end time, so every session the
                deployment has created is listed here and an old one cannot be told from a live one.
                Ending one other sign-in has no operation either, which is why no row offers it.
            </Notice>

            {sessions.length === 0 ? (
                <p className="text-sm text-ink-muted">No sessions are recorded for your account.</p>
            ) : (
                <ul className="divide-y divide-line-subtle">
                    {sessions.map((row) => (
                        <li
                            key={row.id}
                            className="flex flex-wrap items-center gap-x-4 gap-y-1 py-3"
                        >
                            <span className="w-32 shrink-0 font-mono text-sm">
                                {row.clientIdentifier === ''
                                    ? 'unknown client'
                                    : row.clientIdentifier}
                            </span>
                            <span className="min-w-0 flex-1">
                                <span className="block font-mono text-sm">
                                    {row.clientIp === '' ? 'no address' : row.clientIp}
                                </span>
                                <span className="block text-xs text-ink-muted">
                                    {row.countryCode === '' ? 'unknown country' : row.countryCode} ·
                                    started {row.startTime}
                                </span>
                            </span>
                            <span className="hidden text-right text-xs text-ink-faint sm:block">
                                {String(row.bytesSent)} sent / {String(row.bytesReceived)} received
                            </span>
                        </li>
                    ))}
                </ul>
            )}
            <p className="text-xs text-ink-faint">
                One row per session the tenant holds for this account, in the order the server
                states.
            </p>
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
            setOutcome({ ok: true, text: 'The password is changed. Use it at your next sign-in.' });
            await onChanged();
        } catch (error) {
            setOutcome({
                ok: false,
                text: error instanceof Error ? error.message : 'The change was refused.',
            });
        } finally {
            setBusy(false);
        }
    };

    return (
        <section className="card space-y-4 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">Password</h2>
                <p className="text-sm text-ink-muted">
                    Change the password you sign in with. The server checks the current password
                    before it changes anything.
                </p>
            </header>
            <Field label="Current password" hint="Proves the request is yours.">
                <PasswordInput
                    value={current}
                    autoComplete="current-password"
                    onChange={(event) => setCurrent(event.target.value)}
                />
            </Field>
            <NewPasswordField
                policy={policy}
                label="New password"
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
                    Change password
                </Button>
            </div>
        </section>
    );
}

/**
 * What the server says about the account's sign-ins.
 *
 * Read-only, because the server writes this state as a side effect of signing
 * in. An account that has never signed in has no record, and the screen says
 * that rather than showing zeros.
 */
function SignInStatePanel({
    session,
    state,
}: {
    readonly session: SessionView;
    readonly state: LoginInfo | null;
}): ReactNode {
    return (
        <section className="card space-y-4 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">Sign-in state</h2>
                <p className="text-sm text-ink-muted">
                    Read-only. The server writes it as you sign in.
                </p>
            </header>
            <div className="flex flex-wrap gap-2">
                <Tag tone="neutral">{session.username}</Tag>
                <Tag tone={session.passwordResetRequired ? 'warn' : 'muted'}>
                    {session.passwordResetRequired
                        ? 'Password change required'
                        : 'No change required'}
                </Tag>
            </div>
            {state === null ? (
                <p className="text-sm text-ink-muted">
                    This account has no login record yet, so there is no sign-in state to show.
                </p>
            ) : (
                <div className="grid gap-x-6 gap-y-3 sm:grid-cols-2">
                    <Detail
                        label="Last sign-in"
                        value={state.lastLogin === '' ? 'never' : state.lastLogin}
                    />
                    <Detail
                        label="From"
                        value={state.lastAttemptIp === '' ? 'unknown' : state.lastAttemptIp}
                    />
                    <Detail label="Failed attempts" value={String(state.failedLogins)} mono />
                    <Detail label="Account" value={state.locked ? 'locked' : 'not locked'} />
                </div>
            )}
            <p className="text-xs text-ink-faint">
                Signing out is not recorded: nothing writes a session&rsquo;s end time, so this
                panel cannot show when you last left.
            </p>
        </section>
    );
}
