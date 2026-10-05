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

/*
 * PROTOTYPE. Throwaway. Delete with the branch.
 *
 * Three answers to one question: what should the member's Security screen look
 * like? The journey is doc/knowledge/journeys/credentials/journey_protect_my_account.org.
 *
 * The rows are fixtures, because the browser has no read path for the account,
 * the login record or the sessions today. The panels say so on the screen.
 */

import { useState, type ReactNode } from 'react';
import { Button, Detail, Field, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { NewPasswordField, PasswordInput } from '../ui/PasswordField.js';
import { VariantBar, useVariant, type PrototypeVariant } from './VariantBar.js';
import {
    account,
    loginState,
    passwordPolicy,
    sessions,
    type PrototypeLoginState,
    type PrototypeSession,
} from './fixtures.js';

const VARIANTS = [
    {
        id: 'a',
        name: 'A · Password first',
        gist: 'One column: the password form leads, the sign-in state and the sessions follow.',
    },
    {
        id: 'b',
        name: 'B · Sessions first',
        gist: 'The places you are signed in lead, because that is what the member came to check.',
    },
    {
        id: 'c',
        name: 'C · Two columns',
        gist: 'Password on the left, state and sessions on the right, with the gaps stated below.',
    },
] as const satisfies readonly [PrototypeVariant, ...PrototypeVariant[]];

export function ProtectMyAccountPrototype(): ReactNode {
    const { active, choose } = useVariant(VARIANTS, 'a');
    const [currentPassword, setCurrentPassword] = useState('');
    const [newPassword, setNewPassword] = useState('');
    const [acceptable, setAcceptable] = useState(false);
    const [log, setLog] = useState<readonly string[]>([]);

    const record = (entry: string): void => setLog((entries) => [...entries, entry]);
    const changePassword = (): void => {
        record(`change password · current ${currentPassword.length} chars · new ${newPassword.length} chars`);
        setCurrentPassword('');
        setNewPassword('');
        setAcceptable(false);
    };

    const passwordPanel = (
        <PasswordPanel
            current={currentPassword}
            chosen={newPassword}
            acceptable={acceptable}
            onCurrent={setCurrentPassword}
            onChosen={(value, ok) => {
                setNewPassword(value);
                setAcceptable(ok);
            }}
            onSubmit={changePassword}
        />
    );

    const statePanel = <SignInStatePanel state={loginState} />;
    const sessionsPanel = <SessionsPanel sessions={sessions} />;
    const gapsPanel = <GapsPanel />;

    return (
        <>
            <div className="mx-auto max-w-[1100px] space-y-6 pb-[45vh]">
                <PageHeader
                    title="Security"
                    description="Your password, and the places your account is signed in."
                />
                <Notice tone="warn">
                    PROTOTYPE. Every row below is a fixture. The browser has no read path for the
                    account, the login record or the sessions today, so no panel here reads the
                    server.
                </Notice>

                {active.id === 'a' && (
                    <div className="space-y-6">
                        {passwordPanel}
                        {statePanel}
                        {sessionsPanel}
                    </div>
                )}

                {active.id === 'b' && (
                    <div className="space-y-6">
                        {sessionsPanel}
                        <div className="grid gap-6 lg:grid-cols-2">
                            {passwordPanel}
                            {statePanel}
                        </div>
                    </div>
                )}

                {active.id === 'c' && (
                    <div className="grid gap-6 lg:grid-cols-[minmax(0,420px)_minmax(0,1fr)]">
                        <div className="space-y-6">{passwordPanel}</div>
                        <div className="space-y-6">
                            {statePanel}
                            {sessionsPanel}
                            {gapsPanel}
                        </div>
                    </div>
                )}
            </div>

            <VariantBar
                variants={VARIANTS}
                active={active}
                onChoose={choose}
                state={
                    <div className="space-y-2 text-xs">
                        <div className="grid gap-x-6 gap-y-1 text-ink-muted sm:grid-cols-2">
                            <span>
                                variant: <span className="font-mono text-ink">{active.id}</span>
                            </span>
                            <span>
                                current password:{' '}
                                <span className="font-mono text-ink">{currentPassword.length} chars</span>
                            </span>
                            <span>
                                new password:{' '}
                                <span className="font-mono text-ink">{newPassword.length} chars</span>
                            </span>
                            <span>
                                meets the policy:{' '}
                                <span className="font-mono text-ink">{String(acceptable)}</span>
                            </span>
                        </div>
                        {log.length === 0 ? (
                            <p className="text-ink-faint">No action yet.</p>
                        ) : (
                            <ol className="space-y-0.5 font-mono text-ink-muted">
                                {log.map((entry, index) => (
                                    <li key={`${String(index)}-${entry}`}>
                                        {index + 1}. {entry}
                                    </li>
                                ))}
                            </ol>
                        )}
                    </div>
                }
            />
        </>
    );
}

function PasswordPanel({
    current,
    chosen,
    acceptable,
    onCurrent,
    onChosen,
    onSubmit,
}: {
    readonly current: string;
    readonly chosen: string;
    readonly acceptable: boolean;
    readonly onCurrent: (value: string) => void;
    readonly onChosen: (value: string, acceptable: boolean) => void;
    readonly onSubmit: () => void;
}): ReactNode {
    const [changed, setChanged] = useState(false);
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
                    onChange={(event) => onCurrent(event.target.value)}
                />
            </Field>
            <NewPasswordField
                policy={passwordPolicy}
                label="New password"
                value={chosen}
                onChange={(value, ok) => onChosen(value, ok)}
            />
            <div className="flex flex-wrap items-center justify-between gap-3">
                <span className="text-xs text-ink-faint">
                    POST /api/account/password exists and takes both passwords. This screen calls
                    nothing.
                </span>
                <Button
                    variant="primary"
                    disabled={current.length === 0 || !acceptable}
                    onClick={() => {
                        onSubmit();
                        setChanged(true);
                    }}
                >
                    Change password
                </Button>
            </div>
            {changed && (
                <Notice tone="success">
                    The prototype recorded the change on the state panel below. Nothing was sent.
                </Notice>
            )}
        </section>
    );
}

function SignInStatePanel({ state }: { readonly state: PrototypeLoginState }): ReactNode {
    return (
        <section className="card space-y-4 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">Sign-in state</h2>
                <p className="text-sm text-ink-muted">
                    Read-only. The server writes this state as a side effect of signing in.
                </p>
            </header>
            <div className="flex flex-wrap gap-2">
                <Tag tone={state.locked ? 'warn' : 'neutral'}>{state.locked ? 'Locked' : 'Not locked'}</Tag>
                <Tag tone={state.online ? 'accent' : 'muted'}>{state.online ? 'Signed in' : 'Signed out'}</Tag>
                <Tag tone={state.passwordResetRequired ? 'warn' : 'muted'}>
                    {state.passwordResetRequired ? 'Password change required' : 'No change required'}
                </Tag>
            </div>
            <div className="grid gap-x-6 gap-y-3 sm:grid-cols-2">
                <Detail label="Last sign-in" value={state.lastSignInAt} />
                <Detail label="From" value={state.lastSignInFrom} />
                <Detail label="Failed attempts" value={String(state.failedAttempts)} mono />
                <Detail label="Read path" value="none in the browser" />
            </div>
        </section>
    );
}

function SessionsPanel({ sessions: rows }: { readonly sessions: readonly PrototypeSession[] }): ReactNode {
    const others = rows.filter((row) => !row.thisDevice);
    return (
        <section className="card space-y-4 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">Where you are signed in</h2>
                <p className="text-sm text-ink-muted">
                    One row for each session with no end time.
                </p>
            </header>

            {others.length > 0 && (
                <Notice tone="info">
                    {others.length} other sign-in{others.length === 1 ? '' : 's'}. If you do not
                    recognise one, change your password. This screen cannot end that session:
                    no server operation ends one other session today.
                </Notice>
            )}

            <ul className="divide-y divide-line-subtle">
                {rows.map((row) => (
                    <li key={row.id} className="flex flex-wrap items-center gap-x-4 gap-y-1 py-3">
                        <span className="w-24 shrink-0 font-mono text-sm">{row.client}</span>
                        <span className="min-w-0 flex-1">
                            <span className="block font-mono text-sm">{row.address}</span>
                            <span className="block text-xs text-ink-muted">
                                {row.country} · started {row.startedAt} · {row.duration}
                            </span>
                        </span>
                        <span className="hidden text-right text-xs text-ink-faint sm:block">
                            {row.bytesIn} in / {row.bytesOut} out
                        </span>
                        {row.thisDevice ? (
                            <Tag tone="accent">This device</Tag>
                        ) : (
                            <Button
                                size="sm"
                                variant="secondary"
                                disabled
                                title="No server operation ends one other session yet: iam.v1.sessions.end does not exist."
                            >
                                End session
                            </Button>
                        )}
                    </li>
                ))}
            </ul>
            <p className="text-xs text-ink-faint">
                End session is drawn unavailable, not simulated: iam.v1.sessions.end does not exist,
                and iam.v1.auth.logout ends only this session.
            </p>
        </section>
    );
}

function GapsPanel(): ReactNode {
    return (
        <section className="card space-y-3 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">Not available in this build</h2>
                <p className="text-sm text-ink-muted">
                    The operations this journey asks for and the server does not have.
                </p>
            </header>
            <ul className="space-y-2 text-sm">
                <li className="flex gap-2">
                    <Tag tone="warn">missing</Tag>
                    <span>
                        End one other session — candidate <span className="font-mono">iam.v1.sessions.end</span>
                    </span>
                </li>
                <li className="flex gap-2">
                    <Tag tone="warn">missing</Tag>
                    <span>
                        Two-factor enrolment — candidate <span className="font-mono">iam.v1.accounts.enrol-totp</span>
                    </span>
                </li>
                <li className="flex gap-2">
                    <Tag tone="warn">partial</Tag>
                    <span>
                        Active sessions — <span className="font-mono">iam.v1.sessions.active</span> replies with
                        success and no rows, and no route serves it in the browser
                    </span>
                </li>
                <li className="flex gap-2">
                    <Tag tone="warn">missing</Tag>
                    <span>
                        Session statistics — candidate <span className="font-mono">iam.v1.sessions.statistics</span>
                    </span>
                </li>
            </ul>
        </section>
    );
}

/** The account the screen belongs to, shown once so the fixtures are visible. */
export function PrototypeAccountStrip(): ReactNode {
    return (
        <div className="mx-auto mb-4 flex max-w-[1100px] flex-wrap gap-x-6 gap-y-1 text-xs text-ink-faint">
            <span>
                account: <span className="font-mono">{account.username}</span>
            </span>
            <span>
                name: <span className="font-mono">{account.fullName}</span>
            </span>
            <span>
                email: <span className="font-mono">{account.email}</span>
            </span>
            <span>
                type: <span className="font-mono">{account.accountType}</span>
            </span>
        </div>
    );
}
