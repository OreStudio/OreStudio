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
 * PROTOTYPE. Kept on main as the design record; nothing outside the prototype
 * routes imports it.
 *
 * Three answers to one question: how does a tenant administrator get one
 * colleague back into the system, or shut the account down, on one screen? The
 * journey is doc/knowledge/journeys/credentials/journey_rescue_access.org.
 *
 * The rows are fixtures, because the browser has no read path for the account
 * or its login record, and no route reaches the reset, lock or unlock subjects.
 * The panels say so on the screen.
 */

import { useState, type ReactNode } from 'react';
import { Button, Detail, Field, Input, Notice, PageHeader, Tag, cx } from '../ui/Primitives.js';
import { VariantBar, useVariant, type PrototypeVariant } from './VariantBar.js';
import {
    account,
    rescuedAccount,
    rescuedLoginState,
    type PrototypeLoginState,
} from './credentialsFixtures.js';

const VARIANTS = [
    {
        id: 'a',
        name: 'A · One decision screen',
        gist: 'The account, its state and every action on one page, stacked in the order the call goes.',
    },
    {
        id: 'b',
        name: 'B · Diagnose, then act',
        gist: 'The state leads and names the action it suggests; the actions sit under it.',
    },
    {
        id: 'c',
        name: 'C · Roster and panel',
        gist: 'The account list stays in view on the left, because the administrator arrives from it.',
    },
] as const satisfies readonly [PrototypeVariant, ...PrototypeVariant[]];

interface RosterRow {
    readonly username: string;
    readonly fullName: string;
    readonly state: 'locked' | 'active' | 'reset required';
}

const ROSTER: readonly RosterRow[] = [
    { username: 'amara.okafor', fullName: 'Amara Okafor', state: 'active' },
    { username: 'jonas.lindqvist', fullName: 'Jonas Lindqvist', state: 'locked' },
    { username: 'priya.raman', fullName: 'Priya Raman', state: 'active' },
    { username: 'tomas.novak', fullName: 'Tomas Novak', state: 'reset required' },
];

export function RescueAccessPrototype(): ReactNode {
    const { active, choose } = useVariant(VARIANTS, 'a');
    const [sentAt, setSentAt] = useState<string | undefined>(undefined);
    const [nextState, setNextState] = useState<'locked' | 'unlocked'>('locked');
    const [selected, setSelected] = useState(rescuedAccount.username);
    const [log, setLog] = useState<readonly string[]>([]);

    const record = (entry: string): void => setLog((entries) => [...entries, entry]);

    const header = (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-start justify-between gap-3">
                <div className="space-y-1">
                    <h2 className="text-lg font-medium">{rescuedAccount.fullName}</h2>
                    <p className="font-mono text-sm text-ink-muted">
                        {rescuedAccount.username} · {rescuedAccount.email} ·{' '}
                        {rescuedAccount.accountType}
                    </p>
                </div>
                <Tag tone="warn">Locked</Tag>
            </header>
            <div className="grid gap-x-6 gap-y-3 sm:grid-cols-3">
                <Detail label="Last sign-in" value={rescuedLoginState.lastSignInAt} />
                <Detail label="From" value={rescuedLoginState.lastSignInFrom} />
                <Detail
                    label="Failed attempts"
                    value={String(rescuedLoginState.failedAttempts)}
                    mono
                />
            </div>
        </section>
    );

    const recoveryPanel = (
        <section className="card space-y-4 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">Send a recovery link</h2>
                <p className="text-sm text-ink-muted">
                    The administrator does not choose the colleague's password. The system emails a
                    single-use link to the address on the account, and the colleague sets a password
                    that only they know.
                </p>
            </header>
            <Field label="Send it to" hint="The address on the account. It is not edited here.">
                <Input readOnly value={rescuedAccount.email} />
            </Field>
            <div className="flex flex-wrap items-center justify-between gap-3">
                <span className="text-xs text-ink-faint">
                    Nothing sends mail in this tree, no subject requests a reset, and no table holds
                    a token. This screen sends nothing.
                </span>
                <Button
                    variant="primary"
                    onClick={() => {
                        setSentAt('2026-09-30 12:04 UTC');
                        record(`send recovery link · ${rescuedAccount.email}`);
                    }}
                >
                    Send the recovery link
                </Button>
            </div>
            {sentAt !== undefined && (
                <Notice tone="success">
                    The prototype recorded a link sent at {sentAt}. Nothing left the browser.
                </Notice>
            )}
            <details className="text-sm">
                <summary className="cursor-pointer text-ink-muted">
                    Set a password here instead (fallback)
                </summary>
                <p className="mt-2 text-xs text-ink-faint">
                    Kept for an account whose mailbox cannot receive. The administrator then knows
                    the password, so the record has to say so.{' '}
                    <span className="font-mono">iam.v1.accounts.reset-password</span> exists and no
                    route reaches it. Whether this fallback survives is the open question this
                    prototype raises.
                </p>
            </details>
        </section>
    );

    const statePanel = (
        <section className="card space-y-4 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">Lock the account</h2>
                <p className="text-sm text-ink-muted">
                    A locked account cannot sign in. Unlocking clears the failed attempt count.
                </p>
            </header>
            <div className="flex flex-wrap items-center gap-2">
                <div className="inline-flex overflow-hidden rounded-[var(--radius-card)] border border-line">
                    {(['unlocked', 'locked'] as const).map((option) => (
                        <button
                            key={option}
                            type="button"
                            className={cx(
                                'px-4 py-1.5 text-sm capitalize',
                                nextState === option
                                    ? 'bg-accent text-ink-inverse'
                                    : 'text-ink-muted',
                            )}
                            onClick={() => {
                                setNextState(option);
                                record(`lock state drawn as ${option} · nothing sent`);
                            }}
                        >
                            {option}
                        </button>
                    ))}
                </div>
                <span className="text-xs text-ink-faint">
                    The subject exists and needs{' '}
                    <span className="font-mono">iam::accounts:lock</span>; no BFF route reaches it.
                </span>
            </div>
            <Notice tone="info">
                A lock leaves open sessions open. Nothing ends them today, so the colleague's
                existing session keeps working until it expires.
            </Notice>
        </section>
    );

    const gapsPanel = (
        <section className="card space-y-3 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">Not available in this build</h2>
                <p className="text-sm text-ink-muted">
                    What this journey asks for and the server does not have.
                </p>
            </header>
            <ul className="space-y-2 text-sm">
                <li className="flex gap-2">
                    <Tag tone="warn">missing</Tag>
                    <span>
                        Send a recovery link — no subject requests a reset, no table holds a token,
                        and nothing in the tree sends mail
                    </span>
                </li>
                <li className="flex gap-2">
                    <Tag tone="warn">missing</Tag>
                    <span>
                        Self-service recovery — the same machinery, started by the member who cannot
                        sign in. Candidates{' '}
                        <span className="font-mono">iam.v1.accounts.request-password-reset</span>{' '}
                        and <span className="font-mono">complete-password-reset</span>
                    </span>
                </li>
                <li className="flex gap-2">
                    <Tag tone="warn">missing</Tag>
                    <span>Activate or deactivate an account — no active flag exists</span>
                </li>
                <li className="flex gap-2">
                    <Tag tone="warn">missing</Tag>
                    <span>
                        End the sessions a lock leaves open — candidate{' '}
                        <span className="font-mono">iam.v1.sessions.end</span>
                    </span>
                </li>
            </ul>
        </section>
    );

    const recommendation = <Recommendation state={rescuedLoginState} />;

    return (
        <>
            <div className="mx-auto max-w-[1200px] space-y-6 pb-[45vh]">
                <PageHeader
                    title="Rescue access"
                    description="Get one colleague back into the system, or shut the account down."
                />
                <Notice tone="warn">
                    PROTOTYPE. Every row below is a fixture. The browser has no read path for the
                    account or its login record, and no route reaches the reset, lock or unlock
                    subjects, so no panel here reads or writes the server.
                </Notice>

                {active.id === 'a' && (
                    <div className="space-y-6">
                        {header}
                        {recoveryPanel}
                        {statePanel}
                        {gapsPanel}
                    </div>
                )}

                {active.id === 'b' && (
                    <div className="space-y-6">
                        {header}
                        {recommendation}
                        <div className="grid gap-6 lg:grid-cols-2">
                            {recoveryPanel}
                            {statePanel}
                        </div>
                        {gapsPanel}
                    </div>
                )}

                {active.id === 'c' && (
                    <div className="grid gap-6 lg:grid-cols-[minmax(0,280px)_minmax(0,1fr)]">
                        <Roster selected={selected} onSelect={setSelected} />
                        <div className="space-y-6">
                            {header}
                            {recoveryPanel}
                            {statePanel}
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
                                selected account:{' '}
                                <span className="font-mono text-ink">{selected}</span>
                            </span>
                            <span>
                                recovery link sent:{' '}
                                <span className="font-mono text-ink">{sentAt ?? 'no'}</span>
                            </span>
                            <span>
                                lock control drawn as:{' '}
                                <span className="font-mono text-ink">{nextState}</span>
                            </span>
                        </div>
                        <p className="text-ink-faint">
                            Signed in as <span className="font-mono">{account.username}</span>,
                            tenant administrator, tenant Acme Corporation.
                        </p>
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

function Recommendation({ state }: { readonly state: PrototypeLoginState }): ReactNode {
    return (
        <section className="card space-y-2 p-6">
            <h2 className="text-lg font-medium">What the state suggests</h2>
            {state.locked ? (
                <p className="text-sm text-ink-muted">
                    {state.failedAttempts} failed attempts locked this account. Unlock it if the
                    colleague simply forgot the password, or send a recovery link if the attempts
                    were not theirs.
                </p>
            ) : (
                <p className="text-sm text-ink-muted">
                    The account is not locked. Send a recovery link if the colleague has forgotten
                    the password.
                </p>
            )}
            <div className="flex flex-wrap gap-2 pt-1">
                <Tag tone="warn">{state.failedAttempts} failed attempts</Tag>
                <Tag tone="neutral">{state.locked ? 'Locked' : 'Not locked'}</Tag>
                <Tag tone={state.online ? 'accent' : 'muted'}>
                    {state.online ? 'Session open' : 'No session'}
                </Tag>
            </div>
        </section>
    );
}

function Roster({
    selected,
    onSelect,
}: {
    readonly selected: string;
    readonly onSelect: (username: string) => void;
}): ReactNode {
    return (
        <section className="card h-fit space-y-2 p-4">
            <h2 className="px-2 text-sm font-medium text-ink-muted">Accounts</h2>
            <ul>
                {ROSTER.map((row) => (
                    <li key={row.username}>
                        <button
                            type="button"
                            className={cx(
                                'w-full rounded-[var(--radius-card)] px-2 py-2 text-left text-sm',
                                row.username === selected
                                    ? 'bg-surface-hover'
                                    : 'hover:bg-surface-hover',
                            )}
                            onClick={() => onSelect(row.username)}
                        >
                            <span className="block truncate">{row.fullName}</span>
                            <span className="block truncate font-mono text-xs text-ink-faint">
                                {row.username}
                            </span>
                            <span className="mt-1 block text-xs text-ink-muted">{row.state}</span>
                        </button>
                    </li>
                ))}
            </ul>
            <p className="px-2 text-xs text-ink-faint">
                The account list has no read path in the browser today: no route serves{' '}
                <span className="font-mono">iam.v1.accounts.list</span>.
            </p>
        </section>
    );
}
