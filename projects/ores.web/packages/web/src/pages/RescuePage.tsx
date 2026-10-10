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

import { displayName } from '../access/names.js';
import { AccountLockBadge } from '../ui/AccountLockBadge.js';
import { Avatar, imageUrl } from '../ui/Images.js';
import { useCallback, useEffect, useState, type ReactNode } from 'react';
import { useSearchParams } from 'react-router';
import { api } from '../api/client.js';
import {
    Button,
    Detail,
    Dialog,
    Field,
    Input,
    Notice,
    PageHeader,
    Tag,
    cx,
} from '../ui/Primitives.js';
import type { Account, LoginInfo } from '@ores/wire-protocol/browser';
import { AreaTrail } from '../shell/AreaTrail.js';

/**
 * Rescue access, the tenant administrator's screen.
 *
 * The journey is
 * `doc/knowledge/journeys/credentials/journey_rescue_access.org`, and the
 * screen is the accepted prototype's variant B: the account's security state
 * leads and names the action it suggests, and the recovery action and the lock
 * control sit under it.
 *
 * Two things the server cannot do are stated rather than hidden. Nothing sends
 * mail, no subject requests a reset and no table holds a token, so *Send the
 * recovery link* is drawn and unavailable. And nothing ends the sessions a lock
 * leaves open, which the lock panel says before the administrator acts.
 *
 * The account list's permanent home is its own work, so the screen picks a
 * colleague out of the tenant's accounts here and the journey's first step is
 * served by that picker until the page exists.
 */

const FILTER_LIMIT = 20;

interface Rescued {
    readonly account: Account;
    readonly loginInfo: LoginInfo | null;
}

/**
 * The account being rescued, as the screen reads it.
 *
 * Exported so the loaded screen can be rendered from a fixture: the panels are
 * what the journey's acceptance is about, and a test that had to reach the
 * network to see them would be testing the read instead.
 */
export interface RescueViewProps {
    readonly account: Account;
    readonly loginInfo: LoginInfo | null;
    readonly onReload: () => Promise<void>;
}

type State =
    | { readonly kind: 'idle' }
    | { readonly kind: 'loading' }
    | { readonly kind: 'missing'; readonly username: string }
    | { readonly kind: 'ready'; readonly rescued: Rescued }
    | { readonly kind: 'failed'; readonly reason: string };

export function RescuePage(): ReactNode {
    const [params] = useSearchParams();
    const username = params.get('username') ?? '';
    const [state, setState] = useState<State>({ kind: 'idle' });

    const load = useCallback(async (): Promise<void> => {
        setState({ kind: 'loading' });
        try {
            const account = await api.account(username);
            if (account === null) {
                setState({ kind: 'missing', username });
                return;
            }
            const loginInfo = await api.loginInfo(account.id);
            setState({ kind: 'ready', rescued: { account, loginInfo } });
        } catch (error) {
            setState({
                kind: 'failed',
                reason: error instanceof Error ? error.message : 'The read failed.',
            });
        }
    }, [username]);

    useEffect(() => {
        if (username.length === 0) {
            setState({ kind: 'idle' });
            return;
        }
        void load();
    }, [username, load]);

    return (
        <div className="mx-auto max-w-[1100px] space-y-6">
            <div>
                <AreaTrail area="organisation" screen="Rescue access" />
                <PageHeader
                    title="Rescue access"
                    description="Get one colleague back into the system, or shut the account down."
                />
            </div>
            {state.kind === 'idle' && <Finder />}
            {state.kind === 'missing' && (
                <div className="space-y-6">
                    <Notice tone="warn">
                        No account in this tenant is named{' '}
                        <span className="font-mono">{username}</span>. It may have been renamed or
                        removed since the list was read.
                    </Notice>
                    <Finder />
                </div>
            )}
            {state.kind === 'loading' && <Notice tone="info">Reading the account…</Notice>}
            {state.kind === 'failed' && <Notice tone="error">{state.reason}</Notice>}
            {state.kind === 'ready' && (
                <RescueView
                    account={state.rescued.account}
                    loginInfo={state.rescued.loginInfo}
                    onReload={load}
                />
            )}
        </div>
    );
}

/**
 * The colleague, found in the tenant's accounts.
 *
 * The journey starts from the account list, and that page is not built yet, so
 * the picker stands in for it: the tenant's accounts, filtered as the
 * administrator types, one of them chosen. It reads what the list subject
 * answers and states the count, so a search that matches nothing says whether
 * the tenant holds no such account or the page it read was short.
 */
function Finder(): ReactNode {
    const [params, setParams] = useSearchParams();
    const [accounts, setAccounts] = useState<readonly Account[] | null>(null);
    const [totalCount, setTotalCount] = useState(0);
    const [reason, setReason] = useState<string | undefined>(undefined);
    const [filter, setFilter] = useState('');

    useEffect(() => {
        let live = true;
        api.accounts()
            .then((page) => {
                if (!live) {
                    return;
                }
                setAccounts(page.accounts);
                setTotalCount(page.totalCount);
            })
            .catch((error: unknown) => {
                if (!live) {
                    return;
                }
                setReason(error instanceof Error ? error.message : 'The read failed.');
            });
        return () => {
            live = false;
        };
    }, []);

    const needle = filter.trim().toLowerCase();
    const matches =
        accounts === null
            ? []
            : accounts.filter(
                  (row) =>
                      needle.length === 0 ||
                      row.username.toLowerCase().includes(needle) ||
                      row.fullName.toLowerCase().includes(needle),
              );
    const shown = matches.slice(0, FILTER_LIMIT);

    const choose = (row: Account): void => {
        const next = new URLSearchParams(params);
        next.set('username', row.username);
        setParams(next);
    };

    return (
        <section className="card space-y-4 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">Find the colleague</h2>
                <p className="text-sm text-ink-muted">
                    The support call names one account. Pick it here; the account list has its own
                    page in the tree&rsquo;s plan and this is the stand-in until it exists.
                </p>
            </header>
            <Field label="Filter" hint="Matches the username and the full name.">
                <Input
                    value={filter}
                    autoFocus
                    placeholder="amara.okafor"
                    onChange={(event) => setFilter(event.target.value)}
                />
            </Field>
            {reason !== undefined && <Notice tone="error">{reason}</Notice>}
            {accounts === null && reason === undefined && (
                <p className="text-sm text-ink-muted">Reading the tenant&rsquo;s accounts…</p>
            )}
            {accounts !== null && (
                <>
                    <ul className="divide-y divide-line-subtle">
                        {shown.map((row) => (
                            <li key={row.id}>
                                <button
                                    type="button"
                                    className="flex w-full flex-wrap items-center gap-x-3 gap-y-1 py-3 text-left hover:bg-surface-hover"
                                    onClick={() => choose(row)}
                                >
                                    <Avatar
                                        name={displayName(row, row.username)}
                                        src={row.imageId === null ? null : imageUrl(row.imageId)}
                                    />
                                    <span className="font-mono text-sm">{row.username}</span>
                                    <span className="min-w-0 flex-1 text-sm text-ink-muted">
                                        {row.fullName === '' ? 'no full name' : row.fullName}
                                    </span>
                                    <Tag tone="muted">{row.accountType}</Tag>
                                </button>
                            </li>
                        ))}
                    </ul>
                    {matches.length === 0 && (
                        <p className="text-sm text-ink-muted">No account matches that.</p>
                    )}
                    <p className="text-xs text-ink-faint">
                        Showing {String(shown.length)} of {String(matches.length)} matches, from the{' '}
                        {String(totalCount)} accounts the server says this tenant holds. The list
                        read is one page, so a very large tenant is short here.
                    </p>
                </>
            )}
        </section>
    );
}

/** Variant B: the state leads, the actions sit under it, and the gaps close. */
export function RescueView({ account, loginInfo, onReload }: RescueViewProps): ReactNode {
    return (
        <div className="space-y-6">
            <AccountHeader account={account} loginInfo={loginInfo} />
            <Diagnosis account={account} state={loginInfo} />
            <div className="grid gap-6 lg:grid-cols-2">
                <RecoveryPanel account={account} />
                <LockPanel account={account} state={loginInfo} onChanged={onReload} />
            </div>
            <GapsPanel />
            <ChangeAccount />
        </div>
    );
}

/** Who the administrator is rescuing, and what the server says about them. */
function AccountHeader({
    account,
    loginInfo,
}: {
    readonly account: Account;
    readonly loginInfo: LoginInfo | null;
}): ReactNode {
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-start justify-between gap-3">
                <Avatar
                    name={displayName(account, account.username)}
                    size="lg"
                    src={account.imageId === null ? null : imageUrl(account.imageId)}
                />
                <div className="min-w-0 flex-1 space-y-1">
                    <h2 className="text-lg font-medium">
                        {displayName(account, account.username)}
                    </h2>
                    <p className="font-mono text-sm text-ink-muted">
                        {account.username} · {account.email === '' ? 'no address' : account.email} ·{' '}
                        {account.accountType}
                    </p>
                </div>
                <AccountLockBadge locked={loginInfo?.locked === true} />
            </header>
            <div className="grid gap-x-6 gap-y-3 sm:grid-cols-3">
                <Detail
                    label="Last sign-in"
                    value={
                        loginInfo === null || loginInfo.lastLogin === ''
                            ? 'never'
                            : loginInfo.lastLogin
                    }
                />
                <Detail
                    label="From"
                    value={
                        loginInfo === null || loginInfo.lastAttemptIp === ''
                            ? 'unknown'
                            : loginInfo.lastAttemptIp
                    }
                />
                <Detail
                    label="Failed attempts"
                    value={loginInfo === null ? 'no record' : String(loginInfo.failedLogins)}
                    mono
                />
            </div>
            {loginInfo === null && (
                <p className="text-sm text-ink-muted">
                    This account has no login record, which means it has never signed in. The lock
                    state is written by a sign-in attempt, so there is nothing to show yet.
                </p>
            )}
        </section>
    );
}

/**
 * The state, and the action it suggests.
 *
 * This is the panel the accepted variant leads with, and the reason is the
 * support call: a run of failed attempts is one story and a suspected
 * compromise is another, and the count is what tells them apart.
 */
function Diagnosis({
    account,
    state,
}: {
    readonly account: Account;
    readonly state: LoginInfo | null;
}): ReactNode {
    return (
        <section className="card space-y-3 p-6">
            <h2 className="text-lg font-medium">What the state suggests</h2>
            {state === null ? (
                <p className="text-sm text-ink-muted">
                    {account.username} has never signed in, so no attempt count and no lock state
                    exist. There is nothing for a recovery link to interrupt.
                </p>
            ) : state.locked ? (
                <p className="text-sm text-ink-muted">
                    {String(state.failedLogins)} failed{' '}
                    {state.failedLogins === 1 ? 'attempt' : 'attempts'} locked this account. Unlock
                    it if the colleague simply forgot the password, or send a recovery link if the
                    attempts were not theirs.
                </p>
            ) : (
                <p className="text-sm text-ink-muted">
                    The account is not locked. Send a recovery link if the colleague has forgotten
                    the password; failed attempts alone do not lock an account until the
                    server&rsquo;s own threshold is reached.
                </p>
            )}
            {state !== null && (
                <div className="flex flex-wrap gap-2 pt-1">
                    <Tag tone={state.failedLogins > 0 ? 'warn' : 'muted'}>
                        {String(state.failedLogins)} failed{' '}
                        {state.failedLogins === 1 ? 'attempt' : 'attempts'}
                    </Tag>
                    <AccountLockBadge locked={state.locked} />
                    <Tag tone={state.online ? 'accent' : 'muted'}>
                        {state.online ? 'Session open' : 'No session'}
                    </Tag>
                </div>
            )}
        </section>
    );
}

/**
 * The recovery action, which the server cannot perform yet.
 *
 * The product owner settled that the administrator never sets the colleague's
 * password, so the primary action is the emailed link and the address on the
 * account is shown rather than edited. Nothing in the tree sends mail, so the
 * control is drawn unavailable with its reason instead of pretending.
 */
function RecoveryPanel({ account }: { readonly account: Account }): ReactNode {
    return (
        <section className="card space-y-4 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">Send a recovery link</h2>
                <p className="text-sm text-ink-muted">
                    The administrator does not choose the colleague&rsquo;s password. The system
                    emails a single-use, time-limited link to the address on the account, and the
                    colleague sets a password that only they know.
                </p>
            </header>
            <Field label="Send it to" hint="The address on the account. It is not edited here.">
                <Input readOnly value={account.email} />
            </Field>
            <Notice tone="warn">
                This deployment cannot send it. No mail path exists, no subject requests a reset and
                no table holds a token, so the link is not built yet.
            </Notice>
            <div className="flex justify-end">
                <Button variant="primary" disabled>
                    Send the recovery link
                </Button>
            </div>
            <p className="text-xs text-ink-faint">
                An account whose mailbox cannot receive has no fallback on this screen. An
                administrator who types a colleague&rsquo;s password knows that password, and the
                platform has no way to record that, so the screen does not offer it.
            </p>
        </section>
    );
}

/**
 * Lock and unlock as one control that shows the state it produces.
 *
 * The prototype settled that these are not two buttons that can contradict each
 * other: the control reads the server's state and offers the other one. The
 * confirmation is where the screen says what a lock does not do, because that is
 * the moment an administrator expects a lock to end a stolen session.
 */
function LockPanel({
    account,
    state,
    onChanged,
}: {
    readonly account: Account;
    readonly state: LoginInfo | null;
    readonly onChanged: () => Promise<void>;
}): ReactNode {
    const locked = state?.locked ?? false;
    const [confirming, setConfirming] = useState<boolean | undefined>(undefined);
    const [busy, setBusy] = useState(false);
    const [outcome, setOutcome] = useState<
        { readonly ok: boolean; readonly text: string } | undefined
    >(undefined);

    const apply = async (next: boolean): Promise<void> => {
        setBusy(true);
        setOutcome(undefined);
        try {
            await api.setAccountLocked(account.id, next);
            setConfirming(undefined);
            setOutcome({
                ok: true,
                text: next
                    ? 'The account is locked. Its open sessions are still open.'
                    : 'The account is unlocked, and its failed attempt count is cleared.',
            });
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
                <h2 className="text-lg font-medium">Lock the account</h2>
                <p className="text-sm text-ink-muted">
                    A locked account cannot sign in. Unlocking clears the failed attempt count.
                </p>
            </header>
            <div className="flex flex-wrap items-center gap-3">
                <div className="inline-flex overflow-hidden rounded-md border border-line">
                    <button
                        type="button"
                        className={cx(
                            'px-4 py-1.5 text-sm capitalize',
                            !locked ? 'bg-accent text-ink-inverse' : 'text-ink-muted',
                        )}
                        onClick={() => setConfirming(false)}
                    >
                        unlocked
                    </button>
                    <button
                        type="button"
                        className={cx(
                            'px-4 py-1.5 text-sm capitalize',
                            locked ? 'bg-accent text-ink-inverse' : 'text-ink-muted',
                        )}
                        onClick={() => setConfirming(true)}
                    >
                        locked
                    </button>
                </div>
                <span className="text-xs text-ink-faint">
                    The server&rsquo;s state is the filled side.
                </span>
            </div>
            <Notice tone="info">
                A lock leaves open sessions open. Nothing ends them today, so the colleague&rsquo;s
                existing session keeps working until it expires.
            </Notice>
            {outcome !== undefined && (
                <Notice tone={outcome.ok ? 'success' : 'error'}>{outcome.text}</Notice>
            )}
            {confirming !== undefined && (
                <Dialog
                    title={confirming ? 'Lock this account' : 'Unlock this account'}
                    onClose={() => setConfirming(undefined)}
                    footer={
                        <>
                            <Button variant="ghost" onClick={() => setConfirming(undefined)}>
                                Cancel
                            </Button>
                            <Button
                                variant={confirming ? 'danger' : 'primary'}
                                pending={busy}
                                onClick={() => void apply(confirming)}
                            >
                                {confirming ? 'Lock the account' : 'Unlock the account'}
                            </Button>
                        </>
                    }
                >
                    <p className="text-sm text-ink-muted">
                        {confirming
                            ? `${account.username} will not be able to sign in. Sessions it already holds stay open until they expire.`
                            : `${account.username} will be able to sign in again, and its failed attempt count is cleared.`}
                    </p>
                </Dialog>
            )}
        </section>
    );
}

/** What this journey asks for and the server does not have. */
function GapsPanel(): ReactNode {
    return (
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
                        sign in
                    </span>
                </li>
                <li className="flex gap-2">
                    <Tag tone="warn">missing</Tag>
                    <span>Activate or deactivate an account — no active flag exists</span>
                </li>
                <li className="flex gap-2">
                    <Tag tone="warn">missing</Tag>
                    <span>
                        End the sessions a lock leaves open — nothing writes a session&rsquo;s end
                        time
                    </span>
                </li>
            </ul>
        </section>
    );
}

/** The way back to the picker, because variant B does not keep the list in view. */
function ChangeAccount(): ReactNode {
    const [params, setParams] = useSearchParams();

    const back = (): void => {
        const next = new URLSearchParams(params);
        next.delete('username');
        setParams(next);
    };

    return (
        <div className="flex justify-start">
            <Button variant="ghost" onClick={back}>
                Choose another colleague
            </Button>
        </div>
    );
}
