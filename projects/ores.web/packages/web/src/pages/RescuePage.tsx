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
import { useSearchParams } from 'react-router';
import type { Account, LoginInfo } from '@ores/wire-protocol/browser';
import { displayName } from '../access/names.js';
import { PEOPLE, personColumns } from '../access/PeoplePage.js';
import { SignInFacts } from '../access/SignInFacts.js';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { RecordList } from '../refdata/RecordList.js';
import { AreaTrail, useAreaParts } from '../shell/AreaTrail.js';
import { Avatar, imageUrl } from '../ui/Images.js';
import { Button, Dialog, LinkButton, Notice, PageHeader, Tag, cx } from '../ui/Primitives.js';

/**
 * Rescue access, the tenant administrator's screen.
 *
 * The journey is
 * `doc/knowledge/journeys/credentials/journey_rescue_access.org`, and the
 * screen is the accepted prototype's variant B: the account's security state
 * leads and names the action it suggests, and the lock control sits under it.
 *
 * The colleague is picked from the shared list of people, which pages, searches
 * and sorts on the server. Nothing sends mail and no table holds a token, so the
 * screen offers no recovery link: it offers only what the server can do.
 */

const rescuePath = (username: string): string => `/rescue?username=${encodeURIComponent(username)}`;

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
    | { readonly kind: 'loading' }
    | { readonly kind: 'missing'; readonly username: string }
    | { readonly kind: 'ready'; readonly rescued: Rescued }
    | { readonly kind: 'failed'; readonly reason: string };

export function RescuePage(): ReactNode {
    const [params] = useSearchParams();
    const username = params.get('username') ?? '';
    return username === '' ? <Finder /> : <Rescue username={username} />;
}

/** The colleague, found in the tenant's people. */
function Finder(): ReactNode {
    const { t } = useTranslation();
    const title = t('rescue.title');
    return (
        <RecordList
            source={PEOPLE}
            title={title}
            lead={t('rescue.find')}
            crumbs={useAreaParts('organisation', title)}
            pathOf={(account) => rescuePath(account.username)}
            columns={personColumns(t)}
        />
    );
}

function Rescue({ username }: { readonly username: string }): ReactNode {
    const { t } = useTranslation();
    const [state, setState] = useState<State>({ kind: 'loading' });

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
                reason: error instanceof Error ? error.message : t('rescue.readFailed'),
            });
        }
    }, [username, t]);

    useEffect(() => {
        void load();
    }, [load]);

    return (
        <div className="mx-auto max-w-[1100px] space-y-6">
            <div>
                <AreaTrail area="organisation" screen={t('rescue.title')} />
                <PageHeader
                    title={t('rescue.title')}
                    description={t('rescue.lead')}
                    actions={<LinkButton to="/rescue">{t('rescue.another')}</LinkButton>}
                />
            </div>
            {state.kind === 'missing' && (
                <Notice tone="warn">{t('rescue.missing', { username: state.username })}</Notice>
            )}
            {state.kind === 'loading' && <Notice tone="info">{t('rescue.reading')}</Notice>}
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

/** Variant B: the state leads and the action sits under it. */
export function RescueView({ account, loginInfo, onReload }: RescueViewProps): ReactNode {
    return (
        <div className="space-y-6">
            <AccountHeader account={account} loginInfo={loginInfo} />
            <Diagnosis account={account} state={loginInfo} />
            <LockPanel account={account} state={loginInfo} onChanged={onReload} />
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
    const { t } = useTranslation();
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
                        {account.username}
                        {account.email !== '' && (
                            <>
                                {' · '}
                                <a href={`mailto:${account.email}`} className="underline">
                                    {account.email}
                                </a>
                            </>
                        )}
                        {' · '}
                        {account.accountType}
                    </p>
                </div>
                <Tag tone={loginInfo?.locked === true ? 'warn' : 'muted'}>
                    {loginInfo?.locked === true
                        ? t('signInFacts.locked')
                        : t('signInFacts.notLocked')}
                </Tag>
            </header>
            <SignInFacts state={loginInfo} />
        </section>
    );
}

/** The state, and the action it suggests: a run of failed attempts is one story, a lock another. */
function Diagnosis({
    account,
    state,
}: {
    readonly account: Account;
    readonly state: LoginInfo | null;
}): ReactNode {
    const { t, plural } = useTranslation();
    return (
        <section className="card space-y-3 p-6">
            <h2 className="text-lg font-medium">{t('rescue.diagnosis.title')}</h2>
            {state === null ? (
                <p className="text-sm text-ink-muted">
                    {t('rescue.diagnosis.never', { username: account.username })}
                </p>
            ) : (
                <p className="text-sm text-ink-muted">
                    {state.locked
                        ? plural('rescue.diagnosis.locked', state.failedLogins)
                        : t('rescue.diagnosis.open')}
                </p>
            )}
            {state !== null && (
                <div className="flex flex-wrap gap-2 pt-1">
                    <Tag tone={state.failedLogins > 0 ? 'warn' : 'muted'}>
                        {plural('rescue.diagnosis.failed', state.failedLogins)}
                    </Tag>
                    <Tag tone={state.online ? 'accent' : 'muted'}>
                        {state.online
                            ? t('rescue.diagnosis.online')
                            : t('rescue.diagnosis.offline')}
                    </Tag>
                </div>
            )}
        </section>
    );
}

/**
 * Lock and unlock as one control that shows the state it produces.
 *
 * These are not two buttons that can contradict each other: the control reads
 * the server's state and offers the other one. The confirmation is where the
 * screen says what a lock does not do, because that is the moment an
 * administrator expects a lock to end a stolen session.
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
    const { t } = useTranslation();
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
            setOutcome({ ok: true, text: t(next ? 'rescue.lock.locked' : 'rescue.lock.unlocked') });
            await onChanged();
        } catch (error) {
            setOutcome({
                ok: false,
                text: error instanceof Error ? error.message : t('rescue.lock.refused'),
            });
        } finally {
            setBusy(false);
        }
    };

    return (
        <section className="card space-y-4 p-6">
            <header className="space-y-1">
                <h2 className="text-lg font-medium">{t('rescue.lock.title')}</h2>
                <p className="text-sm text-ink-muted">{t('rescue.lock.lead')}</p>
            </header>
            <div className="inline-flex overflow-hidden rounded-md border border-line">
                {[false, true].map((side) => (
                    <button
                        key={String(side)}
                        type="button"
                        aria-pressed={locked === side}
                        className={cx(
                            'px-4 py-1.5 text-sm',
                            locked === side ? 'bg-accent text-ink-inverse' : 'text-ink-muted',
                        )}
                        onClick={() => setConfirming(side)}
                    >
                        {side ? t('rescue.lock.sideLocked') : t('rescue.lock.sideUnlocked')}
                    </button>
                ))}
            </div>
            {outcome !== undefined && (
                <Notice tone={outcome.ok ? 'success' : 'error'}>{outcome.text}</Notice>
            )}
            {confirming !== undefined && (
                <Dialog
                    title={t(confirming ? 'rescue.lock.lockTitle' : 'rescue.lock.unlockTitle')}
                    onClose={() => setConfirming(undefined)}
                    footer={
                        <>
                            <Button variant="ghost" onClick={() => setConfirming(undefined)}>
                                {t('common.cancel')}
                            </Button>
                            <Button
                                variant={confirming ? 'danger' : 'primary'}
                                pending={busy}
                                onClick={() => void apply(confirming)}
                            >
                                {t(
                                    confirming
                                        ? 'rescue.lock.lockAction'
                                        : 'rescue.lock.unlockAction',
                                )}
                            </Button>
                        </>
                    }
                >
                    <p className="text-sm text-ink-muted">
                        {t(confirming ? 'rescue.lock.lockBody' : 'rescue.lock.unlockBody', {
                            username: account.username,
                        })}
                    </p>
                </Dialog>
            )}
        </section>
    );
}
