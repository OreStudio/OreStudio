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
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { useMemo, useState, type ReactNode } from 'react';
import { useQuery, useQueryClient } from '@tanstack/react-query';
import { Link, useNavigate, useParams } from 'react-router';
import {
    authorisationQueue,
    countsOf,
    groupOf,
    rowsOf,
    type QueueClass,
    type QueueGroup,
    type QueueRow,
} from '../api/queue.js';
import { api, type HoldReason } from '../api/client.js';
import { Button, cx, Detail, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { PersonRef } from '../access/PersonRef.js';
import { useTranslation } from '../i18n/Provider.js';
import {
    authorisationRequestPath,
    bookPath,
    counterpartyPath,
    PATHS,
    tradePath,
} from '../shell/paths.js';

/**
 * The Operations queue and the page of one authorisation.
 *
 * A deal waits here once it is live, and an operator decides it. The queue has
 * two divisions over the same rows — how much work the deal is, and the state
 * its authorisation is in — and both are in the rail. Opening a row leaves the
 * queue for the request's own page, where the deal is read and decided.
 *
 * The screen is drawn from the signed-off prototype at
 * doc/prototypes/operations/operational-authorisation-queue.
 */

/**
 * The states, in the order a person works them, with the colour each is drawn in.
 *
 * The colour is the state's meaning and not decoration: a pastel wash on the row
 * and a stronger edge on its left so a queue of forty deals can be read by
 * colour before it is read by word. The choices are deliberate -- butter for a
 * deal that waits, lilac for one that moved, sky for one parked, mint for one
 * approved and blush for one refused -- because those are the readings a person
 * already has of them.
 */
const GROUPS: readonly {
    readonly key: QueueGroup;
    readonly label: string;
    readonly wash: string;
    readonly edge: string;
    readonly ink: string;
}[] = [
    {
        key: 'awaiting',
        label: 'Awaiting authorisation',
        wash: 'rgba(250, 204, 21, 0.10)',
        edge: '#eab308',
        ink: '#a16207',
    },
    {
        key: 'changed',
        label: 'Changed since requested',
        wash: 'rgba(168, 85, 247, 0.10)',
        edge: '#a855f7',
        ink: '#7e22ce',
    },
    {
        key: 'held',
        label: 'Held over',
        wash: 'rgba(56, 189, 248, 0.10)',
        edge: '#38bdf8',
        ink: '#0369a1',
    },
    {
        key: 'authorised',
        label: 'Authorised',
        wash: 'rgba(52, 211, 153, 0.10)',
        edge: '#34d399',
        ink: '#047857',
    },
    {
        key: 'denied',
        label: 'Denied',
        wash: 'rgba(251, 113, 133, 0.10)',
        edge: '#fb7185',
        ink: '#be123c',
    },
];

const EVERY_STATE: readonly QueueGroup[] = GROUPS.map((entry) => entry.key);

/** The colour a row is drawn in, by the state its authorisation is in. */
function paletteOf(row: QueueRow) {
    return GROUPS.find((entry) => entry.key === groupOf(row)) ?? GROUPS[0]!;
}

const CLASSES: readonly { readonly key: QueueClass | 'all'; readonly label: string }[] = [
    { key: 'all', label: 'All deals' },
    { key: 'flow', label: 'Flow' },
    { key: 'non-flow', label: 'Non-flow' },
];

function stateTone(row: QueueRow): 'neutral' | 'accent' | 'up' | 'down' | 'warn' {
    switch (row.stateCode) {
        case 'approved':
            return 'up';
        case 'refused':
        case 'cancelled':
            return 'down';
        case 'held':
            return 'warn';
        default:
            return row.changed ? 'warn' : 'accent';
    }
}

function stateWord(row: QueueRow): string {
    if (row.changed) return 'Changed';
    switch (row.stateCode) {
        case 'waiting':
            return 'Awaiting';
        case 'held':
            return 'Held over';
        case 'approved':
            return 'Authorised';
        case 'refused':
            return 'Denied';
        case 'cancelled':
            return 'Cancelled';
        default:
            return row.stateCode === '' ? 'Awaiting' : row.stateCode;
    }
}

/** A link that is drawn as the house draws one. */
function EntityLink({ to, children }: { readonly to: string; readonly children: ReactNode }) {
    return (
        <Link
            to={to}
            className="underline decoration-line-strong underline-offset-2 hover:decoration-accent"
        >
            {children}
        </Link>
    );
}

/** One node of the rail: its label and its figure, on one line. */
function RailNode({
    label,
    count,
    selected,
    indent = false,
    onSelect,
}: {
    readonly label: string;
    readonly count: number;
    readonly selected: boolean;
    readonly indent?: boolean;
    readonly onSelect: () => void;
}): ReactNode {
    return (
        <button
            type="button"
            onClick={onSelect}
            aria-pressed={selected}
            className={cx(
                'flex w-full items-center justify-between gap-2 rounded-md px-2 py-1.5 text-left text-[13px]',
                indent && 'pl-6',
                selected ? 'bg-accent/15 text-ink' : 'text-ink-dim hover:bg-surface-2 hover:text-ink',
            )}
        >
            <span className="truncate">{label}</span>
            <span className="shrink-0 tabular-nums text-xs text-ink-faint">{count}</span>
        </button>
    );
}

/** The queue read, shared by the list and the page. */
function useQueue() {
    return useQuery({
        queryKey: ['queue', 'authorisations'],
        queryFn: () => authorisationQueue.read(),
    });
}

export function AuthorisationQueuePage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const [klass, setKlass] = useState<QueueClass | 'all'>('all');
    /* Every state is shown until a person says otherwise: a queue that hides a
     * group by default hides work, and the figures on the buttons say what is
     * being hidden. */
    const [states, setStates] = useState<readonly QueueGroup[]>(EVERY_STATE);

    const queue = useQueue();
    const open = queue.data?.items ?? [];
    const answered = queue.data?.answered ?? [];
    const all = useMemo(() => [...open, ...answered], [open, answered]);
    const rows = useMemo(() => rowsOf(all, klass, states), [all, klass, states]);
    const counts = useMemo(() => countsOf(all, klass), [all, klass]);
    const allShown = states.length === EVERY_STATE.length;

    const toggleAll = () => setStates(allShown ? [] : EVERY_STATE);
    const toggle = (key: QueueGroup) =>
        setStates((current) =>
            current.includes(key) ? current.filter((one) => one !== key) : [...current, key],
        );

    return (
        <section className="flex flex-col gap-4">
            <div>
                <AreaTrail area="operations" screen={t('operations.screens.authorisationQueue')} />
                <PageHeader
                    title={t('operations.authorisationQueue.title')}
                    description={t('operations.screens.authorisationQueueBody')}
                />
            </div>

            {queue.isError && <Notice tone="error">The queue could not be read.</Notice>}

            <div className="flex flex-wrap items-center gap-2" role="group" aria-label="States to show">
                <button
                    type="button"
                    onClick={toggleAll}
                    aria-pressed={allShown}
                    className={cx(
                        'rounded-full border px-3 py-1 text-[13px]',
                        allShown
                            ? 'border-accent bg-accent/15 text-ink'
                            : 'border-line text-ink-dim hover:text-ink',
                    )}
                >
                    All states
                    <span className="ml-1.5 tabular-nums text-xs text-ink-faint">{counts.all}</span>
                </button>
                {GROUPS.map((entry) => {
                    const on = states.includes(entry.key);
                    return (
                        <button
                            key={entry.key}
                            type="button"
                            onClick={() => toggle(entry.key)}
                            aria-pressed={on}
                            className="rounded-full border px-3 py-1 text-[13px] transition-opacity"
                            style={{
                                backgroundColor: on ? entry.wash : 'transparent',
                                borderColor: on ? entry.edge : 'var(--line, #30363d)',
                                color: on ? entry.ink : undefined,
                                opacity: on ? 1 : 0.6,
                            }}
                        >
                            {entry.label}
                            <span className="ml-1.5 tabular-nums text-xs">{counts[entry.key]}</span>
                        </button>
                    );
                })}
                <span className="mx-2 h-5 w-px bg-line" aria-hidden />
                <div className="flex gap-1" role="group" aria-label="Class">
                    {CLASSES.map((entry) => (
                        <button
                            key={entry.key}
                            type="button"
                            onClick={() => setKlass(entry.key)}
                            aria-pressed={klass === entry.key}
                            className={cx(
                                'rounded-md px-2.5 py-1 text-[13px]',
                                klass === entry.key
                                    ? 'bg-accent/15 text-ink'
                                    : 'text-ink-dim hover:text-ink',
                            )}
                        >
                            {entry.label}
                        </button>
                    ))}
                </div>
            </div>

            <div className="min-w-0 overflow-x-auto">
                {rows.length === 0 ? (
                    <p className="px-2 py-8 text-sm text-ink-faint">
                        {all.length === 0
                            ? 'Nothing waits here. A deal appears once it is live and awaits authorisation.'
                            : 'No deal is in the states you chose.'}
                    </p>
                ) : (
                    <table className="w-full text-sm">
                        <thead>
                            <tr className="border-b border-line text-left text-xs uppercase tracking-wide text-ink-faint">
                                <th className="px-2 py-2">Deal</th>
                                <th className="px-2 py-2">Type</th>
                                <th className="px-2 py-2">Counterparty</th>
                                <th className="px-2 py-2">Book</th>
                                <th className="px-2 py-2">Trade date</th>
                                <th className="px-2 py-2">State</th>
                            </tr>
                        </thead>
                        <tbody>
                            {rows.map((row) => {
                                const palette = paletteOf(row);
                                return (
                                    <tr
                                        key={`${row.requestId}:${row.tradeId}`}
                                        className="cursor-pointer border-b border-line-soft align-top"
                                        style={{
                                            backgroundColor: palette.wash,
                                            boxShadow: `inset 3px 0 0 ${palette.edge}`,
                                        }}
                                        onClick={() =>
                                            navigate(authorisationRequestPath(row.requestId))
                                        }
                                    >
                                        <td className="whitespace-nowrap px-2 py-2 font-mono text-xs">
                                            <EntityLink
                                                to={authorisationRequestPath(row.requestId)}
                                            >
                                                {row.reference}
                                            </EntityLink>
                                        </td>
                                        <td className="px-2 py-2">
                                            {row.tradeType}
                                            <div className="text-xs text-ink-faint">
                                                {row.classCode === 'flow' ? 'flow' : 'non-flow'}
                                            </div>
                                        </td>
                                        <td className="max-w-[12rem] truncate px-2 py-2">
                                            {row.counterparty}
                                        </td>
                                        <td className="max-w-[10rem] truncate px-2 py-2 text-ink-dim">
                                            {row.book}
                                        </td>
                                        <td className="whitespace-nowrap px-2 py-2 font-mono text-xs">
                                            {row.tradeDate}
                                        </td>
                                        <td className="whitespace-nowrap px-2 py-2">
                                            <span
                                                className="inline-block rounded-full border px-2 py-0.5 text-[11px] leading-tight"
                                                style={{
                                                    borderColor: palette.edge,
                                                    color: palette.ink,
                                                    backgroundColor: 'transparent',
                                                }}
                                            >
                                                {stateWord(row)}
                                            </span>
                                        </td>
                                    </tr>
                                );
                            })}
                        </tbody>
                    </table>
                )}
            </div>

            <p className="text-xs text-ink-faint">
                A decision tells the person who raised the request. The queue is read whole, so
                the figures state the tenant's queue and not a page of it.
            </p>
        </section>
    );
}

/**
 * One authorisation request, as its own page: the deal it names, the person who
 * raised it, and the decision it waits for.
 *
 * Every decision carries a comment, authorisation included: the comment is what
 * the control is audited by. A hold additionally names a reason, because a
 * parked deal without a reason is a deal nobody can pick up.
 */
export function AuthorisationRequestPage(): ReactNode {
    const { requestId = '' } = useParams();
    const { t } = useTranslation();
    const queryClient = useQueryClient();
    const queue = useQueue();

    const [comment, setComment] = useState('');
    const [holdReason, setHoldReason] = useState('');
    const [failure, setFailure] = useState('');
    const [busy, setBusy] = useState(false);
    const reasons = useQuery({ queryKey: ['hold-reasons'], queryFn: () => api.holdReasons() });

    const all = [...(queue.data?.items ?? []), ...(queue.data?.answered ?? [])];
    const row = all.find((one) => one.requestId === requestId);
    const decided =
        row !== undefined &&
        (row.stateCode === 'approved' ||
            row.stateCode === 'refused' ||
            row.stateCode === 'cancelled');
    const canComment = comment.trim().length > 0;

    const decide = async (decisionCode: 'approve' | 'refuse' | 'hold') => {
        if (row === undefined) return;
        setBusy(true);
        setFailure('');
        try {
            await api.decideRequest(row.requestId, {
                version: row.requestVersion,
                decisionCode,
                comment: comment.trim(),
                ...(decisionCode === 'hold' && { holdReasonCode: holdReason }),
            });
            setComment('');
            setHoldReason('');
            await queryClient.invalidateQueries({ queryKey: ['queue', 'authorisations'] });
        } catch (error) {
            setFailure(error instanceof Error ? error.message : String(error));
        } finally {
            setBusy(false);
        }
    };

    if (queue.isPending) {
        return <p className="text-sm text-ink-faint">Reading the request…</p>;
    }

    if (row === undefined) {
        return (
            <section className="flex flex-col gap-4">
                <AreaTrail area="operations" screen={t('operations.screens.authorisationQueue')} />
                <Notice tone="warn">
                    No request {requestId} waits in this queue. It may have been decided and aged
                    out of the answered tail.
                </Notice>
                <Link to={PATHS.authorisationQueue} className="text-sm underline">
                    Back to the queue
                </Link>
            </section>
        );
    }

    return (
        <section className="flex flex-col gap-4">
            <div>
                <AreaTrail area="operations" screen={t('operations.screens.authorisationQueue')} />
                <PageHeader
                    title={`Authorisation · ${row.reference}`}
                    description={`${row.tradeType}, raised by the operations queue`}
                    actions={
                        <Link to={PATHS.authorisationQueue} className="text-sm underline">
                            Back to the queue
                        </Link>
                    }
                />
            </div>

            {row.changed && (
                <Notice tone="warn">
                    The deal moved after this request was raised: version {row.tradeVersion} now,
                    against {row.requestTradeVersion} when it arrived. The row is not locked, and a
                    decision on the version you have read is the decision recorded.
                </Notice>
            )}

            {failure !== '' && <Notice tone="error">{failure}</Notice>}

            <div className="grid grid-cols-1 gap-6 lg:grid-cols-[minmax(0,3fr)_minmax(0,2fr)]">
                <div className="card space-y-4 p-6">
                    <h2 className="text-lg font-medium">The deal</h2>
                    <dl className="grid grid-cols-1 gap-4 sm:grid-cols-2">
                        <Detail
                            label="Deal"
                            value={<EntityLink to={tradePath(row.tradeId)}>{row.reference}</EntityLink>}
                            mono
                        />
                        <Detail label="Type" value={row.tradeType} />
                        <Detail
                            label="Counterparty"
                            value={
                                row.counterpartyId === '' ? (
                                    row.counterparty
                                ) : (
                                    <EntityLink to={counterpartyPath(row.counterpartyId)}>
                                        {row.counterparty}
                                    </EntityLink>
                                )
                            }
                        />
                        <Detail
                            label="Book"
                            value={
                                row.bookId === '' ? (
                                    row.book
                                ) : (
                                    <EntityLink to={bookPath(row.bookId)}>{row.book}</EntityLink>
                                )
                            }
                        />
                        <Detail label="Trade date" value={row.tradeDate} mono />
                        <Detail
                            label="Status"
                            value={
                                <Tag small tone="muted">
                                    {row.status === '' ? 'unknown' : row.status}
                                </Tag>
                            }
                        />
                    </dl>
                </div>

                <div className="card space-y-4 p-6">
                    <h2 className="text-lg font-medium">The request</h2>
                    <dl className="space-y-4">
                        <Detail
                            label="State"
                            value={
                                <Tag small tone={stateTone(row)}>
                                    {stateWord(row)}
                                </Tag>
                            }
                        />
                        <Detail label="Raised by" value={<PersonRef who={row.requestedBy} />} />
                        <Detail label="Raised at" value={row.requestedAt} mono />
                        <Detail label="Reason" value={row.reason} />
                    </dl>
                </div>
            </div>

            <div className="card space-y-4 p-6">
                <h2 className="text-lg font-medium">The decision</h2>
                {decided ? (
                    <Notice tone="info">
                        This deal is decided. Undoing a decision is not offered here yet.
                    </Notice>
                ) : (
                    <>
                        <label className="flex flex-col gap-1">
                            <span className="text-xs uppercase tracking-wide text-ink-faint">
                                Comment, required
                            </span>
                            <textarea
                                value={comment}
                                onChange={(event) => setComment(event.target.value)}
                                rows={3}
                                className="rounded-md border border-line bg-surface px-2 py-1.5 text-sm"
                                placeholder="What did you check, and what did you find?"
                            />
                        </label>

                        {row.stateCode !== 'held' && (
                            <label className="flex max-w-sm flex-col gap-1">
                                <span className="text-xs uppercase tracking-wide text-ink-faint">
                                    Hold reason, required to hold over
                                </span>
                                <select
                                    value={holdReason}
                                    onChange={(event) => setHoldReason(event.target.value)}
                                    className="rounded-md border border-line bg-surface px-2 py-1.5 text-sm"
                                >
                                    <option value="">Choose a reason</option>
                                    {(reasons.data?.reasons ?? []).map((reason: HoldReason) => (
                                        <option key={reason.code} value={reason.code}>
                                            {reason.name}
                                        </option>
                                    ))}
                                </select>
                            </label>
                        )}

                        <div className="flex flex-wrap gap-2">
                            <Button
                                disabled={!canComment || busy}
                                onClick={() => void decide('approve')}
                            >
                                Authorise
                            </Button>
                            {row.stateCode !== 'held' && (
                                <Button
                                    disabled={!canComment || !holdReason || busy}
                                    onClick={() => void decide('hold')}
                                >
                                    Hold over
                                </Button>
                            )}
                            <Button disabled={!canComment || busy} onClick={() => void decide('refuse')}>
                                Deny
                            </Button>
                        </div>
                    </>
                )}
            </div>
        </section>
    );
}
