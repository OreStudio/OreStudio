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
import { Link, useParams } from 'react-router';
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
import {
    localSource,
    RecordList,
    type ListColumn,
} from '../refdata/RecordList.js';
import { AreaTrail, useAreaRoot } from '../shell/AreaTrail.js';
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
        wash: 'rgba(255, 255, 186, 0.16)',
        edge: '#ffffba',
        ink: '#ffffba',
    },
    {
        key: 'changed',
        label: 'Changed since requested',
        wash: 'rgba(255, 223, 186, 0.16)',
        edge: '#ffdfba',
        ink: '#ffdfba',
    },
    {
        key: 'held',
        label: 'Held over',
        wash: 'rgba(186, 225, 255, 0.16)',
        edge: '#bae1ff',
        ink: '#bae1ff',
    },
    {
        key: 'authorised',
        label: 'Authorised',
        wash: 'rgba(186, 255, 201, 0.16)',
        edge: '#baffc9',
        ink: '#baffc9',
    },
    {
        key: 'denied',
        label: 'Denied',
        wash: 'rgba(255, 179, 186, 0.16)',
        edge: '#ffb3ba',
        ink: '#ffb3ba',
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

/** The queue read, shared by the list and the page. */
function useQueue() {
    return useQuery({
        queryKey: ['queue', 'authorisations'],
        queryFn: () => authorisationQueue.read(),
    });
}

export function AuthorisationQueuePage(): ReactNode {
    const { t } = useTranslation();
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

    const columns: readonly ListColumn<QueueRow>[] = [
        {
            id: 'deal',
            header: 'Deal',
            mono: true,
            cell: (row) => (
                <EntityLink to={authorisationRequestPath(row.requestId)}>{row.reference}</EntityLink>
            ),
        },
        {
            id: 'type',
            header: 'Instrument',
            cell: (row) => (
                <>
                    {row.tradeType}
                    <div className="text-xs text-ink-faint">
                        {row.classCode === 'flow' ? 'flow' : 'non-flow'}
                    </div>
                </>
            ),
        },
        { id: 'counterparty', header: 'Counterparty', cell: (row) => row.counterparty },
        { id: 'book', header: 'Book', cell: (row) => row.book },
        {
            id: 'tradeDate',
            header: 'Trade date',
            mono: true,
            cell: (row) => row.tradeDate,
        },
        {
            /* The queue is worked newest first, so the time a deal arrived is on
             * the row rather than in a tooltip: it is the order the list is in. */
            id: 'raisedAt',
            header: 'Raised',
            mono: true,
            cell: (row) => row.requestedAt.replace('T', ' ').slice(0, 16),
        },
    ];

    const source = localSource<QueueRow>({
        key: 'queue-authorisations',
        rows,
        // The rows change under the screen when a decision is taken.
        revision: String(rows.length) + (rows[0]?.requestedAt ?? ''),
        searchOf: (row) =>
            `${row.reference} ${row.tradeType} ${row.counterparty} ${row.book} ${row.tradeDate}`,
        orderOf: (left, right) => right.requestedAt.localeCompare(left.requestedAt),
    });

    return (
        <RecordList
            source={source}
            title={t('operations.authorisationQueue.title')}
            lead={t('operations.screens.authorisationQueueBody')}
            crumbs={[
                ...useAreaRoot('operations'),
                { label: t('operations.authorisationQueue.title') },
            ]}
            columns={columns}
            pathOf={(row) => authorisationRequestPath(row.requestId)}
            keyOf={(row) => `${row.requestId}:${row.tradeId}`}
            rowStyle={(row) => {
                const palette = paletteOf(row);
                return {
                    backgroundColor: palette.wash,
                    boxShadow: `inset 3px 0 0 ${palette.edge}`,
                };
            }}
            actions={
                <div className="flex flex-wrap items-center gap-2">
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
                        <span className="ml-1.5 tabular-nums text-xs text-ink-faint">
                            {counts.all}
                        </span>
                    </button>
                    {GROUPS.map((entry) => {
                        const on = states.includes(entry.key);
                        return (
                            <button
                                key={entry.key}
                                type="button"
                                onClick={() => toggle(entry.key)}
                                aria-pressed={on}
                                className="rounded-full border px-3 py-1 text-[13px]"
                                style={{
                                    backgroundColor: on ? entry.wash : 'transparent',
                                    borderColor: on ? entry.edge : 'var(--line, #30363d)',
                                    color: on ? entry.ink : undefined,
                                    opacity: on ? 1 : 0.6,
                                }}
                            >
                                {entry.label}
                                <span className="ml-1.5 tabular-nums text-xs">
                                    {counts[entry.key]}
                                </span>
                            </button>
                        );
                    })}
                    <span className="mx-1 h-5 w-px bg-line" aria-hidden />
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
            }
        />
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
