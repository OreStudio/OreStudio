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
import { Link } from 'react-router';
import {
    authorisationQueue,
    countsOf,
    rowsOf,
    type QueueClass,
    type QueueGroup,
    type QueueRow,
} from '../api/queue.js';
import { api, type HoldReason } from '../api/client.js';
import { Button, cx, Fact, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { useTranslation } from '../i18n/Provider.js';
import { tradePath } from '../shell/paths.js';

/**
 * The Operations queue.
 *
 * A deal waits here once it is live, and an operator decides it. The queue has
 * two divisions over the same rows — how much work the deal is, and the state
 * its authorisation is in — and both are in the rail, because a person working
 * the queue moves between them without losing the other.
 *
 * The screen is drawn from the signed-off prototype at
 * doc/prototypes/operations/operational-authorisation-queue.
 */

const GROUPS: readonly { readonly key: QueueGroup; readonly label: string }[] = [
    { key: 'awaiting', label: 'Awaiting authorisation' },
    { key: 'changed', label: 'Changed since requested' },
    { key: 'held', label: 'Held over' },
    { key: 'authorised', label: 'Authorised' },
    { key: 'denied', label: 'Denied' },
];

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

/**
 * The detail of one deal, and the decision it is waiting for.
 *
 * Every decision carries a comment, authorisation included: the comment is what
 * the control is audited by. A hold additionally names a reason, because a
 * parked deal without a reason is a deal nobody can pick up.
 */
function DealPanel({
    row,
    onDecided,
}: {
    readonly row: QueueRow;
    readonly onDecided: () => void;
}): ReactNode {
    const [comment, setComment] = useState('');
    const [holdReason, setHoldReason] = useState('');
    const [failure, setFailure] = useState('');
    const [busy, setBusy] = useState(false);
    const reasons = useQuery({ queryKey: ['hold-reasons'], queryFn: () => api.holdReasons() });

    const decided = row.stateCode === 'approved' || row.stateCode === 'refused' || row.stateCode === 'cancelled';
    const canComment = comment.trim().length > 0;

    const decide = async (decisionCode: 'approve' | 'refuse' | 'hold') => {
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
            onDecided();
        } catch (error) {
            setFailure(error instanceof Error ? error.message : String(error));
        } finally {
            setBusy(false);
        }
    };

    return (
        <div className="flex flex-col gap-4">
            <div className="flex items-start justify-between gap-3">
                <div className="min-w-0">
                    <div className="font-mono text-sm">{row.reference}</div>
                    <div className="truncate text-sm text-ink-dim">{row.tradeType}</div>
                </div>
                <Tag small tone={stateTone(row)}>
                    {stateWord(row)}
                </Tag>
            </div>

            {row.changed && (
                <Notice tone="warn">
                    The deal moved after this request was raised: version {row.tradeVersion} now,
                    against {row.requestTradeVersion} when it arrived. The row is not locked, and a
                    decision on the version you have read is the decision recorded.
                </Notice>
            )}

            <dl className="grid grid-cols-[8rem_1fr] gap-x-3 gap-y-2">
                <Fact label="Counterparty" value={row.counterparty} />
                <Fact label="Book" value={row.book} />
                <Fact label="Trade date" value={row.tradeDate} />
                <Fact label="Status" value={row.status} />
                <Fact label="Raised by" value={row.requestedBy} />
                <Fact label="Raised at" value={row.requestedAt} />
            </dl>

            <Link
                to={tradePath(row.tradeId)}
                className="w-fit text-sm underline decoration-line-strong underline-offset-2 hover:decoration-accent"
            >
                Open the deal
            </Link>

            {failure !== '' && <Notice tone="error">{failure}</Notice>}

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
                        <label className="flex flex-col gap-1">
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
    );
}

export function AuthorisationQueuePage(): ReactNode {
    const { t } = useTranslation();
    const queryClient = useQueryClient();
    const [klass, setKlass] = useState<QueueClass | 'all'>('all');
    const [group, setGroup] = useState<QueueGroup>('awaiting');
    const [selected, setSelected] = useState('');

    const queue = useQuery({
        queryKey: ['queue', 'authorisations'],
        queryFn: () => authorisationQueue.read(),
    });

    const open = queue.data?.items ?? [];
    const answered = queue.data?.answered ?? [];
    const all = useMemo(() => [...open, ...answered], [open, answered]);
    const rows = useMemo(() => rowsOf(all, klass, group), [all, klass, group]);
    const counts = useMemo(() => countsOf(all, klass), [all, klass]);
    const chosen = rows.find((row) => row.requestId === selected) ?? rows[0];

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

            <div className="flex min-h-[32rem] gap-4">
                <aside className="w-56 shrink-0 border-r border-line pr-2" aria-label="Read by">
                    <div className="px-2 py-1 text-xs uppercase tracking-wide text-ink-faint">
                        Class
                    </div>
                    {CLASSES.map((entry) => (
                        <RailNode
                            key={entry.key}
                            label={entry.label}
                            count={countsOf(all, entry.key).all}
                            selected={klass === entry.key}
                            onSelect={() => setKlass(entry.key)}
                        />
                    ))}
                    <div className="mt-3 px-2 py-1 text-xs uppercase tracking-wide text-ink-faint">
                        State
                    </div>
                    {GROUPS.map((entry) => (
                        <RailNode
                            key={entry.key}
                            indent
                            label={entry.label}
                            count={counts[entry.key]}
                            selected={group === entry.key}
                            onSelect={() => setGroup(entry.key)}
                        />
                    ))}
                    <RailNode
                        indent
                        label="All trades"
                        count={counts.all}
                        selected={group === 'all'}
                        onSelect={() => setGroup('all')}
                    />
                </aside>

                <div className="min-w-0 flex-1 overflow-x-auto">
                    {rows.length === 0 ? (
                        <p className="px-2 py-8 text-sm text-ink-faint">
                            Nothing waits here. A deal appears once it is live and awaits
                            authorisation.
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
                                {rows.map((row) => (
                                    <tr
                                        key={`${row.requestId}:${row.tradeId}`}
                                        className={cx(
                                            'cursor-pointer border-b border-line-soft align-top',
                                            row.requestId === chosen?.requestId
                                                ? 'bg-accent/10'
                                                : 'hover:bg-surface-2',
                                        )}
                                        onClick={() => setSelected(row.requestId)}
                                    >
                                        <td className="whitespace-nowrap px-2 py-2 font-mono text-xs">
                                            {row.reference}
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
                                            <Tag small tone={stateTone(row)}>
                                                {stateWord(row)}
                                            </Tag>
                                        </td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    )}
                </div>

                <aside className="w-[24rem] shrink-0 border-l border-line pl-4" aria-label="The deal">
                    {chosen === undefined ? (
                        <p className="py-8 text-sm text-ink-faint">Choose a deal to work it.</p>
                    ) : (
                        <DealPanel
                            key={chosen.requestId}
                            row={chosen}
                            onDecided={() =>
                                void queryClient.invalidateQueries({
                                    queryKey: ['queue', 'authorisations'],
                                })
                            }
                        />
                    )}
                </aside>
            </div>

            <p className="text-xs text-ink-faint">
                A decision tells the person who raised the request. The queue is read whole, so
                the figures state the tenant's queue and not a page of it.
            </p>
        </section>
    );
}
