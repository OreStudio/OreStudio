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
    type ListSource,
} from '../refdata/RecordList.js';
import { AreaTrail, useAreaRoot } from '../shell/AreaTrail.js';
import { PersonCell } from '../access/PersonRef.js';
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
 * its authorisation is in — and both are buttons above the list. Opening a row
 * leaves the queue for the request's own page, where the deal is read and
 * decided.
 *
 * The screen is drawn from the signed-off prototype at
 * doc/prototypes/operations/operational-authorisation-queue.
 */

/**
 * The states, in the order a person works them, with the colour each is drawn in.
 *
 * The colour is the state's meaning and not decoration: a tint on the row and a
 * solid edge on its left, so a queue of forty deals reads by colour before it is
 * read by word. Each hue is low-chroma so it survives a dark surface, and each
 * is a reading a person already has: amber for a deal that waits on somebody,
 * indigo for one that moved and needs re-reading, steel for one parked by a
 * query, teal for one settled, oxide for one refused and grey for one that
 * ended.
 */
const GROUPS: readonly {
    readonly key: QueueGroup;
    /** The translation key of the group's name. */
    readonly labelKey: string;
    readonly wash: string;
    readonly edge: string;
    readonly ink: string;
}[] = [
    {
        key: 'awaiting',
        labelKey: 'operations.authorisationQueue.groups.awaiting',
        wash: 'rgba(216, 162, 74, 0.08)',
        edge: '#d8a24a',
        ink: '#e6c48e',
    },
    {
        key: 'changed',
        labelKey: 'operations.authorisationQueue.groups.changed',
        wash: 'rgba(154, 138, 214, 0.08)',
        edge: '#9a8ad6',
        ink: '#c3b8ee',
    },
    {
        key: 'held',
        labelKey: 'operations.authorisationQueue.groups.held',
        wash: 'rgba(111, 159, 196, 0.08)',
        edge: '#6f9fc4',
        ink: '#a8c8e0',
    },
    {
        key: 'authorised',
        labelKey: 'operations.authorisationQueue.groups.authorised',
        wash: 'rgba(95, 168, 127, 0.08)',
        edge: '#5fa87f',
        ink: '#9bccb2',
    },
    {
        key: 'denied',
        labelKey: 'operations.authorisationQueue.groups.denied',
        wash: 'rgba(194, 94, 99, 0.08)',
        edge: '#c25e63',
        ink: '#e0a0a3',
    },
    {
        /* A request that ended without the deal being authorised or denied: taken
         * back, expired, or approved and not applied. Grey, because the deal is
         * neither waiting on anybody nor refused. */
        key: 'closed',
        labelKey: 'operations.authorisationQueue.groups.closed',
        wash: 'rgba(138, 145, 153, 0.08)',
        edge: '#8a9199',
        ink: '#b8bfc6',
    },
];

const EVERY_STATE: readonly QueueGroup[] = GROUPS.map((entry) => entry.key);

/** The colour a row is drawn in, by the state its authorisation is in. */
function paletteOf(row: QueueRow) {
    return GROUPS.find((entry) => entry.key === groupOf(row)) ?? GROUPS[0]!;
}

const CLASSES: readonly { readonly key: QueueClass | 'all'; readonly labelKey: string }[] = [
    { key: 'all', labelKey: 'operations.authorisationQueue.classes.all' },
    { key: 'flow', labelKey: 'operations.authorisationQueue.classes.flow' },
    { key: 'non-flow', labelKey: 'operations.authorisationQueue.classes.nonFlow' },
];

const STATE_KEY = 'operations.authorisationQueue.state';

function stateTone(row: QueueRow): 'neutral' | 'accent' | 'up' | 'down' | 'warn' {
    switch (row.stateCode) {
        case 'approved':
            return 'up';
        case 'refused':
            return 'down';
        case 'held':
            return 'warn';
        case 'apply_failed':
        case 'withdrawn':
        case 'expired':
            return 'neutral';
        default:
            return row.changed ? 'warn' : 'accent';
    }
}

/** The state's name, as a translation key; an unknown state is shown as it came. */
function stateKey(row: QueueRow): string {
    if (row.changed) return `${STATE_KEY}.changed`;
    switch (row.stateCode) {
        case 'waiting':
            return `${STATE_KEY}.awaiting`;
        case 'held':
            return `${STATE_KEY}.held`;
        case 'approved':
            return `${STATE_KEY}.authorised`;
        case 'apply_failed':
            return `${STATE_KEY}.applyFailed`;
        case 'refused':
            return `${STATE_KEY}.denied`;
        case 'withdrawn':
            return `${STATE_KEY}.withdrawn`;
        case 'expired':
            return `${STATE_KEY}.expired`;
        default:
            return row.stateCode === '' ? `${STATE_KEY}.unknown` : row.stateCode;
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
            header: t('operations.authorisationQueue.columns.deal'),
            mono: true,
            cell: (row) => (
                <EntityLink to={authorisationRequestPath(row.requestId)}>
                    {row.reference}
                </EntityLink>
            ),
        },
        {
            id: 'type',
            header: t('operations.authorisationQueue.columns.instrument'),
            cell: (row) => (
                <>
                    {row.tradeTypeName === '' ? row.tradeType : row.tradeTypeName}
                    <div className="text-xs text-ink-faint">
                        {t(
                            row.classCode === 'flow'
                                ? 'operations.authorisationQueue.classes.flow'
                                : 'operations.authorisationQueue.classes.nonFlow',
                        )}
                    </div>
                </>
            ),
        },
        {
            id: 'counterparty',
            header: t('operations.authorisationQueue.columns.counterparty'),
            cell: (row) => row.counterparty,
        },
        {
            id: 'book',
            header: t('operations.authorisationQueue.columns.book'),
            cell: (row) => row.book,
        },
        {
            id: 'tradeDate',
            header: t('operations.authorisationQueue.columns.tradeDate'),
            mono: true,
            cell: (row) => row.tradeDate,
        },
        {
            /* The queue is worked newest first, so the time a deal arrived is on
             * the row rather than in a tooltip: it is the order the list is in. */
            id: 'raisedAt',
            header: t('operations.authorisationQueue.columns.raised'),
            mono: true,
            cell: (row) => row.requestedAt.replace('T', ' ').slice(0, 16),
        },
    ];

    const held = localSource<QueueRow>({
        key: 'queue-authorisations',
        rows,
        // The rows change under the screen when a decision is taken.
        revision: String(rows.length) + (rows[0]?.requestedAt ?? ''),
        searchOf: (row) =>
            `${row.reference} ${row.tradeType} ${row.counterparty} ${row.book} ${row.tradeDate}`,
        orderOf: (left, right) => right.requestedAt.localeCompare(left.requestedAt),
    });

    /*
     * The read is a window, so the list says when the window is not the queue.
     * A silent stop at the read's limit would draw a queue that had emptied.
     */
    const waiting = queue.data?.total ?? 0;
    const source: ListSource<QueueRow> =
        waiting > open.length
            ? {
                  ...held,
                  read: async (page) => ({
                      ...(await held.read(page)),
                      notes: [
                          {
                              tone: 'warn',
                              text: t('operations.authorisationQueue.readWindow', {
                                  shown: open.length,
                                  waiting,
                              }),
                          },
                      ],
                  }),
              }
            : held;

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
                        {t('operations.authorisationQueue.allStates')}
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
                                    backgroundColor: on ? `${entry.edge}22` : 'transparent',
                                    borderColor: on ? entry.edge : 'var(--line, #30363d)',
                                    color: on ? entry.ink : undefined,
                                    opacity: on ? 1 : 0.6,
                                }}
                            >
                                {t(entry.labelKey)}
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
                            {t(entry.labelKey)}
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
    /* A request in a final state takes no further decision here. The three
     * endings that applied to nobody's yes are final too. */
    const decided =
        row !== undefined &&
        ['approved', 'apply_failed', 'refused', 'withdrawn', 'expired'].includes(row.stateCode);
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
        return (
            <p className="text-sm text-ink-faint">{t('operations.authorisationQueue.reading')}</p>
        );
    }

    if (row === undefined) {
        return (
            <section className="flex flex-col gap-4">
                <AreaTrail area="operations" screen={t('operations.screens.authorisationQueue')} />
                <Notice tone="warn">
                    {t('operations.authorisationQueue.notFound', { requestId })}
                </Notice>
                <Link to={PATHS.authorisationQueue} className="text-sm underline">
                    {t('operations.authorisationQueue.backToQueue')}
                </Link>
            </section>
        );
    }

    /*
     * Whether this kind of request may be held at all is stated by the kind, not
     * by the screen. A kind that refuses a hold is answered yes or no, and the
     * panel must not offer what the server will refuse. A kind the rules say
     * nothing about keeps the button, because the server is the authority and
     * hiding it would stop an operator parking a deal nobody can decide.
     */
    const holdAllowed = reasons.data?.allowsHoldByKind[row.kindCode] !== false;

    return (
        <section className="flex flex-col gap-4">
            <div>
                <AreaTrail area="operations" screen={t('operations.screens.authorisationQueue')} />
                <PageHeader
                    title={t('operations.authorisationQueue.pageTitle', {
                        reference: row.reference,
                    })}
                    description={t('operations.authorisationQueue.pageDescription', {
                        type: row.tradeTypeName === '' ? row.tradeType : row.tradeTypeName,
                    })}
                    actions={
                        <Link to={PATHS.authorisationQueue} className="text-sm underline">
                            {t('operations.authorisationQueue.backToQueue')}
                        </Link>
                    }
                />
            </div>

            {row.changed && (
                <Notice tone="warn">
                    {t('operations.authorisationQueue.changedNotice', {
                        tradeVersion: row.tradeVersion,
                        requestTradeVersion: row.requestTradeVersion,
                    })}
                </Notice>
            )}

            {failure !== '' && <Notice tone="error">{failure}</Notice>}

            <div className="grid grid-cols-1 gap-6 lg:grid-cols-[minmax(0,3fr)_minmax(0,2fr)]">
                <div className="card space-y-4 p-6">
                    <h2 className="text-lg font-medium">
                        {t('operations.authorisationQueue.deal.title')}
                    </h2>
                    <dl className="grid grid-cols-1 gap-4 sm:grid-cols-2">
                        <Detail
                            label={t('operations.authorisationQueue.deal.reference')}
                            value={
                                <EntityLink to={tradePath(row.tradeId)}>{row.reference}</EntityLink>
                            }
                            mono
                        />
                        <Detail
                            label={t('operations.authorisationQueue.deal.type')}
                            value={
                                row.tradeTypeName === '' ? (
                                    row.tradeType
                                ) : (
                                    <>
                                        {row.tradeTypeName}
                                        <span className="ml-2 text-xs text-ink-faint">
                                            {row.tradeType}
                                        </span>
                                    </>
                                )
                            }
                        />
                        <Detail
                            label={t('operations.authorisationQueue.deal.counterparty')}
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
                            label={t('operations.authorisationQueue.deal.book')}
                            value={
                                row.bookId === '' ? (
                                    row.book
                                ) : (
                                    <EntityLink to={bookPath(row.bookId)}>{row.book}</EntityLink>
                                )
                            }
                        />
                        <Detail
                            label={t('operations.authorisationQueue.deal.tradeDate')}
                            value={row.tradeDate}
                            mono
                        />
                        <Detail
                            label={t('operations.authorisationQueue.deal.status')}
                            value={
                                <Tag small tone="muted">
                                    {row.status === ''
                                        ? t('operations.authorisationQueue.deal.statusUnknown')
                                        : row.status}
                                </Tag>
                            }
                        />
                    </dl>
                </div>

                <div className="card space-y-4 p-6">
                    <h2 className="text-lg font-medium">
                        {t('operations.authorisationQueue.request.title')}
                    </h2>
                    <dl className="space-y-4">
                        <Detail
                            label={t('operations.authorisationQueue.request.state')}
                            value={
                                <Tag small tone={stateTone(row)}>
                                    {t(stateKey(row))}
                                </Tag>
                            }
                        />
                        <Detail
                            label={t('operations.authorisationQueue.request.raisedBy')}
                            value={<PersonCell who={row.requestedBy} />}
                        />
                        <Detail
                            label={t('operations.authorisationQueue.request.raisedAt')}
                            value={row.requestedAt}
                            mono
                        />
                        <Detail
                            label={t('operations.authorisationQueue.request.reason')}
                            value={row.reason}
                        />
                    </dl>
                </div>
            </div>

            <div className="card space-y-4 p-6">
                <h2 className="text-lg font-medium">
                    {t('operations.authorisationQueue.decision.title')}
                </h2>
                {decided ? (
                    <Notice tone="info">
                        {t('operations.authorisationQueue.decision.decided')}
                    </Notice>
                ) : (
                    <>
                        <label className="flex flex-col gap-1">
                            <span className="text-xs uppercase tracking-wide text-ink-faint">
                                {t('operations.authorisationQueue.decision.comment')}
                            </span>
                            <textarea
                                value={comment}
                                onChange={(event) => setComment(event.target.value)}
                                rows={3}
                                className="rounded-md border border-line bg-surface px-2 py-1.5 text-sm"
                                placeholder={t(
                                    'operations.authorisationQueue.decision.commentPlaceholder',
                                )}
                            />
                        </label>

                        {row.stateCode !== 'held' && holdAllowed && (
                            <label className="flex max-w-sm flex-col gap-1">
                                <span className="text-xs uppercase tracking-wide text-ink-faint">
                                    {t('operations.authorisationQueue.decision.holdReason')}
                                </span>
                                <select
                                    value={holdReason}
                                    onChange={(event) => setHoldReason(event.target.value)}
                                    className="rounded-md border border-line bg-surface px-2 py-1.5 text-sm"
                                >
                                    <option value="">
                                        {t('operations.authorisationQueue.decision.chooseReason')}
                                    </option>
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
                                {t('operations.authorisationQueue.decision.authorise')}
                            </Button>
                            {row.stateCode !== 'held' && holdAllowed && (
                                <Button
                                    disabled={!canComment || !holdReason || busy}
                                    onClick={() => void decide('hold')}
                                >
                                    {t('operations.authorisationQueue.decision.hold')}
                                </Button>
                            )}
                            <Button
                                disabled={!canComment || busy}
                                onClick={() => void decide('refuse')}
                            >
                                {t('operations.authorisationQueue.decision.deny')}
                            </Button>
                        </div>
                    </>
                )}
            </div>
        </section>
    );
}
