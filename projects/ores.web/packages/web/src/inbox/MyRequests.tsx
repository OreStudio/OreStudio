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

import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import type { InboxRequestView } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { AccountPicture } from '../ui/Images.js';
import { Button, Dialog, Field, Notice } from '../ui/Primitives.js';
import { RequestStateChip } from './RequestStateChip.js';
import { askedFor } from './words.js';

/** How many requests a person sees on their own page. */
const PAGE = 50;

/**
 * The person's own requests, and the way to take one back.
 *
 * A waiting request is the only one that can be withdrawn, and the version the
 * row was read at travels with the withdrawal: a request decided while the page
 * was open is refused by the server rather than taken back behind the
 * administrator's answer.
 *
 * A member may not read the account list the roles are joined from, so their
 * own requests come back with no roles at all. The kind's label stands in, and
 * the reason beneath it says what was asked for.
 */
export function MyRequests(): ReactNode {
    const { t } = useTranslation();
    const [withdrawing, setWithdrawing] = useState<InboxRequestView | null>(null);
    const requests = useQuery({
        queryKey: ['my-requests'],
        queryFn: () => api.myRequests({ offset: 0, limit: PAGE }),
    });

    if (requests.isError) {
        return <Notice tone="error">{requests.error.message}</Notice>;
    }
    const rows = requests.data?.items ?? [];
    if (rows.length === 0) {
        return null;
    }

    return (
        <section className="rounded-md border border-line bg-surface-raised">
            <h2 className="px-4 pt-4 text-sm font-semibold">{t('inbox.mine.title')}</h2>
            <ul>
                {rows.map((request) => (
                    <li
                        key={request.id}
                        className="flex items-start gap-3 border-t border-line-subtle px-4 py-3 first:border-t-0"
                    >
                        <div className="min-w-0 flex-1">
                            <div className="flex flex-wrap items-center gap-2 text-sm">
                                <span className="font-medium">{askedFor(t, request)}</span>
                                <RequestStateChip stateCode={request.stateCode} />
                                <span className="text-xs text-ink-faint">
                                    {t('inbox.mine.asked', {
                                        date: request.requestedAt.slice(0, 10),
                                    })}
                                </span>
                            </div>
                            <div className="mt-1 text-sm text-ink-muted">
                                {t('inbox.mine.youWrote', { reason: request.reason })}
                            </div>
                            {request.decision !== null && (
                                <div className="mt-1.5 rounded-md border border-line-subtle px-2.5 py-1.5 text-sm">
                                    <span className="inline-flex items-center gap-1.5">
                                        <AccountPicture
                                            username={request.decision.decidedBy}
                                            name={request.decision.decidedBy}
                                            size="sm"
                                        />
                                        <span className="font-medium">
                                            {request.decision.decidedBy}
                                        </span>
                                    </span>
                                    {request.decision.comment !== '' && (
                                        <span className="text-ink-muted">
                                            : {request.decision.comment}
                                        </span>
                                    )}
                                </div>
                            )}
                        </div>
                        {request.stateCode === 'waiting' && (
                            <Button onClick={() => setWithdrawing(request)}>
                                {t('inbox.mine.withdraw')}
                            </Button>
                        )}
                    </li>
                ))}
            </ul>
            {withdrawing !== null && (
                <WithdrawDialog request={withdrawing} onClose={() => setWithdrawing(null)} />
            )}
        </section>
    );
}

/** Taking a request back, against the version the row was read at. */
function WithdrawDialog({
    request,
    onClose,
}: {
    readonly request: InboxRequestView;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const [comment, setComment] = useState('');
    const withdraw = useMutation({
        mutationFn: () => api.withdrawRequest(request.id, { version: request.version, comment }),
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['my-requests'] });
            onClose();
        },
    });

    return (
        <Dialog
            title={t('inbox.withdraw.title')}
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" onClick={onClose}>
                        {t('entity.cancel')}
                    </Button>
                    <Button
                        variant="primary"
                        pending={withdraw.isPending}
                        onClick={() => withdraw.mutate()}
                    >
                        {t('inbox.withdraw.submit')}
                    </Button>
                </>
            }
        >
            <div className="space-y-4">
                <p className="text-sm text-ink-muted">
                    {t('inbox.withdraw.lead', { role: askedFor(t, request) })}
                </p>
                {withdraw.isError && <Notice tone="error">{withdraw.error.message}</Notice>}
                <Field label={t('inbox.withdraw.comment')}>
                    <textarea
                        className="w-full rounded-md border border-line bg-surface-base px-3 py-2 text-sm text-ink placeholder:text-ink-faint hover:border-line-strong focus:border-accent focus:outline-none focus:ring-3 focus:ring-accent/20"
                        rows={3}
                        value={comment}
                        onChange={(event) => setComment(event.target.value)}
                    />
                </Field>
            </div>
        </Dialog>
    );
}
