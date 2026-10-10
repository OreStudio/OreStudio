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

import { useQuery } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { useNavigate } from 'react-router';
import type { InboxRequestView } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { AccountPicture } from '../ui/Images.js';
import { Notice, PageHeader } from '../ui/Primitives.js';
import { RelativeTime } from '../ui/Time.js';
import { PersonRef } from '../access/PersonRef.js';
import { useHolds } from '../access/holds.js';
import { MyRequests } from './MyRequests.js';
import { RequestStateChip } from './RequestStateChip.js';
import { askedFor } from './words.js';

/** How many requests the queue holds at once. */
const PAGE = 100;

/**
 * The requests waiting for an answer, oldest first, and what was answered
 * recently, newest answer first.
 *
 * The queue is the server's: it picks the kinds this person may decide and the
 * window the answered tail reaches back, so a member who decides nothing gets
 * an empty page rather than a refusal. What is waiting is a table, because the
 * administrator scans it; what has been answered is a list below, because
 * nobody acts on it.
 *
 * What is waiting is the whole of `items`: the server answers no request that
 * has an answer, so there is nothing here to filter out.
 */
/** Marks where a name goes in a translated phrase, so the name can be a link. */
const DECIDER_SLOT = '\u0001';

export function RequestsPage(): ReactNode {
    const { t } = useTranslation();
    const mayDecide = useHolds()('iam::roles:assign');
    return (
        <div className="space-y-6">
            <PageHeader
                title={t('inbox.queue.title')}
                description={mayDecide ? t('inbox.queue.lead') : t('inbox.mine.lead')}
            />
            <MyRequests showEmpty />
            {mayDecide && <Queue />}
        </div>
    );
}

/** What is waiting for this person's answer, and what was answered recently. */
function Queue(): ReactNode {
    const { t } = useTranslation();
    const requests = useQuery({
        queryKey: ['request-queue'],
        queryFn: () => api.requestQueue({ offset: 0, limit: PAGE }),
    });

    if (requests.isError) {
        return <Notice tone="error">{requests.error.message}</Notice>;
    }
    if (requests.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }

    const waiting = requests.data.items;
    const answered = requests.data.answered;

    return (
        <>
            <section className="rounded-md border border-line bg-surface-raised">
                <div className="overflow-x-auto">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                <th className="px-4 py-2 font-medium">{t('inbox.queue.who')}</th>
                                <th className="px-4 py-2 font-medium">
                                    {t('inbox.queue.askedFor')}
                                </th>
                                <th className="px-4 py-2 font-medium">{t('inbox.queue.why')}</th>
                                <th className="px-4 py-2 font-medium">
                                    {t('inbox.queue.waiting')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {waiting.map((request) => (
                                <QueueRow key={request.id} request={request} />
                            ))}
                            {waiting.length === 0 && (
                                <tr>
                                    <td colSpan={4} className="px-4 py-3 text-ink-muted">
                                        {t('inbox.queue.empty')}
                                    </td>
                                </tr>
                            )}
                        </tbody>
                    </table>
                </div>
            </section>

            {answered.length > 0 && (
                <section className="rounded-md border border-line bg-surface-raised">
                    <h2 className="px-4 pt-4 text-sm font-semibold">{t('inbox.queue.answered')}</h2>
                    <ul>
                        {answered.map((request) => (
                            <li
                                key={request.id}
                                className="flex items-start gap-3 border-t border-line-subtle px-4 py-3 first:border-t-0"
                            >
                                <AccountPicture
                                    username={request.requestedBy}
                                    name={request.requestedBy}
                                    size="sm"
                                />
                                <div className="min-w-0 flex-1">
                                    <div className="flex flex-wrap items-center gap-2 text-sm">
                                        <span className="font-medium">
                                            <PersonRef who={request.requestedBy} />
                                        </span>
                                        <span className="text-ink-muted">·</span>
                                        <span>{askedFor(t, request)}</span>
                                        <RequestStateChip stateCode={request.stateCode} />
                                        {request.decision !== null && (
                                            <span className="text-xs text-ink-faint">
                                                {t('inbox.queue.by', { decider: DECIDER_SLOT })
                                                    .split(DECIDER_SLOT)
                                                    .flatMap((part, index) =>
                                                        index === 0
                                                            ? [part]
                                                            : [
                                                                  <PersonRef
                                                                      key="decider"
                                                                      who={
                                                                          request.decision!
                                                                              .decidedBy
                                                                      }
                                                                  />,
                                                                  part,
                                                              ],
                                                    )}
                                            </span>
                                        )}
                                    </div>
                                    {request.decision !== null &&
                                        request.decision.comment !== '' && (
                                            <div className="mt-1 text-sm text-ink-muted">
                                                {request.decision.comment}
                                            </div>
                                        )}
                                </div>
                            </li>
                        ))}
                    </ul>
                </section>
            )}
        </>
    );
}

/** One request waiting, which opens the screen the administrator decides on. */
function QueueRow({ request }: { readonly request: InboxRequestView }): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    return (
        <tr
            className="cursor-pointer border-b border-line-subtle last:border-b-0 hover:bg-surface-hover"
            onClick={() => void navigate(`/requests/${encodeURIComponent(request.id)}`)}
        >
            <td className="px-4 py-2">
                <span className="flex items-center gap-2">
                    <AccountPicture
                        username={request.requestedBy}
                        name={request.requestedBy}
                        size="sm"
                    />
                    <PersonRef who={request.requestedBy} />
                </span>
            </td>
            <td className="px-4 py-2 font-medium">{askedFor(t, request)}</td>
            <td className="max-w-md px-4 py-2 text-ink-muted">
                {request.reason === '' ? t('inbox.queue.noReason') : request.reason}
            </td>
            {/* A held request is still the reader's to answer, so it belongs in
                this table; the chip is what says it was parked and not left. */}
            <td className="px-4 py-2 text-ink-muted">
                <span className="flex items-center gap-2">
                    <RelativeTime at={request.requestedAt} />
                    {request.stateCode !== 'waiting' && (
                        <RequestStateChip stateCode={request.stateCode} />
                    )}
                </span>
            </td>
        </tr>
    );
}
