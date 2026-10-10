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
import { useMemo, useState, type ReactNode } from 'react';
import { Link, useNavigate, useParams } from 'react-router';
import type { InboxRequestView, PermissionEntry, RoleSummary } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { formatDateTime } from '../ui/Time.js';
import { api } from '../api/client.js';
import { areasOf, covers } from '../access/catalogue.js';
import { PermissionAreas } from '../access/PermissionAreas.js';
import { roleLabel } from '../access/words.js';
import { PersonRef } from '../access/PersonRef.js';
import { AccountPicture } from '../ui/Images.js';
import { Button, Detail, Notice, PageHeader } from '../ui/Primitives.js';
import { RequestStateChip } from './RequestStateChip.js';
import { askedFor, stateLabel } from './words.js';

/**
 * One request, and the answer to it.
 *
 * What the role would let the person do is the whole point of the screen: the
 * administrator decides on the bundle, not on the role's name. The request is
 * read on its own, so a notice that names a request already answered still
 * opens it; the roles it asks for arrive with it, through the read IAM keeps
 * for the person who asked.
 *
 * The role request kind cannot be held, so the two answers offered are yes and
 * no. A refusal needs a reason the person will read, so the control stays
 * disabled until one is written.
 */
export function RequestDetailPage({ me }: { readonly me: string }): ReactNode {
    const { t } = useTranslation();
    const { id = '' } = useParams();
    // The request is read on its own rather than looked up in the queue: a
    // notice carries the request it is about, and a notice is read after the
    // request stopped waiting, so the queue would not hold it.
    const detail = useQuery({
        queryKey: ['request', id],
        queryFn: () => api.request(id),
    });
    const roles = useQuery({ queryKey: ['roles'], queryFn: api.roles });
    const catalogue = useQuery({ queryKey: ['permissions'], queryFn: api.permissions });

    if (detail.isError) {
        return <Notice tone="error">{detail.error.message}</Notice>;
    }
    if (detail.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    const request = detail.data;
    if (request === null || request === undefined) {
        return <Notice tone="warn">{t('inbox.request.notFound')}</Notice>;
    }
    return (
        <Request
            key={request.id + String(request.version)}
            request={request}
            me={me}
            roles={roles.data ?? []}
            catalogue={catalogue.data ?? []}
        />
    );
}

function Request({
    request,
    me,
    roles,
    catalogue,
}: {
    readonly request: InboxRequestView;
    readonly me: string;
    readonly roles: readonly RoleSummary[];
    readonly catalogue: readonly PermissionEntry[];
}): ReactNode {
    const { t, language } = useTranslation();
    const navigate = useNavigate();
    const queries = useQueryClient();
    const [comment, setComment] = useState('');
    const areas = useMemo(() => areasOf(catalogue), [catalogue]);

    /*
     * The role's bundle: the queue names the role the request asks for, and the
     * catalogue holds what it grants. A role that left the catalogue, or one
     * read before it arrived, draws nothing.
     */
    const granted = new Set(
        request.roles.flatMap(
            (role) =>
                roles.find((candidate) => candidate.id === role.roleId)?.permissionCodes ?? [],
        ),
    );
    const self = request.requestedBy === me;
    const open = request.stateCode === 'waiting' || request.stateCode === 'held';
    const refusedWithoutReason = comment.trim() === '';

    const decide = useMutation({
        mutationFn: (decisionCode: 'approve' | 'refuse') =>
            api.decideRequest(request.id, {
                version: request.version,
                decisionCode,
                comment: comment.trim(),
            }),
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['request-queue'] });
            void navigate('/requests');
        },
    });

    return (
        <div className="space-y-6">
            <nav className="flex items-center gap-2 text-sm text-ink-muted">
                {/* Back to where this reader works. A member has no queue and
                    an empty one would send them nowhere, so their own request
                    goes back to the screen that lists their own requests. */}
                <Link to={self ? '/access' : '/requests'} className="hover:text-ink">
                    {self ? t('shell.menu.access') : t('inbox.queue.title')}
                </Link>
                {/* The whole story of this request, which is the same request
                    read across every table it wrote rather than one at a time. */}
                <Link
                    to={`/requests/${encodeURIComponent(request.id)}/story`}
                    className="ml-auto hover:text-ink"
                >
                    {t('inbox.story.request')}
                </Link>
            </nav>

            <PageHeader
                title={t('inbox.request.title', { role: askedFor(t, request) })}
                description={t('inbox.request.lead', {
                    state: stateLabel(t, request.stateCode),
                    date: formatDateTime(request.requestedAt, language),
                })}
            />
            {/* The identifier is here for the person who has to quote it, and
                nowhere else on the page: what the request is about is the
                title, and the identifier is not what anybody reads it for. */}
            <p className="-mt-4 text-xs text-ink-faint">
                {t('inbox.request.id', { id: request.id })}
            </p>

            {decide.isError && <Notice tone="error">{decide.error.message}</Notice>}

            <section className="flex items-center gap-3 rounded-md border border-line bg-surface-raised px-4 py-3">
                <AccountPicture
                    username={request.requestedBy}
                    name={request.requestedBy}
                    size="lg"
                />
                <div className="min-w-0">
                    <div className="flex items-center gap-2 text-sm font-medium">
                        {self ? t('inbox.request.you') : <PersonRef who={request.requestedBy} />}
                        <RequestStateChip stateCode={request.stateCode} />
                    </div>
                    {/* A deadline only means something while the request is
                        still waiting. Once it has an answer the deadline has
                        passed out of the story, and showing it says the
                        opposite: that the request is still running. */}
                    {open && (
                        <div className="text-xs text-ink-muted">
                            {request.expiresAt === ''
                                ? t('inbox.request.noDeadline')
                                : t('inbox.request.expires', {
                                      at: formatDateTime(request.expiresAt, language),
                                  })}
                        </div>
                    )}
                </div>
            </section>

            <section className="space-y-3 rounded-md border border-line bg-surface-raised p-4">
                <h2 className="text-sm font-semibold">{t('inbox.request.why')}</h2>
                <p className="text-sm">{request.reason}</p>
                <dl className="grid grid-cols-1 gap-3 sm:grid-cols-2">
                    {request.roles.map((role) => (
                        <Detail
                            key={role.roleId}
                            label={roleLabel(t, role.name)}
                            value={role.description}
                        />
                    ))}
                </dl>
            </section>

            <section className="space-y-3">
                <h2 className="text-sm font-semibold">
                    {t('inbox.request.wouldAllow', { role: askedFor(t, request) })}
                </h2>
                {granted.size === 0 || covers(granted, '*') ? (
                    <p className="text-sm text-ink-muted">{t('access.nothingMatches')}</p>
                ) : (
                    <PermissionAreas areas={areas} granted={granted} onlyGranted />
                )}
            </section>

            {open && (
                <section className="space-y-3 rounded-md border border-line bg-surface-raised p-4">
                    <h2 className="text-sm font-semibold">{t('inbox.request.answer')}</h2>
                    {self && <Notice tone="warn">{t('inbox.request.notYourself')}</Notice>}
                    <label className="block">
                        <span className="mb-1.5 block text-sm font-medium text-ink-muted">
                            {t('inbox.request.reason')}
                        </span>
                        <textarea
                            className="w-full rounded-md border border-line bg-surface-base px-3 py-2 text-sm text-ink placeholder:text-ink-faint hover:border-line-strong focus:border-accent focus:outline-none focus:ring-3 focus:ring-accent/20"
                            rows={4}
                            value={comment}
                            onChange={(event) => setComment(event.target.value)}
                        />
                        <span className="mt-1 block text-xs text-ink-faint">
                            {t('inbox.request.reasonHint', { who: request.requestedBy })}
                        </span>
                    </label>
                    <div className="flex flex-wrap gap-2">
                        <Button
                            variant="primary"
                            disabled={self || decide.isPending}
                            pending={decide.isPending}
                            onClick={() => decide.mutate('approve')}
                        >
                            {t('inbox.request.give', { role: askedFor(t, request) })}
                        </Button>
                        <Button
                            variant="danger"
                            disabled={self || refusedWithoutReason || decide.isPending}
                            onClick={() => decide.mutate('refuse')}
                        >
                            {t('inbox.request.refuse')}
                        </Button>
                    </div>
                </section>
            )}

            {request.decision !== null && (
                <section className="space-y-2 rounded-md border border-line bg-surface-raised p-4">
                    <h2 className="text-sm font-semibold">{t('inbox.request.decided')}</h2>
                    <div className="flex items-center gap-2 text-sm">
                        <AccountPicture
                            username={request.decision.decidedBy}
                            name={request.decision.decidedBy}
                            size="sm"
                        />
                        <span className="font-medium">
                            <PersonRef who={request.decision.decidedBy} />
                        </span>
                        <span className="text-xs text-ink-faint">
                            {formatDateTime(request.decision.decidedAt, language)}
                        </span>
                    </div>
                    {request.decision.comment !== '' && (
                        <p className="text-sm text-ink-muted">{request.decision.comment}</p>
                    )}
                </section>
            )}
        </div>
    );
}
