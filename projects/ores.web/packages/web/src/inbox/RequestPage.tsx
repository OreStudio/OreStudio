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
import { Link, useNavigate, useParams } from 'react-router';
import type { InboxRequestPartView, InboxRequestView } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { PATHS } from '../shell/paths.js';
import { formatDateTime } from '../ui/Time.js';
import { api } from '../api/client.js';
import { PersonRef } from '../access/PersonRef.js';
import { AccountPicture } from '../ui/Images.js';
import {
    Button,
    Field,
    LinkButton,
    Notice,
    PageHeader,
    Select,
    Textarea,
} from '../ui/Primitives.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { ChangeDelta } from './ChangeDelta.js';
import { ChecksPanel } from './ChecksPanel.js';
import { descriptorFor, summaryOf } from './kinds.js';
import { PartsPanel } from './PartsPanel.js';
import { RequestStateChip } from './RequestStateChip.js';
import { partLabel, stateLabel } from './words.js';

/**
 * One request, and the answer to it, whichever kind it is.
 *
 * The request is read on its own, so a notice that names a request already
 * answered still opens it. The parts say who must still answer, the delta says
 * what would change, and the checks say whether the change can still be made.
 * Each is the same for every kind: a kind adds a descriptor and, if it has
 * something of its own to show, a section.
 *
 * The person who asked never answers, and a refusal needs a reason the person
 * will read, so the control stays disabled until one is written.
 */
export function RequestPage({ me }: { readonly me: string }): ReactNode {
    const { t } = useTranslation();
    const { id = '' } = useParams();
    // The request is read on its own rather than looked up in the queue: a
    // notice carries the request it is about, and a notice is read after the
    // request stopped waiting, so the queue would not hold it.
    const detail = useQuery({
        queryKey: ['request', id],
        queryFn: () => api.request(id),
    });

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
    return <Request key={request.id + String(request.version)} request={request} me={me} />;
}

function Request({
    request,
    me,
}: {
    readonly request: InboxRequestView;
    readonly me: string;
}): ReactNode {
    const { t, language } = useTranslation();
    const descriptor = descriptorFor(request.kindCode);
    const summary = summaryOf(t, request);
    const open = request.stateCode === 'waiting' || request.stateCode === 'held';
    const self = request.requestedBy === me;

    // The lines and the checks are the same read for every kind. A request that
    // is no longer open has nothing left to check.
    const changes = useQuery({
        queryKey: ['request-changes', request.id],
        queryFn: () => api.requestChanges(request.id),
        enabled: descriptor.drawsDelta,
    });
    const checks = useQuery({
        queryKey: ['request-checks', request.id],
        queryFn: () => api.requestChecks(request.id),
        enabled: descriptor.drawsDelta && open,
    });

    const Extras = descriptor.Extras;

    return (
        <div className="space-y-6">
            <AreaTrail area="requests" screen={t('inbox.request.title', { role: summary })} />

            <PageHeader
                title={t('inbox.request.title', { role: summary })}
                description={t('inbox.request.lead', {
                    state: stateLabel(t, request.stateCode),
                    date: formatDateTime(request.requestedAt, language),
                })}
                actions={
                    <LinkButton to={`/requests/${encodeURIComponent(request.id)}/story`}>
                        {t('inbox.story.request')}
                    </LinkButton>
                }
            />
            {/* The identifier is here for the person who has to quote it, and
                nowhere else on the page: what the request is about is the
                title, and the identifier is not what anybody reads it for. */}
            <p className="-mt-4 text-xs text-ink-faint">
                {t('inbox.request.id', { id: request.id })}
            </p>

            {request.subjects.length > 0 && (
                <p className="text-sm text-ink-muted">
                    {t('inbox.request.about')}{' '}
                    {request.subjects.map((subject, index) => (
                        <span key={subject.tradeId}>
                            {index > 0 && ', '}
                            <Link
                                className="text-accent hover:text-accent-bright"
                                to={`${PATHS.trades}/${subject.tradeId}`}
                            >
                                {subject.label}
                            </Link>
                            {subject.tradeVersion > 0 && (
                                <span className="text-ink-faint">
                                    {t('inbox.request.atVersion', {
                                        version: String(subject.tradeVersion),
                                    })}
                                </span>
                            )}
                        </span>
                    ))}
                </p>
            )}

            {request.stateCode === 'apply_failed' && (
                <Notice tone="error">{t('inbox.request.applyFailed')}</Notice>
            )}

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
                            {request.expiresAt === null
                                ? t('inbox.request.noDeadline')
                                : t('inbox.request.expires', {
                                      at: formatDateTime(request.expiresAt, language),
                                  })}
                        </div>
                    )}
                </div>
            </section>

            <section className="space-y-2 rounded-md border border-line bg-surface-raised p-4">
                <h2 className="text-sm font-semibold">{t('inbox.request.why')}</h2>
                <p className="text-sm">{request.reason}</p>
            </section>

            <PartsPanel parts={request.parts} />

            {descriptor.drawsDelta && (
                <section className="space-y-2">
                    <h2 className="text-sm font-semibold">{t('inbox.request.changes')}</h2>
                    {changes.isError ? (
                        <Notice tone="error">{changes.error.message}</Notice>
                    ) : changes.isPending ? (
                        <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                    ) : (
                        <ChangeDelta kindCode={request.kindCode} changes={changes.data} />
                    )}
                </section>
            )}

            {Extras !== undefined && <Extras request={request} />}

            {open && checks.data !== undefined && <ChecksPanel checks={checks.data} />}

            {open && (
                <Answer request={request} self={self} refusedChecks={refusalsOf(checks.data)} />
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

/** How many lines the real write would now refuse; none when the checks have not come. */
function refusalsOf(
    checks: { readonly lines: readonly { readonly refusal: string }[] } | undefined,
): number {
    return checks === undefined ? 0 : checks.lines.filter((line) => line.refusal !== '').length;
}

/** The parts whose turn has come, which are the ones a decider may answer for. */
function answerable(parts: readonly InboxRequestPartView[]): readonly InboxRequestPartView[] {
    return parts.filter((part) => part.stateCode === 'waiting');
}

/**
 * The answer: which part is answered for, why, and approve or refuse.
 *
 * A request that names parts is answered for one part at a time, and the server
 * checks that the decider holds that part's permission. A request that names
 * none takes a plain answer.
 */
function Answer({
    request,
    self,
    refusedChecks,
}: {
    readonly request: InboxRequestView;
    readonly self: boolean;
    readonly refusedChecks: number;
}): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const queries = useQueryClient();
    const descriptor = descriptorFor(request.kindCode);
    const turns = answerable(request.parts);
    const [comment, setComment] = useState('');
    const [chosen, setChosen] = useState(turns[0]?.code ?? '');
    // The choice can only name a part that is still waiting.
    const part = turns.some((turn) => turn.code === chosen) ? chosen : (turns[0]?.code ?? '');
    const refusedWithoutReason = comment.trim() === '';
    const namesParts = request.parts.length > 0;

    const decide = useMutation({
        mutationFn: (decisionCode: 'approve' | 'refuse') =>
            api.decideRequest(request.id, {
                version: request.version,
                decisionCode,
                comment: comment.trim(),
                ...(namesParts && { partCode: part }),
            }),
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['request-queue'] });
            void navigate('/requests');
        },
    });

    const cannotAnswer = self || (namesParts && part === '') || decide.isPending;

    return (
        <section className="space-y-3 rounded-md border border-line bg-surface-raised p-4">
            <h2 className="text-sm font-semibold">{t('inbox.request.answer')}</h2>
            {self && <Notice tone="warn">{t('inbox.request.notYourself')}</Notice>}
            {decide.isError && <Notice tone="error">{decide.error.message}</Notice>}
            {/* A refused check warns and does not block: the server runs the lines
                again when it applies them, and a decider may mean to approve a
                change that is about to be made good by another. */}
            {refusedChecks > 0 && <Notice tone="warn">{t('inbox.request.checksRefused')}</Notice>}
            {namesParts && turns.length === 0 && (
                <Notice tone="info">{t('inbox.request.noTurn')}</Notice>
            )}
            {turns.length > 1 && (
                <Field label={t('inbox.request.answeringFor')}>
                    <Select value={part} onChange={(event) => setChosen(event.target.value)}>
                        {turns.map((turn) => (
                            <option key={turn.code} value={turn.code}>
                                {partLabel(t, turn.code, turn.name)}
                            </option>
                        ))}
                    </Select>
                </Field>
            )}
            <Field
                label={t('inbox.request.reason')}
                hint={t('inbox.request.reasonHint', { who: request.requestedBy })}
            >
                <Textarea
                    rows={4}
                    value={comment}
                    onChange={(event) => setComment(event.target.value)}
                />
            </Field>
            <div className="flex flex-wrap gap-2">
                <Button
                    variant="primary"
                    disabled={cannotAnswer}
                    pending={decide.isPending && decide.variables === 'approve'}
                    onClick={() => decide.mutate('approve')}
                >
                    {descriptor.approveLabel(t, request)}
                </Button>
                <Button
                    variant="danger"
                    disabled={cannotAnswer || refusedWithoutReason}
                    pending={decide.isPending && decide.variables === 'refuse'}
                    onClick={() => decide.mutate('refuse')}
                >
                    {t('inbox.request.refuse')}
                </Button>
            </div>
        </section>
    );
}
