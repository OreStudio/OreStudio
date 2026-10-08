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
import { Link, useParams } from 'react-router';
import type { InboxStoryEvent } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { askedFor, stateLabel } from './words.js';
import { formatDateTime } from '../ui/Time.js';
import { DiffLines } from '../ui/Diff.js';
import { AccountPicture } from '../ui/Images.js';
import { Notice, PageHeader, Tag } from '../ui/Primitives.js';

/**
 * One request's whole story, newest first.
 *
 * Every row any component wrote for the request, in one stream: the request's
 * own versions, the answers given on it, the roles it asked for and what became
 * of them, and the notices this reader was given. The journey spreads those
 * across six tables and answers the reader from one at a time, so nothing
 * anywhere shows what happened next.
 *
 * The reader's entitlement is drawn rather than papered over. The request and
 * the roles it asks for are the asker's to read, so they always appear; the
 * decisions another person took are not, so a member's story has a hole exactly
 * where the answer was given, and the screen says so rather than letting the
 * request look as if it went straight from waiting to answered.
 */
export function RequestStoryPage(): ReactNode {
    const { t, language } = useTranslation();
    const { id = '' } = useParams();
    const story = useQuery({
        queryKey: ['request-story', id],
        queryFn: () => api.requestStory(id),
    });
    const detail = useQuery({
        queryKey: ['request', id],
        queryFn: () => api.request(id),
    });

    if (story.isError) {
        return <Notice tone="error">{story.error.message}</Notice>;
    }
    if (story.isPending || detail.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    const request = detail.data;
    const events = story.data.events;
    if (request === null || request === undefined || events.length === 0) {
        return <Notice tone="warn">{t('inbox.request.notFound')}</Notice>;
    }

    // A request that has an answer but shows no answer was answered by somebody
    // whose decision this reader may not read. Saying so is the difference
    // between a story with a hole in it and a story that lies.
    const answered = request.stateCode !== 'waiting' && request.stateCode !== 'held';
    const showsAnswer = events.some((event) => event.kind === 'decided');

    return (
        <div className="space-y-6">
            <nav className="flex items-center gap-2 text-sm text-ink-muted">
                <Link to="/requests" className="hover:text-ink">
                    {t('inbox.queue.title')}
                </Link>
                <span aria-hidden>/</span>
                <Link to={`/requests/${encodeURIComponent(id)}`} className="hover:text-ink">
                    {t('inbox.request.title', { role: askedFor(t, request) })}
                </Link>
            </nav>

            <PageHeader
                title={t('inbox.story.title', { role: askedFor(t, request) })}
                description={t('inbox.story.lead', {
                    state: stateLabel(t, request.stateCode),
                    date: formatDateTime(request.requestedAt, language),
                })}
            />
            <p className="-mt-4 text-xs text-ink-faint">{t('inbox.request.id', { id })}</p>

            {answered && !showsAnswer && (
                <Notice tone="warn">{t('inbox.story.answerNotShown')}</Notice>
            )}

            <ol>
                {events.map((event, index) => (
                    <StoryRow
                        key={`${event.entityType}:${event.entityId}:${event.kind}:${event.version}:${index}`}
                        event={event}
                        before={previousOf(events, index)}
                    />
                ))}
            </ol>
        </div>
    );
}

/**
 * The older event of the same record, which is the one to draw a change
 * against. The stream is newest first, so it is the next event after this one
 * that names the same row; the oldest event of a record has none, and every
 * field it carries is therefore new.
 */
function previousOf(
    events: readonly InboxStoryEvent[],
    index: number,
): InboxStoryEvent | undefined {
    const event = events[index];
    if (event === undefined) return undefined;
    return events
        .slice(index + 1)
        .find(
            (candidate) =>
                candidate.entityType === event.entityType &&
                candidate.entityId === event.entityId,
        );
}

/** What each kind of event is, and the tint that says it without a word. */
function kindTone(kind: string): 'neutral' | 'accent' | 'warn' | 'up' {
    if (kind === 'granted') return 'up';
    if (kind === 'decided') return 'warn';
    if (kind === 'raised') return 'accent';
    return 'neutral';
}

function dotClass(kind: string): string {
    if (kind === 'granted') return 'border-up/60 bg-up/20';
    if (kind === 'decided') return 'border-warn/60 bg-warn/20';
    if (kind === 'raised') return 'border-accent/60 bg-accent/20';
    return 'border-line bg-surface-overlay';
}

function StoryRow({
    event,
    before,
}: {
    readonly event: InboxStoryEvent;
    readonly before: InboxStoryEvent | undefined;
}): ReactNode {
    const { t, language } = useTranslation();
    const changed = event.fields.filter(
        (field) =>
            (before?.fields.find((older) => older.name === field.name)?.value ?? '') !== field.value,
    );

    return (
        <li className="grid grid-cols-[1rem_1fr]">
            <div className="relative flex justify-center">
                <span className="absolute inset-y-0 w-px bg-line" aria-hidden />
                <span
                    className={`relative z-10 mt-3.5 h-2.5 w-2.5 rounded-full border ${dotClass(event.kind)}`}
                    aria-hidden
                />
            </div>
            <div className="min-w-0 pb-5 pl-3">
                <div className="flex flex-wrap items-baseline gap-2 pt-2.5 text-sm">
                    <Tag tone={kindTone(event.kind)}>{t(`inbox.story.kind.${event.kind}`)}</Tag>
                    {event.actor !== '' && (
                        <AccountPicture username={event.actor} name={event.actor} size="sm" />
                    )}
                    <span className="font-medium">
                        {event.actor === '' ? t('inbox.story.noActor') : event.actor}
                    </span>
                    <span className="text-ink-muted">{event.entityType}</span>
                    <span className="text-xs tabular-nums text-ink-faint">
                        {formatDateTime(event.at, language)}
                    </span>
                    {event.commentary !== '' && (
                        <span className="text-xs text-ink-muted italic">“{event.commentary}”</span>
                    )}
                </div>
                {changed.length > 0 && (
                    <table className="mt-1.5 w-full text-left text-sm">
                        <tbody>
                            {changed.map((field) => (
                                <tr key={field.name} className="align-top">
                                    <td className="w-44 py-1 pr-3 text-xs text-ink-muted">
                                        {field.name}
                                    </td>
                                    <td className="py-1">
                                        <DiffLines
                                            before={
                                                before?.fields.find(
                                                    (older) => older.name === field.name,
                                                )?.value ?? ''
                                            }
                                            after={field.value}
                                        />
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                )}
            </div>
        </li>
    );
}
