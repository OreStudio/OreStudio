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
import { askedFor, sourceLabel, stateLabel } from './words.js';
import { formatDateTime } from '../ui/Time.js';
import { PersonRef } from '../access/PersonRef.js';
import { AccountPicture } from '../ui/Images.js';
import { Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { Timeline, kindTone } from '../timeline/Timeline.js';

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
export function RequestStoryPage({ me }: { readonly me: string }): ReactNode {
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
    const mine = request.requestedBy === me;

    return (
        <div className="space-y-6">
            <nav className="flex items-center gap-2 text-sm text-ink-muted">
                {/* Back to where this reader works. A member has no queue and
                    an empty one would send them nowhere, so their own request
                    goes back to the screen that lists their own requests. */}
                <Link to={mine ? '/access' : '/requests'} className="hover:text-ink">
                    {mine ? t('shell.menu.access') : t('inbox.queue.title')}
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

            <Timeline
                timeline={{ subject: 'request', id, events, gaps: [] }}
                renderHead={(event) => <RequestEventHead event={event} />}
            />
        </div>
    );
}

/**
 * What a request's entry says about itself.
 *
 * A request names its own entries rather than drawing the row they came from:
 * what a reader is following is the act — raised, asked, told, answered — and
 * the person who did it, which is why their picture is here and not on a
 * person's stream.
 */
function RequestEventHead({ event }: { readonly event: InboxStoryEvent }): ReactNode {
    const { t, language } = useTranslation();
    return (
        <div className="flex flex-wrap items-baseline gap-2 text-sm">
            <Tag tone={kindTone(event.kind)}>{t(`inbox.story.kind.${event.kind}`)}</Tag>
            {/* An event that names nobody draws nobody. The ask that precedes a
                role is made by the person who raised the request, and naming
                them here would say they acted twice. */}
            {event.actor !== '' && (
                <>
                    <AccountPicture username={event.actor} name={event.actor} size="sm" />
                    <span className="font-medium">
                        <PersonRef who={event.actor} />
                    </span>
                </>
            )}
            <span className="text-ink-muted">{sourceLabel(t, event.entityType)}</span>
            <span className="text-xs tabular-nums text-ink-faint">
                {formatDateTime(event.at, language)}
            </span>
        </div>
    );
}
