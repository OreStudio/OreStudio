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

import type { FastifyInstance, FastifyRequest } from 'fastify';
import { z } from 'zod';
import { readPersonTimeline, readRequestStory } from '@ores/wire-protocol';
import type { Timeline } from '@ores/wire-protocol';
import { invalidRequest } from './errors.js';
import type { LiveSession } from './sessions.js';

/**
 * The timeline routes: one subject's story in one stream.
 *
 * The browser names a subject, never an entity type. Which rows make up a
 * person's story is the server's business, and so is which of them this caller
 * may read: each source the stream draws from is gated by the permission the
 * owning component puts on it, and a source that refuses becomes a gap on the
 * answer rather than a hole a screen cannot explain.
 */

/** The subjects a timeline can be read for, as the reads name them. */
export const TIMELINE_SUBJECTS = ['person', 'request'] as const;
export type TimelineSubject = (typeof TIMELINE_SUBJECTS)[number];

const timelineQuerySchema = z
    .object({
        subject: z.string().min(1).max(32),
        id: z.string().min(1).max(200),
    })
    .strict();

/**
 * One subject's story, newest first.
 *
 * A request's story is read by the inbox, which already answers it as a stream
 * of the same entries; it is wrapped here rather than read again so the two
 * subjects cannot drift into two shapes.
 */
export function registerTimelineRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/timeline', async (request) => {
        const session = requireSession(request);
        const query = timelineQuerySchema.safeParse(request.query ?? {});
        if (!query.success) {
            throw invalidRequest('Name a timeline by its subject and its id.');
        }
        const { subject, id: subjectId } = query.data;
        if (subject === 'person') {
            return await readPersonTimeline(session.client, subjectId);
        }
        if (subject === 'request') {
            const story = await readRequestStory(session.client, subjectId);
            const timeline: Timeline = {
                subject,
                id: subjectId,
                events: story.events,
                gaps: [],
            };
            return timeline;
        }
        throw invalidRequest(
            `A timeline is read for one of ${TIMELINE_SUBJECTS.join(', ')}, not for ${subject}.`,
        );
    });
}
