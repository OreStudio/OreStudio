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
import {
    readAuthEvents,
    readSessionStatistics,
    endSession,
    lookupCountry,
    toWireTimestamp,
} from '@ores/wire-protocol';
import { invalidRequest } from './errors.js';
import type { LiveSession } from './sessions.js';

/**
 * The audit sign-ins routes: the authentication events, the session
 * statistics, and the one write the screen makes.
 *
 * The window a period names is computed here rather than in the browser,
 * because the deployment's clock is the one the events were stamped against;
 * a browser that sent its own times would state a window the deployment never
 * measured. Every window is open at its end, so adjacent windows tile without
 * an event landing in two of them. `all` names no window at all, which the
 * reads read as no filter.
 */

export const AUDIT_PERIODS = ['hour', 'day', 'week', 'all'] as const;
export type AuditPeriod = (typeof AUDIT_PERIODS)[number];

const AUDIT_PERIOD_SECONDS: Readonly<Record<AuditPeriod, number>> = {
    hour: 60 * 60,
    day: 24 * 60 * 60,
    week: 7 * 24 * 60 * 60,
    all: 0,
};

/** The window one period names, as the wire timestamps the reads take. */
export function auditWindow(
    period: AuditPeriod,
    now: number,
): { readonly from: string; readonly to: string } {
    if (period === 'all') {
        return { from: '', to: '' };
    }
    return {
        from: toWireTimestamp(new Date(now - AUDIT_PERIOD_SECONDS[period] * 1000)),
        to: toWireTimestamp(new Date(now)),
    };
}

/**
 * The window and page an audit read takes.
 *
 * The account is empty while the screen offers no account control, which the
 * read reads as the filter turned off rather than as a value to match.
 */
const auditWindowQuerySchema = z
    .object({
        accountId: z.string().max(64).default(''),
        period: z.enum(AUDIT_PERIODS).default('day'),
        offset: z.coerce.number().pipe(z.int().min(0)).default(0),
        limit: z.coerce.number().pipe(z.int().min(1).max(500)).default(100),
    })
    .strict();

/** The country lookup takes the one address it resolves. */
const countryQuerySchema = z
    .object({
        address: z.string().min(1).max(64),
    })
    .strict();

/** The authentication events read narrows further by event type. */
const authEventsQuerySchema = auditWindowQuerySchema.extend({
    eventType: z.string().max(64).default(''),
});

export function registerAuditRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /**
     * The tenant's authentication events, newest first.
     *
     * The account filter is accepted though the screen does not offer it,
     * because the read takes it and the journey names it; an empty value is
     * the filter turned off.
     */
    server.get('/api/auth-events', async (request) => {
        const session = requireSession(request);
        const parsed = authEventsQuerySchema.safeParse(request.query ?? {});
        if (!parsed.success) {
            throw invalidRequest(
                'The auth events read takes an account, a period, an event and a page.',
            );
        }
        const { accountId, eventType, period, offset, limit } = parsed.data;
        const window = auditWindow(period, Date.now());
        const events = await readAuthEvents(session.client, {
            accountId,
            eventType,
            fromTime: window.from,
            toTime: window.to,
            offset,
            limit,
        });
        return { events };
    });

    /**
     * The tenant's session statistics, one row per day and account, newest
     * day first.
     */
    server.get('/api/session-statistics', async (request) => {
        const session = requireSession(request);
        const parsed = auditWindowQuerySchema.safeParse(request.query ?? {});
        if (!parsed.success) {
            throw invalidRequest(
                'The session statistics read takes an account, a period and a page.',
            );
        }
        const { accountId, period, offset, limit } = parsed.data;
        const window = auditWindow(period, Date.now());
        const rows = await readSessionStatistics(session.client, {
            accountId,
            fromTime: window.from,
            toTime: window.to,
            offset,
            limit,
        });
        return { rows };
    });

    /**
     * Resolves one address to the country it came from.
     *
     * The search is the caller's tenant's published ranges. An address they
     * do not cover answers with `found: false` and no code, which is the
     * answer a private address always gets.
     */
    server.get('/api/geo/country', async (request) => {
        const session = requireSession(request);
        const parsed = countryQuerySchema.safeParse(request.query ?? {});
        if (!parsed.success) {
            throw invalidRequest('The country lookup takes one address.');
        }
        const countryCode = await lookupCountry(session.client, parsed.data.address);
        return { found: countryCode !== undefined, countryCode: countryCode ?? '' };
    });

    /**
     * Ends one session of the caller's tenant.
     *
     * The tenant scope is the caller's own session, and the handler refuses a
     * session that is not there or has already ended in the body, which the
     * read turns into the failure it is.
     */
    server.post('/api/sessions/:sessionId/end', async (request) => {
        const session = requireSession(request);
        const { sessionId } = request.params as { sessionId: string };
        if (sessionId === '') {
            throw invalidRequest('A session id is required.');
        }
        await endSession(session.client, sessionId);
        return { success: true };
    });
}
