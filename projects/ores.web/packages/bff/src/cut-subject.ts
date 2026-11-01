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

import type { FastifyBaseLogger } from 'fastify';
import { z } from 'zod';
import { subjects as operationSubjects } from '@ores/wire-protocol/generated/reporting/protocol/report_operations_protocol';
import { resultSchema, row, text } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/*
 * What a cut-authorisation request authorises, joined from the reporting
 * component's own read.
 *
 * A request is a header. What it is about lives in the kind's own detail row,
 * keyed by the request's id, and the cut that row names holds the label, the
 * kind, the instant and the scope. The decider reads the body on the cut's own
 * record page, so the request page draws the subject and points at the body
 * rather than copying it.
 *
 * One read answers the whole page: the component answers every request the page
 * holds in one call, and the label and the state it answers with are its derived
 * words, so the request page and the cut's own page cannot disagree about the
 * body. A reader who may not read a cut keeps the page and loses the subject, and
 * the two reasons a subject can be absent - a refused read and a missing row -
 * travel apart, because a screen that says only "not readable" tells a person
 * nothing they can act on.
 */

/** The kinds of approval request whose subject a cut's detail row holds. */
const CUT_AUTHORISATION_KINDS: ReadonlySet<string> = new Set([
    'reporting.eod_cut_authorisation',
    'reporting.ipv_cut_authorisation',
]);

/** The subjects reply, as this module reads it. */
const subjectsReplySchema = z.looseObject({
    result: resultSchema,
    subjects: z.array(row).default([]),
});

/** One subject as the screen draws it. */
export type CutSubject = Readonly<Record<string, unknown>>;

/**
 * The cut each request on the page authorises, and why one has none.
 */
export async function cutSubjectsOf(
    session: LiveSession,
    requests: readonly { readonly id: string; readonly kindCode: string }[],
    log: FastifyBaseLogger,
): Promise<{
    readonly byRequest: ReadonlyMap<string, CutSubject>;
    readonly absentBecause: 'refused' | 'missing';
}> {
    const ids = requests
        .filter((request) => CUT_AUTHORISATION_KINDS.has(request.kindCode))
        .map((request) => request.id);
    if (ids.length === 0) return { byRequest: new Map(), absentBecause: 'missing' };

    let subjects: readonly Record<string, unknown>[];
    try {
        const reply = await session.client.callAuthenticated(
            operationSubjects.list_cut_authorisation_subjects_request,
            { request_ids: ids },
            subjectsReplySchema,
        );
        if (reply.result.outcome !== 'ok') {
            log.warn({ result: reply.result }, 'the cut authorisation subjects were refused');
            return { byRequest: new Map(), absentBecause: 'refused' };
        }
        subjects = reply.subjects;
    } catch (error) {
        log.warn({ err: error }, 'the cut authorisation subjects could not be read');
        return { byRequest: new Map(), absentBecause: 'refused' };
    }

    const byRequest = new Map<string, CutSubject>();
    for (const subject of subjects) {
        const requestId = text(subject['request_id']);
        if (requestId === '') continue;
        // The wire states an absent instant as the empty string, and the screen's
        // schema states it as null.
        const appliedAt = text(subject['applied_at']);
        byRequest.set(requestId, {
            cut_id: text(subject['cut_id']),
            label: text(subject['label']),
            kind_code: text(subject['kind_code']),
            kind_name: text(subject['kind_name']),
            timepoint: text(subject['timepoint']),
            timepoint_timezone: text(subject['timepoint_timezone']),
            scope_code: text(subject['scope_code']),
            business_centre_code: text(subject['business_centre_code']),
            state: text(subject['state']),
            deal_count: Number(subject['deal_count'] ?? 0),
            market_data_set_count: Number(subject['market_data_set_count'] ?? 0),
            gap_count: Number(subject['gap_count'] ?? 0),
            applied_by: text(subject['applied_by']),
            applied_at: appliedAt === '' ? null : appliedAt,
        });
    }
    return { byRequest, absentBecause: 'missing' };
}

/**
 * A page of requests, each carrying the cut it authorises where the kind is a
 * cut's own.
 *
 * A kind whose subject is not a cut carries no field at all, so every other
 * kind's row is the row it was before this join existed. A cut request whose
 * subject is absent says why, in the same field a publish request does.
 */
export async function withCutSubjects<
    Request extends { readonly id: string; readonly kindCode: string },
>(
    session: LiveSession,
    requests: readonly Request[],
    log: FastifyBaseLogger,
): Promise<readonly Request[]> {
    const { byRequest, absentBecause } = await cutSubjectsOf(session, requests, log);
    return requests.map((request) => {
        if (!CUT_AUTHORISATION_KINDS.has(request.kindCode)) return request;
        const subject = byRequest.get(request.id);
        return subject === undefined
            ? { ...request, cutSubjectState: absentBecause }
            : { ...request, cutSubject: subject };
    });
}
