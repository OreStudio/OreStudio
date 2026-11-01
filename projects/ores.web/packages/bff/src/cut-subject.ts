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

import { z } from 'zod';
import { subjects as detailSubjects } from '@ores/wire-protocol/generated/reporting/protocol/cut_approval_detail_protocol';
import { subjects as operationSubjects } from '@ores/wire-protocol/generated/reporting/protocol/report_operations_protocol';
import { resultSchema, row, text } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/*
 * What a cut-authorisation request authorises, joined from the reporting
 * component's own reads.
 *
 * A request is a header. What it is about lives in the kind's own detail row,
 * keyed by the request's id, and the cut that row names holds the label, the
 * kind, the instant and the scope. The decider reads the body on the cut's own
 * record page, so the request page draws the subject and points at the body
 * rather than copying it.
 *
 * One read answers the whole page: the detail rows come back in one call, and
 * the cuts they name are read beside each other. A reader who may not read a cut
 * keeps the page and loses the subject, and the two reasons a subject can be
 * absent - a refused read and a missing row - travel apart, because a screen
 * that says only "not readable" tells a person nothing they can act on.
 */

/** The kinds of approval request whose subject a cut's detail row holds. */
const CUT_AUTHORISATION_KINDS: ReadonlySet<string> = new Set([
    'reporting.eod_cut_authorisation',
    'reporting.ipv_cut_authorisation',
]);

/** The detail row as this module reads it: the subject and the counts it holds. */
const detailSchema = z.looseObject({
    request_id: z.string().default(''),
    cut_id: z.string().default(''),
    deal_count: z.number().default(0),
    market_data_set_count: z.number().default(0),
    gap_count: z.number().default(0),
    applied_by: z.string().default(''),
    applied_at: z.string().nullable().optional(),
});

const detailsReplySchema = z.looseObject({
    result: resultSchema,
    details: z.array(detailSchema).default([]),
    total: z.number().default(0),
});

/** One cut's record, as the record read answers it. */
const cutRecordReplySchema = z.looseObject({
    result: resultSchema,
    cut: row.nullable().default(null),
    kind: row.nullable().default(null),
    label: z.string().default(''),
    state: z.string().default(''),
});

/** One subject as the screen draws it. */
type CutSubject = Readonly<Record<string, unknown>>;

/**
 * The cut each request on the page authorises, and why one has none.
 */
export async function cutSubjectsOf(
    session: LiveSession,
    requests: readonly { readonly id: string; readonly kindCode: string }[],
): Promise<{
    readonly byRequest: ReadonlyMap<string, CutSubject>;
    readonly absentBecause: 'refused' | 'missing';
}> {
    const ids = requests
        .filter((request) => CUT_AUTHORISATION_KINDS.has(request.kindCode))
        .map((request) => request.id);
    if (ids.length === 0) return { byRequest: new Map(), absentBecause: 'missing' };

    let details: readonly z.infer<typeof detailSchema>[];
    try {
        const reply = await session.client.callAuthenticated(
            detailSubjects.list_cut_approval_details_request,
            {
                offset: 0,
                limit: ids.length,
                order: { field: '', descending: false },
                filter: {
                    request_id: null,
                    cut_id: null,
                    party_id: null,
                    id_one_of: null,
                    request_id_one_of: ids,
                    cut_id_one_of: null,
                    party_id_one_of: null,
                },
                as_of: null,
            },
            detailsReplySchema,
        );
        if (reply.result.outcome !== 'ok') {
            return { byRequest: new Map(), absentBecause: 'refused' };
        }
        details = reply.details;
    } catch {
        return { byRequest: new Map(), absentBecause: 'refused' };
    }

    const records = await Promise.all(
        details.map(async (detail) => {
            if (detail.cut_id === '') return null;
            try {
                const reply = await session.client.callAuthenticated(
                    operationSubjects.get_cut_record_request,
                    { cut_id: detail.cut_id },
                    cutRecordReplySchema,
                );
                if (reply.result.outcome !== 'ok' || reply.cut === null) return null;
                return { detail, record: reply };
            } catch {
                return null;
            }
        }),
    );

    const byRequest = new Map<string, CutSubject>();
    for (const entry of records) {
        if (entry === null) continue;
        const { detail, record } = entry;
        const cut = record.cut ?? {};
        byRequest.set(detail.request_id, {
            cut_id: text(cut['id']),
            label: record.label,
            kind_code: text(cut['kind_code']),
            kind_name: record.kind === null ? '' : text(record.kind['name']),
            timepoint: text(cut['timepoint']),
            timepoint_timezone: text(cut['timepoint_timezone']),
            scope_code: text(cut['scope_code']),
            business_centre_code: text(cut['business_centre_code']),
            state: record.state,
            deal_count: detail.deal_count,
            market_data_set_count: detail.market_data_set_count,
            gap_count: detail.gap_count,
            applied_by: detail.applied_by,
            applied_at: detail.applied_at ?? null,
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
export async function withCutSubjects<Request extends { readonly id: string; readonly kindCode: string }>(
    session: LiveSession,
    requests: readonly Request[],
): Promise<readonly Request[]> {
    const { byRequest, absentBecause } = await cutSubjectsOf(session, requests);
    return requests.map((request) => {
        if (!CUT_AUTHORISATION_KINDS.has(request.kindCode)) return request;
        const subject = byRequest.get(request.id);
        return subject === undefined
            ? { ...request, cutSubjectState: absentBecause }
            : { ...request, cutSubject: subject };
    });
}
