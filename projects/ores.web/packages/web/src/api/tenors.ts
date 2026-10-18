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
import type { BusinessDayConventionType } from '@ores/wire-protocol/generated/refdata/domain/business_day_convention_type';
import type { Calendar } from '@ores/wire-protocol/generated/refdata/domain/calendar';
import type { Tenor } from '@ores/wire-protocol/generated/refdata/domain/tenor';
import type { TenorAnchor } from '@ores/wire-protocol/generated/refdata/domain/tenor_anchor';
import type { TenorConvention } from '@ores/wire-protocol/generated/refdata/domain/tenor_convention';
import type { TenorConventionResolution } from '@ores/wire-protocol/generated/refdata/domain/tenor_convention_resolution';
import type { TenorKind } from '@ores/wire-protocol/generated/refdata/domain/tenor_kind';
import type { TenorResolutionAlgorithm } from '@ores/wire-protocol/generated/refdata/domain/tenor_resolution_algorithm';
import type { TenorSchedule } from '@ores/wire-protocol/generated/refdata/domain/tenor_schedule';
import type { TenorUnit } from '@ores/wire-protocol/generated/refdata/domain/tenor_unit';
import { request } from './transport.js';

/**
 * The tenor screen's reads and writes.
 *
 * Rows are the refdata service's, passed through the BFF under its own field
 * names, so each is typed from the generated domain row and parsed only as an
 * object.
 */

const JSON_HEADERS = { 'Content-Type': 'application/json' } as const;

/** Why a write is made. */
export interface TenorIntent {
    readonly reason_code: string;
    readonly commentary: string;
}

/** The result a write carries. */
export interface TenorResult {
    readonly outcome:
        'ok' | 'invalid' | 'denied' | 'missing' | 'conflict' | 'unavailable' | 'failed';
    readonly code: string;
    readonly message: string;
    readonly fields: readonly {
        readonly field: string;
        readonly code: string;
        readonly message: string;
    }[];
}

/** The outcome of a write as the screen renders it. */
export interface TenorWriteOutcome<Row> {
    readonly success: boolean;
    readonly code: string;
    readonly message: string;
    readonly fields: TenorResult['fields'];
    readonly row: Row | undefined;
}

/** The tenor catalogue, the conventions tenors resolve under and the schedule axes. */
export interface TenorCatalogue {
    readonly tenors: readonly Tenor[];
    readonly conventions: readonly TenorConvention[];
    readonly schedules: readonly TenorSchedule[];
}

/** The lists the screen's pickers draw from. */
export interface TenorPickLists {
    readonly kinds: readonly TenorKind[];
    readonly units: readonly TenorUnit[];
    readonly anchors: readonly TenorAnchor[];
    readonly algorithms: readonly TenorResolutionAlgorithm[];
    readonly rollRules: readonly BusinessDayConventionType[];
    readonly calendars: readonly Calendar[];
}

/** A rule resolved as the form holds it, rather than as the store holds it. */
export interface TenorResolutionDraft {
    readonly tenor: Tenor;
    readonly convention: TenorConvention;
    readonly resolution: TenorConventionResolution;
    readonly calendar_code: string;
    readonly horizon: string;
    readonly spot: string | null;
}

/** The date a rule resolves to, and the path it took to reach it. */
export interface TenorResolution {
    readonly result: TenorResult;
    readonly anchorDate: string;
    readonly dateBeforeRoll: string;
    readonly adjustedDate: string;
    readonly rollRule: string;
    readonly calendar: string;
}

/** The rules as they are stored, asked about one tenor on one calendar. */
export interface TenorResolveQuery {
    readonly tenorCode: string;
    readonly conventionCode: string;
    readonly calendarCode: string;
    readonly horizon: string;
    readonly spot: string | null;
}

const resultSchema: z.ZodType<TenorResult> = z.object({
    outcome: z.enum(['ok', 'invalid', 'denied', 'missing', 'conflict', 'unavailable', 'failed']),
    code: z.string(),
    message: z.string(),
    fields: z.array(z.object({ field: z.string(), code: z.string(), message: z.string() })),
});

const resolutionSchema = z.looseObject({
    result: resultSchema,
    anchor_date: z.string(),
    date_before_roll: z.string(),
    adjusted_date: z.string(),
    roll_rule: z.string(),
    calendar: z.string(),
});

/** The reply in the screen's own names. */
function resolutionOf(answer: z.infer<typeof resolutionSchema>): TenorResolution {
    return {
        result: answer.result,
        anchorDate: answer.anchor_date,
        dateBeforeRoll: answer.date_before_roll,
        adjustedDate: answer.adjusted_date,
        rollRule: answer.roll_rule,
        calendar: answer.calendar,
    };
}

function rowsOf<Row>(): z.ZodType<readonly Row[]> {
    return z.array(z.looseObject({})) as unknown as z.ZodType<readonly Row[]>;
}

function rowOf<Row>(): z.ZodType<Row | null> {
    return z.looseObject({ version: z.int() }).nullable() as unknown as z.ZodType<Row | null>;
}

function outcome<Row>(result: TenorResult, row: Row | null): TenorWriteOutcome<Row> {
    return {
        success: result.outcome === 'ok',
        code: result.code,
        message: result.message,
        fields: result.fields,
        row: result.outcome === 'ok' && row !== null ? row : undefined,
    };
}

async function put<Row>(
    path: string,
    field: string,
    write: unknown,
    version: number | null,
    intent: TenorIntent,
): Promise<TenorWriteOutcome<Row>> {
    const answer = z.looseObject({ result: resultSchema }).parse(
        await request(path, {
            method: 'PUT',
            headers: JSON_HEADERS,
            body: JSON.stringify({ intent, version, write }),
        }),
    );
    return outcome(answer.result, rowOf<Row>().parse(answer[field] ?? null));
}

function membershipPath(convention: string, tenor: string): string {
    return `/api/tenors/conventions/${encodeURIComponent(convention)}/resolutions/${encodeURIComponent(tenor)}`;
}

export const tenors = {
    /** Every tenor, every convention and every schedule axis. */
    async catalogue(): Promise<TenorCatalogue> {
        return z
            .object({
                tenors: rowsOf<Tenor>(),
                conventions: rowsOf<TenorConvention>(),
                schedules: rowsOf<TenorSchedule>(),
            })
            .parse(await request('/api/tenors/catalogue', { method: 'GET' }));
    },

    /** Every list a picker draws from. */
    async pickLists(): Promise<TenorPickLists> {
        return z
            .object({
                kinds: rowsOf<TenorKind>(),
                units: rowsOf<TenorUnit>(),
                anchors: rowsOf<TenorAnchor>(),
                algorithms: rowsOf<TenorResolutionAlgorithm>(),
                rollRules: rowsOf<BusinessDayConventionType>(),
                calendars: rowsOf<Calendar>(),
            })
            .parse(await request('/api/tenors/pick-lists', { method: 'GET' }));
    },

    /**
     * The date a drafted rule resolves to.
     *
     * The answer describes the rows handed to it, not the rows the store holds,
     * and a rule the server declines comes back as an answer rather than an
     * error. The reply's snake_case names are mapped to the screen's own here.
     */
    async previewResolution(draft: TenorResolutionDraft): Promise<TenorResolution> {
        return resolutionOf(
            resolutionSchema.parse(
                await request('/api/tenors/preview-resolution', {
                    method: 'POST',
                    headers: JSON_HEADERS,
                    body: JSON.stringify(draft),
                }),
            ),
        );
    },

    /**
     * The date the rules as they are stored resolve the tenor to.
     *
     * Nothing is written: the question names the rows that already exist. A
     * refused resolution is an answer, so it arrives here rather than throwing.
     */
    async resolve(query: TenorResolveQuery): Promise<TenorResolution> {
        return resolutionOf(
            resolutionSchema.parse(
                await request('/api/tenors/resolve', {
                    method: 'POST',
                    headers: JSON_HEADERS,
                    body: JSON.stringify({
                        tenor_code: query.tenorCode,
                        convention_code: query.conventionCode,
                        calendar_code: query.calendarCode,
                        horizon: query.horizon,
                        spot: query.spot,
                    }),
                }),
            ),
        );
    },

    /** The tenors one convention resolves. */
    async resolutions(convention: string): Promise<readonly TenorConventionResolution[]> {
        return z
            .object({ rows: rowsOf<TenorConventionResolution>() })
            .parse(
                await request(
                    `/api/tenors/conventions/${encodeURIComponent(convention)}/resolutions`,
                    { method: 'GET' },
                ),
            ).rows;
    },

    writeTenor(
        write: Tenor,
        version: number | null,
        intent: TenorIntent,
    ): Promise<TenorWriteOutcome<Tenor>> {
        return put<Tenor>('/api/tenors/tenors', 'tenor', write, version, intent);
    },

    writeConvention(
        write: TenorConvention,
        version: number | null,
        intent: TenorIntent,
    ): Promise<TenorWriteOutcome<TenorConvention>> {
        return put<TenorConvention>(
            '/api/tenors/conventions',
            'convention',
            write,
            version,
            intent,
        );
    },

    /** Puts a tenor into a convention's set, or changes its anchor override. */
    async writeMembership(
        convention: string,
        tenor: string,
        anchorOverride: string | null,
        version: number | null,
        intent: TenorIntent,
    ): Promise<TenorWriteOutcome<null>> {
        const answer = z.object({ result: resultSchema }).parse(
            await request(membershipPath(convention, tenor), {
                method: 'PUT',
                headers: JSON_HEADERS,
                body: JSON.stringify({
                    intent,
                    version,
                    write: { anchor_override: anchorOverride },
                }),
            }),
        );
        return outcome(answer.result, null);
    },

    /** Takes a tenor out of a convention's set. */
    async removeMembership(
        convention: string,
        tenor: string,
        version: number,
        intent: TenorIntent,
    ): Promise<TenorWriteOutcome<null>> {
        const answer = z.object({ result: resultSchema }).parse(
            await request(membershipPath(convention, tenor), {
                method: 'DELETE',
                headers: JSON_HEADERS,
                body: JSON.stringify({ intent, version }),
            }),
        );
        return outcome(answer.result, null);
    },
};
