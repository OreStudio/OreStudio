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
import type { BusinessCentre } from '@ores/wire-protocol/generated/refdata/domain/business_centre';
import type { BusinessDayConventionType } from '@ores/wire-protocol/generated/refdata/domain/business_day_convention_type';
import type { CalendarName } from '@ores/wire-protocol/generated/refdata/domain/calendar_name';
import type { Currency } from '@ores/wire-protocol/generated/refdata/domain/currency';
import type { DayCountFractionType } from '@ores/wire-protocol/generated/refdata/domain/day_count_fraction_type';
import type { FloatingIndexType } from '@ores/wire-protocol/generated/refdata/domain/floating_index_type';
import type { PaymentFrequency } from '@ores/wire-protocol/generated/refdata/domain/payment_frequency';
import type { SubPeriodsCouponType } from '@ores/wire-protocol/generated/refdata/domain/sub_periods_coupon_type';
import { request } from './transport.js';

/**
 * The instrument convention screen's reads and writes.
 *
 * A convention row is the refdata service's, passed through the BFF under its
 * own field names. The 25 families have 25 shapes, so a row here is a record of
 * its columns and the screen reads the columns it draws by name.
 */

const JSON_HEADERS = { 'Content-Type': 'application/json' } as const;

export type { BusinessCentre };

/** Why a write is made. */
export interface ConventionIntent {
    readonly reason_code: string;
    readonly commentary: string;
}

/** The result a write carries. */
export interface ConventionResult {
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

/** One convention row: its columns by name, with the id and the version every row has. */
export type ConventionRow = Readonly<Record<string, unknown>> & {
    readonly id: string;
    readonly version: number;
};

/** An instrument family, the entity that serves it and how many conventions it holds. */
export interface ConventionFamilyCard {
    readonly key: string;
    readonly entity: string;
    readonly writable: boolean;
    /** Null when the count could not be read. */
    readonly count: number | null;
}

/** One page of a family's conventions. */
export interface ConventionPage {
    readonly rows: readonly ConventionRow[];
    readonly total: number;
}

/** The term lists the pickers draw from. */
export interface ConventionPickLists {
    readonly calendars: readonly CalendarName[];
    readonly businessDayConventions: readonly BusinessDayConventionType[];
    readonly dayCountFractions: readonly DayCountFractionType[];
    readonly floatingIndices: readonly FloatingIndexType[];
    readonly paymentFrequencies: readonly PaymentFrequency[];
    readonly subPeriodsCouponTypes: readonly SubPeriodsCouponType[];
    readonly currencies: readonly Currency[];
}

/** The outcome of a write as the screen renders it. */
export interface ConventionWriteOutcome {
    readonly success: boolean;
    readonly code: string;
    readonly message: string;
    readonly fields: ConventionResult['fields'];
    readonly row: ConventionRow | undefined;
}

const resultSchema: z.ZodType<ConventionResult> = z.object({
    outcome: z.enum(['ok', 'invalid', 'denied', 'missing', 'conflict', 'unavailable', 'failed']),
    code: z.string(),
    message: z.string(),
    fields: z.array(z.object({ field: z.string(), code: z.string(), message: z.string() })),
});

const rowSchema = z.looseObject({
    id: z.string(),
    version: z.int(),
}) as unknown as z.ZodType<ConventionRow>;

function rowsOf<Row>(): z.ZodType<readonly Row[]> {
    return z.array(z.looseObject({})) as unknown as z.ZodType<readonly Row[]>;
}

export const conventions = {
    /** Every family, with the entity that serves it and how many conventions it holds. */
    async families(): Promise<readonly ConventionFamilyCard[]> {
        return z
            .object({
                families: z.array(
                    z.object({
                        key: z.string(),
                        entity: z.string(),
                        writable: z.boolean(),
                        count: z.int().nonnegative().nullable(),
                    }),
                ),
            })
            .parse(await request('/api/conventions/families', { method: 'GET' })).families;
    },

    /** The conventions of one family. */
    async page(family: string): Promise<ConventionPage> {
        return z
            .object({ rows: z.array(rowSchema), total: z.int().nonnegative() })
            .parse(
                await request(`/api/conventions/${encodeURIComponent(family)}`, { method: 'GET' }),
            );
    },

    /** Every list a term picker draws from. */
    async pickLists(): Promise<ConventionPickLists> {
        return z
            .object({
                calendars: rowsOf<CalendarName>(),
                businessDayConventions: rowsOf<BusinessDayConventionType>(),
                dayCountFractions: rowsOf<DayCountFractionType>(),
                floatingIndices: rowsOf<FloatingIndexType>(),
                paymentFrequencies: rowsOf<PaymentFrequency>(),
                subPeriodsCouponTypes: rowsOf<SubPeriodsCouponType>(),
                currencies: rowsOf<Currency>(),
            })
            .parse(await request('/api/conventions/pick-lists', { method: 'GET' }));
    },

    /** Writes a convention: as new when no version is given, else against the version read. */
    async write(
        family: string,
        write: Readonly<Record<string, unknown>>,
        version: number | null,
        intent: ConventionIntent,
    ): Promise<ConventionWriteOutcome> {
        const answer = z
            .looseObject({ result: resultSchema, convention: rowSchema.nullable().optional() })
            .parse(
                await request(`/api/conventions/${encodeURIComponent(family)}`, {
                    method: 'PUT',
                    headers: JSON_HEADERS,
                    body: JSON.stringify({ intent, version, write }),
                }),
            );
        const ok = answer.result.outcome === 'ok';
        return {
            success: ok,
            code: answer.result.code,
            message: answer.result.message,
            fields: answer.result.fields,
            row: ok ? (answer.convention ?? undefined) : undefined,
        };
    },
};
