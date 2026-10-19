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
import { nullableWireTimestampSchema } from './time.js';

/**
 * The shapes the reporting screens read over HTTP.
 *
 * The BFF composes a definition with its latest run, the health of that run,
 * its problem counts and its next fire, so the browser draws cells and derives
 * nothing. What the browser does derive is the duration, from the two instants
 * it is given: the wire timestamp has one owner and this package parses it.
 */

/**
 * The five words a row's health cell holds, and the order the tally counts them
 * in. The server derives each one from the definition's latest run; a
 * definition that never ran has no run, and the BFF states its word.
 */
export const REPORT_HEALTH_WORDS = [
    'failed',
    'completed with errors',
    'running',
    'never run',
    'completed clean',
] as const;

export const reportHealthWordSchema = z.enum(REPORT_HEALTH_WORDS);

export type ReportHealthWord = z.infer<typeof reportHealthWordSchema>;

/**
 * The latest run of one definition, as a row states it.
 *
 * The problem counts are null when the run's problems could not be read, which
 * the page's `unread` list names; a run that recorded nothing states zero.
 */
export const reportRunViewSchema = z.object({
    id: z.string(),
    startedAt: nullableWireTimestampSchema,
    completedAt: nullableWireTimestampSchema,
    errors: z.int().nonnegative().nullable(),
    warnings: z.int().nonnegative().nullable(),
});

export type ReportRunView = z.infer<typeof reportRunViewSchema>;

/**
 * One row: a definition and everything the list says about it.
 *
 * `id` is the row's identity, and the only name a page is opened by: the
 * definition's name is not unique, because the table is unique on
 * (tenant, name, party) and a session sees several parties. Two definitions of
 * the same name are two rows, and only the id tells them apart.
 *
 * The party is stated by name, because a person reads the party's name and not
 * its short code; `partyCode` is carried beside it for the screens that already
 * speak in codes.
 *
 * `health` is null when the server states no word for the run's state, which is
 * the two states the mapping has not decided; `stateName` is the state the word
 * would have come from, so the row can say which one it was. A null cell beside
 * an entry in the page's `unread` list is a cell that could not be read, and the
 * entry says why.
 */
export const reportRowViewSchema = z.object({
    id: z.string(),
    name: z.string(),
    reportType: z.string(),
    partyId: z.string(),
    partyName: z.string(),
    partyCode: z.string().nullable(),
    scheduleExpression: z.string(),
    health: reportHealthWordSchema.nullable(),
    stateName: z.string(),
    latestRun: reportRunViewSchema.nullable(),
    nextFireAt: nullableWireTimestampSchema,
});

export type ReportRowView = z.infer<typeof reportRowViewSchema>;

/**
 * A read the list composes that failed.
 *
 * `cell` names the part of the row it would have filled, so a screen draws those
 * cells empty and says which read failed rather than failing the list.
 */
export const reportUnreadSchema = z.object({
    cell: z.enum(['parties', 'latestRun', 'health', 'errors', 'nextFire']),
    message: z.string(),
});

export type ReportUnread = z.infer<typeof reportUnreadSchema>;

/** One page of rows and the server's total, which is what the pager is built from. */
export const reportsPageSchema = z.object({
    rows: z.array(reportRowViewSchema),
    total: z.int().nonnegative(),
    unread: z.array(reportUnreadSchema),
});

export type ReportsPage = z.infer<typeof reportsPageSchema>;

/**
 * The definition as the detail screen reads it, under the field names the record
 * holds.
 *
 * It is a record row, so the shared record parts draw and edit it: the field
 * table names these fields, and the server's history names the same ones. The
 * fields a save must not blank are not the browser's to send at all: the BFF
 * rebuilds the write from this read rather than from what the browser returns.
 *
 * The two identifiers the record also carries, its scheduler job and its run
 * grant, are left out because a screen never shows an identifier, and the write
 * carries them over from the read.
 */
export const reportDefinitionViewSchema = z.object({
    id: z.string(),
    name: z.string(),
    description: z.string(),
    report_type: z.string(),
    party_id: z.string(),
    party_name: z.string(),
    party_code: z.string().nullable(),
    schedule_expression: z.string(),
    concurrency_policy: z.string(),
    is_official: z.boolean(),
    pre_processing: z.string(),
    prepared_input_key: z.string(),
    post_processing: z.string(),
    version: z.int().nonnegative(),
    modified_by: z.string(),
    performed_by: z.string(),
    change_reason_code: z.string(),
    change_commentary: z.string(),
    recorded_at: z.string(),
});

export type ReportDefinitionView = z.infer<typeof reportDefinitionViewSchema>;

/**
 * One definition, the row the list would draw for it, and the reads that
 * failed. The detail screen names a failed read so that a run it did not see is
 * not read as a definition that never ran.
 */
export const reportDetailSchema = z.object({
    report: reportRowViewSchema,
    definition: reportDefinitionViewSchema,
    unread: z.array(reportUnreadSchema),
});

export type ReportDetail = z.infer<typeof reportDetailSchema>;

/**
 * The row a retired name-based address resolves to.
 *
 * A definition is opened by its id, because its name is shared by the same
 * report in other parties. An address saved before the change still names a
 * name, so it is answered with the one row that carries it, or refused when
 * more than one does.
 */
export const reportIdReplySchema = z.object({ id: z.string() });

export type ReportId = z.infer<typeof reportIdReplySchema>;
