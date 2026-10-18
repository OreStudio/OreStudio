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
 * `health` is null when the server states no word for the run's state, which is
 * the two states the mapping has not decided; `stateName` is the state the word
 * would have come from, so the row can say which one it was. A null cell beside
 * an entry in the page's `unread` list is a cell that could not be read, and the
 * entry says why.
 */
export const reportRowViewSchema = z.object({
    name: z.string(),
    reportType: z.string(),
    partyId: z.string(),
    partyCode: z.string().nullable(),
    scheduleExpression: z.string(),
    health: reportHealthWordSchema.nullable(),
    stateName: z.string(),
    latestRun: reportRunViewSchema.nullable(),
    nextFireAt: nullableWireTimestampSchema,
});

export type ReportRowView = z.infer<typeof reportRowViewSchema>;

/** The rows counted by health word, so the shape of the party reads before any row does. */
export const reportTallyViewSchema = z.object({
    failed: z.int().nonnegative(),
    completedWithErrors: z.int().nonnegative(),
    running: z.int().nonnegative(),
    neverRun: z.int().nonnegative(),
    completedClean: z.int().nonnegative(),
    /** The rows whose run states a word no health word covers. */
    unstated: z.int().nonnegative(),
});

export type ReportTallyView = z.infer<typeof reportTallyViewSchema>;

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

export const reportsPageSchema = z.object({
    rows: z.array(reportRowViewSchema),
    tally: reportTallyViewSchema,
    unread: z.array(reportUnreadSchema),
});

export type ReportsPage = z.infer<typeof reportsPageSchema>;

/**
 * The definition as the detail screen reads it, field for field.
 *
 * It is the record the edit form fills from, and the fields it does not offer
 * are the ones a save must not blank: the BFF rebuilds the write from this read
 * rather than from what the browser sends back.
 */
export const reportDefinitionViewSchema = z.object({
    name: z.string(),
    description: z.string(),
    reportType: z.string(),
    partyId: z.string(),
    scheduleExpression: z.string(),
    concurrencyPolicy: z.string(),
    isOfficial: z.boolean(),
    preProcessing: z.string(),
    preparedInputKey: z.string(),
    postProcessing: z.string(),
    schedulerJobId: z.string().nullable(),
    runGrantId: z.string().nullable(),
    fsmStateId: z.string().nullable(),
    version: z.int().nonnegative(),
    modifiedBy: z.string(),
    performedBy: z.string(),
    changeReasonCode: z.string(),
    changeCommentary: z.string(),
    recordedAt: z.string(),
});

export type ReportDefinitionView = z.infer<typeof reportDefinitionViewSchema>;

/** One definition, and the row the list would draw for it. */
export const reportDetailSchema = z.object({
    report: reportRowViewSchema,
    definition: reportDefinitionViewSchema,
});

export type ReportDetail = z.infer<typeof reportDetailSchema>;
