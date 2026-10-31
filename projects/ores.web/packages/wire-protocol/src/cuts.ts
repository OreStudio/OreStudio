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
import { nullableWireTimestampSchema, wireTimestampSchema } from './time.js';

/**
 * The shapes the cut screens read over HTTP.
 *
 * A cut stores no state and no label: both are functions of the facts the cut
 * records, and the reporting service derives them so the list, the record and
 * the resolution cannot disagree. The BFF therefore passes the derived words
 * through, and the browser classifies them and prints them and derives nothing.
 *
 * The type list and the type record read a kind of cut, which the design calls
 * reference data: a lookup a person maintains, whose rules the take, the
 * resolution and the archive read.
 */

/**
 * The seven states the design words for a cut, in the order its table states
 * them. The state is derived and stored nowhere, so a caller reads the word and
 * never the facts that would derive it a second time.
 */
export const CUT_STATE_WORDS = [
    'draft',
    'incomplete',
    'taken',
    'in_use',
    'authorised',
    'refused',
    'superseded',
] as const;

export const cutStateWordSchema = z.enum(CUT_STATE_WORDS);

export type CutStateWord = z.infer<typeof cutStateWordSchema>;

/**
 * One row of the cuts list: a body, its derived state and label, the acts that
 * made it and how many runs priced from it.
 *
 * `label` is empty when the store holds no kind of the row's code, which is what
 * a retired kind leaves behind; the row then names its kind by code and says the
 * kind is missing rather than inventing a label.
 *
 * `supersededById` names the later body that holds this one's place, so a person
 * reading a superseded cut is pointed at the one in use.
 */
export const cutRowSchema = z.object({
    id: z.string(),
    label: z.string(),
    state: cutStateWordSchema,
    kindCode: z.string(),
    kindName: z.string(),
    timepoint: wireTimestampSchema,
    timepointTimezone: z.string(),
    scopeCode: z.string(),
    businessCentreCode: z.string(),
    takenBy: z.string(),
    takenAt: nullableWireTimestampSchema,
    approvedBy: z.string(),
    approvedAt: nullableWireTimestampSchema,
    refusedAt: nullableWireTimestampSchema,
    requiresAuthorisation: z.boolean(),
    outputIsIndicative: z.boolean(),
    takeOrdinal: z.int(),
    runCount: z.int().nonnegative(),
    supersededById: z.string().nullable(),
});

export type CutRow = z.infer<typeof cutRowSchema>;

/**
 * One page of cuts, the server's total, and whether the counts on the page are
 * of every cut or of the newest page of them.
 *
 * A scope grows at the rate of its occasion, so the read answers the newest
 * rows it will answer and states both numbers rather than pretending the page is
 * the whole of it.
 */
export const cutsPageSchema = z.object({
    rows: z.array(cutRowSchema),
    total: z.int().nonnegative(),
    partial: z.boolean(),
});

export type CutsPage = z.infer<typeof cutsPageSchema>;

/** A part of the body the assembly could not fill, with the reason it stands. */
export const cutGapSchema = z.object({
    id: z.string(),
    partCode: z.string(),
    item: z.string(),
    reason: z.string(),
});

export type CutGap = z.infer<typeof cutGapSchema>;

/**
 * A kind of cut, as the type list and the type record read it.
 *
 * `signed` is derived from the approval kind the type points at and stored
 * nowhere, as the design states: a firm that gives the close to another function
 * changes one approval kind and no type, and every screen reads the new words.
 * `approvalKindName` and `decidePermissionCode` are that target's, read through
 * the key.
 */
export const cutKindRowSchema = z.object({
    code: z.string(),
    name: z.string(),
    description: z.string(),
    familyCode: z.string(),
    familyName: z.string(),
    signed: z.boolean(),
    approvalKindCode: z.string().nullable(),
    approvalKindName: z.string().nullable(),
    approvalsRequired: z.int().nullable(),
    decidePermissionCode: z.string().nullable(),
    outputIsIndicative: z.boolean(),
    resolutionRuleCode: z.string(),
    resolutionRuleName: z.string(),
    retentionMonths: z.int(),
    retentionDestinationCode: z.string(),
    legalHoldDefault: z.boolean(),
    displayOrder: z.int(),
    cutsCount: z.int().nonnegative(),
    version: z.int().nonnegative(),
});

export type CutKindRow = z.infer<typeof cutKindRowSchema>;

/**
 * The types a person maintains, and whether each row's cut count is the whole of
 * that type or the newest page of it.
 */
export const cutKindsPageSchema = z.object({
    rows: z.array(cutKindRowSchema),
    total: z.int().nonnegative(),
    partial: z.boolean(),
});

export type CutKindsPage = z.infer<typeof cutKindsPageSchema>;

/**
 * One type and the cuts the party holds of it, newest first.
 *
 * The screen groups the rows by the period one step above the type's frequency,
 * and the grouping is the screen's: the read answers the cuts, and a rule that
 * lived here would be a second statement of the period.
 */
export const cutKindRecordSchema = z.object({
    kind: cutKindRowSchema,
    cuts: z.array(cutRowSchema),
    partial: z.boolean(),
});

export type CutKindRecord = z.infer<typeof cutKindRecordSchema>;

/** The rules a type may name, and the families it may belong to, as the lists offer them. */
export const cutKindChoicesSchema = z.object({
    families: z.array(z.object({ code: z.string(), name: z.string() })),
    resolutionRules: z.array(z.object({ code: z.string(), name: z.string() })),
    approvalKinds: z.array(z.object({ code: z.string(), name: z.string() })),
    retentionDestinations: z.array(z.string()),
});

export type CutKindChoices = z.infer<typeof cutKindChoicesSchema>;

/** The body's own facts, as the record page draws them. */
export const cutBodySchema = z.object({
    id: z.string(),
    version: z.int().nonnegative(),
    partyId: z.string(),
    kindCode: z.string(),
    scopeCode: z.string(),
    businessCentreCode: z.string(),
    timepoint: wireTimestampSchema,
    timepointTimezone: z.string(),
    takeOrdinal: z.int(),
    cutTimeCode: z.string().nullable(),
    marketDataSnapshotId: z.string().nullable(),
    dealPopulationId: z.string().nullable(),
    referenceDataVersion: z.string().nullable(),
    engineVersion: z.string().nullable(),
    codeVersion: z.string().nullable(),
    takenBy: z.string(),
    takenAt: nullableWireTimestampSchema,
    requiresAuthorisation: z.boolean(),
    approvalRequestId: z.string().nullable(),
    approvedBy: z.string(),
    approvedAt: nullableWireTimestampSchema,
    refusedAt: nullableWireTimestampSchema,
    refusalComment: z.string(),
    retentionMonths: z.int(),
    retentionDestinationCode: z.string(),
    legalHold: z.boolean(),
    outputIsIndicative: z.boolean(),
    modifiedBy: z.string(),
    performedBy: z.string(),
    changeReasonCode: z.string(),
    changeCommentary: z.string(),
    recordedAt: wireTimestampSchema,
});

export type CutBody = z.infer<typeof cutBodySchema>;

/**
 * One cut as its record page draws it: the body, the derived state and label,
 * the kind's declarations, the gaps the assembly could not fill and the runs
 * that priced from it.
 *
 * `supersededByLabel` is the label of the body that holds this one's place, so
 * the state's basis names a cut a person can quote rather than an id.
 */
export const cutRecordSchema = z.object({
    cut: cutBodySchema,
    label: z.string(),
    state: cutStateWordSchema,
    supersededById: z.string().nullable(),
    supersededByLabel: z.string().nullable(),
    kind: cutKindRowSchema,
    gaps: z.array(cutGapSchema),
    runCount: z.int().nonnegative(),
});

export type CutRecord = z.infer<typeof cutRecordSchema>;

/** The fields the type form offers, and the only ones a save may change. */
export const cutKindWriteSchema = z.object({
    name: z.string().max(200),
    description: z.string().max(2000),
    familyCode: z.string().trim().min(1).max(100),
    approvalKindCode: z.string().trim().min(1).max(200).nullable(),
    outputIsIndicative: z.boolean(),
    resolutionRuleCode: z.string().trim().min(1).max(100),
    retentionMonths: z.int().min(0).max(1200),
    legalHoldDefault: z.boolean(),
    displayOrder: z.int().min(0).max(1_000_000),
});

export type CutKindWrite = z.infer<typeof cutKindWriteSchema>;
