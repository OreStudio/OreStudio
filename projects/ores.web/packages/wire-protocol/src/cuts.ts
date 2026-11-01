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
import { reportHealthWordSchema } from './reporting.js';
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

/**
 * The rules a type may name, the families it may belong to, and the rates
 * centres a take may fix a body against.
 *
 * The centres are not a type's: they are the take's, because the scope revalues
 * against one of them and a scope that names none denotes whichever centre
 * answered first. They are offered here because the two screens that read this
 * lookup are the type form and the take's first step, and a second lookup would
 * be the same rows read twice.
 */
export const cutKindChoicesSchema = z.object({
    families: z.array(z.object({ code: z.string(), name: z.string() })),
    resolutionRules: z.array(z.object({ code: z.string(), name: z.string() })),
    approvalKinds: z.array(z.object({ code: z.string(), name: z.string() })),
    retentionDestinations: z.array(z.string()),
    businessCentres: z.array(z.object({ code: z.string(), name: z.string() })),
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
 * The subject of one cut's approval request: the cut whose body was frozen and
 * the counts of the body's parts.
 *
 * A decider reads the counts here before answering; the cut's own page states
 * the same numbers, because the two records of one act are read from one row.
 * The row also carries the facts an apply will stamp when the request is
 * approved, and no apply exists yet, so nothing here reads them.
 */
export const cutApprovalDetailSchema = z.object({
    id: z.string(),
    requestId: z.string(),
    cutId: z.string(),
    dealCount: z.int().nonnegative(),
    marketDataSetCount: z.int().nonnegative(),
    gapCount: z.int().nonnegative(),
});

export type CutApprovalDetail = z.infer<typeof cutApprovalDetailSchema>;

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
    /** The subject of the take's approval request, or null where the kind required none. */
    detail: cutApprovalDetailSchema.nullable(),
});

export type CutRecord = z.infer<typeof cutRecordSchema>;

/**
 * One run that priced from a cut: the run, the state it reached and the instant
 * it started.
 *
 * The comparison a run of one cut makes against another is the audit journey's,
 * which reads the two runs' results; this list states the runs and their state
 * and points at the run, where that journey begins.
 */
export const cutRunSchema = z.object({
    id: z.string(),
    /** The report the run executes, as a person knows it. */
    name: z.string(),
    /** The state the run reached, as the machines declare it. */
    stateName: z.string(),
    /** The health word the service derives from that state, or null where it states none. */
    health: reportHealthWordSchema.nullable(),
    startedAt: nullableWireTimestampSchema,
    completedAt: nullableWireTimestampSchema,
    pricedAt: nullableWireTimestampSchema,
});

export type CutRun = z.infer<typeof cutRunSchema>;

/** The runs that priced from one cut, newest start first. */
export const cutRunsSchema = z.object({
    runs: z.array(cutRunSchema),
});

export type CutRuns = z.infer<typeof cutRunsSchema>;

/**
 * One part of a body the assembly answered, with the state it reached.
 *
 * `state` is `closed` where the read answered the part and `gapped` where it did
 * not, so a screen draws every part with its own state rather than inferring one
 * from the gaps. `snapshotNote` states what the store cannot say about a part it
 * answered and materialises no reference for.
 */
export const cutBodyPartSchema = z.object({
    partCode: z.string(),
    state: z.enum(['closed', 'gapped']),
    summary: z.string(),
    reference: z.string(),
    item: z.string(),
    reason: z.string(),
    remedy: z.string(),
    snapshotNote: z.string(),
});

export type CutBodyPart = z.infer<typeof cutBodyPartSchema>;

/**
 * The body an assembly answered and the take it wrote.
 *
 * `cutState` is empty on a preview, which writes nothing. A take the server
 * refuses is a failure of the call and not a body, so its code and its sentence
 * travel in the failure the client raises. The gaps are the answer and not a
 * failure: a body that could not be assembled says what closes each one.
 */
export const cutTakeSchema = z.object({
    cutId: z.string(),
    cutLabel: z.string(),
    cutState: z.union([cutStateWordSchema, z.literal('')]),
    requiresAuthorisation: z.boolean(),
    outputIsIndicative: z.boolean(),
    retentionMonths: z.int(),
    retentionDestinationCode: z.string(),
    approvalRequestId: z.string(),
    approvalKindCode: z.string(),
    approvalKindName: z.string(),
    decidePermissionCode: z.string(),
    parts: z.array(cutBodyPartSchema),
    gaps: z.array(cutGapSchema),
    dealCount: z.int().nonnegative(),
    marketDataSetCount: z.int().nonnegative(),
    referenceDataVersion: z.string(),
    engineVersion: z.string(),
    codeVersion: z.string(),
});

export type CutTake = z.infer<typeof cutTakeSchema>;

/** What a person states when they assemble and take a cut by hand. */
export const cutTakeRequestSchema = z.object({
    kindCode: z.string().trim().min(1).max(100),
    scopeCode: z.string().trim().min(1).max(100),
    businessCentreCode: z.string().trim().min(1).max(100),
    timepoint: z.string().trim().min(1).max(64),
    timepointTimezone: z.string().trim().min(1).max(64),
    cutTimeCode: z.string().trim().max(100).default(''),
    reason: z.string().trim().max(2000).default(''),
    preview: z.boolean().default(false),
});

export type CutTakeRequest = z.infer<typeof cutTakeRequestSchema>;

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
