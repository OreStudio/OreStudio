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
import type { AuthenticatedCaller } from './account-operations.js';
import { OperationFailedError } from './errors.js';
import type { InboxRequestView } from './inbox.js';
import { resultEnvelopeSchema } from './operations.js';
import {
    subjects as engineSettingsProposalSubjects,
    type GetEngineSettingsChangesRequest,
    type RecheckEngineSettingsChangesRequest,
} from './generated/reporting/protocol/engine_settings_proposal_operations_protocol.js';
import {
    subjects as bookProposalSubjects,
    type GetBookChangesRequest,
    type PreviewBookChangesRequest,
    type RaiseBookChangesRequest,
    type RecheckBookChangesRequest,
} from './generated/refdata/protocol/book_proposal_operations_protocol.js';

/**
 * What a request proposes, and what running it again says, for any kind.
 *
 * The web draws every kind of request with the same screens, so a kind answers
 * in one shape. A line names the operation, the thing it is about, and each
 * field beside the value it would replace. The kind decides where the lines are
 * kept: a book change keeps typed lines of its own, and a role request keeps
 * the roles it asks for. A kind this module does not know has no lines, which a
 * screen draws as a request with nothing to show.
 */

/** One field of a line beside the value it would replace. */
export interface RequestFieldChange {
    readonly name: string;
    readonly before: string;
    readonly after: string;
    readonly changed: boolean;
}

/** One line of a request, as a decider reads it. */
export interface RequestChange {
    readonly lineNo: number;
    /** put, delete or grant: what the line does to its subject. */
    readonly operation: string;
    readonly subjectId: string;
    /** The version of the subject the maker read; zero for a new one. */
    readonly baseVersion: number;
    readonly applied: boolean;
    readonly fields: readonly RequestFieldChange[];
}

/** What running one line through the real write again said. */
export interface RequestCheck {
    readonly lineNo: number;
    readonly operation: string;
    readonly subjectId: string;
    /** Empty when the write would stand. */
    readonly refusal: string;
}

export interface RequestChecks {
    readonly lines: readonly RequestCheck[];
    /** The parts the policy needs for the lines as they stand now. */
    readonly partCodes: readonly string[];
}

/*
 * What the BFF answers the browser with, parsed by the schemas below. The
 * interfaces above are the shapes the functions return, and these are the same
 * shapes as the browser reads them.
 */

const requestFieldChangeViewSchema = z.object({
    name: z.string(),
    before: z.string().default(''),
    after: z.string().default(''),
    changed: z.boolean().default(false),
});

export const requestChangesViewSchema = z.object({
    requestId: z.string(),
    kindCode: z.string().default(''),
    changes: z
        .array(
            z.object({
                lineNo: z.int().nonnegative(),
                operation: z.string(),
                subjectId: z.string().default(''),
                baseVersion: z.int().nonnegative().default(0),
                applied: z.boolean().default(false),
                fields: z.array(requestFieldChangeViewSchema).default([]),
            }),
        )
        .default([]),
});

const requestCheckViewSchema = z.object({
    lineNo: z.int().nonnegative(),
    operation: z.string(),
    subjectId: z.string().default(''),
    refusal: z.string().default(''),
});

export const requestChecksViewSchema = z.object({
    requestId: z.string(),
    lines: z.array(requestCheckViewSchema).default([]),
    partCodes: z.array(z.string()).default([]),
});

export const bookPreviewViewSchema = z.object({
    lines: z
        .array(
            requestCheckViewSchema.extend({
                columns: z.array(z.string()).default([]),
                partCodes: z.array(z.string()).default([]),
            }),
        )
        .default([]),
    partCodes: z.array(z.string()).default([]),
});

export const bookRaiseViewSchema = bookPreviewViewSchema.extend({
    requestId: z.string().nullable().default(null),
    refusalCode: z.string().default(''),
    refusalMessage: z.string().default(''),
});

const BOOK_CHANGE_KIND = 'refdata.book_change';
const ROLE_GRANT_KIND = 'iam.role_grant';
const ENGINE_SETTINGS_CHANGE_KIND = 'reporting.engine_settings_change';

const lineOutcomeSchema = z.object({
    line_no: z.int().nonnegative().default(0),
    operation: z.string().default(''),
    entity_id: z.string().default(''),
    columns: z.array(z.string()).default([]),
    refusal: z.string().default(''),
    part_codes: z.array(z.string()).default([]),
});

const changesReplySchema = z.object({
    result: resultEnvelopeSchema,
    changes: z
        .array(
            z.object({
                line_no: z.int().nonnegative().default(0),
                operation: z.string().default(''),
                entity_id: z.string().default(''),
                base_version: z.int().nonnegative().default(0),
                applied: z.boolean().default(false),
                fields: z
                    .array(
                        z.object({
                            name: z.string().default(''),
                            before: z.string().default(''),
                            after: z.string().default(''),
                            changed: z.boolean().default(false),
                        }),
                    )
                    .default([]),
            }),
        )
        .default([]),
});

const recheckReplySchema = z.object({
    result: resultEnvelopeSchema,
    lines: z.array(lineOutcomeSchema).default([]),
    part_codes: z.array(z.string()).default([]),
});

const raiseReplySchema = z.object({
    result: resultEnvelopeSchema,
    request_id: z.string().nullable().default(null),
    lines: z.array(lineOutcomeSchema).default([]),
    part_codes: z.array(z.string()).default([]),
});

const settingsChangesReplySchema = z.object({
    result: resultEnvelopeSchema,
    changes: z
        .array(
            z.object({
                line_no: z.int().nonnegative().default(0),
                group_code: z.string().default(''),
                engine_settings_id: z.string().default(''),
                base_version: z.int().nonnegative().default(0),
                applied: z.boolean().default(false),
                fields: z
                    .array(
                        z.object({
                            column_name: z.string().default(''),
                            ore_name: z.string().default(''),
                            before: z.string().default(''),
                            after: z.string().default(''),
                            changed: z.boolean().default(false),
                        }),
                    )
                    .default([]),
            }),
        )
        .default([]),
});

const settingsRecheckReplySchema = z.object({
    result: resultEnvelopeSchema,
    changes: settingsChangesReplySchema.shape.changes,
    refusals: z.array(z.string()).default([]),
    part_codes: z.array(z.string()).default([]),
});

function ok(subject: string, result: z.infer<typeof resultEnvelopeSchema>): void {
    if (result.outcome !== 'ok') {
        throw new OperationFailedError(subject, result.message);
    }
}

/**
 * The lines of a request, for any kind.
 *
 * A request the caller may not open answers no lines, which is what opening it
 * answers too.
 */
export async function readRequestChanges(
    caller: AuthenticatedCaller,
    request: Pick<InboxRequestView, 'id' | 'kindCode' | 'roles'>,
): Promise<readonly RequestChange[]> {
    if (request.kindCode === ROLE_GRANT_KIND) {
        return request.roles.map((role, index) => ({
            lineNo: index + 1,
            operation: 'grant',
            subjectId: role.roleId,
            baseVersion: 0,
            applied: false,
            fields: [
                { name: 'role', before: '', after: role.name, changed: true },
                { name: 'description', before: '', after: role.description, changed: true },
            ],
        }));
    }
    if (request.kindCode === ENGINE_SETTINGS_CHANGE_KIND) {
        const body: GetEngineSettingsChangesRequest = { request_id: request.id };
        const subject = engineSettingsProposalSubjects.get_engine_settings_changes_request;
        const reply = await caller.callAuthenticated(subject, body, settingsChangesReplySchema);
        if (reply.result.outcome === 'missing' || reply.result.outcome === 'denied') return [];
        ok(subject, reply.result);
        return reply.changes.map((line) => ({
            lineNo: line.line_no,
            // The line names the group of settings it proposes, so a reader
            // sees which section of the screen the change is about.
            operation: line.group_code,
            subjectId: line.engine_settings_id,
            baseVersion: line.base_version,
            applied: line.applied,
            fields: line.fields.map((field) => ({
                name: field.column_name,
                before: field.before,
                after: field.after,
                changed: field.changed,
            })),
        }));
    }
    if (request.kindCode !== BOOK_CHANGE_KIND) return [];

    const body: GetBookChangesRequest = { request_id: request.id };
    const subject = bookProposalSubjects.get_book_changes_request;
    const reply = await caller.callAuthenticated(subject, body, changesReplySchema);
    if (reply.result.outcome === 'missing' || reply.result.outcome === 'denied') return [];
    ok(subject, reply.result);
    return reply.changes.map((line) => ({
        lineNo: line.line_no,
        operation: line.operation,
        subjectId: line.entity_id,
        baseVersion: line.base_version,
        applied: line.applied,
        fields: line.fields,
    }));
}

/**
 * What running the request's lines through the real write now says.
 *
 * The live record may have moved since the request was raised, so a decider
 * reads the checks as they stand. A kind that keeps no lines has no checks.
 */
export async function readRequestChecks(
    caller: AuthenticatedCaller,
    request: Pick<InboxRequestView, 'id' | 'kindCode'>,
): Promise<RequestChecks> {
    if (request.kindCode === ENGINE_SETTINGS_CHANGE_KIND) {
        const body: RecheckEngineSettingsChangesRequest = { request_id: request.id };
        const subject = engineSettingsProposalSubjects.recheck_engine_settings_changes_request;
        const reply = await caller.callAuthenticated(subject, body, settingsRecheckReplySchema);
        if (reply.result.outcome === 'missing' || reply.result.outcome === 'denied') {
            return { lines: [], partCodes: [] };
        }
        ok(subject, reply.result);
        return {
            lines: reply.changes.map((line, index) => ({
                lineNo: line.line_no,
                operation: line.group_code,
                subjectId: line.engine_settings_id,
                refusal: reply.refusals[index] ?? '',
            })),
            partCodes: reply.part_codes,
        };
    }
    if (request.kindCode !== BOOK_CHANGE_KIND) return { lines: [], partCodes: [] };

    const body: RecheckBookChangesRequest = { request_id: request.id };
    const subject = bookProposalSubjects.recheck_book_changes_request;
    const reply = await caller.callAuthenticated(subject, body, recheckReplySchema);
    if (reply.result.outcome === 'missing' || reply.result.outcome === 'denied') {
        return { lines: [], partCodes: [] };
    }
    ok(subject, reply.result);
    return {
        lines: reply.lines.map((line) => ({
            lineNo: line.line_no,
            operation: line.operation,
            subjectId: line.entity_id,
            refusal: line.refusal,
        })),
        partCodes: reply.part_codes,
    };
}

/**
 * One book as the maker states it: the columns the person edits, and the
 * version they read.
 *
 * A base version of zero says the book is new.
 */
export interface BookLineInput {
    readonly operation: 'put' | 'delete';
    readonly entityId: string;
    readonly baseVersion: number;
    readonly partyId: string;
    readonly name: string;
    readonly description: string;
    readonly parentPortfolioId: string;
    readonly ownerUnitId: string | null;
    readonly functionalCurrency: string;
    readonly glAccountRef: string;
    readonly costCenter: string;
    readonly bookStatus: string;
    readonly regulatoryBookType: string;
    readonly bookPurposeType: string;
    readonly ledgerFeedType: string;
    readonly isSweepable: boolean;
    readonly ratesCentreCode: string;
    readonly sandboxId: string | null;
}

/** The answer to a preview of book changes. */
export interface BookPreview {
    readonly lines: readonly (RequestCheck & {
        readonly columns: readonly string[];
        /** The parts the policy names for this line alone. */
        readonly partCodes: readonly string[];
    })[];
    readonly partCodes: readonly string[];
}

/** The answer to a raise of book changes. */
export interface BookRaise extends BookPreview {
    /** Set when the request was raised. */
    readonly requestId: string | null;
    /** Why nothing was raised, when it was not. */
    readonly refusalCode: string;
    readonly refusalMessage: string;
}

/**
 * The line as the server reads it.
 *
 * The server numbers the lines and the policy names the parts, so the request,
 * the line number and the provenance are placeholders it does not read.
 */
function wireLine(
    line: BookLineInput,
    tenantId: string,
): PreviewBookChangesRequest['lines'][number] {
    return {
        version: 0,
        tenant_id: tenantId,
        id: line.entityId,
        request_id: line.entityId,
        line_no: 0,
        operation: line.operation,
        base_version: line.baseVersion,
        applied: false,
        entity_id: line.entityId,
        party_id: line.partyId,
        name: line.name,
        description: line.description,
        parent_portfolio_id: line.parentPortfolioId,
        owner_unit_id: line.ownerUnitId,
        functional_currency: line.functionalCurrency,
        gl_account_ref: line.glAccountRef,
        cost_center: line.costCenter,
        book_status: line.bookStatus,
        regulatory_book_type: line.regulatoryBookType,
        book_purpose_type: line.bookPurposeType,
        ledger_feed_type: line.ledgerFeedType,
        is_sweepable: line.isSweepable,
        rates_centre_code: line.ratesCentreCode,
        sandbox_id: line.sandboxId,
        modified_by: '',
        performed_by: '',
        change_reason_code: 'system.update',
        change_commentary: '',
        recorded_at: '1970-01-01 00:00:00Z',
    };
}

function toPreview(reply: z.infer<typeof recheckReplySchema>): BookPreview {
    return {
        lines: reply.lines.map((line) => ({
            lineNo: line.line_no,
            operation: line.operation,
            subjectId: line.entity_id,
            refusal: line.refusal,
            columns: line.columns,
            partCodes: line.part_codes,
        })),
        partCodes: reply.part_codes,
    };
}

/**
 * Runs the proposed writes through the real service and writes nothing.
 *
 * Every refusal the write would give comes back at once, and the parts the
 * policy needs for the lines together. An empty list of parts says the lines
 * need no request.
 */
export async function previewBookChanges(
    caller: AuthenticatedCaller,
    tenantId: string,
    lines: readonly BookLineInput[],
): Promise<BookPreview> {
    const body: PreviewBookChangesRequest = {
        lines: lines.map((line) => wireLine(line, tenantId)),
    };
    const subject = bookProposalSubjects.preview_book_changes_request;
    const reply = await caller.callAuthenticated(subject, body, recheckReplySchema);
    ok(subject, reply.result);
    return toPreview(reply);
}

/**
 * Raises one request that holds the proposed writes.
 *
 * A refusal of the lines, or of a proposal that needs no request, comes back as
 * an answer and not as an error, because the maker has something to correct.
 */
export async function raiseBookChanges(
    caller: AuthenticatedCaller,
    tenantId: string,
    input: { readonly reason: string; readonly lines: readonly BookLineInput[] },
): Promise<BookRaise> {
    const body: RaiseBookChangesRequest = {
        reason: input.reason,
        lines: input.lines.map((line) => wireLine(line, tenantId)),
    };
    const subject = bookProposalSubjects.raise_book_changes_request;
    const reply = await caller.callAuthenticated(subject, body, raiseReplySchema);
    if (reply.result.outcome !== 'ok' && reply.result.outcome !== 'invalid') {
        ok(subject, reply.result);
    }
    return {
        ...toPreview(reply),
        requestId: reply.result.outcome === 'ok' ? reply.request_id : null,
        refusalCode: reply.result.outcome === 'ok' ? '' : reply.result.code,
        refusalMessage: reply.result.outcome === 'ok' ? '' : reply.result.message,
    };
}
