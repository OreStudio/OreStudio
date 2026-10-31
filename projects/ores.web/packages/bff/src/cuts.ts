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
import { subjects as cutKindSubjects } from '@ores/wire-protocol/generated/reporting/protocol/cut_kind_protocol';
import { subjects as cutFamilySubjects } from '@ores/wire-protocol/generated/reporting/protocol/cut_family_protocol';
import { subjects as resolutionRuleSubjects } from '@ores/wire-protocol/generated/reporting/protocol/cut_resolution_rule_protocol';
import { subjects as operationSubjects } from '@ores/wire-protocol/generated/reporting/protocol/report_operations_protocol';
import { subjects as approvalKindSubjects } from '@ores/wire-protocol/generated/inbox/protocol/approval_kind_protocol';
import { HttpFailure, invalidRequest, notFound } from './errors.js';
import {
    input,
    intentSchema,
    readAll,
    refusal,
    resultSchema,
    row,
    text,
    valueText,
} from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/*
 * The cuts, and the kinds of cut they are taken under.
 *
 * A cut stores no state and no label: the reporting service derives both from
 * the facts the cut records, and answers them with the rows. The BFF therefore
 * passes the derived words through and derives nothing, so the list, the record
 * and the resolution cannot disagree about a body.
 *
 * The kind is reference data the reporting component holds: a lookup a person
 * maintains, whose rules the take, the resolution and the archive read. Its
 * authorisation is a foreign key into the inbox's own approval kinds, so the
 * words a screen prints about who signs a cut are that target's and never the
 * kind's.
 *
 * Every read is scoped to the session's party on the server, so no route here
 * states a party a caller could choose.
 */

/** The fields a kind's form offers, and the only ones a save may change. */
const kindWriteSchema = z.object({
    name: z.string().max(200),
    description: z.string().max(2000),
    family_code: z.string().trim().min(1).max(100),
    approval_kind_code: z.string().trim().min(1).max(200).nullable(),
    output_is_indicative: z.boolean(),
    resolution_rule_code: z.string().trim().min(1).max(100),
    retention_months: z.int().min(0).max(1200),
    legal_hold_default: z.boolean(),
    display_order: z.int().min(0).max(1_000_000),
});

const kindEditBodySchema = z.object({
    version: z.int().nonnegative(),
    change: kindWriteSchema,
    intent: intentSchema,
});

const kindRemovalBodySchema = z.object({
    version: z.int().nonnegative(),
    intent: intentSchema,
});

/** The cuts list's own narrowing: a kind, a month, a text in the label and a state. */
const cutsQuerySchema = z.object({
    kind_code: z.string().trim().max(100).default(''),
    month: z.string().trim().max(7).default(''),
    search: z.string().trim().max(256).default(''),
    state: z.enum([
        'draft',
        'incomplete',
        'taken',
        'in_use',
        'authorised',
        'refused',
        'superseded',
    ])
        .or(z.literal(''))
        .default(''),
});

const codeSchema = z.string().trim().min(1).max(100);

/** A cut's id, as the address states it. */
const cutIdSchema = z.uuid();

/** The code an address names, or a 400. */
function codeOf(request: FastifyRequest): string {
    return input(z.object({ code: codeSchema }), request.params).code;
}

/** The id an address names, or a 400. */
function cutIdOf(request: FastifyRequest): string {
    return input(z.object({ id: cutIdSchema }), request.params).id;
}

/** One approval kind, as the inbox holds it. */
interface ApprovalKind {
    readonly code: string;
    readonly name: string;
    readonly approvalsRequired: number | null;
    readonly decidePermissionCode: string | null;
}

/** One kind of cut, as the type list and the type record read it. */
interface KindRow {
    readonly code: string;
    readonly name: string;
    readonly description: string;
    readonly familyCode: string;
    readonly familyName: string;
    readonly signed: boolean;
    readonly approvalKindCode: string | null;
    readonly approvalKindName: string | null;
    readonly approvalsRequired: number | null;
    readonly decidePermissionCode: string | null;
    readonly outputIsIndicative: boolean;
    readonly resolutionRuleCode: string;
    readonly resolutionRuleName: string;
    readonly retentionMonths: number;
    readonly retentionDestinationCode: string;
    readonly legalHoldDefault: boolean;
    readonly displayOrder: number;
    readonly cutsCount: number;
    readonly version: number;
}

/** A row the lists read, before this module carries what the labels need. */
type Read = Readonly<Record<string, unknown>>;

/** A number a row states, or the fallback a screen draws. */
function count(value: unknown): number {
    return typeof value === 'number' ? value : 0;
}

/** Whether a row states a true boolean. */
function flag(value: unknown): boolean {
    return value === true;
}

/** The text a nullable field holds, or null. */
function nullable(value: unknown): string | null {
    return valueText(value);
}

/** The names of the lookups a kind points at, keyed by code. */
async function lookupNames(
    session: LiveSession,
    subject: string,
    rows: string,
): Promise<Map<string, string>> {
    const entries = await readAll(session, subject, rows, { filter: null, as_of: null });
    const names = new Map<string, string>();
    for (const entry of entries) {
        names.set(text(entry['code']), text(entry['name']));
    }
    return names;
}

/**
 * The inbox's approval kinds, keyed by code.
 *
 * A kind of cut names one of these, and the words a screen prints about who
 * signs a cut are the target's. An account that may not read the inbox gets an
 * empty map rather than a failed screen: the cut's own facts state whether a
 * signature was required, so the absence costs the decider's name and nothing
 * else. Only a refusal is read that way; an outage is raised.
 */
async function approvalKinds(session: LiveSession): Promise<Map<string, ApprovalKind>> {
    let entries: readonly Read[];
    try {
        entries = await readAll(session, approvalKindSubjects.list_approval_kinds_request, 'kinds', {
            filter: null,
            as_of: null,
        });
    } catch (failure) {
        if (failure instanceof HttpFailure && failure.status === 403) {
            return new Map();
        }
        throw failure;
    }
    const kinds = new Map<string, ApprovalKind>();
    for (const entry of entries) {
        const code = text(entry['code']);
        kinds.set(code, {
            code,
            name: text(entry['name']),
            approvalsRequired:
                typeof entry['approvals_required'] === 'number'
                    ? entry['approvals_required']
                    : null,
            decidePermissionCode: nullable(entry['decide_permission_code']),
        });
    }
    return kinds;
}

/** One cut row the operations read answered. */
interface CutRow {
    readonly id: string;
    readonly label: string;
    readonly state: string;
    readonly kindCode: string;
    readonly kindName: string;
    readonly timepoint: string;
    readonly timepointTimezone: string;
    readonly scopeCode: string;
    readonly businessCentreCode: string;
    readonly takenBy: string;
    readonly takenAt: string | null;
    readonly approvedBy: string;
    readonly approvedAt: string | null;
    readonly refusedAt: string | null;
    readonly requiresAuthorisation: boolean;
    readonly outputIsIndicative: boolean;
    readonly takeOrdinal: number;
    readonly runCount: number;
    readonly supersededById: string | null;
}

/** The shape `list_cut_rows` answers, as this module reads it. */
const cutRowsSchema = z.looseObject({
    result: resultSchema,
    rows: z.array(row).default([]),
    total: z.int().nonnegative().default(0),
});

/**
 * The cuts one query names.
 *
 * The state and the label arrive derived from the server, because both are
 * functions of the facts a cut records and a second derivation would be a
 * second rule. `partial` states that the rows are the newest page of a scope
 * that grows at the rate of its occasion.
 */
async function readCutRows(
    session: LiveSession,
    narrow: {
        readonly kindCode: string;
        readonly month: string;
        readonly search: string;
        readonly state: string;
    },
): Promise<{ rows: readonly CutRow[]; total: number; partial: boolean }> {
    const reply = await session.client.callAuthenticated(
        operationSubjects.list_cut_rows_request,
        {
            kind_code: narrow.kindCode,
            month: narrow.month,
            search: narrow.search,
            state: narrow.state,
        },
        cutRowsSchema,
    );
    if (reply.result.outcome !== 'ok') {
        throw refusal(reply.result);
    }
    const rows = reply.rows.map((entry) => ({
        id: text(entry['cut_id']),
        label: text(entry['label']),
        state: text(entry['state']),
        kindCode: text(entry['kind_code']),
        kindName: text(entry['kind_name']),
        timepoint: text(entry['timepoint']),
        timepointTimezone: text(entry['timepoint_timezone']),
        scopeCode: text(entry['scope_code']),
        businessCentreCode: text(entry['business_centre_code']),
        takenBy: text(entry['taken_by']),
        takenAt: nullable(entry['taken_at']),
        approvedBy: text(entry['approved_by']),
        approvedAt: nullable(entry['approved_at']),
        refusedAt: nullable(entry['refused_at']),
        requiresAuthorisation: flag(entry['requires_authorisation']),
        outputIsIndicative: flag(entry['output_is_indicative']),
        takeOrdinal: count(entry['take_ordinal']),
        runCount: count(entry['run_count']),
        supersededById: nullable(entry['superseded_by_id']),
    }));
    return { rows, total: count(reply.total), partial: count(reply.total) > rows.length };
}

/**
 * The kinds of cut and the fields a page draws for each, with the cuts the party
 * holds of each kind.
 *
 * One read of the cuts answers the counts of every kind on the list, so the list
 * costs one cut read rather than one per row.
 */
async function kindRows(
    session: LiveSession,
): Promise<{ rows: readonly KindRow[]; partial: boolean }> {
    const [kinds, families, rules, approvals, cuts] = await Promise.all([
        readAll(session, cutKindSubjects.list_cut_kinds_request, 'kinds', {
            filter: null,
            as_of: null,
        }),
        lookupNames(session, cutFamilySubjects.list_cut_families_request, 'families'),
        lookupNames(session, resolutionRuleSubjects.list_cut_resolution_rules_request, 'rules'),
        approvalKinds(session),
        readCutRows(session, { kindCode: '', month: '', search: '', state: '' }),
    ]);

    const counts = new Map<string, number>();
    for (const cut of cuts.rows) {
        counts.set(cut.kindCode, (counts.get(cut.kindCode) ?? 0) + 1);
    }

    const rows = kinds.map((kind): KindRow => {
        const code = text(kind['code']);
        const approvalKindCode = nullable(kind['approval_kind_code']);
        const approval = approvalKindCode === null ? null : approvals.get(approvalKindCode);
        return {
            code,
            name: text(kind['name']),
            description: text(kind['description']),
            familyCode: text(kind['family_code']),
            familyName: families.get(text(kind['family_code'])) ?? text(kind['family_code']),
            // The signed-or-not a person reads is the approval kind's, and the
            // column that names it is the whole of the rule.
            signed: approvalKindCode !== null,
            approvalKindCode,
            approvalKindName:
                approvalKindCode === null ? null : (approval?.name ?? approvalKindCode),
            approvalsRequired: approval?.approvalsRequired ?? null,
            decidePermissionCode: approval?.decidePermissionCode ?? null,
            outputIsIndicative: flag(kind['output_is_indicative']),
            resolutionRuleCode: text(kind['resolution_rule_code']),
            resolutionRuleName:
                rules.get(text(kind['resolution_rule_code'])) ?? text(kind['resolution_rule_code']),
            retentionMonths: count(kind['retention_months']),
            retentionDestinationCode: text(kind['retention_destination_code']),
            legalHoldDefault: flag(kind['legal_hold_default']),
            displayOrder: count(kind['display_order']),
            cutsCount: counts.get(code) ?? 0,
            version: count(kind['version']),
        };
    });
    rows.sort((left, right) => left.displayOrder - right.displayOrder || left.code.localeCompare(right.code));
    return { rows, partial: cuts.partial };
}

/** One cut's record, as `get_cut_record` answers it. */
const cutRecordReplySchema = z.looseObject({
    result: resultSchema,
    cut: row.nullable().default(null),
    kind: row.nullable().default(null),
    label: z.string().default(''),
    state: z.string().default(''),
    superseded_by_id: z.string().default(''),
    gaps: z.array(row).default([]),
    run_count: z.int().nonnegative().default(0),
});

/**
 * One cut as its record page draws it.
 *
 * The body's parts are references: the copy of the deal population, the fixed
 * market data snapshot, the reference data version and the engine and code
 * versions. No read returns what a population or a snapshot holds, so the page
 * draws each reference and says what it is; the gap is the screens'.
 */
async function cutRecord(session: LiveSession, id: string): Promise<Record<string, unknown>> {
    const reply = await session.client.callAuthenticated(
        operationSubjects.get_cut_record_request,
        { cut_id: id },
        cutRecordReplySchema,
    );
    if (reply.result.outcome !== 'ok') {
        throw refusal(reply.result);
    }
    if (reply.cut === null) {
        throw notFound('There is no such cut, or you may not see it.');
    }
    const body = reply.cut;
    const cutKindCode = text(body['kind_code']);

    const [kinds, approvals, superseding] = await Promise.all([
        kindRows(session),
        approvalKinds(session),
        reply.superseded_by_id === ''
            ? Promise.resolve(null)
            : session.client
                  .callAuthenticated(
                      operationSubjects.get_cut_record_request,
                      { cut_id: reply.superseded_by_id },
                      cutRecordReplySchema,
                  )
                  .then((later) => (later.result.outcome === 'ok' ? later.label : null))
                  .catch(() => null),
    ]);

    const kind = kinds.rows.find((entry) => entry.code === cutKindCode) ?? kindOfItself(body);
    const approval = kind.approvalKindCode === null ? null : approvals.get(kind.approvalKindCode);

    return {
        cut: {
            id: text(body['id']),
            version: count(body['version']),
            partyId: text(body['party_id']),
            kindCode: cutKindCode,
            scopeCode: text(body['scope_code']),
            businessCentreCode: text(body['business_centre_code']),
            timepoint: text(body['timepoint']),
            timepointTimezone: text(body['timepoint_timezone']),
            takeOrdinal: count(body['take_ordinal']),
            cutTimeCode: nullable(body['cut_time_code']),
            marketDataSnapshotId: nullable(body['market_data_snapshot_id']),
            dealPopulationId: nullable(body['deal_population_id']),
            referenceDataVersion: nullable(body['reference_data_version']),
            engineVersion: nullable(body['engine_version']),
            codeVersion: nullable(body['code_version']),
            takenBy: text(body['taken_by']),
            takenAt: nullable(body['taken_at']),
            requiresAuthorisation: flag(body['requires_authorisation']),
            approvalRequestId: nullable(body['approval_request_id']),
            approvedBy: text(body['approved_by']),
            approvedAt: nullable(body['approved_at']),
            refusedAt: nullable(body['refused_at']),
            refusalComment: text(body['refusal_comment']),
            retentionMonths: count(body['retention_months']),
            retentionDestinationCode: text(body['retention_destination_code']),
            legalHold: flag(body['legal_hold']),
            outputIsIndicative: flag(body['output_is_indicative']),
            modifiedBy: text(body['modified_by']),
            performedBy: text(body['performed_by']),
            changeReasonCode: text(body['change_reason_code']),
            changeCommentary: text(body['change_commentary']),
            recordedAt: text(body['recorded_at']),
        },
        label: reply.label,
        state: reply.state,
        supersededById: reply.superseded_by_id === '' ? null : reply.superseded_by_id,
        supersededByLabel: superseding,
        kind: { ...kind, approvalsRequired: approval?.approvalsRequired ?? kind.approvalsRequired },
        gaps: reply.gaps.map((gap) => ({
            id: text(gap['id']),
            partCode: text(gap['part_code']),
            item: text(gap['item']),
            reason: text(gap['reason']),
        })),
        runCount: reply.run_count,
    };
}

/**
 * The kind of a cut whose kind the store no longer holds.
 *
 * A retired kind leaves its cuts behind, and a cut records what it copied from
 * its kind when it was taken: whether a signature was required, whether its
 * values are indicative, and the retention it promises. The record therefore
 * draws those facts and states that the kind is missing, rather than refusing
 * to show a body that exists.
 */
function kindOfItself(cut: Read): KindRow {
    const code = text(cut['kind_code']);
    return {
        code,
        name: '',
        description: '',
        familyCode: '',
        familyName: '',
        signed: flag(cut['requires_authorisation']),
        approvalKindCode: null,
        approvalKindName: null,
        approvalsRequired: null,
        decidePermissionCode: null,
        outputIsIndicative: flag(cut['output_is_indicative']),
        resolutionRuleCode: '',
        resolutionRuleName: '',
        retentionMonths: count(cut['retention_months']),
        retentionDestinationCode: text(cut['retention_destination_code']),
        legalHoldDefault: false,
        displayOrder: 0,
        cutsCount: 0,
        version: 0,
    };
}

/**
 * The write a kind's form makes, rebuilt from the read.
 *
 * The form offers nine fields and the row has fifteen, so the write is assembled
 * from the kind just read and only the offered fields are replaced. A save
 * therefore cannot blank the retention destination, which no lookup of tiers
 * states yet, whatever the browser sends.
 */
function kindWriteOf(
    kind: Read,
    change: z.infer<typeof kindWriteSchema>,
): Record<string, unknown> {
    return {
        code: text(kind['code']),
        name: change.name,
        description: change.description,
        family_code: change.family_code,
        approval_kind_code: change.approval_kind_code,
        output_is_indicative: change.output_is_indicative,
        resolution_rule_code: change.resolution_rule_code,
        retention_months: change.retention_months,
        retention_destination_code: text(kind['retention_destination_code']),
        legal_hold_default: change.legal_hold_default,
        display_order: change.display_order,
    };
}

/** One kind as the store holds it, by code, or a 404. */
async function readKind(session: LiveSession, code: string): Promise<Read> {
    const reply = await session.client.callAuthenticated(
        cutKindSubjects.get_cut_kind_request,
        { key: { code } },
        z.looseObject({ result: resultSchema, cut_kind: row.nullable().default(null) }),
    );
    if (reply.result.outcome !== 'ok') {
        throw refusal(reply.result);
    }
    if (reply.cut_kind === null) {
        throw notFound('There is no such cut type, or you may not see it.');
    }
    return reply.cut_kind;
}

/**
 * The cut routes: the kinds of cut, which Reference data draws, and the cuts
 * themselves, which Reporting draws.
 *
 * A read that the session may not make is the server's refusal and not an empty
 * list, so a person is told they may not see the cuts rather than that there are
 * none.
 */
export function registerCutRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /*
     * The types a person maintains: the code, the family they belong to, whether
     * they are signed and by whom, whether their values are indicative, the
     * retention they carry and how many cuts the party holds of each.
     */
    server.get('/api/reporting/cut-kinds', async (request) => {
        const session = requireSession(request);
        const { rows, partial } = await kindRows(session);
        return { rows, total: rows.length, partial };
    });

    /*
     * The lookups a type names, so the form offers what a save will accept: the
     * families, the resolution rules, the inbox's approval kinds and the
     * retention destinations.
     *
     * No lookup states the tiers, so the destinations are the codes the kinds
     * themselves hold: the answer names the tiers the store knows and invents
     * none, and a form shows the destination its kind keeps rather than asking
     * a person to type one.
     */
    server.get('/api/reporting/cut-kind-choices', async (request) => {
        const session = requireSession(request);
        const codes = async (subject: string, rows: string) =>
            (await readAll(session, subject, rows, { filter: null, as_of: null })).map((entry) => ({
                code: text(entry['code']),
                name: text(entry['name']),
            }));
        const [families, rules, approvals, kinds] = await Promise.all([
            codes(cutFamilySubjects.list_cut_families_request, 'families'),
            codes(resolutionRuleSubjects.list_cut_resolution_rules_request, 'rules'),
            approvalKinds(session),
            readAll(session, cutKindSubjects.list_cut_kinds_request, 'kinds', {
                filter: null,
                as_of: null,
            }),
        ]);
        const destinations = new Set<string>();
        for (const kind of kinds) {
            const destination = text(kind['retention_destination_code']);
            if (destination !== '') {
                destinations.add(destination);
            }
        }
        return {
            families,
            resolutionRules: rules,
            approvalKinds: [...approvals.values()].map((kind) => ({
                code: kind.code,
                name: kind.name,
            })),
            retentionDestinations: [...destinations].sort(),
        };
    });

    /*
     * One type and the cuts the party holds of it. The page groups them by the
     * period above the type's frequency, and the grouping is the screen's: the
     * read answers the cuts and a rule that lived here would state the period
     * twice.
     */
    server.get('/api/reporting/cut-kinds/:code', async (request) => {
        const session = requireSession(request);
        const code = codeOf(request);
        const [kinds, cuts] = await Promise.all([
            kindRows(session),
            readCutRows(session, { kindCode: code, month: '', search: '', state: '' }),
        ]);
        const kind = kinds.rows.find((entry) => entry.code === code);
        if (kind === undefined) {
            throw notFound('There is no such cut type, or you may not see it.');
        }
        return { kind, cuts: cuts.rows, partial: cuts.partial };
    });

    /*
     * The naming and the rules of a type. The write names the version the form
     * was filled from, so a type somebody else changed in between is a conflict
     * rather than a silent overwrite. A new type is not offered: the retention
     * destination is a tier the store knows by a check, and no lookup states the
     * tiers yet, so a form that created one would have to invent the value.
     */
    server.put('/api/reporting/cut-kinds/:code', async (request) => {
        const session = requireSession(request);
        const code = codeOf(request);
        const body = input(kindEditBodySchema, request.body);
        const kind = await readKind(session, code);
        const reply = await session.client.callAuthenticated(
            cutKindSubjects.put_cut_kind_request,
            {
                change: {
                    write: kindWriteOf(kind, body.change),
                    precondition: { kind: 'must_match_version', version: body.version },
                },
                intent: body.intent,
            },
            z.looseObject({ result: resultSchema, cut_kind: row.nullable().default(null) }),
        );
        if (reply.result.outcome !== 'ok') {
            throw refusal(reply.result);
        }
        const types = await kindRows(session);
        const written = types.rows.find((entry) => entry.code === code);
        if (written === undefined) {
            throw notFound('There is no such cut type, or you may not see it.');
        }
        return written;
    });

    /*
     * A type no cut was taken under. A type a cut names cannot be removed,
     * because the cut keeps the facts it copied and the label reads the kind's
     * family; the store refuses that removal and the screen states its sentence.
     */
    server.delete('/api/reporting/cut-kinds/:code', async (request, reply) => {
        const session = requireSession(request);
        const code = codeOf(request);
        const body = input(kindRemovalBodySchema, request.body);
        const answer = await session.client.callAuthenticated(
            cutKindSubjects.delete_cut_kind_request,
            {
                removal: {
                    key: { code },
                    precondition: { kind: 'must_match_version', version: body.version },
                },
                intent: body.intent,
            },
            z.looseObject({ result: resultSchema }),
        );
        const result = resultSchema.parse(answer.result);
        if (result.outcome !== 'ok') {
            throw refusal(result);
        }
        return reply.code(204).send();
    });

    /*
     * The cuts a person reads: the derived state and label beside the facts, the
     * taker and the authoriser, and the runs that priced from each body.
     */
    server.get('/api/reporting/cuts', async (request) => {
        const session = requireSession(request);
        const query = cutsQuerySchema.safeParse(request.query);
        if (!query.success) {
            throw invalidRequest(
                'The cuts list names a kind, a month of the form YYYY-MM, a search of at most 256 characters and a state.',
            );
        }
        return readCutRows(session, {
            kindCode: query.data.kind_code,
            month: query.data.month,
            search: query.data.search,
            state: query.data.state,
        });
    });

    /* One cut's record: the body, the acts, the gaps and the derived state. */
    server.get('/api/reporting/cuts/:id', async (request) => {
        const session = requireSession(request);
        const id = cutIdOf(request);
        return cutRecord(session, id);
    });
}
