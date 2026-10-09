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
import type { Account } from './domain.js';
import { OperationFailedError } from './errors.js';
import { subjects as accountSubjects } from './generated/iam/protocol/account_operations_protocol.js';
import { decidedResultSchema, mapAccount, wireAccountSchema } from './operations.js';

/**
 * The membership read path: the parties the caller's own account works in.
 *
 * The association read answers with a party identifier and nothing on the
 * browser's path turns one into a name, so the server names them here. The
 * request names no account, because the session names it, so the read cannot
 * answer about anybody else and it needs no permission.
 */

/** One party the caller works in, as the member's own screen reads it. */
export const myPartySchema = z.object({
    partyId: z.string(),
    /** The party's registered name. Empty when the server cannot name it. */
    name: z.string(),
    shortCode: z.string(),
    partyCategory: z.string(),
    businessCenterCode: z.string(),
});

export type MyParty = z.infer<typeof myPartySchema>;

export const myPartiesSchema = z.object({
    /** The party quick sign-in uses, or empty when the account stores none. */
    defaultPartyId: z.string(),
    parties: z.array(myPartySchema),
});

export type MyParties = z.infer<typeof myPartiesSchema>;

/**
 * The wire shape, snake_case as the protocol states it. A party the server
 * cannot name arrives with empty strings, which the screen states as a gap.
 */
const myPartiesReplySchema = z.object({
    result: decidedResultSchema,
    default_party_id: z.string(),
    parties: z.array(
        z.object({
            party_id: z.string(),
            name: z.string(),
            short_code: z.string(),
            party_category: z.string(),
            business_center_code: z.string(),
        }),
    ),
});

export async function readMyParties(caller: AuthenticatedCaller): Promise<MyParties> {
    const reply = await caller.callAuthenticated(
        accountSubjects.get_my_parties_request,
        {},
        myPartiesReplySchema,
    );
    if (reply.result.outcome !== 'ok') {
        throw new OperationFailedError(
            accountSubjects.get_my_parties_request,
            reply.result.message,
        );
    }
    return myPartiesSchema.parse({
        defaultPartyId: reply.default_party_id,
        parties: reply.parties.map((party) => ({
            partyId: party.party_id,
            name: party.name,
            shortCode: party.short_code,
            partyCategory: party.party_category,
            businessCenterCode: party.business_center_code,
        })),
    });
}

const setMyDefaultPartyReplySchema = z.object({
    success: z.boolean(),
    message: z.string().default(''),
});

/**
 * The body of the default-party write, as the BFF takes it from the browser.
 *
 * An empty =partyId= clears the stored default, which is the only way the
 * operation can say "no default".
 */
export const defaultPartyRequestSchema = z.object({
    partyId: z.string(),
});

/**
 * Sets or clears the party quick sign-in uses.
 *
 * An empty party clears the stored default, which is the only way this
 * operation can say "no default". The server checks the party is one the
 * account works in and refuses the write otherwise, so the screen cannot store
 * a party the member cannot act for.
 */
export async function setMyDefaultParty(
    caller: AuthenticatedCaller,
    partyId: string,
): Promise<void> {
    const reply = await caller.callAuthenticated(
        accountSubjects.set_my_default_party_request,
        { party_id: partyId },
        setMyDefaultPartyReplySchema,
    );
    if (!reply.success) {
        throw new OperationFailedError(
            accountSubjects.set_my_default_party_request,
            reply.message,
        );
    }
}

const setReportingLineReplySchema = z
    .object({
        result: decidedResultSchema,
        account: wireAccountSchema.nullable().default(null),
    })
    .transform((row) => ({
        result: row.result,
        account: row.account === null ? null : mapAccount(row.account),
    }));

/** The reporting-line write, as the BFF takes it from the browser. */
export const reportingLineRequestSchema = z.object({
    /** The manager's account id, or empty to clear the line. */
    reportsToAccountId: z.string(),
    /** The account version the screen read, or empty for no precondition. */
    expectedVersion: z.string().default(''),
    reasonCode: z.string().default(''),
    commentary: z.string().default(''),
});

export type ReportingLineWrite = z.infer<typeof reportingLineRequestSchema>;

/**
 * Sets or clears who one account reports to, and nothing else.
 *
 * The write names one field, so a field changed elsewhere is not overwritten,
 * and it states the version the screen read: the server refuses the write as a
 * conflict when the account has moved on. It needs iam::accounts:update, so a
 * plain member cannot reach it.
 */
export async function setReportingLine(
    caller: AuthenticatedCaller,
    accountId: string,
    write: ReportingLineWrite,
): Promise<Account> {
    const reply = await caller.callAuthenticated(
        accountSubjects.set_reporting_line_request,
        {
            account_id: accountId,
            reports_to_account_id: write.reportsToAccountId,
            expected_version: write.expectedVersion,
            change_reason_code: write.reasonCode,
            change_commentary: write.commentary,
        },
        setReportingLineReplySchema,
    );
    if (reply.result.outcome !== 'ok') {
        throw new OperationFailedError(
            accountSubjects.set_reporting_line_request,
            reply.result.message,
        );
    }
    if (reply.account === null) {
        throw new OperationFailedError(
            accountSubjects.set_reporting_line_request,
            'The write answered without the account it wrote.',
        );
    }
    return reply.account;
}

/** One account in the tenant's reporting shape, as the screen reads it. */
export const reportingTreeNodeSchema = z.object({
    accountId: z.string(),
    username: z.string(),
    fullName: z.string(),
    jobTitle: z.string(),
    /** The manager, or null when the account is a root. */
    reportsToAccountId: z.string().nullable(),
    /** Managers between this account and a root, or -1 when it reaches none. */
    depth: z.int(),
    directReports: z.int(),
});

export const reportingTreeSchema = z.object({
    /** How many accounts reach no root, so the screen can state the gap. */
    unrooted: z.int(),
    nodes: z.array(reportingTreeNodeSchema),
});

export type ReportingTree = z.infer<typeof reportingTreeSchema>;
export type ReportingTreeNode = z.infer<typeof reportingTreeNodeSchema>;

const wireReportingTreeNodeSchema = z.object({
    account_id: z.string(),
    username: z.string(),
    full_name: z.string(),
    job_title: z.string(),
    reports_to_account_id: z.string().default(''),
    depth: z.int(),
    direct_reports: z.int(),
});

const wireReportingTreeSchema = z
    .object({
        result: decidedResultSchema,
        nodes: z.array(wireReportingTreeNodeSchema).default([]),
        unrooted: z.int().default(0),
    })
    .transform((row) => ({
        result: row.result,
        tree: {
            unrooted: row.unrooted,
            nodes: row.nodes.map((node) => ({
                accountId: node.account_id,
                username: node.username,
                fullName: node.full_name,
                jobTitle: node.job_title,
                reportsToAccountId:
                    node.reports_to_account_id === '' ? null : node.reports_to_account_id,
                depth: node.depth,
                directReports: node.direct_reports,
            })),
        },
    }));

/**
 * Reads the tenant's reporting shape, or one account's branch of it.
 *
 * An empty root asks for the whole tenant, ordered by depth; a stated root asks
 * for that account's branch. The server needs iam::accounts:read, and an
 * account that reaches no root is answered with depth -1 and counted in
 * `unrooted` rather than being dropped.
 */
export async function readReportingTree(
    caller: AuthenticatedCaller,
    rootAccountId = '',
): Promise<ReportingTree> {
    const reply = await caller.callAuthenticated(
        accountSubjects.get_reporting_tree_request,
        { root_account_id: rootAccountId },
        wireReportingTreeSchema,
    );
    if (reply.result.outcome !== 'ok') {
        throw new OperationFailedError(
            accountSubjects.get_reporting_tree_request,
            reply.result.message,
        );
    }
    return reportingTreeSchema.parse(reply.tree);
}
