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
import { uuidSchema } from './domain.js';
import { OperationFailedError, ServerError } from './errors.js';
import {
    subjects as approvalDecisionSubjects,
    type ListApprovalDecisionsRequest,
} from './generated/inbox/protocol/approval_decision_protocol.js';
import {
    subjects as approvalSubjects,
    type DecideApprovalRequestRequest,
    type GetApprovalRequest,
    type ListApprovalQueueRequest,
    type ListMyApprovalRequestsRequest,
    type WithdrawApprovalRequestRequest,
} from './generated/inbox/protocol/approval_operations_protocol.js';
import {
    subjects as notificationSubjects,
    type ClearNotificationsRequest,
    type CountUnreadNotificationsRequest,
    type ListMyNotificationsRequest,
    type MarkNotificationsReadRequest,
} from './generated/inbox/protocol/notification_operations_protocol.js';
import {
    subjects as accountSubjects,
    type ListAccountsRequest,
} from './generated/iam/protocol/account_protocol.js';
import {
    subjects as roleGrantRequestSubjects,
    type ListRoleGrantRequestsRequest,
} from './generated/iam/protocol/role_grant_request_protocol.js';
import {
    subjects as roleRequestSubjects,
    type AskForRolesRequest,
    type GetRequestRolesRequest,
} from './generated/iam/protocol/role_request_operations_protocol.js';
import { resultEnvelopeSchema } from './operations.js';

/**
 * The inbox: asking for roles, answering the requests, and reading the
 * notifications that say what happened.
 *
 * The C++ service serves every one of these operations. What it does not serve
 * is a request that names the roles it asks for or the person who asked: an
 * approval request carries the kind, the state, the reason and the account id,
 * and nothing a screen can draw. The names are joined here, in a bounded
 * number of reads per page, rather than in the browser, which must not open a
 * broker connection at all.
 */

/** Subjects for the inbox operations, kept beside the operations that use them. */
export const INBOX_SUBJECTS = {
    askForRoles: roleRequestSubjects.ask_for_roles_request,
    getRequestRoles: roleRequestSubjects.get_request_roles_request,
    mine: approvalSubjects.list_my_approval_requests_request,
    queue: approvalSubjects.list_approval_queue_request,
    getRequest: approvalSubjects.get_approval_request,
    withdraw: approvalSubjects.withdraw_approval_request_request,
    decide: approvalSubjects.decide_approval_request_request,
    myNotifications: notificationSubjects.list_my_notifications_request,
    unreadCount: notificationSubjects.count_unread_notifications_request,
    markRead: notificationSubjects.mark_notifications_read_request,
    clear: notificationSubjects.clear_notifications_request,
} as const;

/**
 * The reads that put the names back into an approval request.
 *
 * None of them is an inbox operation: each is the store that holds the piece
 * the request itself omits. The roles a request asks for are not here, because
 * no store serves them to the person who asked: IAM answers those through the
 * operation named in {@link INBOX_SUBJECTS}. The rest are named here rather
 * than in a screen because a screen may not make them, and refused together
 * rather than one at a time, because a member holds none of the permissions
 * they need.
 */
export const INBOX_JOIN_SUBJECTS = {
    decisions: approvalDecisionSubjects.list_approval_decisions_request,
    roleGrantRequests: roleGrantRequestSubjects.list_role_grant_requests_request,
    accounts: accountSubjects.list_accounts_request,
} as const;

/** The service refuses a filter longer than this, so a join never asks for more. */
const JOIN_PAGE = 1000;

/** A list read pages from the start, because the stores here page in key order. */
const UNORDERED = { field: '', descending: false } as const;

const wireApprovalRequestSchema = z.object({
    version: z.int().nonnegative().default(0),
    id: uuidSchema,
    kind_code: z.string().default(''),
    state_code: z.string().default(''),
    requested_by: z.string().default(''),
    requested_at: z.string().default(''),
    reason: z.string().default(''),
    expires_at: z.string().nullable().default(null),
});

const wireApprovalDecisionSchema = z.object({
    request_id: z.string().default(''),
    decision_code: z.string().default(''),
    decided_by: z.string().default(''),
    decided_at: z.string().default(''),
    comment: z.string().default(''),
});

const wireNotificationArgumentSchema = z.object({
    name: z.string().default(''),
    value: z.string().default(''),
});

const wireNotificationSchema = z.object({
    id: uuidSchema,
    kind_code: z.string().default(''),
    message_key: z.string().default(''),
    raised_by: z.string().default(''),
    raised_at: z.string().default(''),
    link_route: z.string().default(''),
    link_id: z.string().default(''),
    arguments: z.array(wireNotificationArgumentSchema).default([]),
    read_at: z.string().default(''),
});

const wireRoleGrantRequestSchema = z.object({
    request_id: z.string().default(''),
    account_id: z.string().default(''),
});

const wireAccountSchema = z.object({
    id: z.string().default(''),
    username: z.string().default(''),
});

const resultReplySchema = z.object({ result: resultEnvelopeSchema });

const askReplySchema = z.object({
    result: resultEnvelopeSchema,
    request_id: z.string().default(''),
});

const requestsReplySchema = z.object({
    result: resultEnvelopeSchema,
    requests: z.array(wireApprovalRequestSchema).default([]),
    total: z.int().nonnegative().default(0),
});

const requestReplySchema = z.object({
    result: resultEnvelopeSchema,
    request: wireApprovalRequestSchema.nullable().default(null),
});

const decisionsReplySchema = z.object({
    result: resultEnvelopeSchema,
    decisions: z.array(wireApprovalDecisionSchema).default([]),
    total: z.int().nonnegative().default(0),
});

const requestRolesReplySchema = z.object({
    result: resultEnvelopeSchema,
    roles: z.array(
        z.object({
            id: z.string().default(''),
            name: z.string().default(''),
            description: z.string().default(''),
        }),
    ).default([]),
});

const roleGrantRequestsReplySchema = z.object({
    result: resultEnvelopeSchema,
    role_grant_requests: z.array(wireRoleGrantRequestSchema).default([]),
    total: z.int().nonnegative().default(0),
});

const accountsReplySchema = z.object({
    result: resultEnvelopeSchema,
    accounts: z.array(wireAccountSchema).default([]),
    total: z.int().nonnegative().default(0),
});

const notificationsReplySchema = z.object({
    result: resultEnvelopeSchema,
    notifications: z.array(wireNotificationSchema).default([]),
    total: z.int().nonnegative().default(0),
});

const unreadReplySchema = z.object({
    result: resultEnvelopeSchema,
    unread: z.int().nonnegative().default(0),
});

const markedReplySchema = z.object({
    result: resultEnvelopeSchema,
    marked: z.int().nonnegative().default(0),
});

const clearedReplySchema = z.object({
    result: resultEnvelopeSchema,
    cleared: z.int().nonnegative().default(0),
});

/** One role a request asks for, as a screen draws it. */
export const inboxRequestRoleViewSchema = z.object({
    roleId: z.string().default(''),
    name: z.string().default(''),
    description: z.string().default(''),
});

/**
 * What was decided about a request, as the person who asked reads it.
 *
 * The decider is named by account id. A screen that shows only their own
 * requests has nobody to name but themselves.
 */
export const inboxRequestDecisionViewSchema = z.object({
    decisionCode: z.string().default(''),
    decidedBy: z.string().default(''),
    decidedAt: z.string().default(''),
    comment: z.string().default(''),
});

/**
 * One approval request as a screen draws it.
 *
 * `kindCode` and `stateCode` are the server's own codes, and the screen
 * translates them: what a kind is called, and which states are open, are the
 * interface's business and not this module's. `requestedBy` is the account's
 * username when the join could read it and the account's id when it could
 * not, because a plain member may not read the account list.
 */
export const inboxRequestViewSchema = z.object({
    id: z.string().default(''),
    version: z.int().nonnegative().default(0),
    kindCode: z.string().default(''),
    stateCode: z.string().default(''),
    requestedBy: z.string().default(''),
    requestedAt: z.string().default(''),
    reason: z.string().default(''),
    expiresAt: z.string().default(''),
    roles: z.array(inboxRequestRoleViewSchema).default([]),
    decision: inboxRequestDecisionViewSchema.nullable().default(null),
});

/**
 * One notification as the bell draws it.
 *
 * The message is a key and the values its wording names, not prose: the
 * interface renders it in the reader's language. An empty `readAt` is what
 * makes it unread.
 */
export const inboxNotificationViewSchema = z.object({
    id: z.string().default(''),
    kindCode: z.string().default(''),
    messageKey: z.string().default(''),
    raisedBy: z.string().default(''),
    raisedAt: z.string().default(''),
    linkRoute: z.string().default(''),
    linkId: z.string().default(''),
    arguments: z.array(wireNotificationArgumentSchema).default([]),
    readAt: z.string().default(''),
});

/** One page of anything, with the size of the whole so a screen can page. */
export interface InboxPage<Item> {
    readonly items: readonly Item[];
    readonly total: number;
}

/** The page a screen parses, built from the item schema it holds. */
export function inboxPageSchema<Item extends z.ZodType>(item: Item) {
    return z.object({
        items: z.array(item).default([]),
        total: z.int().nonnegative().default(0),
    });
}

export const inboxRequestPageSchema = inboxPageSchema(inboxRequestViewSchema);
export const inboxNotificationPageSchema = inboxPageSchema(inboxNotificationViewSchema);

/** The types a screen reads, as the schemas above describe them. */
export type InboxRequestRoleView = z.infer<typeof inboxRequestRoleViewSchema>;
export type InboxRequestDecisionView = z.infer<typeof inboxRequestDecisionViewSchema>;
export type InboxRequestView = z.infer<typeof inboxRequestViewSchema>;
export type InboxNotificationView = z.infer<typeof inboxNotificationViewSchema>;

function ok(subject: string, result: z.infer<typeof resultEnvelopeSchema>): void {
    if (result.outcome !== 'ok') {
        throw new OperationFailedError(subject, result.message);
    }
}

/**
 * Whether the server refused the call rather than failing it.
 *
 * A permission the caller does not hold reaches the transport as an `X-Error`
 * header. The envelope carries the same refusal in-band, and both are read
 * here so a join is abandoned for one reason rather than two.
 */
function isRefusal(error: unknown): boolean {
    return (
        error instanceof ServerError &&
        (error.code === 'forbidden' || error.code === 'unauthorized')
    );
}

/**
 * A read a join needs and the caller may not make.
 *
 * Answers nothing when the server refuses, because the permission needed for
 * the join is not the permission needed for the operation that asked for it: a
 * plain member reads their own requests and may read neither the decisions
 * taken on them nor the account list behind them. Anything that is not a
 * refusal still throws, so a store that is broken is not mistaken for one that
 * is closed.
 */
async function readJoin<Schema extends z.ZodType>(
    caller: AuthenticatedCaller,
    subject: string,
    body: unknown,
    schema: Schema,
): Promise<z.infer<Schema> | undefined> {
    let reply: z.infer<Schema>;
    try {
        reply = await caller.callAuthenticated(subject, body, schema);
    } catch (error) {
        if (isRefusal(error)) return undefined;
        throw error;
    }
    const result = (reply as { result: z.infer<typeof resultEnvelopeSchema> }).result;
    if (result.outcome === 'ok') return reply;
    if (result.outcome === 'denied') return undefined;
    throw new OperationFailedError(subject, result.message);
}

/**
 * Who is reading, so a join can name the one account it always may: their own.
 *
 * A member may not read the account list, so the store cannot turn the account
 * id on their own request into a username. The reader is that person, which
 * makes their own name the one answer no store has to be asked for.
 */
export interface RequestViewer {
    readonly accountId: string;
    readonly username: string;
}

/**
 * Everything the join knows, keyed by what it is looked up by.
 *
 * An empty map is not a failure: it is what a caller who may read none of
 * these stores gets, and every request is then drawn with no role names and
 * the account id in place of a username.
 */
interface RequestJoin {
    readonly rolesByRequest: ReadonlyMap<string, InboxRequestRoleView[]>;
    readonly accountByRequest: ReadonlyMap<string, string>;
    readonly usernameByAccount: ReadonlyMap<string, string>;
    readonly decisionByRequest: ReadonlyMap<string, InboxRequestDecisionView>;
}

const NO_JOIN: RequestJoin = {
    rolesByRequest: new Map(),
    accountByRequest: new Map(),
    usernameByAccount: new Map(),
    decisionByRequest: new Map(),
};

/**
 * The roles each request asks for.
 *
 * The store refuses every filter on its own read of these roles, so the join
 * goes through the operation IAM keeps for the purpose, which is entitled to
 * the person who raised the request. One call answers for one request, and a
 * request the caller may not read answers as absent rather than as refused, so
 * the two are one answer here.
 */
async function readRolesByRequest(
    caller: AuthenticatedCaller,
    requestIds: readonly string[],
): Promise<Map<string, InboxRequestRoleView[]>> {
    const rolesByRequest = new Map<string, InboxRequestRoleView[]>();
    const readOne = async (requestId: string): Promise<InboxRequestRoleView[] | undefined> => {
        const request: GetRequestRolesRequest = { request_id: requestId };
        let reply: z.infer<typeof requestRolesReplySchema>;
        try {
            reply = await caller.callAuthenticated(
                INBOX_SUBJECTS.getRequestRoles,
                request,
                requestRolesReplySchema,
            );
        } catch (error) {
            if (isRefusal(error)) return undefined;
            throw error;
        }
        if (reply.result.outcome === 'missing' || reply.result.outcome === 'denied') {
            return undefined;
        }
        ok(INBOX_SUBJECTS.getRequestRoles, reply.result);
        return reply.roles.map((role) => ({
            roleId: role.id,
            name: role.name,
            description: role.description,
        }));
    };

    const answers = await Promise.all(
        requestIds.map(async (id) => [id, await readOne(id)] as const),
    );
    for (const [id, roles] of answers) {
        if (roles !== undefined) rolesByRequest.set(id, roles);
    }
    return rolesByRequest;
}

/** The account that raised each request, and the username of each account. */
async function readRequesters(
    caller: AuthenticatedCaller,
    requestIds: readonly string[],
): Promise<{ accountByRequest: Map<string, string>; usernameByAccount: Map<string, string> }> {
    const accountByRequest = new Map<string, string>();
    const usernameByAccount = new Map<string, string>();
    const request: ListRoleGrantRequestsRequest = {
        offset: 0,
        limit: JOIN_PAGE,
        order: UNORDERED,
        filter: {
            account_id: null,
            request_id_one_of: requestIds.slice(0, JOIN_PAGE),
            account_id_one_of: null,
        },
        as_of: null,
    };
    const grants = await readJoin(
        caller,
        INBOX_JOIN_SUBJECTS.roleGrantRequests,
        request,
        roleGrantRequestsReplySchema,
    );
    for (const grant of grants?.role_grant_requests ?? []) {
        accountByRequest.set(grant.request_id, grant.account_id);
    }
    if (accountByRequest.size === 0) return { accountByRequest, usernameByAccount };

    const accounts: ListAccountsRequest = {
        offset: 0,
        limit: JOIN_PAGE,
        order: UNORDERED,
        filter: {
            id_one_of: [...new Set(accountByRequest.values())].slice(0, JOIN_PAGE),
            search: null,
        },
        as_of: null,
    };
    const found = await readJoin(
        caller,
        INBOX_JOIN_SUBJECTS.accounts,
        accounts,
        accountsReplySchema,
    );
    for (const account of found?.accounts ?? []) {
        usernameByAccount.set(account.id, account.username);
    }
    return { accountByRequest, usernameByAccount };
}

/**
 * The latest thing said about each request.
 *
 * Read in one call for the whole page rather than one call per request. A
 * member may not read decisions at all, and reads none of them; the outcome
 * reaches them as a notification instead.
 */
async function readDecisionsByRequest(
    caller: AuthenticatedCaller,
    requestIds: readonly string[],
): Promise<Map<string, InboxRequestDecisionView>> {
    const decisionByRequest = new Map<string, InboxRequestDecisionView>();
    const request: ListApprovalDecisionsRequest = {
        offset: 0,
        limit: JOIN_PAGE,
        order: UNORDERED,
        filter: {
            request_id: null,
            id_one_of: null,
            request_id_one_of: requestIds.slice(0, JOIN_PAGE),
        },
        as_of: null,
    };
    const reply = await readJoin(
        caller,
        INBOX_JOIN_SUBJECTS.decisions,
        request,
        decisionsReplySchema,
    );
    for (const decision of reply?.decisions ?? []) {
        const held = decisionByRequest.get(decision.request_id);
        if (held !== undefined && held.decidedAt > decision.decided_at) continue;
        decisionByRequest.set(decision.request_id, {
            decisionCode: decision.decision_code,
            decidedBy: decision.decided_by,
            decidedAt: decision.decided_at,
            comment: decision.comment,
        });
    }
    return decisionByRequest;
}

/**
 * The names the approval requests do not carry.
 *
 * An empty page makes no call at all. A refusal of any read leaves that part
 * of the join empty and the rest intact, so a member still sees their own
 * requests with the ids they can act on.
 */
async function joinRequests(
    caller: AuthenticatedCaller,
    requestIds: readonly string[],
    withDecision: boolean,
    viewer: RequestViewer,
): Promise<RequestJoin> {
    if (requestIds.length === 0) return NO_JOIN;

    const rolesByRequest = await readRolesByRequest(caller, requestIds);
    const { accountByRequest, usernameByAccount } = await readRequesters(caller, requestIds);
    const decisionByRequest = withDecision
        ? await readDecisionsByRequest(caller, requestIds)
        : new Map<string, InboxRequestDecisionView>();

    if (viewer.accountId !== '') usernameByAccount.set(viewer.accountId, viewer.username);
    return { rolesByRequest, accountByRequest, usernameByAccount, decisionByRequest };
}

function toRequestView(
    wire: z.infer<typeof wireApprovalRequestSchema>,
    join: RequestJoin,
): InboxRequestView {
    const accountId = join.accountByRequest.get(wire.id) ?? wire.requested_by;
    return {
        id: wire.id,
        version: wire.version,
        kindCode: wire.kind_code,
        stateCode: wire.state_code,
        requestedBy: join.usernameByAccount.get(accountId) ?? accountId,
        requestedAt: wire.requested_at,
        reason: wire.reason,
        expiresAt: wire.expires_at ?? '',
        roles: join.rolesByRequest.get(wire.id) ?? [],
        decision: join.decisionByRequest.get(wire.id) ?? null,
    };
}

function toNotificationView(wire: z.infer<typeof wireNotificationSchema>): InboxNotificationView {
    return {
        id: wire.id,
        kindCode: wire.kind_code,
        messageKey: wire.message_key,
        raisedBy: wire.raised_by,
        raisedAt: wire.raised_at,
        linkRoute: wire.link_route,
        linkId: wire.link_id,
        arguments: wire.arguments,
        readAt: wire.read_at,
    };
}

/**
 * Asks for roles for the signed-in person, and answers the request raised.
 *
 * The server refuses a role that is unknown, already held, or already asked
 * for in a request that still waits. The refusal is an error, as it is for
 * every other operation here: the person asked for something, and what came
 * back was not what they asked for.
 */
export async function askForRoles(
    caller: AuthenticatedCaller,
    input: { readonly roleIds: readonly string[]; readonly reason: string },
): Promise<{ requestId: string }> {
    const request: AskForRolesRequest = {
        role_ids: [...input.roleIds],
        reason: input.reason,
    };
    const reply = await caller.callAuthenticated(
        INBOX_SUBJECTS.askForRoles,
        request,
        askReplySchema,
    );
    ok(INBOX_SUBJECTS.askForRoles, reply.result);
    return { requestId: reply.request_id };
}

/**
 * The requests the signed-in person raised, newest first, each with the roles
 * it asks for and the latest thing said about it.
 */
export async function readMyRequests(
    caller: AuthenticatedCaller,
    input: { readonly offset: number; readonly limit: number },
    viewer: RequestViewer,
): Promise<InboxPage<InboxRequestView>> {
    const request: ListMyApprovalRequestsRequest = {
        offset: input.offset,
        limit: input.limit,
    };
    const reply = await caller.callAuthenticated(INBOX_SUBJECTS.mine, request, requestsReplySchema);
    ok(INBOX_SUBJECTS.mine, reply.result);
    const join = await joinRequests(
        caller,
        reply.requests.map((raised) => raised.id),
        true,
        viewer,
    );
    return {
        items: reply.requests.map((raised) => toRequestView(raised, join)),
        total: reply.total,
    };
}

/**
 * The one request an identifier names, when the caller may open it.
 *
 * A notice carries the request it is about, so opening a notice needs a read
 * that does not depend on the request still being in a queue. A request the
 * caller may not open answers null, and the caller shows that as the request
 * not being there, because telling a stranger it exists is the thing the
 * server refuses to do.
 */
export async function readRequest(
    caller: AuthenticatedCaller,
    requestId: string,
    viewer: RequestViewer,
): Promise<InboxRequestView | null> {
    const request: GetApprovalRequest = { request_id: requestId };
    const reply = await caller.callAuthenticated(
        INBOX_SUBJECTS.getRequest,
        request,
        requestReplySchema,
    );
    if (reply.result.outcome === 'missing') return null;
    ok(INBOX_SUBJECTS.getRequest, reply.result);
    if (reply.request === null) return null;
    const join = await joinRequests(caller, [reply.request.id], true, viewer);
    return toRequestView(reply.request, join);
}

/**
 * The open requests the signed-in person may decide, oldest first.
 *
 * The server picks the queue: a request is in it when the person holds the
 * permission its kind names to decide it and did not raise it themselves.
 */
export async function readRequestQueue(
    caller: AuthenticatedCaller,
    input: { readonly offset: number; readonly limit: number },
    viewer: RequestViewer,
): Promise<InboxPage<InboxRequestView>> {
    const request: ListApprovalQueueRequest = {
        offset: input.offset,
        limit: input.limit,
    };
    const reply = await caller.callAuthenticated(
        INBOX_SUBJECTS.queue,
        request,
        requestsReplySchema,
    );
    ok(INBOX_SUBJECTS.queue, reply.result);
    const join = await joinRequests(
        caller,
        reply.requests.map((raised) => raised.id),
        false,
        viewer,
    );
    return {
        items: reply.requests.map((raised) => toRequestView(raised, join)),
        total: reply.total,
    };
}

/**
 * Takes back a request the person raised, against the version they saw.
 *
 * Only an open request can be withdrawn, and only by the person who asked.
 */
export async function withdrawRequest(
    caller: AuthenticatedCaller,
    input: { readonly requestId: string; readonly version: number; readonly comment: string },
): Promise<void> {
    const request: WithdrawApprovalRequestRequest = {
        request_id: input.requestId,
        version: input.version,
        comment: input.comment,
    };
    const reply = await caller.callAuthenticated(
        INBOX_SUBJECTS.withdraw,
        request,
        resultReplySchema,
    );
    ok(INBOX_SUBJECTS.withdraw, reply.result);
}

/**
 * Approves, refuses, holds or resumes a request, against the version the
 * decider saw.
 *
 * The version is the claim: a second decision on a request somebody else has
 * just decided is refused rather than silently overwriting them.
 */
export async function decideRequest(
    caller: AuthenticatedCaller,
    input: {
        readonly requestId: string;
        readonly version: number;
        readonly decisionCode: string;
        readonly comment: string;
    },
): Promise<void> {
    const request: DecideApprovalRequestRequest = {
        request_id: input.requestId,
        version: input.version,
        decision_code: input.decisionCode,
        comment: input.comment,
    };
    const reply = await caller.callAuthenticated(INBOX_SUBJECTS.decide, request, resultReplySchema);
    ok(INBOX_SUBJECTS.decide, reply.result);
}

/** The signed-in person's notifications that they have not cleared, newest first. */
export async function readMyNotifications(
    caller: AuthenticatedCaller,
    input: {
        readonly unreadOnly: boolean;
        readonly offset: number;
        readonly limit: number;
    },
): Promise<InboxPage<InboxNotificationView>> {
    const request: ListMyNotificationsRequest = {
        unread_only: input.unreadOnly,
        offset: input.offset,
        limit: input.limit,
    };
    const reply = await caller.callAuthenticated(
        INBOX_SUBJECTS.myNotifications,
        request,
        notificationsReplySchema,
    );
    ok(INBOX_SUBJECTS.myNotifications, reply.result);
    return { items: reply.notifications.map(toNotificationView), total: reply.total };
}

/** How many notifications are unread, for the bell on every screen. */
export async function readUnreadNotificationCount(caller: AuthenticatedCaller): Promise<number> {
    const request: CountUnreadNotificationsRequest = {};
    const reply = await caller.callAuthenticated(
        INBOX_SUBJECTS.unreadCount,
        request,
        unreadReplySchema,
    );
    ok(INBOX_SUBJECTS.unreadCount, reply.result);
    return reply.unread;
}

/**
 * Marks notifications read, and answers how many that was.
 *
 * An empty list means every unread one. The server reads it that way, so a
 * caller that means "these three" must name three.
 */
export async function markNotificationsRead(
    caller: AuthenticatedCaller,
    input: { readonly ids: readonly string[] },
): Promise<number> {
    const request: MarkNotificationsReadRequest = { notification_ids: [...input.ids] };
    const reply = await caller.callAuthenticated(
        INBOX_SUBJECTS.markRead,
        request,
        markedReplySchema,
    );
    ok(INBOX_SUBJECTS.markRead, reply.result);
    return reply.marked;
}

/**
 * Removes notifications from the person's list, and answers how many went.
 *
 * An empty list means every read one. A cleared notification is also read.
 */
export async function clearNotifications(
    caller: AuthenticatedCaller,
    input: { readonly ids: readonly string[] },
): Promise<number> {
    const request: ClearNotificationsRequest = { notification_ids: [...input.ids] };
    const reply = await caller.callAuthenticated(INBOX_SUBJECTS.clear, request, clearedReplySchema);
    ok(INBOX_SUBJECTS.clear, reply.result);
    return reply.cleared;
}
