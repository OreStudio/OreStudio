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

import type { AuthenticatedCaller } from './account-operations.js';
import { ACCOUNT_SUBJECTS, setAccountsLocked } from './account-operations.js';
import { OperationFailedError } from './errors.js';
import type { Account, AccountSignIns, LoginInfo } from './domain.js';
import { subjects as loginInfoSubjects } from './generated/iam/protocol/login_info_protocol.js';
import {
    subjects as sessionSubjects,
    type ListSessionsRequest as GeneratedListSessionsRequest,
} from './generated/iam/protocol/session_protocol.js';
import { subjects as sessionOperationSubjects } from './generated/iam/protocol/session_operations_protocol.js';
import {
    SUBJECTS,
    accountPageSchema,
    accountReplySchema,
    accountUsernameRequestSchema,
    activeSessionsReplySchema,
    listAccountsRequestSchema,
    listLoginInfoRequestSchema,
    listSessionsRequestSchema,
    loginInfoKeyRequestSchema,
    loginInfoPageSchema,
    loginInfoReplySchema,
    sessionPageSchema,
    type WireAccountPage,
    type WireActiveSessions,
    type WireLoginInfoPage,
    type WireSessionPage,
} from './operations.js';

/**
 * The reads the credentials screens make.
 *
 * Plain functions over a narrow caller rather than more methods on the client,
 * for the reason the account operations give: the subjects stay beside the
 * shapes they carry, and a route can name what it reads.
 *
 * The reads here are the ones the member's screen and the administrator's
 * screen share. The one write, {@link setAccountLocked}, is the administrator's
 * lock and unlock, and it collapses the wire's per-account reply into the one
 * account the screen asked about.
 */

/** Subjects for the credentials reads, kept beside the operations that use them. */
export const CREDENTIAL_SUBJECTS = {
    getAccount: 'iam.v1.accounts.get',
    getLoginInfo: loginInfoSubjects.get_login_info_request,
    listSessions: sessionSubjects.list_sessions_request,
    activeSessions: sessionOperationSubjects.get_active_sessions_request,
} as const;

/** The page of accounts the caller may see. */
export async function readAccountsPage(
    caller: AuthenticatedCaller,
    input: { readonly offset?: number; readonly limit?: number } = {},
): Promise<WireAccountPage> {
    return caller.callAuthenticated(
        SUBJECTS.listAccounts,
        listAccountsRequestSchema.parse(input),
        accountPageSchema,
    );
}

/** One account by username, or nothing when the tenant has none with that name. */
export async function readAccount(
    caller: AuthenticatedCaller,
    username: string,
): Promise<Account | null> {
    return caller.callAuthenticated(
        CREDENTIAL_SUBJECTS.getAccount,
        accountUsernameRequestSchema.parse({ key: { username } }),
        accountReplySchema,
    );
}

/** The page of login records the caller may see. */
export async function readLoginInfoPage(
    caller: AuthenticatedCaller,
    input: { readonly offset?: number; readonly limit?: number } = {},
): Promise<WireLoginInfoPage> {
    return caller.callAuthenticated(
        loginInfoSubjects.list_login_info_request,
        listLoginInfoRequestSchema.parse(input),
        loginInfoPageSchema,
    );
}

/**
 * One account's login record, or nothing when the account has never signed in.
 *
 * A record is created by signing in, so an account that has never signed in
 * legitimately has none. The caller decides what that means on the screen.
 */
export async function readLoginInfo(
    caller: AuthenticatedCaller,
    accountId: string,
): Promise<LoginInfo | null> {
    return caller.callAuthenticated(
        CREDENTIAL_SUBJECTS.getLoginInfo,
        loginInfoKeyRequestSchema.parse({ key: { account_id: accountId } }),
        loginInfoReplySchema,
    );
}

/**
 * One account's sign-ins: the account, its sign-in state and one page of its
 * sessions, newest first. Null when no account has the username.
 *
 * The session list is filtered by the account on the server and ordered by
 * start time, so the page holds that account's sessions and no other.
 */
export async function readAccountSignIns(
    caller: AuthenticatedCaller,
    username: string,
    page: { readonly offset: number; readonly limit: number },
): Promise<AccountSignIns | null> {
    const account = await readAccount(caller, username);
    if (account === null) {
        return null;
    }
    const request: GeneratedListSessionsRequest = {
        offset: page.offset,
        limit: page.limit,
        order: { field: 'start_time', descending: true },
        filter: { account_id: account.id, account_id_one_of: null },
    };
    const [loginInfo, sessions] = await Promise.all([
        readLoginInfo(caller, account.id),
        caller.callAuthenticated(CREDENTIAL_SUBJECTS.listSessions, request, sessionPageSchema),
    ]);
    return { account, loginInfo, sessions: sessions.sessions, totalCount: sessions.totalCount };
}

/** The page of sessions the caller may see, open and closed alike. */
export async function readSessionsPage(
    caller: AuthenticatedCaller,
    input: { readonly offset?: number; readonly limit?: number } = {},
): Promise<WireSessionPage> {
    return caller.callAuthenticated(
        CREDENTIAL_SUBJECTS.listSessions,
        listSessionsRequestSchema.parse(input),
        sessionPageSchema,
    );
}

/**
 * The sessions with no end time.
 *
 * The handler behind this subject answers `{success: true}` and no rows today,
 * so an empty list is the server's answer and not a failure. The screen states
 * that the read is a stub rather than pretending the tenant has no sessions.
 */
export async function readActiveSessions(caller: AuthenticatedCaller): Promise<WireActiveSessions> {
    return caller.callAuthenticated(
        CREDENTIAL_SUBJECTS.activeSessions,
        {},
        activeSessionsReplySchema,
    );
}

/**
 * Locks or unlocks one account.
 *
 * The wire answers per account, so a refusal is a result inside a successful
 * reply rather than a failed call. A caller that returned the list would make
 * every screen decide what a one-row list means, so the single account the
 * screen asked about is collapsed here and a refusal is thrown as the failure
 * it is.
 */
export async function setAccountLocked(
    caller: AuthenticatedCaller,
    input: { readonly accountId: string; readonly locked: boolean },
): Promise<void> {
    const subject = input.locked ? ACCOUNT_SUBJECTS.lock : ACCOUNT_SUBJECTS.unlock;
    const results = await setAccountsLocked(caller, {
        accountIds: [input.accountId],
        locked: input.locked,
    });
    const result = results[0];
    if (result === undefined || !result.success) {
        throw new OperationFailedError(
            subject,
            result?.message ?? 'The server refused the change.',
        );
    }
}
