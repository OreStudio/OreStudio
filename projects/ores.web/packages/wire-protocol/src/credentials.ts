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
import type {
    Account,
    AccountSignIns,
    AuthEvent,
    LoginInfo,
    SessionStatisticsRow,
} from './domain.js';
import { subjects as loginInfoSubjects } from './generated/iam/protocol/login_info_protocol.js';
import {
    subjects as authEventSubjects,
    type ListAuthEventsRequest as GeneratedListAuthEventsRequest,
} from './generated/iam/protocol/auth_event_operations_protocol.js';
import {
    subjects as sessionStatisticsSubjects,
    type GetSessionStatisticsRequest as GeneratedGetSessionStatisticsRequest,
} from './generated/iam/protocol/session_statistics_operations_protocol.js';
import {
    subjects as sessionSubjects,
    type ListSessionsRequest as GeneratedListSessionsRequest,
} from './generated/iam/protocol/session_protocol.js';
import {
    subjects as sessionOperationSubjects,
    type EndSessionRequest as GeneratedEndSessionRequest,
} from './generated/iam/protocol/session_operations_protocol.js';
import {
    subjects as geoOperationSubjects,
    type LookupCountryRequest as GeneratedLookupCountryRequest,
} from './generated/iam/protocol/geo_operations_protocol.js';
import {
    SUBJECTS,
    accountPageSchema,
    accountReplySchema,
    accountUsernameRequestSchema,
    activeSessionsReplySchema,
    authEventListSchema,
    endSessionReplySchema,
    lookupCountryReplySchema,
    listAccountsRequestSchema,
    listLoginInfoRequestSchema,
    listSessionsRequestSchema,
    loginInfoKeyRequestSchema,
    loginInfoPageSchema,
    loginInfoReplySchema,
    sessionPageSchema,
    sessionStatisticsListSchema,
    type WireAccountPage,
    type WireActiveSessions,
    type WireAuthEventList,
    type WireLoginInfoPage,
    type WireSessionPage,
    type WireSessionStatisticsList,
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
    listAuthEvents: authEventSubjects.list_auth_events_request,
    sessionStatistics: sessionStatisticsSubjects.get_session_statistics_request,
    endSession: sessionOperationSubjects.end_session_request,
    lookupCountry: geoOperationSubjects.lookup_country_request,
} as const;

/**
 * The window and page every audit read takes.
 *
 * An empty account or time means the filter is off rather than a value to
 * match, which is the wire's own rule: each empty field is skipped by the
 * read. The period is an instant computed by the BFF from the deployment's
 * clock, so the browser never states a window the deployment did not measure.
 */
export interface AuditWindow {
    readonly accountId?: string;
    readonly eventType?: string;
    readonly fromTime?: string;
    readonly toTime?: string;
    readonly offset?: number;
    readonly limit?: number;
}

/** The page of accounts the caller may see. */
export async function readAccountsPage(
    caller: AuthenticatedCaller,
    input: {
        readonly offset?: number;
        readonly limit?: number;
        readonly search?: string;
        readonly sort?: string;
        readonly descending?: boolean;
    } = {},
): Promise<WireAccountPage> {
    const search = input.search ?? '';
    return caller.callAuthenticated(
        SUBJECTS.listAccounts,
        listAccountsRequestSchema.parse({
            offset: input.offset,
            limit: input.limit,
            order: { field: input.sort ?? '', descending: input.descending ?? false },
            filter: search === '' ? null : { id_one_of: null, search },
        }),
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
 * The handler behind this subject reads the sessions whose end time is empty,
 * so an empty list is the tenant's answer and not a failure: no session is
 * open.
 */
export async function readActiveSessions(caller: AuthenticatedCaller): Promise<WireActiveSessions> {
    return caller.callAuthenticated(
        CREDENTIAL_SUBJECTS.activeSessions,
        {},
        activeSessionsReplySchema,
    );
}

/**
 * The authentication events the caller may see, newest first.
 *
 * The empty fields are the filter turned off, which is the read's own rule.
 * The window is start-inclusive and end-exclusive, so adjacent windows tile
 * without an event landing in two of them.
 */
export async function readAuthEvents(
    caller: AuthenticatedCaller,
    window: AuditWindow = {},
): Promise<readonly AuthEvent[]> {
    const request: GeneratedListAuthEventsRequest = {
        account_id: window.accountId ?? '',
        event_type: window.eventType ?? '',
        from_time: window.fromTime ?? '',
        to_time: window.toTime ?? '',
        offset: window.offset ?? 0,
        limit: window.limit ?? 100,
    };
    const answer: WireAuthEventList = await caller.callAuthenticated(
        CREDENTIAL_SUBJECTS.listAuthEvents,
        request,
        authEventListSchema,
    );
    return answer.events;
}

/** The session statistics the caller may see, newest day first. */
export async function readSessionStatistics(
    caller: AuthenticatedCaller,
    window: AuditWindow = {},
): Promise<readonly SessionStatisticsRow[]> {
    const request: GeneratedGetSessionStatisticsRequest = {
        account_id: window.accountId ?? '',
        from_time: window.fromTime ?? '',
        to_time: window.toTime ?? '',
        offset: window.offset ?? 0,
        limit: window.limit ?? 100,
    };
    const answer: WireSessionStatisticsList = await caller.callAuthenticated(
        CREDENTIAL_SUBJECTS.sessionStatistics,
        request,
        sessionStatisticsListSchema,
    );
    return answer.rows;
}

/**
 * Ends one session of the caller's tenant.
 *
 * The tenant scope is the caller's own, so a session of another tenant is not
 * there to end. The handler states a refusal in the body rather than as a
 * failed call, so the answer is read and a refusal is thrown as the failure it
 * is.
 */
export async function endSession(caller: AuthenticatedCaller, sessionId: string): Promise<void> {
    const request: GeneratedEndSessionRequest = { session_id: sessionId };
    const answer = await caller.callAuthenticated(
        CREDENTIAL_SUBJECTS.endSession,
        request,
        endSessionReplySchema,
    );
    if (!answer.success) {
        throw new OperationFailedError(
            CREDENTIAL_SUBJECTS.endSession,
            answer.message.length > 0 ? answer.message : 'The server refused to end the session.',
        );
    }
}

/**
 * Resolves one address to the country it came from.
 *
 * The search is the caller's tenant's published ranges, so an address they do
 * not cover is not found. Not found is an answer rather than a failure: a
 * private address never resolves, and a caller that treated it as an error
 * would have to invent a country.
 */
export async function lookupCountry(
    caller: AuthenticatedCaller,
    address: string,
): Promise<string | undefined> {
    const request: GeneratedLookupCountryRequest = { address };
    const answer = await caller.callAuthenticated(
        CREDENTIAL_SUBJECTS.lookupCountry,
        request,
        lookupCountryReplySchema,
    );
    if (!answer.success) {
        throw new OperationFailedError(
            CREDENTIAL_SUBJECTS.lookupCountry,
            answer.message.length > 0 ? answer.message : 'The country lookup failed.',
        );
    }
    return answer.found ? answer.country_code : undefined;
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
