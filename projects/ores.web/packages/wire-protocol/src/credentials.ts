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
import type { Account, LoginInfo } from './domain.js';
import { subjects as loginInfoSubjects } from './generated/iam/protocol/login_info_protocol.js';
import {
    SUBJECTS,
    accountPageSchema,
    accountReplySchema,
    accountUsernameRequestSchema,
    listAccountsRequestSchema,
    listLoginInfoRequestSchema,
    loginInfoKeyRequestSchema,
    loginInfoPageSchema,
    loginInfoReplySchema,
    type WireAccountPage,
    type WireLoginInfoPage,
} from './operations.js';

/**
 * The reads the credentials screens make.
 *
 * Plain functions over a narrow caller rather than more methods on the client,
 * for the reason the account operations give: the subjects stay beside the
 * shapes they carry, and a route can name what it reads.
 *
 * Nothing here writes. The account and login-record reads are the two the
 * member's screen and the administrator's screen share, and both answer over
 * subjects that already work.
 */

/** Subjects for the credentials reads, kept beside the operations that use them. */
export const CREDENTIAL_SUBJECTS = {
    getAccount: 'iam.v1.accounts.get',
    getLoginInfo: loginInfoSubjects.get_login_info_request,
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
