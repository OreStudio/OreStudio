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
import {
  accountOperationResultSchema,
  accountPageSchema,
  changePasswordResultSchema,
  lockResultSchema,
} from './operations.js';
import { OperationFailedError } from './errors.js';
import type { WireAccountPage } from './operations.js';

/**
 * Account operations beyond the plain list.
 *
 * Each is a thin, typed wrapper over one subject. They are gathered here
 * rather than inlined into the client so that the reason a call can fail is
 * modelled once: the server answers most of these with `success: false` and a
 * message rather than an error header, and a caller that ignores that would
 * report a rejected write as a success.
 */

/** The subset of the client these operations need. */
export interface AuthenticatedCaller {
  callAuthenticated<Schema extends z.ZodType>(
    subject: string,
    body: unknown,
    schema: Schema,
  ): Promise<z.infer<Schema>>;
}

/** Subjects for account mutations, kept beside the operations that use them. */
export const ACCOUNT_SUBJECTS = {
  lock: 'iam.v1.accounts.lock',
  unlock: 'iam.v1.accounts.unlock',
  delete: 'iam.v1.accounts.delete',
  changePassword: 'iam.v1.accounts.change-password',
} as const;

/**
 * Locks or unlocks a set of accounts.
 *
 * The reply is per-account, so a partial failure is a success at the
 * transport level. The results are returned rather than collapsed, because a
 * caller needs to know which ids were refused.
 */
export async function setAccountsLocked(
  caller: AuthenticatedCaller,
  input: { readonly accountIds: readonly string[]; readonly locked: boolean },
): Promise<readonly z.infer<typeof accountOperationResultSchema>[]> {
  const subject = input.locked ? ACCOUNT_SUBJECTS.lock : ACCOUNT_SUBJECTS.unlock;
  const reply = await caller.callAuthenticated(
    subject,
    { account_ids: [...input.accountIds] },
    lockResultSchema,
  );
  return reply.results;
}

/** Deletes one account. */
export async function deleteAccount(
  caller: AuthenticatedCaller,
  accountId: string,
): Promise<void> {
  const reply = await caller.callAuthenticated(
    ACCOUNT_SUBJECTS.delete,
    { account_id: accountId },
    z.object({ success: z.boolean().default(false), message: z.string().default('') }),
  );
  if (!reply.success) {
    throw new OperationFailedError(ACCOUNT_SUBJECTS.delete, reply.message);
  }
}

/** Changes the signed-in account's own password. */
export async function changeOwnPassword(
  caller: AuthenticatedCaller,
  input: { readonly currentPassword: string; readonly newPassword: string },
): Promise<void> {
  const reply = await caller.callAuthenticated(
    ACCOUNT_SUBJECTS.changePassword,
    { current_password: input.currentPassword, new_password: input.newPassword },
    changePasswordResultSchema,
  );
  if (!reply.success) {
    throw new OperationFailedError(ACCOUNT_SUBJECTS.changePassword, reply.message);
  }
}

/** Re-exported for callers that only need the page type. */
export type { WireAccountPage };
