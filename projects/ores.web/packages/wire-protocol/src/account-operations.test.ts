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

import { describe, expect, it } from 'vitest';
import { changeOwnPassword, deleteAccount, setAccountsLocked } from './account-operations.js';
import { OperationFailedError } from './errors.js';
import type { AuthenticatedCaller } from './account-operations.js';

/** A caller that answers with a scripted reply and records the subject. */
function scriptedCaller(reply: unknown): AuthenticatedCaller & { subjects: string[]; bodies: unknown[] } {
  const subjects: string[] = [];
  const bodies: unknown[] = [];
  return {
    subjects,
    bodies,
    async callAuthenticated(subject: string, body: unknown): Promise<unknown> {
      subjects.push(subject);
      bodies.push(body);
      return reply;
    },
  } as AuthenticatedCaller & { subjects: string[]; bodies: unknown[] };
}

describe('setAccountsLocked', () => {
  it('uses the lock subject when locking', async () => {
    const caller = scriptedCaller({ results: [{ success: true, message: '' }] });
    await setAccountsLocked(caller, { accountIds: ['a'], locked: true });
    expect(caller.subjects).toEqual(['iam.v1.accounts.lock']);
    expect(caller.bodies[0]).toEqual({ account_ids: ['a'] });
  });

  it('uses the unlock subject when unlocking', async () => {
    const caller = scriptedCaller({ results: [] });
    await setAccountsLocked(caller, { accountIds: [], locked: false });
    expect(caller.subjects).toEqual(['iam.v1.accounts.unlock']);
  });

  it('returns the per-account results rather than collapsing them', async () => {
    const caller = scriptedCaller({
      results: [
        { success: true, message: '' },
        { success: false, message: 'cannot lock the system account' },
      ],
    });
    const results = await setAccountsLocked(caller, { accountIds: ['a', 'b'], locked: true });
    expect(results).toHaveLength(2);
    expect(results[1]?.message).toBe('cannot lock the system account');
  });
});

describe('deleteAccount', () => {
  it('resolves on success', async () => {
    const caller = scriptedCaller({ success: true, message: '' });
    await expect(deleteAccount(caller, 'a')).resolves.toBeUndefined();
  });

  it('raises the server message when the body reports failure', async () => {
    const caller = scriptedCaller({ success: false, message: 'account is in use' });
    await expect(deleteAccount(caller, 'a')).rejects.toThrow(OperationFailedError);
    await expect(deleteAccount(caller, 'a')).rejects.toThrow('account is in use');
  });
});

describe('changeOwnPassword', () => {
  it('sends both passwords and resolves on success', async () => {
    const caller = scriptedCaller({ success: true, message: '' });
    await changeOwnPassword(caller, { currentPassword: 'old', newPassword: 'new' });
    expect(caller.bodies[0]).toEqual({ current_password: 'old', new_password: 'new' });
  });

  it('raises when the server rejects the change', async () => {
    const caller = scriptedCaller({ success: false, message: 'password too weak' });
    await expect(
      changeOwnPassword(caller, { currentPassword: 'old', newPassword: 'x' }),
    ).rejects.toThrow('password too weak');
  });
});
