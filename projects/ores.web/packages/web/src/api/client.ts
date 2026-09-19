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

import {
  accountListSchema,
  loginResultSchema,
  sessionViewSchema,
  type AccountList,
  type LoginResult,
  type SessionView,
} from '@ores/wire-protocol/browser';
import { ApiFailure, request } from './transport.js';

/**
 * The session and account calls.
 *
 * These run after a connection has been chosen and a credential accepted. The
 * browser holds an opaque cookie rather than a token, so it cannot decide
 * locally whether it is signed in and asks instead.
 */

const JSON_HEADERS = { 'Content-Type': 'application/json' } as const;

export interface Credentials {
  readonly username: string;
  readonly password: string;
}

export const api = {
  async login(credentials: Credentials): Promise<LoginResult> {
    const payload = await request('/api/session', {
      method: 'POST',
      headers: JSON_HEADERS,
      body: JSON.stringify(credentials),
    });
    return loginResultSchema.parse(payload);
  },

  async selectParty(partyId: string): Promise<SessionView> {
    const payload = await request('/api/session/party', {
      method: 'POST',
      headers: JSON_HEADERS,
      body: JSON.stringify({ partyId }),
    });
    return sessionViewSchema.parse(payload);
  },

  /** Returns null when no session is open, which is not an error. */
  async session(): Promise<SessionView | null> {
    try {
      return sessionViewSchema.parse(await request('/api/session', { method: 'GET' }));
    } catch (error) {
      if (error instanceof ApiFailure && error.status === 401) {
        return null;
      }
      throw error;
    }
  },

  async logout(): Promise<void> {
    await request('/api/session', { method: 'DELETE' });
  },

  async accounts(
    input: { readonly offset?: number; readonly limit?: number } = {},
  ): Promise<AccountList> {
    const query = new URLSearchParams();
    if (input.offset !== undefined) {
      query.set('offset', String(input.offset));
    }
    if (input.limit !== undefined) {
      query.set('limit', String(input.limit));
    }
    const suffix = query.size === 0 ? '' : `?${query.toString()}`;
    return accountListSchema.parse(await request(`/api/accounts${suffix}`, { method: 'GET' }));
  },

  async setAccountLocked(accountId: string, locked: boolean): Promise<void> {
    await request(`/api/accounts/${accountId}/${locked ? 'lock' : 'unlock'}`, { method: 'POST' });
  },

  async deleteAccount(accountId: string): Promise<void> {
    await request(`/api/accounts/${accountId}`, { method: 'DELETE' });
  },
};

export { ApiFailure };
