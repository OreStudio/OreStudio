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

import { createHash, randomBytes } from 'node:crypto';
import type { ActiveSession, OresClient, PartySummary } from '@ores/wire-protocol';

/**
 * Server-side browser sessions.
 *
 * The browser holds an opaque identifier in an HttpOnly cookie. Everything
 * that matters, which is the bearer token and the party the session is scoped
 * to, stays here. The identifier is stored hashed, so the map cannot be
 * turned into a set of usable credentials by reading memory.
 *
 * Each session owns its own NATS connection. That is one TCP connection per
 * signed-in browser, which the broker handles comfortably, and it keeps one
 * user's token out of every other user's request path by construction.
 */

interface SessionRecord {
  readonly client: OresClient;
  username: string;
  email: string;
  accountId: string;
  tenantId: string;
  tenantName: string;
  /** Absent until a party has been chosen. */
  party: PartySummary | undefined;
  availableParties: readonly PartySummary[];
  accessLifetimeSeconds: number;
  passwordResetRequired: boolean;
  /** Carried so a pending party selection can complete on a later request. */
  sessionId: string;
  expiresAt: number;
}

/** A session as the rest of the BFF reads it. */
export interface LiveSession {
  readonly id: string;
  readonly client: OresClient;
  readonly username: string;
  readonly email: string;
  readonly accountId: string;
  readonly tenantId: string;
  readonly tenantName: string;
  /** Absent only while a login is waiting on party selection. */
  readonly party: PartySummary | undefined;
  readonly availableParties: readonly PartySummary[];
  readonly accessLifetimeSeconds: number;
  readonly passwordResetRequired: boolean;
  /** The IAM session id, forwarded as `Nats-Session-Id`. */
  readonly sessionId: string;
}

export interface SessionStore {
  /**
   * Registers a freshly authenticated client.
   *
   * Pass `session` as null when the login still needs a party. The client
   * already holds the token and the IAM session id, so only the display
   * fields and the `sessionId` matter until `activate` runs.
   */
  create(input: {
    readonly client: OresClient;
    readonly session: ActiveSession | null;
    readonly username: string;
    readonly email: string;
    readonly accountId: string;
    readonly tenantId: string;
    readonly tenantName: string;
    readonly availableParties: readonly PartySummary[];
    readonly accessLifetimeSeconds: number;
    readonly passwordResetRequired: boolean;
    readonly sessionId: string;
  }): LiveSession;
  get(id: string): LiveSession | undefined;
  /** Records the party chosen after a pending login. */
  activate(id: string, session: ActiveSession): LiveSession | undefined;
  /** Records a re-issued token and its new lifetime after a refresh. */
  refresh(id: string, accessLifetimeSeconds: number): void;
  /** Closes the connection and forgets the session. */
  destroy(id: string): Promise<void>;
  destroyAll(): Promise<void>;
  readonly size: number;
}

export interface SessionStoreOptions {
  readonly ttlSeconds: number;
  /** Injected so tests can control expiry. */
  readonly now?: () => number;
}

export function createSessionStore(options: SessionStoreOptions): SessionStore {
  const sessions = new Map<string, SessionRecord>();
  const now = options.now ?? (() => Date.now());
  const ttlMs = options.ttlSeconds * 1000;

  function hash(id: string): string {
    return createHash('sha256').update(id).digest('hex');
  }

  function toLive(id: string, record: SessionRecord): LiveSession {
    return {
      id,
      client: record.client,
      username: record.username,
      email: record.email,
      accountId: record.accountId,
      tenantId: record.tenantId,
      tenantName: record.tenantName,
      party: record.party,
      availableParties: record.availableParties,
      accessLifetimeSeconds: record.accessLifetimeSeconds,
      passwordResetRequired: record.passwordResetRequired,
      sessionId: record.sessionId,
    };
  }

  function create(input: Parameters<SessionStore['create']>[0]): LiveSession {
    const id = randomBytes(32).toString('base64url');
    const session = input.session;

    const record: SessionRecord = {
      client: input.client,
      username: input.username,
      email: input.email,
      accountId: input.accountId,
      tenantId: input.tenantId,
      tenantName: input.tenantName,
      party: session?.party,
      availableParties: input.availableParties,
      accessLifetimeSeconds: input.accessLifetimeSeconds,
      passwordResetRequired: input.passwordResetRequired,
      sessionId: input.sessionId,
      expiresAt: now() + ttlMs,
    };
    sessions.set(hash(id), record);
    return toLive(id, record);
  }

  function get(id: string): LiveSession | undefined {
    const record = sessions.get(hash(id));
    if (record === undefined) {
      return undefined;
    }
    if (record.expiresAt <= now()) {
      void closeAndRemove(id, record);
      return undefined;
    }
    // Sliding expiry: an active browser keeps its session.
    record.expiresAt = now() + ttlMs;
    return toLive(id, record);
  }

  async function closeAndRemove(id: string, record: SessionRecord): Promise<void> {
    sessions.delete(hash(id));
    await record.client.close().catch(() => undefined);
  }

  return {
    create,

    get,

    activate(id, session) {
      const record = sessions.get(hash(id));
      if (record === undefined) {
        return undefined;
      }
      record.party = session.party;
      record.accessLifetimeSeconds = session.accessLifetimeSeconds;
      record.passwordResetRequired = session.passwordResetRequired;
      record.expiresAt = now() + ttlMs;
      return toLive(id, record);
    },

    refresh(id, accessLifetimeSeconds) {
      const record = sessions.get(hash(id));
      if (record !== undefined) {
        record.accessLifetimeSeconds = accessLifetimeSeconds;
      }
    },

    async destroy(id) {
      const record = sessions.get(hash(id));
      if (record !== undefined) {
        await closeAndRemove(id, record);
      }
    },

    async destroyAll() {
      const entries = [...sessions.entries()];
      sessions.clear();
      await Promise.all(entries.map(([, record]) => record.client.close().catch(() => undefined)));
    },

    get size() {
      return sessions.size;
    },
  };
}
