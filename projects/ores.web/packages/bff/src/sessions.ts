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
import type {
    ActiveSession,
    DatabaseInfo,
    OresClient,
    PartySummary,
    SessionMode,
} from '@ores/wire-protocol';

/**
 * The database row a session states when the login answer carried none.
 *
 * The login answer always carries the row now, so this stands only for a
 * session a test opened without one and for a read that failed. An empty row
 * is not a claim: the versions screen states the database as unknown.
 */
const UNKNOWN_DATABASE: DatabaseInfo = {
    fingerprint: '',
    environment: '',
    commit: '',
    created: '',
};

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
    /** Whether the tenant was still being set up when the login was accepted. */
    tenantBootstrapping: boolean;
    /** The context the session runs in, decided when the login was accepted. */
    mode: SessionMode;
    /** The build the login was answered by. */
    version: string;
    /** The database the login answer carried, beside the build it states. */
    database: DatabaseInfo;
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
    /** Whether the caller's tenant is still being set up. */
    readonly tenantBootstrapping: boolean;
    /** The context the session runs in, decided when the login was accepted. */
    readonly mode: SessionMode;
    /** The build the login was answered by. */
    readonly version: string;
    /** The database the login answer carried, beside the build it states. */
    readonly database: DatabaseInfo;
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
        readonly tenantBootstrapping: boolean;
        readonly mode: SessionMode;
        readonly version: string;
        readonly database?: DatabaseInfo;
        readonly availableParties: readonly PartySummary[];
        readonly accessLifetimeSeconds: number;
        readonly passwordResetRequired: boolean;
        readonly sessionId: string;
    }): LiveSession;
    get(id: string): LiveSession | undefined;
    /** Records the party chosen after a pending login. */
    activate(id: string, session: ActiveSession): LiveSession | undefined;
    /**
     * Records a re-scoped session.
     *
     * A party added after the session opened is not in the list the login
     * answered with, so the party and the list are both written here: the
     * bearer token has been re-issued for the new party, and the browser's
     * session view states which parties the account may work in.
     */
    switchParty(
        id: string,
        party: PartySummary,
        availableParties: readonly PartySummary[],
        accessLifetimeSeconds: number,
    ): LiveSession | undefined;
    /** Records a re-issued token and its new lifetime after a refresh. */
    refresh(id: string, accessLifetimeSeconds: number): void;
    /**
     * Records that the session's tenant finished being set up.
     *
     * The bootstrap flag is a snapshot the login took, so a session opened
     * while its tenant was bootstrapping still reports it that way until this
     * runs. The tenant's setup run is what flips the tenant's status, so its
     * finish is the fact that ends the rail.
     */
    tenantBootstrapped(id: string): LiveSession | undefined;
    /**
     * Records that the signed-in account set a password of its own.
     *
     * The flag is a snapshot the login took, so a session that has just
     * changed its password still reports the change as outstanding until this
     * runs, and the next read would ask for it a second time.
     */
    passwordChanged(id: string): LiveSession | undefined;
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
            tenantBootstrapping: record.tenantBootstrapping,
            mode: record.mode,
            version: record.version,
            database: record.database,
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
            tenantBootstrapping: input.tenantBootstrapping,
            mode: input.mode,
            version: input.version,
            database: input.database ?? UNKNOWN_DATABASE,
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
            record.version = session.version;
            record.database = session.database;
            record.tenantBootstrapping = session.tenantBootstrapping;
            record.accessLifetimeSeconds = session.accessLifetimeSeconds;
            record.passwordResetRequired = session.passwordResetRequired;
            record.expiresAt = now() + ttlMs;
            return toLive(id, record);
        },

        switchParty(id, party, availableParties, accessLifetimeSeconds) {
            const record = sessions.get(hash(id));
            if (record === undefined) {
                return undefined;
            }
            record.party = party;
            record.availableParties = availableParties;
            record.accessLifetimeSeconds = accessLifetimeSeconds;
            record.expiresAt = now() + ttlMs;
            return toLive(id, record);
        },

        refresh(id, accessLifetimeSeconds) {
            const record = sessions.get(hash(id));
            if (record !== undefined) {
                record.accessLifetimeSeconds = accessLifetimeSeconds;
            }
        },

        tenantBootstrapped(id) {
            const record = sessions.get(hash(id));
            if (record === undefined) {
                return undefined;
            }
            record.tenantBootstrapping = false;
            return toLive(id, record);
        },

        passwordChanged(id) {
            const record = sessions.get(hash(id));
            if (record === undefined) {
                return undefined;
            }
            record.passwordResetRequired = false;
            return toLive(id, record);
        },

        async destroy(id) {
            const record = sessions.get(hash(id));
            if (record !== undefined) {
                /*
                 * A person signing out tells the IAM service, which marks the
                 * account offline, writes the session's end time and records
                 * the sign-out. Closing the connection alone leaves the session
                 * looking open for ever. The call is best effort: a service
                 * that is down must not keep someone signed in here.
                 */
                try {
                    await record.client.logout();
                } catch {
                    // The session is removed below whatever the service said.
                }
                await closeAndRemove(id, record);
            }
        },

        async destroyAll() {
            const entries = [...sessions.entries()];
            sessions.clear();
            await Promise.all(
                entries.map(([, record]) => record.client.close().catch(() => undefined)),
            );
        },

        get size() {
            return sessions.size;
        },
    };
}
