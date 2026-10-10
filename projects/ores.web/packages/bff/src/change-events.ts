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

import { SYSTEM_TENANT_ID, type OresClient } from '@ores/wire-protocol';

/**
 * Who is watching what, and the one subscription that serves them.
 *
 * A subscription per screen would be wrong twice over: a deployment with fifty
 * people on the accounts list would open fifty NATS subscriptions to the same
 * subject, and every one of them would carry the same message. So a subscription
 * is shared, keyed by the entity, and this keeps the map from that key to the
 * sessions watching it.
 *
 * The subject is the same for every tenant, so the broker delivers every
 * tenant's events here. Each event names its tenant, and its party when the row
 * has one, in the envelope, and a session is told only of the events it may
 * hear: those of its own tenant and of the system tenant, and, for a member, of
 * a party it works in. An event that names no tenant is passed to nobody,
 * because it cannot be shown to be anyone's.
 *
 * Reference counted. The subscription is opened for the first watcher and closed
 * with the last, so nothing is held open for an entity nobody is looking at.
 */

/**
 * What one session is watching.
 *
 * The entity is the events collection as it stands in the subject, such as
 * `accounts` or `tenant_types`, so no plural is guessed here.
 */
export interface Watch {
    readonly component: string;
    readonly entity: string;
}

/** How a change is delivered to a session. */
export type ChangeListener = (change: ChangeEvent) => void;

export interface ChangeEvent {
    readonly component: string;
    readonly entity: string;
    /** When the change happened, server-side. */
    readonly at: string;
}

/** Whose events a session may hear. */
export interface Audience {
    readonly tenantId: string;
    /**
     * Whether the session sees every party of its tenant. An administrator does;
     * a member sees the parties they work in.
     */
    readonly everyParty: boolean;
    /** The parties the session works in, read when an event arrives so a switch counts. */
    readonly parties: () => ReadonlySet<string>;
}

/** What an event's envelope says about whose it is. */
export interface Envelope {
    readonly tenantId: string | undefined;
    readonly partyId: string | undefined;
}

/**
 * Whether an event may be told to a session.
 *
 * The tenant must be the session's own or the system tenant's, whose rows are
 * shared. A party-owned event also needs a party the session can see; an event
 * with no party concerns the whole tenant.
 */
export function mayHear(audience: Audience, envelope: Envelope): boolean {
    if (envelope.tenantId === undefined) return false;
    if (envelope.tenantId !== audience.tenantId && envelope.tenantId !== SYSTEM_TENANT_ID) {
        return false;
    }
    if (envelope.partyId === undefined || audience.everyParty) return true;
    return audience.parties().has(envelope.partyId);
}

/** A name that can stand in a subject: it cannot be a wildcard or add a segment. */
const SUBJECT_NAME = /^[a-z][a-z0-9_]*$/;

/** Whether a watch names a component and an events collection, and nothing else. */
export function isWatchable(watch: Watch): boolean {
    return SUBJECT_NAME.test(watch.component) && SUBJECT_NAME.test(watch.entity);
}

/**
 * The subject the services publish an entity's changes on.
 *
 * The canonical form is `component.v1.collection_events.action`, so a watch
 * listens on the collection's wildcard and hears created, updated and deleted
 * alike.
 */
export function eventSubject(component: string, entity: string): string {
    return `${component}.v1.${entity}_events.>`;
}

export class ChangeEventRegistry {
    readonly #client: OresClient;
    /** Entity to the sessions listening and the way to stop the subscription. */
    readonly #shared = new Map<
        string,
        {
            listeners: Map<string, { listener: ChangeListener; audience: Audience }>;
            stop: () => void;
        }
    >();
    /** Session id to what it watches, so a session can be forgotten wholesale. */
    readonly #watching = new Map<string, { audience: Audience; watches: readonly Watch[] }>();
    /** How to reach a session, registered once when its stream opens. */
    readonly #listeners = new Map<string, ChangeListener>();

    constructor(client: OresClient) {
        this.#client = client;
    }

    /**
     * Remembers how to reach a session.
     *
     * Registered when the session's stream opens and not before, because a
     * subscription opened for a session with nowhere to deliver to would be work
     * done for nothing.
     */
    attach(sessionId: string, listener: ChangeListener): void {
        this.#listeners.set(sessionId, listener);
    }

    /**
     * Declares what a session is watching, replacing what it watched before.
     *
     * Called whenever a screen changes, so it is written to be safe to call with
     * the same set repeatedly: the subscriptions that are still wanted are left
     * alone rather than torn down and rebuilt, because rebuilding is how a screen
     * misses the change that arrives during the gap.
     */
    watch(sessionId: string, audience: Audience, watches: readonly Watch[]): void {
        if (!this.#listeners.has(sessionId)) return;
        // A name with a wildcard in it would listen to more than was asked for, and
        // the names arrive from the browser, so only plain names are watched.
        watches = watches.filter(isWatchable);
        const previous = this.#watching.get(sessionId);
        if (previous !== undefined) {
            for (const watch of previous.watches) {
                if (!watches.some((w) => sameWatch(w, watch))) {
                    this.#release(watch, sessionId);
                }
            }
        }
        for (const watch of watches) {
            this.#acquire(watch, sessionId, audience);
        }
        this.#watching.set(sessionId, { audience, watches });
    }

    /** Forgets a session, dropping whatever only it was watching. */
    forget(sessionId: string): void {
        const entry = this.#watching.get(sessionId);
        if (entry !== undefined) {
            for (const watch of entry.watches) {
                this.#release(watch, sessionId);
            }
            this.#watching.delete(sessionId);
        }
        this.#listeners.delete(sessionId);
    }

    #acquire(watch: Watch, sessionId: string, audience: Audience): void {
        const key = keyFor(watch);
        let entry = this.#shared.get(key);

        if (entry === undefined) {
            // The subscription is opened once for everyone watching, and the listener
            // map starts empty: it is the sessions that listen, and they arrive next.
            const listeners = new Map<string, { listener: ChangeListener; audience: Audience }>();
            const stop = this.#client.subscribeToEvents(
                eventSubject(watch.component, watch.entity),
                (change) => {
                    const event: ChangeEvent = {
                        component: watch.component,
                        entity: watch.entity,
                        at: change.at,
                    };
                    for (const heard of listeners.values()) {
                        if (mayHear(heard.audience, change)) heard.listener(event);
                    }
                },
            );
            entry = { listeners, stop };
            this.#shared.set(key, entry);
        }

        const listener = this.#listeners.get(sessionId);
        if (listener !== undefined) entry.listeners.set(sessionId, { listener, audience });
    }

    #release(watch: Watch, sessionId: string): void {
        const key = keyFor(watch);
        const entry = this.#shared.get(key);
        if (entry === undefined) return;

        entry.listeners.delete(sessionId);
        // The last watcher leaving closes the subscription, so nothing is held open
        // for an entity nobody is looking at.
        if (entry.listeners.size === 0) {
            entry.stop();
            this.#shared.delete(key);
        }
    }

    /** How many shared subscriptions are open, which is what a test asserts on. */
    get size(): number {
        return this.#shared.size;
    }
}

function keyFor(watch: Watch): string {
    return `${watch.component}\u0000${watch.entity}`;
}

function sameWatch(a: Watch, b: Watch): boolean {
    return a.component === b.component && a.entity === b.entity;
}
