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

/** What the stream needs of the browser's EventSource, so a test can stand in. */
export interface EventSourceLike {
    addEventListener(type: string, listener: (event: { readonly data?: string }) => void): void;
    close(): void;
}

/** A change to an entity: when it happened, and nothing about which records. */
export interface EntityChange {
    readonly component: string;
    readonly entity: string;
    readonly at: string;
}

export type ChangeListener = (change: EntityChange) => void;

/**
 * The one stream of change news a signed-in session holds.
 *
 * The stream opens for the first screen that watches something and closes with
 * the last, so a person on a screen that watches nothing holds no connection. A
 * screen declares what it watches here, and the union of every screen's watches
 * is sent to the server whenever it changes, because the server forgets them
 * when a stream closes. A reconnect sends them again.
 *
 * The news is a time, and a listener decides what to do with it. This class
 * reloads nothing.
 */
export class EntityEventStream {
    readonly #open: () => EventSourceLike;
    readonly #post: (watches: readonly { component: string; entity: string }[]) => Promise<void>;
    readonly #listeners = new Map<string, Set<ChangeListener>>();
    #source: EventSourceLike | undefined;
    #connected = false;
    #pending = false;

    constructor(
        open: () => EventSourceLike,
        post: (watches: readonly { component: string; entity: string }[]) => Promise<void>,
    ) {
        this.#open = open;
        this.#post = post;
    }

    /** Listens for changes to an entity, and returns the way to stop. */
    watch(component: string, entity: string, listener: ChangeListener): () => void {
        const key = keyOf(component, entity);
        const listeners = this.#listeners.get(key) ?? new Set<ChangeListener>();
        listeners.add(listener);
        this.#listeners.set(key, listeners);
        this.#source ??= this.#connect();
        this.#send();
        return () => {
            listeners.delete(listener);
            if (listeners.size === 0) this.#listeners.delete(key);
            if (this.#listeners.size === 0) this.close();
            else this.#send();
        };
    }

    /** Closes the stream and forgets every listener. */
    close(): void {
        this.#source?.close();
        this.#source = undefined;
        this.#connected = false;
    }

    /** Whether a stream is open, which is what a test asserts on. */
    get isOpen(): boolean {
        return this.#source !== undefined;
    }

    #connect(): EventSourceLike {
        const source = this.#open();
        source.addEventListener('connected', () => {
            this.#connected = true;
            this.#send();
        });
        source.addEventListener('entity-changed', (event) => {
            const change = parse(event.data);
            if (change === undefined) return;
            for (const listener of this.#listeners.get(keyOf(change.component, change.entity)) ??
                []) {
                listener(change);
            }
        });
        return source;
    }

    /**
     * Tells the server what is watched, once for a burst of changes.
     *
     * Nothing is sent before the stream says it is connected: the server has no
     * session to attach a watch to until then, and the connected message sends
     * the whole set.
     */
    #send(): void {
        if (!this.#connected || this.#pending) return;
        this.#pending = true;
        queueMicrotask(() => {
            this.#pending = false;
            if (this.#source === undefined) return;
            const watches = [...this.#listeners.keys()].map(split);
            // A watch that was not delivered is sent again with the next change of the
            // set or the next reconnect, so a failure is not reported.
            void this.#post(watches).catch(() => undefined);
        });
    }
}

function keyOf(component: string, entity: string): string {
    return `${component}\u0000${entity}`;
}

function split(key: string): { component: string; entity: string } {
    const [component = '', entity = ''] = key.split('\u0000');
    return { component, entity };
}

function parse(data: string | undefined): EntityChange | undefined {
    if (data === undefined) return undefined;
    try {
        const value = JSON.parse(data) as Partial<EntityChange>;
        return typeof value.component === 'string' &&
            typeof value.entity === 'string' &&
            typeof value.at === 'string'
            ? { component: value.component, entity: value.entity, at: value.at }
            : undefined;
    } catch {
        return undefined;
    }
}
