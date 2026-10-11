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

/**
 * The trail of what a person did, kept for the logs.
 *
 * A failed screen is traced from the log, and the log only knows what the
 * services saw. This keeps the rest: which screen was entered, which form was
 * opened and how it ended, which operation failed. The entries are sent to the
 * BFF, which writes them in the services' own line form beside the requests
 * they led to, so the journal reads as one story per session.
 *
 * Names follow the services: a dotted logger name under `ores.web.client`, a
 * severity, a short message, and the facts as `key=value` with lower-case
 * snake names. A fact is never a secret, a password or a value a person typed.
 */

export type Level = 'trace' | 'debug' | 'info' | 'warn' | 'error';
export type Fact = string | number | boolean;
export type Facts = Readonly<Record<string, Fact | undefined>>;

export interface Entry {
    readonly at: string;
    readonly level: Level;
    readonly logger: string;
    readonly message: string;
    readonly fields: Readonly<Record<string, Fact>>;
}

/** Entries held when the BFF cannot be reached; the oldest go first. */
const HELD = 200;
const FLUSH_MS = 500;

export class ClientLog {
    readonly #send: (entries: readonly Entry[]) => Promise<void>;
    readonly #now: () => Date;
    #queue: Entry[] = [];
    #timer: ReturnType<typeof setTimeout> | undefined;

    constructor(
        send: (entries: readonly Entry[]) => Promise<void>,
        now: () => Date = () => new Date(),
    ) {
        this.#send = send;
        this.#now = now;
    }

    write(level: Level, area: string, message: string, facts: Facts = {}): void {
        const fields: Record<string, Fact> = {};
        for (const [key, found] of Object.entries(facts)) {
            if (found !== undefined)
                fields[key] = typeof found === 'string' ? found.slice(0, 300) : found;
        }
        this.#queue.push({
            at: this.#now().toISOString(),
            level,
            logger: `ores.web.client.${area}`,
            message: message.slice(0, 200),
            fields,
        });
        if (this.#queue.length > HELD) this.#queue = this.#queue.slice(-HELD);
        if (level === 'warn' || level === 'error') {
            void this.flush();
        } else if (this.#timer === undefined) {
            this.#timer = setTimeout(() => void this.flush(), FLUSH_MS);
        }
    }

    /** Sends what is held. What cannot be sent is kept for the next try. */
    async flush(): Promise<void> {
        if (this.#timer !== undefined) {
            clearTimeout(this.#timer);
            this.#timer = undefined;
        }
        const batch = this.#queue.slice(0, 100);
        if (batch.length === 0) return;
        this.#queue = this.#queue.slice(batch.length);
        try {
            await this.#send(batch);
        } catch {
            this.#queue = [...batch, ...this.#queue].slice(-HELD);
        }
    }

    /** How many entries wait to be sent, which a test reads. */
    get waiting(): number {
        return this.#queue.length;
    }
}

async function post(entries: readonly Entry[]): Promise<void> {
    const response = await fetch('/api/client-log', {
        method: 'POST',
        credentials: 'same-origin',
        keepalive: true,
        headers: { 'content-type': 'application/json' },
        body: JSON.stringify({ entries }),
    });
    // Nobody is signed in, so there is nothing to trace and nothing to keep.
    if (!response.ok && response.status !== 401) throw new Error(`client log ${response.status}`);
}

export const clientLog = new ClientLog(post);

/** A logger for one area of the interface: `route`, `form`, `request`, `save`. */
export function trail(area: string): {
    readonly debug: (message: string, facts?: Facts) => void;
    readonly info: (message: string, facts?: Facts) => void;
    readonly warn: (message: string, facts?: Facts) => void;
    readonly error: (message: string, facts?: Facts) => void;
} {
    return {
        debug: (message, facts) => clientLog.write('debug', area, message, facts),
        info: (message, facts) => clientLog.write('info', area, message, facts),
        warn: (message, facts) => clientLog.write('warn', area, message, facts),
        error: (message, facts) => clientLog.write('error', area, message, facts),
    };
}

if (typeof document !== 'undefined') {
    // Leaving the page is when the last entries are most worth having.
    document.addEventListener('visibilitychange', () => {
        if (document.visibilityState === 'hidden') void clientLog.flush();
    });
}
