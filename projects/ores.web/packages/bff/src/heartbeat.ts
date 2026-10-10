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
 */

import { randomUUID } from 'node:crypto';
import { readFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';

/**
 * The web service's heartbeat.
 *
 * Every service the registry expects tells it, on an interval, that it is
 * running. The services written in C++ do so through one publisher; this is the
 * same message from the one process that is not C++, so the services screen can
 * show the web service running instead of missing.
 */

/** The name the registry expects, as the services seed declares it. */
export const WEB_SERVICE_NAME = 'ores.web.service';

/** The interval the C++ publisher uses, so the roster's reporting window fits both. */
export const HEARTBEAT_INTERVAL_MS = 15_000;

/** How many directories above the start the declaration is searched for. */
const MAX_CLIMB = 6;

/** What a heartbeat is sent through; the wire client satisfies it. */
export interface HeartbeatSink {
    publishServiceHeartbeat(message: {
        service_name: string;
        instance_id: string;
        host_id: string;
        version: string;
    }): void;
}

/** The part of the server's logger the heartbeat uses. */
export interface HeartbeatLog {
    warn(details: object, message: string): void;
    info(message: string): void;
}

export interface HeartbeatOptions {
    readonly sink: HeartbeatSink;
    readonly version: string;
    readonly log: HeartbeatLog;
    readonly intervalMs?: number;
    /** Generated once per process; a test passes its own. */
    readonly instanceId?: string;
}

/**
 * Starts the heartbeat, and returns the way to stop it.
 *
 * One heartbeat goes out at once and then one per interval. A failed publish is
 * logged when the failure begins and again when it ends, not on every beat:
 * a broker that is down for an hour is one fact, not two hundred and forty
 * lines. The timer does not hold the process open.
 */
export function startHeartbeat(options: HeartbeatOptions): () => void {
    const instanceId = options.instanceId ?? randomUUID();
    let failing = false;

    const beat = (): void => {
        try {
            options.sink.publishServiceHeartbeat({
                service_name: WEB_SERVICE_NAME,
                instance_id: instanceId,
                host_id: '',
                version: options.version,
            });
            if (failing) {
                failing = false;
                options.log.info('heartbeat publishing again');
            }
        } catch (error) {
            if (!failing) {
                failing = true;
                options.log.warn({ err: error }, 'heartbeat could not be published');
            }
        }
    };

    beat();
    const timer = setInterval(beat, options.intervalMs ?? HEARTBEAT_INTERVAL_MS);
    timer.unref();
    return () => clearInterval(timer);
}

/**
 * The release the project declares, in the form the other services report.
 *
 * Read from the project's own declaration so the number is written in one
 * place. The service runs from its own package directory, not from the
 * checkout root, so the search climbs until it finds the declaration. The C++
 * services report the number without a leading `v`, and the services screen
 * compares releases as text, so this does the same. A tree with no declaration
 * reports `unknown`, which the screen shows rather than hides.
 */
export function releaseVersion(start: string): string {
    let directory = resolve(start);
    for (let depth = 0; depth < MAX_CLIMB; depth += 1) {
        try {
            const cmake = readFileSync(resolve(directory, 'CMakeLists.txt'), 'utf8');
            const release = /project\(\s*\w+\s+VERSION\s+([0-9.]+)/.exec(cmake)?.[1];
            if (release !== undefined) {
                return release;
            }
        } catch {
            // No file at this level: the declaration may be further up.
        }
        const parent = dirname(directory);
        if (parent === directory) {
            break;
        }
        directory = parent;
    }
    return 'unknown';
}
