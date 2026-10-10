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

/**
 * The trail the browser keeps, as it arrives.
 *
 * A name that begins `ores.web.client` is the only name accepted, so the browser
 * can describe what a person did and cannot write as a service.
 */
const entrySchema = z.object({
    at: z.string().max(40),
    level: z.enum(['trace', 'debug', 'info', 'warn', 'error']),
    logger: z.string().regex(/^ores\.web\.client(\.[a-z][a-z0-9_]*)*$/),
    message: z.string().max(200),
    fields: z
        .record(
            z.string().regex(/^[a-z][a-z0-9_]*$/),
            z.union([z.string().max(300), z.number(), z.boolean()]),
        )
        .default({}),
});

export const clientLogSchema = z.object({
    entries: z.array(entrySchema).max(100),
});

export type ClientLogEntry = z.infer<typeof entrySchema>;

/** Keys a record already uses, which a browser fact may not replace. */
const RESERVED = new Set(['level', 'time', 'msg', 'name', 'pid', 'hostname', 'req', 'res', 'err']);

interface Sink {
    child(bindings: {
        name: string;
    }): Record<ClientLogEntry['level'], (o: object, m: string) => void>;
}

/** Writes one entry under its own logger name, with what the session adds. */
export function writeClientEntry(
    log: Sink,
    entry: ClientLogEntry,
    session: Readonly<Record<string, string>>,
): void {
    const facts: Record<string, string | number | boolean> = {};
    for (const [key, found] of Object.entries(entry.fields)) {
        if (!RESERVED.has(key)) facts[key] = found;
    }
    log.child({ name: entry.logger })[entry.level](
        { ...facts, ...session, client_time: entry.at },
        entry.message,
    );
}
