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
 * The BFF's log lines, in the form every ORE Studio service writes.
 *
 *   2026-10-10 18:13:01.967257 [INFO] [ores.web.bff.http] request completed status=403
 *
 * A timestamp, a severity, the logger's dotted name, the message, and the facts
 * as `key=value`. The C++ services write this form, so one search across the
 * journal follows a request from the browser through the BFF to the service
 * that answered it, by `request_id`, `correlation_id` or `session_id`.
 */

const LEVELS: Readonly<Record<number, string>> = {
    10: 'TRACE',
    20: 'DEBUG',
    30: 'INFO',
    40: 'WARN',
    50: 'ERROR',
    60: 'FATAL',
};

/** The logger a record without a name of its own is written by. */
export const DEFAULT_LOGGER = 'ores.web.bff';

/** Fields pino adds to every record that carry nothing a reader needs. */
const OMITTED = new Set(['level', 'time', 'pid', 'hostname', 'msg', 'name', 'v']);

function pad(value: number, width: number): string {
    return String(value).padStart(width, '0');
}

/** The timestamp, to the microsecond like the services, in UTC. */
export function stamp(ms: number): string {
    const d = new Date(ms);
    return (
        `${d.getUTCFullYear()}-${pad(d.getUTCMonth() + 1, 2)}-${pad(d.getUTCDate(), 2)} ` +
        `${pad(d.getUTCHours(), 2)}:${pad(d.getUTCMinutes(), 2)}:${pad(d.getUTCSeconds(), 2)}.` +
        `${pad(d.getUTCMilliseconds(), 3)}000`
    );
}

function value(text: unknown): string {
    const plain = typeof text === 'string' ? text : JSON.stringify(text);
    return /^[^\s="]+$/.test(plain) ? plain : JSON.stringify(plain);
}

/** The facts of a record, flattened to `key=value` pairs in a stable order. */
function facts(record: Readonly<Record<string, unknown>>): string[] {
    const pairs: string[] = [];
    const add = (key: string, found: unknown): void => {
        if (found !== undefined && found !== null && found !== '') {
            pairs.push(`${key}=${value(found)}`);
        }
    };
    add('request_id', record['reqId']);
    const req = record['req'] as { method?: string; url?: string } | undefined;
    add('method', req?.method);
    add('url', req?.url);
    const res = record['res'] as { statusCode?: number } | undefined;
    add('status', res?.statusCode);
    const took = record['responseTime'];
    add('response_ms', typeof took === 'number' ? Math.round(took) : undefined);
    const err = record['err'] as { message?: string; type?: string } | undefined;
    add('error', err?.message);
    add('error_type', err?.type);
    for (const [key, found] of Object.entries(record)) {
        if (
            OMITTED.has(key) ||
            ['reqId', 'req', 'res', 'responseTime', 'err'].includes(key) ||
            typeof found === 'object'
        ) {
            continue;
        }
        add(key, found);
    }
    return pairs;
}

/** One pino record, as the line a service would have written. */
export function formatLogLine(record: Readonly<Record<string, unknown>>): string {
    const level = LEVELS[Number(record['level'])] ?? 'INFO';
    const name = typeof record['name'] === 'string' ? record['name'] : DEFAULT_LOGGER;
    const message = typeof record['msg'] === 'string' ? record['msg'] : '';
    const time = typeof record['time'] === 'number' ? record['time'] : Date.now();
    return [`${stamp(time)} [${level}] [${name}]`, message, ...facts(record)]
        .filter((part) => part !== '')
        .join(' ')
        .replace(/ {2,}/g, ' ');
}

/** A stream pino writes to: each JSON record becomes one service-style line. */
export function logStream(write: (line: string) => void): { write: (chunk: string) => void } {
    return {
        write(chunk: string): void {
            for (const raw of chunk.split('\n')) {
                if (raw.trim() === '') continue;
                try {
                    write(formatLogLine(JSON.parse(raw) as Record<string, unknown>));
                } catch {
                    write(raw);
                }
            }
        },
    };
}
