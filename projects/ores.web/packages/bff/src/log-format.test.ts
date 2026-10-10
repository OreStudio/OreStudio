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
import { formatLogLine, logStream, stamp } from './log-format.js';

describe('the BFF log line', () => {
    it('is written like a service line: time, severity, logger, message, facts', () => {
        const line = formatLogLine({
            level: 40,
            time: Date.UTC(2026, 9, 10, 18, 13, 1, 964),
            name: 'ores.web.client.form',
            msg: 'form closed',
            session_id: 's-1',
            form: 'identity',
        });
        expect(line).toBe(
            '2026-10-10 18:13:01.964000 [WARN] [ores.web.client.form] form closed session_id=s-1 form=identity',
        );
    });

    it('names a request by request_id and states its method, url and status', () => {
        const line = formatLogLine({
            level: 30,
            time: 0,
            reqId: 'r-9',
            req: { method: 'GET', url: '/api/accounts?offset=0' },
            res: { statusCode: 403 },
            responseTime: 25.6,
            msg: 'request completed',
        });
        expect(line).toContain('[INFO] [ores.web.bff] request completed');
        expect(line).toContain('request_id=r-9');
        expect(line).toContain('method=GET');
        expect(line).toContain('status=403');
        expect(line).toContain('response_ms=26');
    });

    it('quotes a value that holds a space', () => {
        expect(formatLogLine({ level: 30, time: 0, msg: 'x', reason: 'not allowed' })).toContain(
            'reason="not allowed"',
        );
    });

    it('writes each record of a chunk as its own line, and passes text through', () => {
        const lines: string[] = [];
        const stream = logStream((line) => lines.push(line));
        stream.write(`${JSON.stringify({ level: 30, time: 0, msg: 'a' })}\nnot json\n`);
        expect(lines).toHaveLength(2);
        expect(lines[1]).toBe('not json');
    });

    it('stamps in UTC to the microsecond', () => {
        expect(stamp(Date.UTC(2026, 0, 2, 3, 4, 5, 6))).toBe('2026-01-02 03:04:05.006000');
    });
});
