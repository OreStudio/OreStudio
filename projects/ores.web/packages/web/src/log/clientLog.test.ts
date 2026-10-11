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

import { describe, expect, it, vi } from 'vitest';
import { ClientLog, type Entry } from './clientLog.js';

const NOW = new Date('2026-10-10T18:00:00.000Z');

describe('the client trail', () => {
    it('names the logger under ores.web.client and keeps the facts', async () => {
        const sent: Entry[] = [];
        const log = new ClientLog(
            async (entries) => void sent.push(...entries),
            () => NOW,
        );
        log.write('info', 'form', 'form opened', { form: 'identity', skipped: undefined });
        await log.flush();
        expect(sent).toEqual([
            {
                at: '2026-10-10T18:00:00.000Z',
                level: 'info',
                logger: 'ores.web.client.form',
                message: 'form opened',
                fields: { form: 'identity' },
            },
        ]);
    });

    it('keeps the order entries were written in', async () => {
        const sent: string[] = [];
        const log = new ClientLog(
            async (entries) => void sent.push(...entries.map((e) => e.message)),
        );
        log.write('info', 'route', 'entered');
        log.write('info', 'form', 'opened');
        log.write('info', 'form', 'closed');
        await log.flush();
        expect(sent).toEqual(['entered', 'opened', 'closed']);
    });

    it('sends a warning at once and batches the rest', async () => {
        vi.useFakeTimers();
        const batches: number[] = [];
        const log = new ClientLog(async (entries) => void batches.push(entries.length));
        log.write('info', 'route', 'a');
        log.write('info', 'route', 'b');
        expect(batches).toEqual([]);
        log.write('warn', 'request', 'failed');
        await vi.advanceTimersByTimeAsync(0);
        expect(batches).toEqual([3]);
        vi.useRealTimers();
    });

    it('keeps what could not be sent for the next try', async () => {
        let up = false;
        const sent: string[] = [];
        const log = new ClientLog(async (entries) => {
            if (!up) throw new Error('down');
            sent.push(...entries.map((e) => e.message));
        });
        log.write('info', 'route', 'a');
        await log.flush();
        expect(log.waiting).toBe(1);
        up = true;
        await log.flush();
        expect(sent).toEqual(['a']);
    });
});
