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
import { clientLogSchema, writeClientEntry, type ClientLogEntry } from './client-log.js';

function entry(over: Partial<ClientLogEntry> = {}): ClientLogEntry {
    return {
        at: '2026-10-10T18:00:00.000Z',
        level: 'info',
        logger: 'ores.web.client.form',
        message: 'form opened',
        fields: { form: 'identity' },
        ...over,
    };
}

describe('the browser trail', () => {
    it('accepts only loggers under ores.web.client', () => {
        const ok = clientLogSchema.safeParse({ entries: [entry()] });
        const service = clientLogSchema.safeParse({
            entries: [entry({ logger: 'ores.iam.service' })],
        });
        expect(ok.success).toBe(true);
        expect(service.success).toBe(false);
    });

    it('writes an entry under its own logger with the session beside it', () => {
        const written: { name: string; facts: Record<string, unknown>; message: string }[] = [];
        const sink = {
            child: ({ name }: { name: string }) => ({
                trace: () => undefined,
                debug: () => undefined,
                warn: () => undefined,
                error: () => undefined,
                info: (facts: object, message: string) =>
                    written.push({ name, facts: facts as Record<string, unknown>, message }),
            }),
        };
        writeClientEntry(sink, entry(), { session_id: 's-1', account: 'hazel' });
        expect(written).toEqual([
            {
                name: 'ores.web.client.form',
                message: 'form opened',
                facts: {
                    form: 'identity',
                    session_id: 's-1',
                    account: 'hazel',
                    client_time: '2026-10-10T18:00:00.000Z',
                },
            },
        ]);
    });

    it('does not let a browser fact replace a field of the record', () => {
        const written: Record<string, unknown>[] = [];
        const sink = {
            child: () => ({
                trace: () => undefined,
                debug: () => undefined,
                warn: () => undefined,
                error: () => undefined,
                info: (facts: object) => written.push(facts as Record<string, unknown>),
            }),
        };
        writeClientEntry(sink, entry({ fields: { level: 'fatal', msg: 'x', form: 'a' } }), {});
        expect(written[0]).not.toHaveProperty('level');
        expect(written[0]).not.toHaveProperty('msg');
        expect(written[0]).toHaveProperty('form', 'a');
    });
});
