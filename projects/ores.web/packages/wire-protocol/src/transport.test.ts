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

import { describe, expect, it } from 'vitest';
import { TransportError } from './errors.js';
import { NatsTransport } from './transport.js';

describe('publishing on the NATS transport', () => {
    it('refuses before the connection is up, so the caller can log it', () => {
        const transport = new NatsTransport({
            server: 'nats://127.0.0.1:1',
            subjectPrefix: 'ores.test',
            tls: { ca: 'ca', cert: 'cert', key: 'key' },
        });

        expect(() => {
            transport.publish('telemetry.v1.ops.service_heartbeat', new Uint8Array());
        }).toThrow(TransportError);
    });
});
