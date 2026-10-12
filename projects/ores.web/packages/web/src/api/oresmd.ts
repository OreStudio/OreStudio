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

import {
    oresmdComposeResultSchema,
    oresmdParseResultSchema,
    oresmdSchemaSchema,
    type OresmdComposeRequest,
    type OresmdComposeResult,
    type OresmdParseResult,
    type OresmdSchema,
} from '@ores/contracts';
import { request } from './transport.js';

function post(path: string, body: unknown): Promise<unknown> {
    return request(path, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(body),
    });
}

/**
 * The oresmd URI reads and writes. The server's codec holds the grammar, so the
 * browser asks it what a URI may hold, how a URI reads and what parts write. A
 * URI the codec refuses comes back as an answer with its reason, not as a
 * failure.
 */
export const oresmd = {
    /** Every instrument type with its fields, the quotes and the index families. */
    async schema(): Promise<OresmdSchema> {
        return oresmdSchemaSchema.parse(await request('/api/oresmd/schema', { method: 'GET' }));
    },

    /** The parts of a URI, or the codec's reason for refusing it. */
    async parse(uri: string): Promise<OresmdParseResult> {
        return oresmdParseResultSchema.parse(await post('/api/oresmd/parse', { uri }));
    },

    /** The URI the codec writes for the parts, or the reason they make none. */
    async compose(parts: OresmdComposeRequest): Promise<OresmdComposeResult> {
        return oresmdComposeResultSchema.parse(await post('/api/oresmd/compose', parts));
    },
};
