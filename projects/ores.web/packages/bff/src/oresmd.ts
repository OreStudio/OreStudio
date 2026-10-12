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

import type { FastifyInstance, FastifyRequest } from 'fastify';
import { z } from 'zod';
import {
    oresmdComposeRequestSchema,
    oresmdComposeResultSchema,
    oresmdParseRequestSchema,
    oresmdParseResultSchema,
    oresmdSchemaSchema,
} from '@ores/contracts';
import {
    subjects,
    type ComposeOresmdUriRequest,
    type ComposeOresmdUriResponse,
    type DescribeOresmdSchemaResponse,
    type ParseOresmdUriRequest,
    type ParseOresmdUriResponse,
} from '@ores/wire-protocol/generated/marketdata/protocol/operations_protocol';
import { HttpFailure } from './errors.js';
import { input } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/**
 * The oresmd URI routes: what a URI may hold, how a URI reads into its parts,
 * and how parts write a URI.
 *
 * The server's codec holds the grammar. Each route forwards the structure to the
 * server's subject and returns the answer, so the BFF decides nothing about
 * whether a URI is valid and spells none. The server checks the permission. The
 * routes check the size of a request and nothing else, because a request comes
 * from a browser.
 *
 * A URI the codec refuses is an answer and not a fault. Parse and compose return
 * it with \`success\` false and the codec's words, so the editor can show them where
 * the person is typing. A refusal of the call itself, such as a missing
 * permission, is an error as for any other read.
 */

/** How long a browser may keep the schema. It is the codec's own table and changes with a release. */
const SCHEMA_MAX_AGE_SECONDS = 300;

const schemaReply = oresmdSchemaSchema.extend({
    success: z.boolean(),
    message: z.string(),
}) satisfies z.ZodType<DescribeOresmdSchemaResponse>;

// The shapes the browser parses are the shapes the server generates. A change to
// the model that the contracts do not follow fails the typecheck here.
oresmdParseResultSchema satisfies z.ZodType<ParseOresmdUriResponse>;
oresmdComposeResultSchema satisfies z.ZodType<ComposeOresmdUriResponse>;
oresmdParseRequestSchema satisfies z.ZodType<ParseOresmdUriRequest>;
oresmdComposeRequestSchema satisfies z.ZodType<
    ComposeOresmdUriRequest,
    z.input<typeof oresmdComposeRequestSchema>
>;

export function registerOresmdRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/oresmd/schema', async (request, reply) => {
        const session = requireSession(request);
        const answer = await session.client.callAuthenticated(
            subjects.describe_oresmd_schema_request,
            {},
            schemaReply,
        );
        if (!answer.success) {
            throw new HttpFailure(422, { code: 'invalid-request', message: answer.message });
        }
        reply.header('Cache-Control', `private, max-age=${SCHEMA_MAX_AGE_SECONDS}`);
        return {
            assets: answer.assets,
            instruments: answer.instruments,
            quotes: answer.quotes,
            indexes: answer.indexes,
        };
    });

    server.post('/api/oresmd/parse', async (request) => {
        const session = requireSession(request);
        const body = input(oresmdParseRequestSchema, request.body);
        return session.client.callAuthenticated(
            subjects.parse_oresmd_uri_request,
            body,
            oresmdParseResultSchema,
        );
    });

    server.post('/api/oresmd/compose', async (request) => {
        const session = requireSession(request);
        const body = input(oresmdComposeRequestSchema, request.body);
        return session.client.callAuthenticated(
            subjects.compose_oresmd_uri_request,
            body,
            oresmdComposeResultSchema,
        );
    });
}
