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
import { subjects } from '@ores/wire-protocol/generated/trading/protocol/product_schema_operation_protocol';
import { notFound, unexpectedReply } from './errors.js';
import { text } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/*
 * The products a screen can book, and the description of one: the fields of
 * its element and of every schema type they reach, the inputs and codes of the
 * scripts its forms run, and the forms. The trading service answers both; the
 * routes name each field so the screen reads one shape whatever the service adds.
 */

const record = z.looseObject({});

const listReply = z.looseObject({
    success: z.boolean().default(false),
    message: z.string().default(''),
    products: z.array(record).default([]),
});

const describeReply = z.looseObject({
    success: z.boolean().default(false),
    message: z.string().default(''),
    product: record.default({}),
    element: z.string().default(''),
    fields: z.array(record).default([]),
    inputs: z.array(record).default([]),
    codes: z.array(record).default([]),
    forms: z.array(record).default([]),
});

const codeParams = z.object({ code: z.string().trim().min(1).max(128) });

function summary(entry: Record<string, unknown>) {
    return {
        code: text(entry['code']),
        name: text(entry['name']),
        construction: text(entry['construction']),
    };
}

export function registerProductRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/trading/products', async (request) => {
        const session = requireSession(request);
        const reply = await session.client.callAuthenticated(
            subjects.list_products_request,
            {},
            listReply,
        );
        if (!reply.success) {
            throw unexpectedReply(reply.message);
        }
        return { rows: reply.products.map(summary) };
    });

    server.get('/api/trading/products/:code', async (request) => {
        const session = requireSession(request);
        const { code } = codeParams.parse(request.params);
        const reply = await session.client.callAuthenticated(
            subjects.describe_product_request,
            { code },
            describeReply,
        );
        if (!reply.success) {
            throw notFound(reply.message === '' ? `No product is named ${code}.` : reply.message);
        }
        return {
            product: summary(reply.product),
            element: reply.element,
            fields: reply.fields.map((f) => ({
                type_name: text(f['type_name']),
                name: text(f['name']),
                position: typeof f['position'] === 'number' ? f['position'] : 0,
                label: text(f['label']),
                help: text(f['help']),
                description: text(f['description']),
                value_domain: text(f['value_domain']),
                is_required: f['is_required'] === true,
                default_text: text(f['default_text']),
                format: text(f['format']),
                xpath: text(f['xpath']),
            })),
            inputs: reply.inputs.map((i) => ({
                script_name: text(i['script_name']),
                name: text(i['input_name']),
                data_type: text(i['data_type']),
                is_array: i['is_array'] === true,
                value_rule: text(i['value_rule']),
                description: text(i['description']),
            })),
            codes: reply.codes.map((c) => ({
                script_name: text(c['script_name']),
                input_name: text(c['input_name']),
                code: typeof c['code'] === 'number' ? c['code'] : 0,
                label: text(c['label']),
            })),
            forms: reply.forms.map((f) => ({
                kind: text(f['kind']),
                script_name: text(f['script_name']),
                product_tag: text(f['product_tag']),
            })),
        };
    });
}
