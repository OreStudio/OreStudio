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
import { subjects as scriptSubjects } from '@ores/wire-protocol/generated/trading/protocol/script_library_operation_protocol';
import { subjects as operationsSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_operations_protocol';
import { subjects as productSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_product_protocol';
import { subjects as scriptedSubjects } from '@ores/wire-protocol/generated/trading/protocol/scripted_instrument_protocol';
import { refusalOfBooking, tradeHeader } from './booking.js';
import { HttpFailure, invalidRequest, notFound } from './errors.js';
import { input, resultSchema, text } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/*
 * Booking through the trading service's own operations: a script validated
 * against a trade's variables, a scripted trade, a product type in either form,
 * and the form a trade was booked in, which a screen reopens it in.
 */

const variableSchema = z.object({
    name: z.string().min(1).max(128),
    type: z.string().min(1).max(32),
    isArray: z.boolean().default(false),
    values: z.array(z.string()).default([]),
});

const scriptSchema = z.object({
    scriptName: z.string().trim().max(128).default(''),
    scriptVersion: z.number().int().min(0).default(0),
    code: z.string().default(''),
    variables: z.array(variableSchema).default([]),
});

const headerSchema = z.object({
    bookId: z.uuid(),
    counterpartyId: z.uuid(),
    tradeDate: z.iso.date(),
    draftReasonCode: z.string().trim().min(1).max(64).optional(),
});

const scriptedBodySchema = headerSchema.extend(scriptSchema.shape).extend({
    npvVariable: z.string().max(128).default(''),
    productCode: z.string().trim().max(128).default(''),
});

const productBodySchema = headerSchema.extend({
    productCode: z.string().trim().min(1).max(128),
    form: z.enum(['native', 'scripted']),
    scriptName: z.string().trim().max(128).default(''),
    instruments: z.looseObject({}),
});

const tradeParams = z.object({ tradeId: z.uuid() });

const diagnosticSchema = z.looseObject({
    purpose: z.string().default(''),
    severity: z.string().default(''),
    code: z.string().default(''),
    line: z.number().default(0),
    column: z.number().nullable().default(null),
    message: z.string().default(''),
    subject: z.string().default(''),
});

const bookingReply = z.looseObject({
    result: resultSchema,
    activity_id: z.uuid().nullable().default(null),
    diagnostics: z.array(diagnosticSchema).default([]),
});

function diagnosticsOf(found: readonly z.infer<typeof diagnosticSchema>[]) {
    return found.map((d) => ({
        purpose: d.purpose,
        severity: d.severity,
        code: d.code,
        line: d.line,
        column: d.column,
        message: d.message,
        subject: d.subject,
    }));
}

function wireVariables(variables: readonly z.infer<typeof variableSchema>[]) {
    return variables.map((v) => ({
        name: v.name,
        type: v.type,
        is_array: v.isArray,
        values: v.values,
    }));
}

function partyOf(session: LiveSession): string {
    const partyId = session.party?.id;
    if (partyId === undefined) {
        throw invalidRequest('A booking is made by a party, and this session works for none.');
    }
    return partyId;
}

/**
 * A refused booking, in the status the screen acts on: a script that fails its
 * validation is unprocessable and carries its findings; a script, product or form
 * that does not exist is not found; the rest as any refused booking.
 */
function refusalOf(reply: z.infer<typeof bookingReply>): HttpFailure {
    const { code, message } = reply.result;
    if (code === 'script_invalid') {
        return new HttpFailure(
            422,
            { code: 'invalid-request', message },
            { diagnostics: diagnosticsOf(reply.diagnostics) },
        );
    }
    if (code === 'script_not_found' || code === 'product_unknown' || code === 'form_not_offered') {
        return notFound(message);
    }
    return refusalOfBooking(reply.result);
}

/** The trading service answered, and its answer cannot be served. */
function upstreamFailure(message: string): HttpFailure {
    return new HttpFailure(502, { code: 'upstream-unavailable', message });
}

export function registerScriptedBookingRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.post('/api/trading/booking/validate-script', async (request) => {
        const session = requireSession(request);
        const body = input(scriptSchema, request.body);
        const reply = await session.client.callAuthenticated(
            scriptSubjects.validate_script_request,
            {
                script_name: body.scriptName,
                script_version: body.scriptVersion,
                code: body.code,
                variables: wireVariables(body.variables),
            },
            z.looseObject({
                success: z.boolean().default(false),
                message: z.string().default(''),
                code: z.string().default(''),
                is_valid: z.boolean().default(false),
                diagnostics: z.array(diagnosticSchema).default([]),
            }),
        );
        if (reply.code === 'script_not_found') {
            throw notFound(reply.message);
        }
        if (reply.code === 'request_invalid') {
            throw invalidRequest(reply.message);
        }
        if (!reply.success) {
            throw upstreamFailure(reply.message);
        }
        return {
            isValid: reply.is_valid,
            message: reply.message,
            diagnostics: diagnosticsOf(reply.diagnostics),
        };
    });

    server.post('/api/trading/booking/scripted', async (request, reply) => {
        const session = requireSession(request);
        const body = input(scriptedBodySchema, request.body);
        const header = tradeHeader({
            partyId: partyOf(session),
            counterpartyId: body.counterpartyId,
            bookId: body.bookId,
            tradeDate: body.tradeDate,
            draftReasonCode: body.draftReasonCode,
            tradeType: 'ScriptedTrade',
        });
        const booked = await session.client.callAuthenticated(
            scriptSubjects.book_scripted_request,
            {
                ...header,
                script_name: body.scriptName,
                script_version: body.scriptVersion,
                code: body.code,
                npv_variable: body.npvVariable,
                variables: wireVariables(body.variables),
                product_code: body.productCode,
            },
            bookingReply,
        );
        if (booked.result.outcome !== 'ok') {
            throw refusalOf(booked);
        }
        reply.code(201);
        return { id: header.anchor.id, activityId: booked.activity_id };
    });

    server.post('/api/trading/booking/product', async (request, reply) => {
        const session = requireSession(request);
        const body = input(productBodySchema, request.body);
        const header = tradeHeader({
            partyId: partyOf(session),
            counterpartyId: body.counterpartyId,
            bookId: body.bookId,
            tradeDate: body.tradeDate,
            draftReasonCode: body.draftReasonCode,
            tradeType: body.productCode,
        });
        const booked = await session.client.callAuthenticated(
            operationsSubjects.book_product_request,
            {
                ...header,
                product_code: body.productCode,
                booking_form: body.form,
                script_name: body.scriptName,
                instruments: body.instruments,
            },
            bookingReply,
        );
        if (booked.result.outcome !== 'ok') {
            throw refusalOf(booked);
        }
        reply.code(201);
        return { id: header.anchor.id, activityId: booked.activity_id };
    });

    server.get('/api/trading/booking/:tradeId/form', async (request) => {
        const session = requireSession(request);
        const { tradeId } = input(tradeParams, request.params);
        const product = await session.client.callAuthenticated(
            productSubjects.get_trade_product_request,
            { key: { trade_id: tradeId } },
            z.looseObject({
                result: resultSchema,
                trade_product: z.looseObject({}).nullable().default(null),
            }),
        );
        if (product.result.outcome !== 'ok' && product.result.outcome !== 'missing') {
            throw refusalOfBooking(product.result);
        }
        if (product.trade_product === null) {
            throw notFound(`Trade ${tradeId} records no product it was booked as.`);
        }
        const form = text(product.trade_product['booking_form']);
        let scriptName = '';
        let scriptVersion: number | null = null;
        if (form === 'scripted') {
            const scripted = await session.client.callAuthenticated(
                scriptedSubjects.get_scripted_instrument_request,
                { key: { trade_id: tradeId } },
                z.looseObject({
                    result: resultSchema,
                    scripted_instrument: z.looseObject({}).nullable().default(null),
                }),
            );
            if (scripted.result.outcome !== 'ok' && scripted.result.outcome !== 'missing') {
                throw refusalOfBooking(scripted.result);
            }
            const row = scripted.scripted_instrument;
            if (row === null) {
                throw upstreamFailure(
                    `Trade ${tradeId} was booked scripted, and the trading service holds no script for it.`,
                );
            }
            scriptName = text(row['script_name']);
            const version = row['script_version'];
            scriptVersion = typeof version === 'number' ? version : null;
        }
        return {
            tradeId,
            productCode: text(product.trade_product['product_code']),
            form,
            scriptName,
            scriptVersion,
        };
    });
}
