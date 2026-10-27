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
import { subjects as counterpartySubjects } from '@ores/wire-protocol/generated/refdata/protocol/counterparty_protocol';
import { subjects as identifierSubjects } from '@ores/wire-protocol/generated/trading/protocol/structure_identifier_protocol';
import { subjects as memberSubjects } from '@ores/wire-protocol/generated/trading/protocol/structure_member_protocol';
import { subjects as structureSubjects } from '@ores/wire-protocol/generated/trading/protocol/structure_protocol';
import { subjects as summarySubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_summary_protocol';
import { subjects as tradeIdentifierSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_identifier_protocol';
import { HttpFailure, invalidRequest, notFound, notPermitted } from './errors.js';
import { input, readAll, resultSchema, row, text } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/** How many trades the add-a-leg picker offers, newest first. */
const CANDIDATE_PAGE = 200;

/** The roles a leg may take when the structure names no template of its own. */
const FREE_ROLES = ['leg'] as const;

const structureParamsSchema = z.object({ id: z.uuid() });

const legBodySchema = z.object({
    tradeId: z.uuid(),
    role: z.string().trim().min(1).max(64),
});

/** The intent every leg this screen writes carries. */
function intent(): { readonly reason_code: string; readonly commentary: string } {
    return { reason_code: 'system.new_record', commentary: 'Added to the structure from its page.' };
}

/** The precondition that refuses a write over a row that already exists. */
function mustNotExist(): { readonly kind: string; readonly version: null } {
    return { kind: 'must_not_exist', version: null };
}

/**
 * Why a refused leg was refused, in the status the screen acts on.
 *
 * A leg the service declines is the caller's business: a kind it does not know,
 * a trade already in a structure, a role the structure does not offer. Only a
 * fault behind the service is the server's, and that answers 502.
 */
function refusalOfLeg(result: z.infer<typeof resultSchema>): HttpFailure {
    switch (result.outcome) {
        case 'denied':
            return notPermitted(result.message);
        case 'conflict':
            return new HttpFailure(409, { code: 'conflict', message: result.message });
        case 'unavailable':
            return new HttpFailure(502, { code: 'upstream-unavailable', message: result.message });
        default:
            return invalidRequest(result.message);
    }
}

/**
 * The members of one structure.
 *
 * The store answers members by trade and not by structure, so the set is read
 * whole and narrowed here. A tenant holds as many members as it has legs, and
 * the read is bounded by the page size, which is what makes this affordable.
 */
async function membersOf(
    session: LiveSession,
    structureId: string,
): Promise<readonly Record<string, unknown>[]> {
    const all = await readAll(session, memberSubjects.list_structure_members_request, 'members', {
        filter: { trade_id_one_of: null },
    });
    return all.filter((entry) => text(entry['structure_id']) === structureId);
}

/** The references the trades of a set of legs are known by, keyed by trade. */
async function referencesOf(
    session: LiveSession,
    tradeIds: readonly string[],
): Promise<ReadonlyMap<string, string>> {
    const labels = new Map<string, string>();
    if (tradeIds.length === 0) {
        return labels;
    }
    const found = await readAll(
        session,
        tradeIdentifierSubjects.list_trade_identifiers_request,
        'identifiers',
        { filter: { trade_id_one_of: [...tradeIds] } },
    );
    for (const entry of found) {
        const tradeId = text(entry['trade_id']);
        const value = text(entry['id_value']);
        if (tradeId !== '' && value !== '' && !labels.has(tradeId)) {
            labels.set(tradeId, value);
        }
    }
    return labels;
}

/**
 * The trades that may become a leg of this structure.
 *
 * A trade is a leg of one structure at a time, so a trade already grouped
 * anywhere is not offered: a picker that offers it sets the desk up to be
 * refused. The set already grouped is read whole because the store answers
 * members by trade and not by structure.
 */
async function legCandidates(
    session: LiveSession,
    structureId: string,
): Promise<readonly Record<string, unknown>[]> {
    const memberships = await readAll(
        session,
        memberSubjects.list_structure_members_request,
        'members',
        { filter: { trade_id_one_of: null } },
    );
    const taken = new Set(memberships.map((entry) => text(entry['trade_id'])));
    const summaries = await readAll(
        session,
        summarySubjects.list_trade_summaries_request,
        'trade_summaries',
        {
            filter: {
                party_id: null,
                counterparty_id: null,
                book_id: null,
                instrument_type: null,
                status: null,
                trade_date_from: null,
                trade_date_to: null,
                search: null,
                row_kind: 'trade',
                row_kind_one_of: null,
            },
            order: { field: 'trade_date', descending: true },
            limit: CANDIDATE_PAGE,
        },
    );
    return summaries
        .filter((entry) => text(entry['row_kind']) === 'trade' && !taken.has(text(entry['id'])))
        .map((entry) => ({
            id: text(entry['id']),
            instrument_type: text(entry['instrument_type']),
            trade_date: text(entry['trade_date']),
            structure_id: structureId,
        }));
}

export function registerStructureRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    server.get('/api/structures/:id', async (request) => {
        const session = requireSession(request);
        const { id } = input(structureParamsSchema, request.params);

        const structures = await readAll(
            session,
            structureSubjects.list_structures_request,
            'structures',
            { filter: { id_one_of: [id] } },
        );
        const structure = structures[0];
        if (structure === undefined) {
            throw notFound('No structure has that id.');
        }

        const [identifiers, members] = await Promise.all([
            readAll(session, identifierSubjects.list_structure_identifiers_request, 'identifiers', {
                filter: { structure_id_one_of: [id] },
            }),
            membersOf(session, id),
        ]);
        const references = await referencesOf(
            session,
            members.map((member) => text(member['trade_id'])).filter((value) => value !== ''),
        );

        const counterpartyId = text(structure['counterparty_id']);
        const counterparties =
            counterpartyId === ''
                ? []
                : await readAll(
                      session,
                      counterpartySubjects.list_counterparties_request,
                      'counterparties',
                      { filter: { id_one_of: [counterpartyId] } },
                  );

        const legs = members
            .map((member) => ({
                trade_id: text(member['trade_id']),
                reference: references.get(text(member['trade_id'])) ?? '',
                role: text(member['role']),
                sequence_number: Number(member['sequence_number'] ?? 0),
            }))
            .sort((left, right) => left.sequence_number - right.sequence_number);

        return {
            structure: {
                ...structure,
                counterparty_name:
                    text(counterparties[0]?.['full_name']) || counterpartyId,
            },
            reference: text(identifiers[0]?.['id_value']),
            identifiers,
            legs,
            roles: [...FREE_ROLES],
        };
    });

    server.get('/api/structures/:id/leg-candidates', async (request) => {
        const session = requireSession(request);
        const { id } = input(structureParamsSchema, request.params);
        const rows = await legCandidates(session, id);
        const references = await referencesOf(
            session,
            rows.map((entry) => text(entry['id'])),
        );
        return {
            rows: rows.map((entry) => ({
                ...entry,
                reference: references.get(text(entry['id'])) ?? '',
            })),
        };
    });

    server.post('/api/structures/:id/legs', async (request, reply) => {
        const session = requireSession(request);
        const { id } = input(structureParamsSchema, request.params);
        const body = input(legBodySchema, request.body);

        const structures = await readAll(
            session,
            structureSubjects.list_structures_request,
            'structures',
            { filter: { id_one_of: [id] } },
        );
        if (structures[0] === undefined) {
            throw notFound('No structure has that id.');
        }
        if (!(FREE_ROLES as readonly string[]).includes(body.role)) {
            throw invalidRequest('The structure does not offer that role.');
        }

        /*
         * A trade is a leg of one structure at a time, which the store states by
         * keying members on the trade. The check is made here so the screen is
         * told which structure already holds it rather than a bare conflict.
         */
        const held = await readAll(
            session,
            memberSubjects.list_structure_members_request,
            'members',
            { filter: { trade_id_one_of: [body.tradeId] } },
        );
        const existing = held[0];
        if (existing !== undefined) {
            const holder = text(existing['structure_id']);
            throw new HttpFailure(409, {
                code: 'conflict',
                message:
                    holder === id
                        ? 'The trade is already a leg of this structure.'
                        : 'The trade is a leg of another structure.',
            });
        }

        const members = await membersOf(session, id);
        const sequence =
            members.reduce(
                (highest, member) => Math.max(highest, Number(member['sequence_number'] ?? 0)),
                0,
            ) + 1;

        const written = await session.client.callAuthenticated(
            memberSubjects.put_structure_member_request,
            {
                change: {
                    write: {
                        trade_id: body.tradeId,
                        structure_id: id,
                        role: body.role,
                        sequence_number: sequence,
                    },
                    precondition: mustNotExist(),
                },
                intent: intent(),
            },
            z.looseObject({ result: resultSchema }),
        );
        if (written.result.outcome !== 'ok') {
            throw refusalOfLeg(written.result);
        }
        reply.status(201);
        return { tradeId: body.tradeId, role: body.role, sequenceNumber: sequence };
    });
}
