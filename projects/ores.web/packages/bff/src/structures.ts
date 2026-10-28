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

import { randomUUID } from 'node:crypto';
import type { FastifyInstance, FastifyRequest } from 'fastify';
import { z } from 'zod';
import { subjects as counterpartySubjects } from '@ores/wire-protocol/generated/refdata/protocol/counterparty_protocol';
import { subjects as partySubjects } from '@ores/wire-protocol/generated/refdata/protocol/party_protocol';
import { subjects as identifierSubjects } from '@ores/wire-protocol/generated/trading/protocol/structure_identifier_protocol';
import { subjects as memberSubjects } from '@ores/wire-protocol/generated/trading/protocol/structure_member_protocol';
import { subjects as bondSubject } from '@ores/wire-protocol/generated/trading/protocol/bond_instrument_protocol';
import { subjects as structureKindSubjects } from '@ores/wire-protocol/generated/trading/protocol/structure_kind_protocol';
import { subjects as structureSubjects } from '@ores/wire-protocol/generated/trading/protocol/structure_protocol';
import { subjects as summarySubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_summary_protocol';
import { subjects as tradeIdentifierSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_identifier_protocol';
import { subjects as tradeTypeSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_type_protocol';
import { subjects as tradeSubjects } from '@ores/wire-protocol/generated/trading/protocol/trade_protocol';
import { HttpFailure, invalidRequest, notFound, notPermitted } from './errors.js';
import { input, readAll, resultSchema, text } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';

/** How many trades the add-a-leg picker offers, newest first. */
const CANDIDATE_PAGE = 200;

/** The roles a leg may take when the structure names no template of its own. */
const FREE_ROLES = ['leg'] as const;

/** The parent a root states, which the store holds as the nil uuid. */
const NO_PARENT = '00000000-0000-0000-0000-000000000000';

/**
 * The key a structure is addressed by: its own id, or the reference the desk
 * gave it.
 *
 * A person knows a deal by the reference and not by a uuid, so the address the
 * screen shows is the reference, and the page resolves it. Both forms are
 * accepted, because a client that already holds the id should not have to look
 * the reference up.
 */
const structureKeySchema = z.object({
    key: z.string().trim().min(1).max(64),
});

const uuidPattern = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;

const listQuerySchema = z.object({
    offset: z.coerce.number().int().min(0).default(0),
    limit: z.coerce.number().int().min(1).max(200).default(25),
    search: z.string().trim().max(256).default(''),
    kind: z.string().trim().max(64).default(''),
});

const unlinkParamsSchema = z.object({ tradeId: z.uuid() });

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

/** Why a refused leg was refused, in the status the screen acts on. */
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

/** The structures a tenant may see. */
async function allStructures(session: LiveSession): Promise<readonly Record<string, unknown>[]> {
    return readAll(session, structureSubjects.list_structures_request, 'structures', {
        filter: { id_one_of: null },
    });
}

/** The members a tenant may see, which is every leg of every deal. */
async function allMembers(session: LiveSession): Promise<readonly Record<string, unknown>[]> {
    return readAll(session, memberSubjects.list_structure_members_request, 'members', {
        filter: { trade_id_one_of: null },
    });
}

/** The reference each identifier's owner carries, keyed by the owner's id. */
async function referencesOf(
    session: LiveSession,
    owner: 'structure' | 'trade',
    ownerIds: readonly string[],
): Promise<ReadonlyMap<string, string>> {
    const references = new Map<string, string>();
    if (ownerIds.length === 0) {
        return references;
    }
    const read =
        owner === 'structure'
            ? await readAll(
                  session,
                  identifierSubjects.list_structure_identifiers_request,
                  'identifiers',
                  { filter: { structure_id: null, structure_id_one_of: [...ownerIds] } },
              )
            : await readAll(
                  session,
                  tradeIdentifierSubjects.list_trade_identifiers_request,
                  'identifiers',
                  { filter: { trade_id: null, trade_id_one_of: [...ownerIds] } },
              );
    const column = owner === 'structure' ? 'structure_id' : 'trade_id';
    for (const entry of read) {
        const id = text(entry[column]);
        const value = text(entry['id_value']);
        if (id !== '' && value !== '' && !references.has(id)) {
            references.set(id, value);
        }
    }
    return references;
}

/**
 * The structure a key names, or a refusal.
 *
 * A reference is resolved against the identifiers the tenant holds. The read is
 * made whole rather than by value because the store answers identifiers by
 * structure and not by value; a tenant holds as many references as it has
 * structures, which is what keeps that affordable.
 */
async function resolveStructure(
    session: LiveSession,
    key: string,
): Promise<Record<string, unknown>> {
    let structureId = key;
    if (!uuidPattern.test(key)) {
        const identifiers = await readAll(
            session,
            identifierSubjects.list_structure_identifiers_request,
            'identifiers',
            { filter: { structure_id: null, structure_id_one_of: null } },
        );
        const match = identifiers.find(
            (entry) => text(entry['id_value']).toLowerCase() === key.toLowerCase(),
        );
        structureId = text(match?.['structure_id']);
        if (structureId === '') {
            throw notFound('No structure has that reference.');
        }
    }

    const found = await readAll(
        session,
        structureSubjects.list_structures_request,
        'structures',
        { filter: { id_one_of: [structureId] } },
    );
    const structure = found[0];
    if (structure === undefined) {
        throw notFound('No structure has that reference.');
    }
    return structure;
}

/** The trades a set of ids names, with the facts a tree row and a leg states. */
async function tradesByIds(
    session: LiveSession,
    tradeIds: readonly string[],
): Promise<ReadonlyMap<string, Record<string, unknown>>> {
    if (tradeIds.length === 0) {
        return new Map();
    }
    const trades = await readAll(session, tradeSubjects.list_trades_request, 'trades', {
        filter: {
            counterparty_id: null,
            trade_type: null,
            counterparty_scope: null,
            id_one_of: [...tradeIds],
            counterparty_id_one_of: null,
            trade_type_one_of: null,
            counterparty_scope_one_of: null,
            search: null,
        },
    });
    return new Map(trades.map((entry) => [text(entry['id']), entry]));
}

interface TreeNode {
    id: string;
    reference: string;
    /** A structure's kind, or a trade's type on a leaf. */
    kind: string;
    template_code: string;
    /** The role this node takes under its parent. */
    role: string;
    sequence_number: number;
    /** The product's name on a leaf, which is the type's description. */
    product: string;
    notional: string;
    currency: string;
    external_version: number | null;
    children: TreeNode[];
}

/**
 * The tree a structure belongs to.
 *
 * The store holds a structure's parent as an id, so the shape is built here: the
 * tenant's structures and members are read once, the requested structure's root
 * is found by walking up, and the tree is assembled from that root down. The
 * model nests one level at most, and a deeper tree would draw here without a
 * word changing.
 *
 * A structure states its children as the structures naming it as parent, and its
 * legs as the trades booked into it: the two are held in different places, so
 * both are looked up rather than assumed.
 */
function assembleTree(
    structureId: string,
    structures: readonly Record<string, unknown>[],
    members: readonly Record<string, unknown>[],
    structureReferences: ReadonlyMap<string, string>,
    tradeReferences: ReadonlyMap<string, string>,
    trades: ReadonlyMap<string, Record<string, unknown>>,
    products: ReadonlyMap<string, string>,
    notionals: ReadonlyMap<string, { notional: string; currency: string }>,
): TreeNode | null {
    const byId = new Map(structures.map((entry) => [text(entry['id']), entry]));
    if (!byId.has(structureId)) {
        return null;
    }

    let rootId = structureId;
    for (let step = 0; step < structures.length; step += 1) {
        const parent = text(byId.get(rootId)?.['parent_structure_id']);
        if (parent === '' || parent === NO_PARENT || !byId.has(parent)) {
            break;
        }
        rootId = parent;
    }

    const legsOf = (id: string): readonly Record<string, unknown>[] =>
        members.filter((member) => text(member['structure_id']) === id);
    const childrenOf = (id: string): readonly string[] =>
        structures
            .filter((entry) => text(entry['parent_structure_id']) === id)
            .map((entry) => text(entry['id']))
            .filter((childId) => childId !== '' && childId !== NO_PARENT);

    const assemble = (id: string, role: string, sequence: number): TreeNode => {
        const row = byId.get(id);
        return {
            id,
            reference: structureReferences.get(id) ?? '',
            kind: text(row?.['kind']),
            template_code: text(row?.['template_code']),
            role,
            sequence_number: sequence,
            product: '',
            notional: '',
            currency: '',
            external_version: null,
            children: [
                ...childrenOf(id).map((childId) => assemble(childId, 'structure', 0)),
                ...legsOf(id).map((member) => {
                    const tradeId = text(member['trade_id']);
                    const trade = trades.get(tradeId);
                    const type = text(trade?.['trade_type']);
                    return {
                        id: tradeId,
                        reference: tradeReferences.get(tradeId) ?? '',
                        kind: type,
                        template_code: '',
                        role: text(member['role']),
                        sequence_number: Number(member['sequence_number'] ?? 0),
                        product: products.get(type) ?? type,
                        notional: notionals.get(tradeId)?.notional ?? '',
                        currency: notionals.get(tradeId)?.currency ?? '',
                        external_version:
                            trade === undefined ? null : Number(trade['external_version'] ?? 0),
                        children: [],
                    };
                }),
            ],
        };
    };

    return assemble(rootId, '', 0);
}


const groupBodySchema = z.object({
    kind: z.string().trim().min(1).max(64),
    tradeIds: z.array(z.uuid()).min(2).max(50),
});

/** The trades no structure has grouped yet, with what a picker shows of them. */
async function ungroupedTrades(
    session: LiveSession,
    limit: number,
): Promise<readonly Record<string, unknown>[]> {
    const taken = new Set((await allMembers(session)).map((entry) => text(entry['trade_id'])));
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
            limit,
        },
    );
    const free = summaries.filter(
        (entry) => text(entry['row_kind']) === 'trade' && !taken.has(text(entry['id'])),
    );
    const references = await referencesOf(
        session,
        'trade',
        free.map((entry) => text(entry['id'])),
    );
    // The counterparty is what the grouping rule is about, so the picker names
    // it: a person picks a pair that shares one, and the page says so first.
    const counterpartyIds = [
        ...new Set(free.map((entry) => text(entry['counterparty_id'])).filter((id) => id !== '')),
    ];
    const counterparties =
        counterpartyIds.length === 0
            ? []
            : await readAll(
                  session,
                  counterpartySubjects.list_counterparties_request,
                  'counterparties',
                  { filter: { id_one_of: counterpartyIds } },
              );
    const counterpartyOf = new Map(
        counterparties.map((entry) => [text(entry['id']), text(entry['full_name'])]),
    );
    return free.map((entry) => ({
        id: text(entry['id']),
        reference: references.get(text(entry['id'])) ?? '',
        instrument_type: text(entry['instrument_type']),
        counterparty_id: text(entry['counterparty_id']),
        counterparty_name:
            counterpartyOf.get(text(entry['counterparty_id'])) ??
            text(entry['counterparty_id']),
        trade_date: text(entry['trade_date']),
    }));
}


/** How a kind confirms and prices, which the screen states beside the kind. */
function verdictsOf(kind: string): { confirms: string; prices: string } {
    const asOne = kind !== 'Package';
    return {
        confirms: asOne
            ? 'Confirmed as one document'
            : 'Each leg confirms on its own',
        prices: asOne ? 'Priced as a whole' : 'Priced leg by leg',
    };
}

/** The name each trade type is known by, keyed by its code. */
async function productNames(session: LiveSession): Promise<ReadonlyMap<string, string>> {
    const types = await readAll(session, tradeTypeSubjects.list_trade_types_request, 'trade_types', {
        filter: { code: null, code_one_of: null },
    });
    return new Map(
        types.map((entry) => [text(entry['code']), text(entry['description'])] as const),
    );
}

/** The state each trade is in, keyed by the trade's id. */
async function statesOf(
    session: LiveSession,
    tradeIds: readonly string[],
): Promise<ReadonlyMap<string, string>> {
    const states = new Map<string, string>();
    if (tradeIds.length === 0) {
        return states;
    }
    const summaries = await readAll(
        session,
        summarySubjects.list_trade_summaries_request,
        'trade_summaries',
        { filter: { id_one_of: [...tradeIds] } },
    );
    for (const entry of summaries) {
        const id = text(entry['id']);
        if (id !== '') {
            states.set(id, text(entry['status']));
        }
    }
    return states;
}

/**
 * The notional each trade holds, keyed by the trade's id.
 *
 * Only the bond instrument is read: the other families are booked into their own
 * tables and no reader in this service names them yet, so a leg of another
 * family states no notional rather than a wrong one. Reading the rest is the
 * same shape repeated per family, and it belongs with the instrument screens.
 */
async function notionalsOf(
    session: LiveSession,
    tradeIds: readonly string[],
): Promise<ReadonlyMap<string, { notional: string; currency: string }>> {
    const notionals = new Map<string, { notional: string; currency: string }>();
    if (tradeIds.length === 0) {
        return notionals;
    }
    const bonds = await readAll(session, bondSubject.list_bond_instruments_request, 'bond_instruments', {
        filter: { trade_id: null, trade_id_one_of: [...tradeIds] },
    });
    for (const entry of bonds) {
        const id = text(entry['trade_id']);
        if (id !== '') {
            notionals.set(id, {
                notional: text(entry['notional']),
                currency: text(entry['currency']),
            });
        }
    }
    return notionals;
}


/**
 * The answer to a read the page can do without, or nothing.
 *
 * The product's name, the trade's state, its notional and its party are
 * enrichment: a caller who may read a deal and not the catalogues still gets the
 * deal, and the page states nothing where it could not read. A refusal of a read
 * the page is about still refuses loudly.
 */
async function optional<T>(read: () => Promise<T>, fallback: T): Promise<T> {
    try {
        return await read();
    } catch {
        return fallback;
    }
}

export function registerStructureRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {

    /*
     * FIXME: the tree a structure sits in is read whole, because the store
     * answers structures by id and members by trade. A tenant with many thousand
     * structures would want a read by parent, and this is the seam it belongs at.
     */
    server.get('/api/structure-kinds', async (request) => {
        const session = requireSession(request);
        const kinds = await readAll(
            session,
            structureKindSubjects.list_structure_kinds_request,
            'structure_kinds',
            { filter: { code_one_of: null } },
        );
        return {
            rows: kinds.map((entry) => ({
                code: text(entry['code']),
                confirms_as_whole: entry['confirms_as_whole'] === true,
            })),
        };
    });

    server.get('/api/ungrouped-trades', async (request) => {
        const session = requireSession(request);
        return { rows: await ungroupedTrades(session, CANDIDATE_PAGE) };
    });

    server.post('/api/structures', async (request, reply) => {
        const session = requireSession(request);
        const body = input(groupBodySchema, request.body);
        const partyId = session.party?.id;
        if (partyId === undefined) {
            throw invalidRequest('A grouping is made by a party, and this session works for none.');
        }

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
                    row_kind: null,
                    row_kind_one_of: null,
                },
            },
        );
        const chosen = body.tradeIds.map((id) => summaries.find((entry) => text(entry['id']) === id));
        if (chosen.some((entry) => entry === undefined)) {
            throw invalidRequest('A trade being grouped is not one this session can read.');
        }
        const counterparties = new Set(
            chosen.map((entry) => text(entry?.['counterparty_id'])).filter((id) => id !== ''),
        );
        if (counterparties.size !== 1) {
            throw invalidRequest(
                'Every trade in a structure shares one counterparty, and these do not.',
            );
        }

        const structureId = randomUUID();
        const written = await session.client.callAuthenticated(
            structureSubjects.put_structure_request,
            {
                change: {
                    write: {
                        id: structureId,
                        counterparty_id: [...counterparties][0] ?? '',
                        kind: body.kind,
                        template_code: '',
                        parent_structure_id: NO_PARENT,
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

        /*
         * The legs are added in the order the desk chose them, so the sequence a
         * person reads follows the order they picked. A leg that fails is
         * reported, and the structure stays: a half-grouped deal is visible and
         * can be finished, which is better than a write that silently did nothing.
         */
        let sequence = 0;
        for (const tradeId of body.tradeIds) {
            sequence += 1;
            const member = await session.client.callAuthenticated(
                memberSubjects.put_structure_member_request,
                {
                    change: {
                        write: {
                            trade_id: tradeId,
                            structure_id: structureId,
                            role: FREE_ROLES[0],
                            sequence_number: sequence,
                        },
                        precondition: mustNotExist(),
                    },
                    intent: intent(),
                },
                z.looseObject({ result: resultSchema }),
            );
            if (member.result.outcome !== 'ok') {
                throw refusalOfLeg(member.result);
            }
        }

        const references = await referencesOf(session, 'structure', [structureId]);
        reply.status(201);
        return { id: structureId, reference: references.get(structureId) ?? '' };
    });


    /**
     * The structures the tenant holds, as the list screen draws them.
     *
     * The list is a tree flattened for reading: a root row, then its children
     * indented under it. Each row states what it holds — the trades inside it
     * and the structures under it — because that is what tells a person whether
     * they are looking at a deal or a container.
     */

    /**
     * The structures the tenant holds, as the list screen draws them.
     *
     * The list is a tree flattened for reading: a root row, then its children
     * indented under it. Each row states what it holds — the trades inside it and
     * the structures under it — because that is what tells a person whether they
     * are looking at a deal or a container.
     */
    server.get('/api/structures', async (request) => {
        const session = requireSession(request);
        const query = input(listQuerySchema, request.query);

        const structures = await allStructures(session);
        const [members, references] = await Promise.all([
            allMembers(session),
            referencesOf(
                session,
                'structure',
                structures.map((entry) => text(entry['id'])).filter((id) => id !== ''),
            ),
        ]);
        const counterpartyIds = [
            ...new Set(
                structures.map((entry) => text(entry['counterparty_id'])).filter((id) => id !== ''),
            ),
        ];
        const counterparties =
            counterpartyIds.length === 0
                ? []
                : await readAll(
                      session,
                      counterpartySubjects.list_counterparties_request,
                      'counterparties',
                      { filter: { id_one_of: counterpartyIds } },
                  );
        const counterpartyOf = new Map(
            counterparties.map((entry) => [text(entry['id']), text(entry['full_name'])]),
        );

        const byId = new Map(structures.map((entry) => [text(entry['id']), entry] as const));
        const parentOf = (id: string): string => {
            const parent = text(byId.get(id)?.['parent_structure_id']);
            return parent === NO_PARENT ? '' : parent;
        };
        const rootOf = (id: string): string => {
            let current = id;
            for (let step = 0; step < structures.length; step += 1) {
                const parent = parentOf(current);
                if (parent === '' || !byId.has(parent)) {
                    return current;
                }
                current = parent;
            }
            return current;
        };
        const depthOf = (id: string): number => {
            let current = parentOf(id);
            let depth = 0;
            while (current !== '' && byId.has(current) && depth < structures.length) {
                depth += 1;
                current = parentOf(current);
            }
            return depth;
        };

        const legsOf = (id: string): number =>
            members.filter((entry) => text(entry['structure_id']) === id).length;
        const innerOf = (id: string): number =>
            structures.filter((entry) => parentOf(text(entry['id'])) === id).length;

        const rows = structures.map((entry) => {
            const id = text(entry['id']);
            const kind = text(entry['kind']);
            return {
                id,
                reference: references.get(id) ?? '',
                kind,
                template_code: text(entry['template_code']),
                counterparty_id: text(entry['counterparty_id']),
                counterparty_name: counterpartyOf.get(text(entry['counterparty_id'])) ?? '',
                legs: legsOf(id),
                inner_structures: innerOf(id),
                ...verdictsOf(kind),
                parent_id: parentOf(id),
                depth: depthOf(id),
            };
        });

        /*
         * A search keeps the rows it matches, everything under them and the
         * path above them: a tree with a hole in it is not readable, so a match
         * brings its root and its children with it.
         */
        const needle = query.search.toLowerCase();
        const matches = (row: (typeof rows)[number]): boolean => {
            const kindOk = query.kind === '' || row.kind === query.kind;
            const textOk =
                needle === '' ||
                [row.reference, row.kind, row.template_code, row.counterparty_name]
                    .join(' ')
                    .toLowerCase()
                    .includes(needle);
            return kindOk && textOk;
        };

        const visible = new Set<string>();
        if (needle === '' && query.kind === '') {
            for (const row of rows) {
                visible.add(row.id);
            }
        } else {
            const childrenOf = new Map<string, string[]>();
            for (const row of rows) {
                childrenOf.set(row.parent_id, [...(childrenOf.get(row.parent_id) ?? []), row.id]);
            }
            const addUnder = (id: string): void => {
                visible.add(id);
                for (const child of childrenOf.get(id) ?? []) {
                    addUnder(child);
                }
            };
            for (const row of rows) {
                if (!matches(row)) {
                    continue;
                }
                addUnder(row.id);
                let above = row.parent_id;
                while (above !== '' && byId.has(above)) {
                    visible.add(above);
                    above = parentOf(above);
                }
            }
        }

        const ordered = rows
            .filter((row) => visible.has(row.id))
            .sort((left, right) => {
                const leftRoot = rootOf(left.id);
                const rightRoot = rootOf(right.id);
                if (leftRoot !== rightRoot) {
                    return (references.get(leftRoot) ?? leftRoot).localeCompare(
                        references.get(rightRoot) ?? rightRoot,
                    );
                }
                return left.depth - right.depth;
            });

        const kinds = [...new Set(rows.map((row) => row.kind))].filter((kind) => kind !== '');
        return {
            rows: ordered.slice(query.offset, query.offset + query.limit),
            total: ordered.length,
            kinds: kinds.sort(),
        };
    });

    server.get('/api/structures/:key', async (request) => {
        const session = requireSession(request);
        const { key } = input(structureKeySchema, request.params);
        const structure = await resolveStructure(session, key);
        const structureId = text(structure['id']);

        const [structures, members] = await Promise.all([
            allStructures(session),
            allMembers(session),
        ]);
        const mine = members.filter((entry) => text(entry['structure_id']) === structureId);
        const allTradeIds = [
            ...new Set(
                members.map((entry) => text(entry['trade_id'])).filter((id) => id !== ''),
            ),
        ];

        const allIds = [
            ...new Set([
                ...allTradeIds,
                ...structures.map((entry) => text(entry['parent_structure_id'])),
            ]),
        ];
        const [structureReferences, tradeReferences, trades, history, products, states, notionals] =
            await Promise.all([
            referencesOf(
                session,
                'structure',
                structures.map((entry) => text(entry['id'])).filter((id) => id !== ''),
            ),
            referencesOf(session, 'trade', allTradeIds),
            tradesByIds(session, allTradeIds),
            readAll(session, structureSubjects.list_structure_versions_request, 'versions', {
                key: { id: structureId },
                order: { field: 'version', descending: true },
            }),
            optional(() => productNames(session), new Map<string, string>()),
            optional(() => statesOf(session, allTradeIds), new Map<string, string>()),
            optional(
                () => notionalsOf(session, allTradeIds),
                new Map<string, { notional: string; currency: string }>(),
            ),
        ]);
        void allIds;

        const legs = mine
            .map((member) => {
                const tradeId = text(member['trade_id']);
                const trade = trades.get(tradeId);
                const type = text(trade?.['trade_type']);
                return {
                    trade_id: tradeId,
                    reference: tradeReferences.get(tradeId) ?? '',
                    role: text(member['role']),
                    sequence_number: Number(member['sequence_number'] ?? 0),
                    kind: type,
                    product: products.get(type) ?? type,
                    state: states.get(tradeId) ?? '',
                    notional: notionals.get(tradeId)?.notional ?? '',
                    currency: notionals.get(tradeId)?.currency ?? '',
                    external_version:
                        trade === undefined ? null : Number(trade['external_version'] ?? 0),
                };
            })
            .sort((left, right) => left.sequence_number - right.sequence_number);

        /*
         * A structure holds no party of its own: every leg is booked as the same
         * party, which the database enforces, so the party is read from the legs
         * and stated once on the page.
         */
        const legPartyIds = [
            ...new Set(
                legs
                    .map((leg) => text(trades.get(leg.trade_id)?.['party_id']))
                    .filter((id) => id !== ''),
            ),
        ];
        const parties =
            legPartyIds.length === 0
                ? []
                : await optional(
                      () =>
                          readAll(session, partySubjects.list_parties_request, 'parties', {
                              filter: { id_one_of: legPartyIds, search: null },
                          }),
                      [] as readonly Record<string, unknown>[],
                  );
        const party = {
            id: legPartyIds[0] ?? '',
            name: text(parties[0]?.['full_name']),
            shared: legPartyIds.length <= 1,
        };

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

        const parentId = text(structure['parent_structure_id']);
        return {
            structure: {
                ...structure,
                counterparty_name: text(counterparties[0]?.['full_name']) || counterpartyId,
            },
            reference: structureReferences.get(structureId) ?? '',
            party,
            ...verdictsOf(text(structure['kind'])),
            parent_id: parentId === NO_PARENT ? '' : parentId,
            tree: assembleTree(
                structureId,
                structures,
                members,
                structureReferences,
                tradeReferences,
                trades,
                products,
                notionals,
            ),
            legs,
            roles: [...FREE_ROLES],
            history,
        };
    });


    /**
     * Taking a trade out of a structure.
     *
     * The leg is removed, not the trade: the trade stays booked and becomes
     * groupable again, which is what "unlink" means to the desk.
     */
    server.delete('/api/structures/:key/legs/:tradeId', async (request, reply) => {
        const session = requireSession(request);
        const { key } = input(structureKeySchema, request.params);
        const { tradeId } = input(unlinkParamsSchema, request.params);
        const structure = await resolveStructure(session, key);
        const structureId = text(structure['id']);

        const written = await session.client.callAuthenticated(
            memberSubjects.delete_structure_member_request,
            {
                removal: {
                    key: { trade_id: tradeId },
                    precondition: { kind: 'must_match_version', version: null },
                },
                intent: {
                    reason_code: 'system.amend_record',
                    commentary: 'Taken out of the structure from its page.',
                },
            },
            z.looseObject({ result: resultSchema }),
        );
        if (written.result.outcome !== 'ok') {
            throw refusalOfLeg(written.result);
        }
        reply.status(204);
        return null;
    });

    server.get('/api/structures/:key/leg-candidates', async (request) => {
        const session = requireSession(request);
        const { key } = input(structureKeySchema, request.params);
        const structure = await resolveStructure(session, key);

        const taken = new Set((await allMembers(session)).map((entry) => text(entry['trade_id'])));
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
        const free = summaries.filter(
            (entry) => text(entry['row_kind']) === 'trade' && !taken.has(text(entry['id'])),
        );
        const references = await referencesOf(
            session,
            'trade',
            free.map((entry) => text(entry['id'])),
        );
        return {
            structure_id: text(structure['id']),
            rows: free.map((entry) => ({
                id: text(entry['id']),
                reference: references.get(text(entry['id'])) ?? '',
                instrument_type: text(entry['instrument_type']),
                trade_date: text(entry['trade_date']),
            })),
        };
    });

    server.post('/api/structures/:key/legs', async (request, reply) => {
        const session = requireSession(request);
        const { key } = input(structureKeySchema, request.params);
        const body = input(legBodySchema, request.body);
        const structure = await resolveStructure(session, key);
        const id = text(structure['id']);

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

        const members = (await allMembers(session)).filter(
            (entry) => text(entry['structure_id']) === id,
        );
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
