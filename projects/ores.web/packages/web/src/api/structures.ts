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

import { z } from 'zod';
import { request } from './transport.js';

/** One trade that is a leg of the structure, as the structure's page reads it. */
export interface StructureLeg {
    readonly trade_id: string;
    /** The reference the trade is known by, or empty when it has none. */
    readonly reference: string;
    readonly role: string;
    readonly sequence_number: number;
    /** The trade's type code, and the name that type is known by. */
    readonly kind: string;
    readonly product: string;
    /** The state the trade is in, empty when the read could not name it. */
    readonly state: string;
    readonly external_version: number | null;
}

/** One row of the structures list: a deal or the container above one. */
export interface StructureListRow {
    readonly id: string;
    readonly reference: string;
    readonly kind: string;
    readonly template_code: string;
    readonly counterparty_id: string;
    readonly counterparty_name: string;
    /** How many trades are inside it, and how many structures under it. */
    readonly legs: number;
    readonly inner_structures: number;
    readonly confirms: string;
    readonly prices: string;
    readonly parent_id: string;
    /** How deep the row sits, which is how far it is indented. */
    readonly depth: number;
}

/** One page of structures and the kinds the tenant holds. */
export interface StructureListPage {
    readonly rows: readonly StructureListRow[];
    readonly total: number;
    readonly kinds: readonly string[];
}

/**
 * A node of the tree the structure sits in.
 *
 * A structure states its children as the structures naming it as parent, and the
 * trades booked into it; the two arrive in one shape because the page draws them
 * as one tree.
 */
export interface StructureTreeNode {
    readonly id: string;
    readonly reference: string;
    readonly kind: string;
    readonly template_code: string;
    /** The role this node takes under its parent. */
    readonly role: string;
    readonly sequence_number: number;
    /** The product's name on a leaf. */
    readonly product: string;
    readonly external_version: number | null;
    readonly children: readonly StructureTreeNode[];
}

/** One version of the structure, newest first. */
export interface StructureVersion {
    readonly version: number;
    readonly modified_by: string;
    readonly recorded_at: string;
    readonly change_reason_code: string;
    readonly change_commentary: string;
}

/** The deal a page is on, with the tree it sits in and the legs it holds. */
export interface StructurePageRecord {
    readonly id: string;
    /** The reference that addresses the deal, or empty when it has no identifier. */
    readonly reference: string;
    readonly kind: string;
    readonly template_code: string;
    readonly counterparty_id: string;
    readonly counterparty_name: string;
    /** The structure one rung up, when this one is not the root of its tree. */
    readonly parent_id: string;
    /** The party every leg is booked as, and whether they all agree. */
    readonly party: { readonly id: string; readonly name: string; readonly shared: boolean };
    /** How the kind confirms and prices. */
    readonly confirms: string;
    readonly prices: string;
    readonly tree: StructureTreeNode | null;
    readonly legs: readonly StructureLeg[];
    /** The roles a leg may take on this deal. */
    readonly roles: readonly string[];
    readonly history: readonly StructureVersion[];
}

/** A trade the deal may take as a leg: one no structure has grouped yet. */
export interface LegCandidate {
    readonly id: string;
    readonly reference: string;
    readonly instrument_type: string;
}

const treeNodeSchema: z.ZodType<StructureTreeNode> = z.lazy(() =>
    z.object({
        id: z.string(),
        reference: z.string().default(''),
        kind: z.string().default(''),
        template_code: z.string().default(''),
        role: z.string().default(''),
        sequence_number: z.coerce.number().int().default(0),
        product: z.string().default(''),
        external_version: z.coerce.number().int().nullable().default(null),
        children: z.array(treeNodeSchema).default([]),
    }),
);

const structurePageRecordSchema = z.object({
    structure: z.looseObject({
        id: z.string(),
        kind: z.string().default(''),
        template_code: z.string().default(''),
        counterparty_id: z.string().default(''),
        counterparty_name: z.string().default(''),
    }),
    reference: z.string().default(''),
    parent_id: z.string().default(''),
    party: z
        .looseObject({
            id: z.string().default(''),
            name: z.string().default(''),
            shared: z.boolean().default(true),
        })
        .default({ id: '', name: '', shared: true }),
    confirms: z.string().default(''),
    prices: z.string().default(''),
    tree: treeNodeSchema.nullable().default(null),
    legs: z
        .array(
            z.looseObject({
                trade_id: z.string(),
                reference: z.string().default(''),
                role: z.string().default(''),
                sequence_number: z.coerce.number().int().default(0),
                kind: z.string().default(''),
                product: z.string().default(''),
                state: z.string().default(''),
                external_version: z.coerce.number().int().nullable().default(null),
            }),
        )
        .default([]),
    roles: z.array(z.string()).default([]),
    history: z
        .array(
            z.looseObject({
                version: z.coerce.number().int(),
                modified_by: z.string().default(''),
                recorded_at: z.string().default(''),
                change_reason_code: z.string().default(''),
                change_commentary: z.string().default(''),
            }),
        )
        .default([]),
});

/** A trade a grouping may take: one no structure holds yet. */
export interface UngroupedTrade {
    readonly id: string;
    readonly reference: string;
    readonly instrument_type: string;
    readonly counterparty_id: string;
    readonly counterparty_name: string;
}

/** A kind a grouping may choose, and how its legs confirm. */
export interface StructureKind {
    readonly code: string;
    readonly confirms_as_whole: boolean;
}

export const structures = {
    /**
     * One deal with its reference, the tree it sits in and its legs.
     *
     * The key is the reference the desk knows or the deal's id; the server
     * resolves either, so a link can carry the reference a person reads.
     */
    async one(key: string): Promise<StructurePageRecord> {
        const parsed = structurePageRecordSchema.parse(
            await request(`/api/structures/${encodeURIComponent(key)}`, { method: 'GET' }),
        );
        return {
            id: parsed.structure.id,
            reference: parsed.reference,
            kind: parsed.structure.kind,
            template_code: parsed.structure.template_code,
            counterparty_id: parsed.structure.counterparty_id,
            counterparty_name: parsed.structure.counterparty_name,
            parent_id: parsed.parent_id,
            party: parsed.party,
            confirms: parsed.confirms,
            prices: parsed.prices,
            tree: parsed.tree,
            legs: parsed.legs,
            roles: parsed.roles,
            history: parsed.history,
        };
    },

    /** One page of the structures the tenant holds, as the list screen draws it. */
    async list(query: {
        readonly offset: number;
        readonly limit: number;
        readonly search: string;
        readonly kind: string;
    }): Promise<StructureListPage> {
        const params = new URLSearchParams({
            offset: String(query.offset),
            limit: String(query.limit),
            search: query.search,
            kind: query.kind,
        });
        return z
            .object({
                rows: z.array(
                    z.looseObject({
                        id: z.string(),
                        reference: z.string().default(''),
                        kind: z.string().default(''),
                        template_code: z.string().default(''),
                        counterparty_id: z.string().default(''),
                        counterparty_name: z.string().default(''),
                        legs: z.coerce.number().int().default(0),
                        inner_structures: z.coerce.number().int().default(0),
                        confirms: z.string().default(''),
                        prices: z.string().default(''),
                        parent_id: z.string().default(''),
                        depth: z.coerce.number().int().default(0),
                    }),
                ),
                total: z.coerce.number().int().default(0),
                kinds: z.array(z.string()).default([]),
            })
            .parse(
                await request(`/api/structures?${params.toString()}`, { method: 'GET' }),
            ) as StructureListPage;
    },

    /** Takes a trade out of the structure: the trade stays, the leg goes. */
    async unlink(key: string, tradeId: string): Promise<void> {
        await request(
            `/api/structures/${encodeURIComponent(key)}/legs/${encodeURIComponent(tradeId)}`,
            { method: 'DELETE' },
        );
    },

    /** The trades no structure has grouped, which are the ones this one may take. */
    async legCandidates(key: string): Promise<readonly LegCandidate[]> {
        const answer = z
            .object({
                rows: z
                    .array(
                        z.looseObject({
                            id: z.string(),
                            reference: z.string().default(''),
                            instrument_type: z.string().default(''),
                        }),
                    )
                    .default([]),
            })
            .parse(
                await request(
                    `/api/structures/${encodeURIComponent(key)}/leg-candidates`,
                    { method: 'GET' },
                ),
            );
        return answer.rows;
    },

    /** The trades no structure has grouped yet. */
    async ungroupedTrades(): Promise<readonly UngroupedTrade[]> {
        const answer = z
            .object({
                rows: z
                    .array(
                        z.looseObject({
                            id: z.string(),
                            reference: z.string().default(''),
                            instrument_type: z.string().default(''),
                            counterparty_id: z.string().default(''),
                            counterparty_name: z.string().default(''),
                        }),
                    )
                    .default([]),
            })
            .parse(await request('/api/ungrouped-trades', { method: 'GET' }));
        return answer.rows;
    },

    /** The kinds a grouping may choose. */
    async kinds(): Promise<readonly StructureKind[]> {
        const answer = z
            .object({
                rows: z
                    .array(
                        z.looseObject({
                            code: z.string().default(''),
                            confirms_as_whole: z.boolean().default(false),
                        }),
                    )
                    .default([]),
            })
            .parse(await request('/api/structure-kinds', { method: 'GET' }));
        return answer.rows;
    },

    /** Groups the chosen trades into one structure of the kind stated. */
    async group(kind: string, tradeIds: readonly string[]): Promise<{ id: string; reference: string }> {
        return z
            .object({ id: z.string(), reference: z.string().default('') })
            .parse(
                await request('/api/structures', {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ kind, tradeIds }),
                }),
            );
    },

    /** Adds a trade to the deal, in the role the desk gives it. */
    async addLeg(key: string, tradeId: string, role: string): Promise<void> {
        await request(`/api/structures/${encodeURIComponent(key)}/legs`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ tradeId, role }),
        });
    },
};
