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
}

/** The structure a page is on, with the legs grouped into it. */
export interface StructurePageRecord {
    readonly id: string;
    /** The reference that addresses the structure, or empty when it has no identifier. */
    readonly reference: string;
    readonly kind: string;
    readonly template_code: string;
    readonly counterparty_id: string;
    readonly counterparty_name: string;
    readonly legs: readonly StructureLeg[];
    /** The roles a leg may take on this structure. */
    readonly roles: readonly string[];
}

const structurePageRecordSchema = z.object({
    structure: z.looseObject({
        id: z.string(),
        kind: z.string().default(''),
        template_code: z.string().default(''),
        counterparty_id: z.string().default(''),
        counterparty_name: z.string().default(''),
    }),
    reference: z.string().default(''),
    legs: z
        .array(
            z.looseObject({
                trade_id: z.string(),
                reference: z.string().default(''),
                role: z.string().default(''),
                sequence_number: z.coerce.number().int().default(0),
            }),
        )
        .default([]),
    roles: z.array(z.string()).default([]),
});

/** A trade the structure may take as a leg: one no structure has grouped yet. */
export interface LegCandidate {
    readonly id: string;
    readonly reference: string;
    readonly instrument_type: string;
}

export const structures = {
    /** One structure with its reference and its legs. */
    async one(id: string): Promise<StructurePageRecord> {
        const parsed = structurePageRecordSchema.parse(
            await request(`/api/structures/${encodeURIComponent(id)}`, { method: 'GET' }),
        );
        return {
            id: parsed.structure.id,
            reference: parsed.reference,
            kind: parsed.structure.kind,
            template_code: parsed.structure.template_code,
            counterparty_id: parsed.structure.counterparty_id,
            counterparty_name: parsed.structure.counterparty_name,
            legs: parsed.legs,
            roles: parsed.roles,
        };
    },

    /** The trades no structure has grouped, which are the ones this one may take. */
    async legCandidates(id: string): Promise<readonly LegCandidate[]> {
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
                await request(`/api/structures/${encodeURIComponent(id)}/leg-candidates`, {
                    method: 'GET',
                }),
            );
        return answer.rows;
    },

    /** Adds a trade to the structure, in the role the desk gives it. */
    async addLeg(id: string, tradeId: string, role: string): Promise<void> {
        await request(`/api/structures/${encodeURIComponent(id)}/legs`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ tradeId, role }),
        });
    },
};
