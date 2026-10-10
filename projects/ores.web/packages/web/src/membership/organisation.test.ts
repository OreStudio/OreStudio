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
import type { ReportingTreeNode, ReportingTreeParty } from '@ores/wire-protocol/browser';
import { inParty, partyForest, shapeOf } from './organisation.js';

function person(
    id: string,
    manager: string | null,
    parties: readonly string[],
    over: Partial<ReportingTreeNode> = {},
): ReportingTreeNode {
    return {
        accountId: id,
        username: id,
        fullName: id.toUpperCase(),
        jobTitle: '',
        imageId: null,
        reportsToAccountId: manager,
        reportsOutsideScope: false,
        depth: 0,
        directReports: 0,
        partyIds: [...parties],
        ...over,
    };
}

function party(id: string, parent: string | null = null): ReportingTreeParty {
    return { partyId: id, name: id.toUpperCase(), shortCode: id, parentPartyId: parent };
}

const PEOPLE = [
    person('ceo', null, ['holding']),
    person('ann', 'ceo', ['uk']),
    person('bob', 'ann', ['uk', 'us']),
    person('cy', 'ceo', ['us']),
    person('dee', null, []),
];
const PARTIES = [party('holding'), party('uk', 'holding'), party('us', 'holding')];
const ids = (nodes: readonly ReportingTreeNode[]): string[] => nodes.map((node) => node.accountId);

describe('inParty', () => {
    it('keeps everybody when no party is chosen', () => {
        expect(ids(inParty(PEOPLE, ''))).toEqual(['ceo', 'ann', 'bob', 'cy', 'dee']);
    });

    it('keeps the people who work in the party, including those who work in others too', () => {
        expect(ids(inParty(PEOPLE, 'us'))).toEqual(['bob', 'cy']);
    });
});

describe('shapeOf', () => {
    it('puts each person once under their manager', () => {
        const shape = shapeOf(PEOPLE);

        expect(ids(shape.roots)).toEqual(['ceo', 'dee']);
        expect(ids(shape.children.get('ceo') ?? [])).toEqual(['ann', 'cy']);
        expect(ids(shape.children.get('ann') ?? [])).toEqual(['bob']);
    });

    it('draws a person whose manager is left out as a root, not as a person under nobody', () => {
        const shape = shapeOf(inParty(PEOPLE, 'us'));

        // Bob's manager, Ann, works only in the uk, and Cy's manager is the CEO.
        expect(ids(shape.roots)).toEqual(['bob', 'cy']);
        expect(shape.children.size).toBe(0);
    });

    it('draws a person whose manager the reader may not see as a root', () => {
        const hidden = person('eve', null, ['uk'], { reportsOutsideScope: true });

        expect(ids(shapeOf([hidden]).roots)).toEqual(['eve']);
    });
});

describe('partyForest', () => {
    it('nests the parties by their parent and lists the people of each', () => {
        const forest = partyForest(PARTIES, PEOPLE);

        expect(forest.branches.map((branch) => branch.party.partyId)).toEqual(['holding']);
        const holding = forest.branches[0];
        expect(ids(holding?.members ?? [])).toEqual(['ceo']);
        expect(holding?.below.map((branch) => branch.party.partyId)).toEqual(['uk', 'us']);
    });

    it('lists a person who works in two parties under each of them', () => {
        const forest = partyForest(PARTIES, PEOPLE);
        const [uk, us] = forest.branches[0]?.below ?? [];

        expect(ids(uk?.members ?? [])).toEqual(['ANN'.toLowerCase(), 'bob']);
        expect(ids(us?.members ?? [])).toEqual(['bob', 'cy']);
    });

    it('lists the people who work in no party apart', () => {
        expect(ids(partyForest(PARTIES, PEOPLE).unaffiliated)).toEqual(['dee']);
    });

    it('narrows to one party and the parties below it', () => {
        const forest = partyForest(PARTIES, PEOPLE, 'holding');

        expect(forest.branches.map((branch) => branch.party.partyId)).toEqual(['holding']);
        expect(forest.branches[0]?.below).toHaveLength(2);
        expect(forest.unaffiliated).toEqual([]);
    });

    it('draws a party whose parent is not in the answer as a top party', () => {
        const forest = partyForest([party('uk', 'holding')], PEOPLE);

        expect(forest.branches.map((branch) => branch.party.partyId)).toEqual(['uk']);
    });
});
