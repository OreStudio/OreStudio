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

import type { ReportingTreeNode, ReportingTreeParty } from '@ores/wire-protocol/browser';

/**
 * The shapes the Hierarchy screen draws from one tree read.
 *
 * The read is a forest of people, each with one manager and any number of
 * parties. Two lenses draw it: the reporting line, where each person appears
 * once under their manager, and the party, where each party lists the people who
 * work in it and a person in several parties appears in each. The parties nest
 * by their own parent, which is a separate hierarchy from the reporting line.
 */

/** The people who work in a party, or everybody when none is chosen. */
export function inParty(
    nodes: readonly ReportingTreeNode[],
    party: string,
): readonly ReportingTreeNode[] {
    return party === '' ? nodes : nodes.filter((node) => node.partyIds.includes(party));
}

/** A reporting forest: who is at the top, and who reports to whom. */
export interface Shape {
    readonly roots: readonly ReportingTreeNode[];
    readonly children: ReadonlyMap<string, ReportingTreeNode[]>;
}

/**
 * The reporting forest of the people given.
 *
 * A person is a root of it when their manager is not among the people given.
 * That is true of a real root, of a person whose manager the reader may not
 * see, and of a person whose manager a party filter has left out; the first is
 * the whole story and the others are marked or filtered, never invented.
 */
export function shapeOf(nodes: readonly ReportingTreeNode[]): Shape {
    const present = new Set(nodes.map((node) => node.accountId));
    const children = new Map<string, ReportingTreeNode[]>();
    const roots: ReportingTreeNode[] = [];
    for (const node of nodes) {
        const manager = node.reportsToAccountId;
        if (manager === null || !present.has(manager)) {
            roots.push(node);
            continue;
        }
        const siblings = children.get(manager) ?? [];
        siblings.push(node);
        children.set(manager, siblings);
    }
    return { roots, children };
}

/** One party in the party lens: the people who work in it and the parties below it. */
export interface PartyBranch {
    readonly party: ReportingTreeParty;
    readonly members: readonly ReportingTreeNode[];
    readonly below: readonly PartyBranch[];
}

/** The party lens: parties nested by their parent, and the people who work in none of them. */
export interface PartyForest {
    readonly branches: readonly PartyBranch[];
    readonly unaffiliated: readonly ReportingTreeNode[];
}

const byName = (a: ReportingTreeNode, b: ReportingTreeNode): number =>
    (a.fullName === '' ? a.username : a.fullName).localeCompare(
        b.fullName === '' ? b.username : b.fullName,
    );

/** A party as people read it: its name, or its short code, or its id. */
export function partyLabel(party: ReportingTreeParty): string {
    return party.name !== ''
        ? party.name
        : party.shortCode !== ''
          ? party.shortCode
          : party.partyId;
}

/**
 * The people by party, with the parties nested by their parent.
 *
 * `party` narrows it to one party and the parties below it, which is what
 * choosing a party in the filter means for a group. A party's parent counts
 * only when that parent is in the answer, so a reader is never drawn a branch
 * under a party they cannot see.
 */
export function partyForest(
    parties: readonly ReportingTreeParty[],
    nodes: readonly ReportingTreeNode[],
    party = '',
): PartyForest {
    const known = new Set(parties.map((entry) => entry.partyId));
    const belowOf = new Map<string, ReportingTreeParty[]>();
    const tops: ReportingTreeParty[] = [];
    for (const entry of parties) {
        const parent = entry.parentPartyId;
        if (parent === null || !known.has(parent)) {
            tops.push(entry);
            continue;
        }
        const siblings = belowOf.get(parent) ?? [];
        siblings.push(entry);
        belowOf.set(parent, siblings);
    }
    const sortParties = (entries: readonly ReportingTreeParty[]) =>
        [...entries].sort((a, b) => partyLabel(a).localeCompare(partyLabel(b)));
    const branch = (entry: ReportingTreeParty): PartyBranch => ({
        party: entry,
        members: nodes.filter((node) => node.partyIds.includes(entry.partyId)).sort(byName),
        below: sortParties(belowOf.get(entry.partyId) ?? []).map(branch),
    });
    const chosen = party === '' ? tops : parties.filter((entry) => entry.partyId === party);
    return {
        branches: sortParties(chosen).map(branch),
        unaffiliated:
            party === '' ? nodes.filter((node) => node.partyIds.length === 0).sort(byName) : [],
    };
}
