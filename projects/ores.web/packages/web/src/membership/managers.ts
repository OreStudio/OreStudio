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

import type { ReportingTreeNode } from '@ores/wire-protocol/browser';

/** The people who may be this person's manager: everyone but their own branch. */
export function managerCandidates(
    nodes: readonly ReportingTreeNode[],
    selected: ReportingTreeNode,
): readonly ReportingTreeNode[] {
    const forbidden = new Set<string>([selected.accountId]);
    const childrenOf = (id: string): readonly ReportingTreeNode[] =>
        nodes.filter((node) => node.reportsToAccountId === id);
    const walk = (id: string): void => {
        for (const child of childrenOf(id)) {
            if (!forbidden.has(child.accountId)) {
                forbidden.add(child.accountId);
                walk(child.accountId);
            }
        }
    };
    walk(selected.accountId);
    return nodes.filter((node) => !forbidden.has(node.accountId));
}
