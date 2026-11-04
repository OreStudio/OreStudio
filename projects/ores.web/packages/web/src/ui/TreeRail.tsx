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

import { useMemo, type ReactNode } from 'react';

/** A node of a rail: where it hangs, what it is called and how many rows sit under it. */
export interface RailNode {
    readonly id: string;
    /** The node it hangs under; the root has the id '' and no parent. */
    readonly parent: string;
    readonly label: string;
    readonly count: number;
}

function childrenOf<Node extends RailNode>(
    nodes: readonly Node[],
): ReadonlyMap<string, readonly Node[]> {
    const children = new Map<string, Node[]>();
    for (const node of nodes) {
        if (node.id === '') {
            continue;
        }
        children.set(node.parent, [...(children.get(node.parent) ?? []), node]);
    }
    return children;
}

function RailItem<Node extends RailNode>({
    node,
    depth,
    selected,
    onSelect,
    marker,
}: {
    readonly node: Node;
    readonly depth: number;
    readonly selected: string;
    readonly onSelect: (id: string) => void;
    readonly marker?: ((node: Node) => ReactNode) | undefined;
}): ReactNode {
    return (
        <button
            type="button"
            onClick={() => {
                onSelect(node.id === selected ? '' : node.id);
            }}
            aria-pressed={node.id === selected}
            className={
                node.id === selected
                    ? 'flex w-full items-center justify-between gap-2 rounded px-2 py-1 text-left text-accent-bright bg-accent/10'
                    : 'flex w-full items-center justify-between gap-2 rounded px-2 py-1 text-left hover:bg-surface-raised'
            }
            style={{ paddingLeft: `${0.5 + depth * 0.9}rem` }}
        >
            <span className="flex min-w-0 items-center gap-2">
                {marker?.(node)}
                <span className="truncate text-sm">{node.label}</span>
            </span>
            <span className="shrink-0 text-xs tabular-nums text-ink-muted">{node.count}</span>
        </button>
    );
}

/**
 * A tree of the places rows sit, each with the number of rows under it, that
 * narrows the list beside it to the node chosen. Choosing the chosen node again
 * clears the choice.
 *
 * A node with nothing under it is still drawn: an empty place is a fact, not a
 * row to hide.
 */
export function TreeRail<Node extends RailNode>({
    nodes,
    selected,
    onSelect,
    label,
    marker,
}: {
    readonly nodes: readonly Node[];
    readonly selected: string;
    readonly onSelect: (id: string) => void;
    readonly label: string;
    /** What a node shows before its label, such as a colour. */
    readonly marker?: (node: Node) => ReactNode;
}): ReactNode {
    const children = useMemo(() => childrenOf(nodes), [nodes]);
    const root = nodes.find((node) => node.id === '');
    const walk = (parent: string, depth: number): ReactNode[] =>
        (children.get(parent) ?? []).flatMap((node) => [
            <RailItem
                key={node.id}
                node={node}
                depth={depth}
                selected={selected}
                onSelect={onSelect}
                marker={marker}
            />,
            ...walk(node.id, depth + 1),
        ]);
    return (
        <nav aria-label={label} className="mt-2 flex flex-col gap-0.5">
            {root === undefined ? null : (
                <RailItem
                    node={root}
                    depth={0}
                    selected={selected}
                    onSelect={onSelect}
                    marker={marker}
                />
            )}
            {walk('', 1)}
        </nav>
    );
}
