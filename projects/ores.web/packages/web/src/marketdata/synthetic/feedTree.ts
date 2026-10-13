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

import type { FeedFilter, FeedFolder, FolderFeed } from '../../api/client.js';
import { PATHS } from '../../shell/paths.js';
import { providersOf } from './providers.js';
import { totalsOf, type Totals } from './totals.js';

/** What a node of the tree stands for. */
export type TreeRole = 'provider' | 'collection' | 'asset_class' | 'instrument_type';

/**
 * A node of the feed tree. A provider is made from the collections that stand for
 * it; the rest are the folders of the hierarchy, whose children the service
 * counts when a node is opened.
 */
export interface TreeNode {
    /** The address of the node, kept in the URL: provider:bloomberg or folder:<id>. */
    readonly key: string;
    readonly role: TreeRole;
    /** The provider code for a provider node, else the folder's own name. */
    readonly name: string;
    readonly providerCode?: string;
    /** The folder, for a node that is one. */
    readonly folder?: FeedFolder;
    /** The collections a provider node stands for. */
    readonly collectionIds: readonly string[];
    readonly totals: Totals;
}

export const providerKey = (code: string): string => `provider:${code}`;
export const folderKey = (id: string): string => `folder:${id}`;

/** A key taken apart, or undefined when it is neither. */
export type ParsedKey =
    | { readonly kind: 'provider'; readonly code: string }
    | { readonly kind: 'folder'; readonly id: string };

export function parseKey(key: string): ParsedKey | undefined {
    const [head, ...rest] = key.split(':');
    const value = rest.join(':');
    if (head === 'provider' && value !== '') {
        return { kind: 'provider', code: value };
    }
    if (head === 'folder' && value !== '' && !value.includes(':')) {
        return { kind: 'folder', id: value };
    }
    return undefined;
}

/** The node of one folder of the hierarchy. */
export function folderNode(folder: FeedFolder): TreeNode {
    const role: TreeRole =
        folder.kind === 'collection'
            ? 'collection'
            : folder.kind === 'instrument_type'
              ? 'instrument_type'
              : 'asset_class';
    return {
        key: folderKey(folder.id),
        role,
        name: folder.name,
        folder,
        collectionIds: [],
        totals: totalsOf([folder]),
    };
}

/** The roots of the tree: one node for each provider. */
export function providerNodes(collections: readonly FeedFolder[]): readonly TreeNode[] {
    return providersOf(collections).map((entry) => ({
        key: providerKey(entry.code),
        role: 'provider' as const,
        name: entry.code,
        providerCode: entry.code,
        collectionIds: entry.collections.map((collection) => collection.id),
        totals: entry.totals,
    }));
}

/**
 * The nodes under a provider that holds several collections: the collections.
 * A provider with one collection is that collection, so its children are the
 * folders under it (see `loadsFrom`). A folder's children come from the service.
 */
export function madeChildren(
    node: TreeNode,
    collections: readonly FeedFolder[],
): readonly TreeNode[] | undefined {
    if (node.role === 'provider' && node.collectionIds.length !== 1) {
        return collections
            .filter((collection) => node.collectionIds.includes(collection.id))
            .map(folderNode);
    }
    return undefined;
}

/** The folder whose children the service gives for a node, if any. */
export function loadsFrom(node: TreeNode): string | undefined {
    if (node.role === 'provider') {
        return node.collectionIds.length === 1 ? node.collectionIds[0] : undefined;
    }
    return node.role === 'instrument_type' ? undefined : node.folder?.id;
}

/** Whether a node may have nodes under it, so it gets a twisty. */
export function mayOpen(node: TreeNode): boolean {
    return node.role !== 'instrument_type' && (node.folder?.has_children ?? true);
}

/** The filter that lists the feeds under a node. */
export function filterOf(node: TreeNode | undefined): FeedFilter {
    if (node === undefined) {
        return {};
    }
    return node.role === 'provider'
        ? { provider: node.providerCode ?? '' }
        : { folder: node.folder?.id ?? '' };
}

/** The plain facts of a folder row, as the folders list gives them. */
export interface FolderRow {
    readonly id: string;
    readonly parent_id: string;
    readonly name: string;
    readonly kind: string;
}

/**
 * The folders from a feed's folder up to the collection, outermost first,
 * without the root, which the providers take the place of.
 */
export function folderChain(rows: readonly FolderRow[], id: string): readonly FolderRow[] {
    const byId = new Map(rows.map((row) => [row.id, row]));
    const chain: FolderRow[] = [];
    // A folder has one parent and the database refuses a cycle; the bound is for a bad read.
    for (
        let at = byId.get(id);
        at !== undefined && chain.length < 16;
        at = byId.get(at.parent_id)
    ) {
        if (at.kind !== 'root') {
            chain.unshift(at);
        }
    }
    return chain;
}

/** The address of a feed's own page. */
export function feedPageOf(feed: Pick<FolderFeed, 'kind' | 'id'>): string {
    return `${feed.kind === 'ir_curve' ? PATHS.irFeed : PATHS.fxFeed}/${encodeURIComponent(feed.id)}`;
}

/** The asset classes a feed can have, by the code the feed carries. */
export const ASSET_CLASSES = ['fx', 'interest_rates'] as const;

/**
 * The collection a person is looking at: the one a provider holds when it holds one, or the
 * collection a folder sits in. Empty when the node names none, or a provider holds several.
 */
export function collectionOf(
    parsed: ParsedKey | undefined,
    collections: readonly FeedFolder[],
    rows: readonly FolderRow[],
): string {
    if (parsed?.kind === 'provider') {
        const held = collections.filter((one) => one.provider_code === parsed.code);
        return held.length === 1 ? (held[0]?.id ?? '') : '';
    }
    if (parsed?.kind === 'folder') {
        return folderChain(rows, parsed.id).find((row) => row.kind === 'collection')?.id ?? '';
    }
    return '';
}
