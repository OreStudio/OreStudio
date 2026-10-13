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

import type { FeedFolder } from '../api/client.js';
import {
    providerOf,
    providersOf,
    scopesOf,
    totalsOf,
    type OverviewScope,
    type Totals,
} from './feedOverview.js';

/** What a node of the tree stands for. */
export type TreeRole = 'scope' | 'provider' | 'collection' | 'asset_class' | 'instrument_type';

/**
 * A node of the feed tree. The scopes and the providers are made from the
 * collections; the rest are the folders of the hierarchy, whose children the
 * service counts when a node is opened.
 */
export interface TreeNode {
    /** The address of the node, kept in the URL: scope:system, provider:system:bloomberg, folder:<id>. */
    readonly key: string;
    readonly role: TreeRole;
    /** The scope or provider code for a made node, else the folder's own name. */
    readonly name: string;
    readonly scope?: OverviewScope;
    readonly providerCode?: string;
    /** The folder, for a node that is one. */
    readonly folder?: FeedFolder;
    /** The folders a Start or Stop acts on: the collections of a made node, the folder itself else. */
    readonly folderIds: readonly string[];
    readonly totals: Totals;
}

export const scopeKey = (scope: OverviewScope): string => `scope:${scope}`;
export const providerKey = (scope: OverviewScope, code: string): string =>
    `provider:${scope}:${code}`;
export const folderKey = (id: string): string => `folder:${id}`;

/** A key taken apart, or undefined when it is none of the three. */
export type ParsedKey =
    | { readonly kind: 'scope'; readonly scope: OverviewScope }
    | { readonly kind: 'provider'; readonly scope: OverviewScope; readonly code: string }
    | { readonly kind: 'folder'; readonly id: string };

export function parseKey(key: string): ParsedKey | undefined {
    const [head, first, ...rest] = key.split(':');
    if (head === 'scope' && (first === 'system' || first === 'test') && rest.length === 0) {
        return { kind: 'scope', scope: first };
    }
    if (head === 'provider' && (first === 'system' || first === 'test') && rest.length > 0) {
        return { kind: 'provider', scope: first, code: rest.join(':') };
    }
    if (head === 'folder' && first !== undefined && first !== '' && rest.length === 0) {
        return { kind: 'folder', id: first };
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
        folderIds: [folder.id],
        totals: totalsOf([folder]),
    };
}

/** The two scopes, the roots of the tree, system first. */
export function scopeNodes(collections: readonly FeedFolder[]): readonly TreeNode[] {
    return scopesOf(collections).map((entry) => ({
        key: scopeKey(entry.scope),
        role: 'scope' as const,
        name: entry.scope,
        scope: entry.scope,
        folderIds: entry.collections.map((collection) => collection.id),
        totals: entry.totals,
    }));
}

/**
 * The nodes under a made node, or undefined when the node is a folder, whose
 * children the service gives. The system scope holds a node for each provider;
 * the test scope holds its collections; a provider holds its collections, unless
 * it has one, which the provider stands for, so its children are the folders
 * under that collection (see `loadsFrom`).
 */
export function madeChildren(
    node: TreeNode,
    collections: readonly FeedFolder[],
): readonly TreeNode[] | undefined {
    if (node.role === 'scope' && node.scope !== undefined) {
        const inScope = collections.filter((collection) =>
            node.scope === 'system' ? collection.scope === 'system' : collection.scope !== 'system',
        );
        if (node.scope === 'system') {
            return providersOf(inScope).map((entry) => ({
                key: providerKey('system', entry.code),
                role: 'provider' as const,
                name: entry.code,
                scope: 'system' as const,
                providerCode: entry.code,
                folderIds: entry.collections.map((collection) => collection.id),
                totals: entry.totals,
            }));
        }
        return inScope.map(folderNode);
    }
    if (node.role === 'provider' && node.folderIds.length !== 1) {
        return collections
            .filter((collection) => node.folderIds.includes(collection.id))
            .map(folderNode);
    }
    return undefined;
}

/** The folder whose children the service gives for a node, if any. */
export function loadsFrom(node: TreeNode): string | undefined {
    if (node.role === 'provider') {
        return node.folderIds.length === 1 ? node.folderIds[0] : undefined;
    }
    if (node.role === 'scope') {
        return undefined;
    }
    return node.role === 'instrument_type' ? undefined : node.folder?.id;
}

/** Whether a node may have nodes under it, so it gets a twisty. */
export function mayOpen(node: TreeNode): boolean {
    return node.role !== 'instrument_type' && (node.folder?.has_children ?? true);
}

/** Whether a node's feeds are listed in its panel: the folder that holds them. */
export function holdsFeeds(node: TreeNode): boolean {
    return node.role === 'instrument_type';
}

/** The key a node of a collection has when a collection's provider is not unique: its provider code. */
export function providerCodeOf(node: TreeNode): string | undefined {
    return node.providerCode ?? (node.folder === undefined ? undefined : providerOf(node.folder));
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
 * without the root, which the scopes take the place of.
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
