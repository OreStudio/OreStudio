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

import type { HeldRole, PermissionEntry } from '@ores/wire-protocol/browser';

/**
 * The permission catalogue as a screen reads it.
 *
 * A code is =component::resource:action=, with two wildcards: =*= grants
 * everything and =component::*= grants a whole area. Almost every action is a
 * read, a write or a delete, so an area is drawn as a grid of what against
 * those three, with the rare other actions beside them.
 */

export const MAIN_ACTIONS = ['read', 'write', 'delete'] as const;

export const EVERYTHING = '*';

export interface ParsedCode {
    readonly component: string;
    readonly resource: string;
    readonly action: string;
}

export function parseCode(code: string): ParsedCode {
    if (code === EVERYTHING) return { component: '*', resource: '*', action: '*' };
    const separator = code.indexOf('::');
    const component = code.slice(0, separator);
    const rest = code.slice(separator + 2);
    if (rest === '*') return { component, resource: '*', action: '*' };
    const colon = rest.lastIndexOf(':');
    return { component, resource: rest.slice(0, colon), action: rest.slice(colon + 1) };
}

export function areaWildcard(component: string): string {
    return `${component}::*`;
}

/** Whether a set of granted codes covers one code, wildcards included. */
export function covers(granted: ReadonlySet<string>, code: string): boolean {
    if (granted.has(EVERYTHING) || granted.has(code)) return true;
    return granted.has(areaWildcard(parseCode(code).component));
}

export interface Resource {
    readonly name: string;
    readonly actions: readonly string[];
}

export interface Area {
    readonly component: string;
    readonly resources: readonly Resource[];
    /** How many codes the area holds, its wildcard aside. */
    readonly size: number;
}

/** The catalogue grouped by area, the largest first, each area's resources by name. */
export function areasOf(catalogue: readonly PermissionEntry[]): readonly Area[] {
    const byComponent = new Map<string, Map<string, string[]>>();
    for (const { code } of catalogue) {
        const parsed = parseCode(code);
        if (parsed.resource === '*') continue;
        const resources = byComponent.get(parsed.component) ?? new Map<string, string[]>();
        byComponent.set(parsed.component, resources);
        const actions = resources.get(parsed.resource) ?? [];
        resources.set(parsed.resource, actions);
        actions.push(parsed.action);
    }
    return [...byComponent.entries()]
        .map(([component, resources]) => ({
            component,
            resources: [...resources.entries()]
                .map(([name, actions]) => ({ name, actions }))
                .sort((a, b) => a.name.localeCompare(b.name)),
            size: [...resources.values()].reduce((sum, actions) => sum + actions.length, 0),
        }))
        .sort((a, b) => b.size - a.size || a.component.localeCompare(b.component));
}

/** The codes a person's roles grant, together. */
export function grantedBy(roles: readonly HeldRole[]): ReadonlySet<string> {
    return new Set(roles.flatMap((role) => role.permissionCodes));
}

/** The names of the roles that grant one code. */
export function rolesGranting(roles: readonly HeldRole[], code: string): readonly string[] {
    return roles.filter((role) => covers(new Set(role.permissionCodes), code)).map((r) => r.name);
}

/** How many of the catalogue's codes a set covers, wildcards aside. */
export function countCovered(
    granted: ReadonlySet<string>,
    catalogue: readonly PermissionEntry[],
): number {
    return catalogue.filter(({ code }) => parseCode(code).resource !== '*' && covers(granted, code))
        .length;
}

/**
 * The catalogue entries that answer a question, such as "delete trades".
 *
 * Each word must appear in the code or the description, so a person can ask
 * in their own words or in the code's.
 */
export function search(
    catalogue: readonly PermissionEntry[],
    question: string,
    limit: number,
): readonly PermissionEntry[] {
    const words = question
        .toLowerCase()
        .split(/\s+/)
        .filter((word) => word !== '');
    if (words.length === 0) return [];
    return catalogue
        .filter(({ code }) => parseCode(code).resource !== '*')
        .filter(({ code, description }) => {
            const text = `${code.replace(/_/g, ' ')} ${description}`.toLowerCase();
            return words.every((word) => text.includes(word));
        })
        .slice(0, limit);
}
