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
 */

import { useQuery } from '@tanstack/react-query';
import type { SensitivityTemplateChoices } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { PERMISSION } from '../access/permissions.js';

/** The entity type a template's history is read by. */
export const ENTITY_TYPE = 'ores.analytics.sensitivity_template';

/** Where one page of the list is cached, and what a write marks stale. */
export const TEMPLATES_KEY = ['sensitivity-templates'] as const;

/**
 * The two lookups a template states: the methods, each with the words a person
 * reads and whether it needs an asset class, and the asset classes, each with
 * the prefix a key spells and the words a person reads. Both come from the
 * tables that hold them, so the form offers what the store holds.
 */
export function useChoices(): SensitivityTemplateChoices | undefined {
    return useQuery({
        queryKey: ['sensitivity-templates', 'choices'],
        queryFn: api.sensitivityTemplateChoices,
        staleTime: Infinity,
    }).data;
}

/** One template, read by its code. */
export function useSensitivityTemplate(code: string) {
    return useQuery({
        queryKey: ['sensitivity-templates', 'key', code],
        queryFn: () => api.sensitivityTemplate(code),
        retry: false,
    });
}

/**
 * What the signed-in person may do to the templates. The server checks each
 * write again; this only decides which actions a screen draws.
 */
export function useSensitivityTemplatePermissions(): {
    readonly write: boolean;
    readonly remove: boolean;
} {
    const access = useQuery({ queryKey: ['my-access'], queryFn: api.myAccess });
    const codes = new Set((access.data?.roles ?? []).flatMap((role) => role.permissionCodes));
    const holds = (code: string): boolean =>
        codes.has('*') || codes.has('analytics::*') || codes.has(code);
    return {
        write: holds(PERMISSION.sensitivityTemplatesWrite),
        remove: holds(PERMISSION.sensitivityTemplatesDelete),
    };
}
