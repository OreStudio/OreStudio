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

import { useQuery } from '@tanstack/react-query';
import { useMemo, type ReactNode } from 'react';
import type { InboxRequestView } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { areasOf, covers } from '../access/catalogue.js';
import { PermissionAreas } from '../access/PermissionAreas.js';
import { roleLabel } from '../access/words.js';
import { Detail } from '../ui/Primitives.js';
import { kindLabel } from './words.js';

/**
 * What only a role request shows: the roles it asks for, and what they would let
 * the person do.
 *
 * What the role would let the person do is the whole point of the request: the
 * administrator decides on the bundle, not on the role's name. A role that left
 * the catalogue, or one read before it arrived, draws nothing.
 */
export function RoleRequestExtras({ request }: { readonly request: InboxRequestView }): ReactNode {
    const { t } = useTranslation();
    const roles = useQuery({ queryKey: ['roles'], queryFn: api.roles });
    const catalogue = useQuery({ queryKey: ['permissions'], queryFn: api.permissions });
    const areas = useMemo(() => areasOf(catalogue.data ?? []), [catalogue.data]);

    const granted = new Set(
        request.roles.flatMap(
            (role) =>
                (roles.data ?? []).find((candidate) => candidate.id === role.roleId)
                    ?.permissionCodes ?? [],
        ),
    );
    const names =
        request.roles
            .map((role) => role.name)
            .filter((name) => name !== '')
            .join(', ') || kindLabel(t, request.kindCode);

    return (
        <>
            <section className="space-y-3 rounded-md border border-line bg-surface-raised p-4">
                <dl className="grid grid-cols-1 gap-3 sm:grid-cols-2">
                    {request.roles.map((role) => (
                        <Detail
                            key={role.roleId}
                            label={roleLabel(t, role.name)}
                            value={role.description}
                        />
                    ))}
                </dl>
            </section>

            <section className="space-y-3">
                <h2 className="text-sm font-semibold">
                    {t('inbox.request.wouldAllow', { role: names })}
                </h2>
                {granted.size === 0 || covers(granted, '*') ? (
                    <p className="text-sm text-ink-muted">{t('access.nothingMatches')}</p>
                ) : (
                    <PermissionAreas areas={areas} granted={granted} onlyGranted />
                )}
            </section>
        </>
    );
}
