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
import type { ReactNode } from 'react';
import { Link } from 'react-router';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { trail } from '../log/clientLog.js';
import { Notice } from '../ui/Primitives.js';
import { useEffect } from 'react';
import { usePermissions } from './holds.js';
import { wordsFor } from './permissionWords.js';
import type { Needs, Permission } from './permissions.js';

const access = trail('access');

/** The permissions a reader lacks of what a screen needs, in the order declared. */
export function missingOf(
    needs: Needs,
    held: (code: Permission) => boolean,
): readonly Permission[] {
    const all = (needs.all ?? []).filter((code) => !held(code));
    const any = needs.any ?? [];
    return all.length > 0 || any.length === 0 || any.some(held) ? all : any;
}

/**
 * Says why a screen cannot be opened, and what to do about it.
 *
 * Not "this is not available to you". It names the screen, says in words what
 * the reader would need to be able to do, and offers the roles they could ask
 * for that allow it, with a link that opens the request already filled in. When
 * no such role exists it says whom to ask instead, and offers no link, because
 * a link that leads nowhere is worse than none.
 */
export function NotAvailable({
    screen,
    needs,
}: {
    readonly screen: string;
    readonly needs: Needs;
}): ReactNode {
    const { t } = useTranslation();
    const { can } = usePermissions();
    const held = (code: Permission): boolean => can({ all: [code] });
    const missing = missingOf(needs, held);
    const first = missing[0];
    const ways = useQuery({
        queryKey: ['ways-to-hold', first],
        queryFn: () => api.waysToHold(first ?? ''),
        enabled: first !== undefined,
        meta: { quiet: true },
    });
    useEffect(() => {
        access.warn('access denied', { screen, missing: missing.join(',') });
    }, [screen, missing.join(',')]);

    const need = missing.map((code) => t(wordsFor(code))).join('; ');
    const roles = ways.data ?? [];
    return (
        <div className="max-w-xl space-y-3">
            <Notice tone="warn">
                <div className="space-y-2">
                    <p className="font-medium">{t('unavailable.title', { screen })}</p>
                    <p>
                        {missing.length > 1
                            ? t('unavailable.needMany', { need })
                            : t('unavailable.need', { need })}
                    </p>
                    {ways.isPending && first !== undefined && (
                        <p className="text-ink-muted">{t('unavailable.checking')}</p>
                    )}
                    {!ways.isPending && roles.length === 0 && <p>{t('unavailable.none')}</p>}
                    {roles.map((role) => (
                        <p key={role.id} className="flex flex-wrap items-center gap-3">
                            <span>{t('unavailable.roleHas', { role: role.name })}</span>
                            <Link
                                className="font-medium underline"
                                to={`/access?ask=${encodeURIComponent(role.id)}&why=${encodeURIComponent(t('unavailable.askReason', { screen }))}`}
                            >
                                {t('unavailable.ask', { role: role.name })}
                            </Link>
                        </p>
                    ))}
                </div>
            </Notice>
        </div>
    );
}
