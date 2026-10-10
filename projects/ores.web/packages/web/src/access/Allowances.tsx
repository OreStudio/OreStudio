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

import { useMemo, useState, type ReactNode } from 'react';
import type { HeldRole, PermissionEntry } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { DEFAULT_PAGE_SIZE, Pager } from '../ui/Pager.js';
import { Input } from '../ui/Primitives.js';
import { areasOf, grantedBy, rolesGranting, search, type Area } from './catalogue.js';
import { AreaFilter, PermissionAreas, areaLabel, permissionRows } from './PermissionAreas.js';
import { roleLabel } from './words.js';

/** How many answers "Can I…?" shows at once. */
const ANSWERS = 8;

/**
 * "Can I…?": a question about one thing, answered with the role behind a yes.
 *
 * It is the question a person actually has, so it comes before the full
 * picture. It serves the signed-in person and, for an administrator, the
 * person whose page is open: the roles are whoever's the caller passes.
 */
export function CanIPanel({
    roles,
    catalogue,
}: {
    readonly roles: readonly HeldRole[];
    readonly catalogue: readonly PermissionEntry[];
}): ReactNode {
    const { t } = useTranslation();
    const [question, setQuestion] = useState('');
    const answers = search(catalogue, question, ANSWERS);
    return (
        <section className="space-y-3 rounded-md border border-line bg-surface-raised p-4">
            <h2 className="text-sm font-semibold">{t('access.mine.canI')}</h2>
            <Input
                type="search"
                value={question}
                onChange={(event) => setQuestion(event.target.value)}
                placeholder={t('access.mine.canIHint')}
                aria-label={t('access.mine.canI')}
            />
            {question.trim() !== '' && answers.length === 0 && (
                <p className="text-sm text-ink-muted">{t('access.nothingMatches')}</p>
            )}
            <ul>
                {answers.map((entry) => {
                    const by = rolesGranting(roles, entry.code);
                    return (
                        <li
                            key={entry.code}
                            className="flex items-start gap-3 border-t border-line-subtle py-2 first:border-t-0"
                        >
                            <span
                                className={`mt-1.5 h-2 w-2 shrink-0 rounded-full ${by.length > 0 ? 'bg-up' : 'bg-ink-faint'}`}
                            />
                            <div className="min-w-0 flex-1">
                                <div className="text-sm">{entry.description}</div>
                                <div className="font-mono text-xs text-ink-faint">{entry.code}</div>
                            </div>
                            <div className="text-sm text-ink-muted">
                                {by.length > 0
                                    ? t('access.mine.yesThrough', {
                                          roles: by.map((name) => roleLabel(t, name)).join(', '),
                                      })
                                    : t('access.mine.no')}
                            </div>
                        </li>
                    );
                })}
            </ul>
        </section>
    );
}

/**
 * What a set of roles lets a person do, a page of permissions at a time.
 *
 * The catalogue is nearly nine hundred codes in dozens of areas, and a role can
 * grant an area whole, so even one area is hundreds of rows. The rows are paged
 * with the standard pager and its page sizes, a combo box picks one area, such
 * as reference data or data quality, and a search narrows the rows. A role that
 * grants everything says so, instead of listing every row.
 */
export function RolesAllow({
    roles,
    catalogue,
}: {
    readonly roles: readonly HeldRole[];
    readonly catalogue: readonly PermissionEntry[];
}): ReactNode {
    const { t } = useTranslation();
    const [area, setArea] = useState('');
    const [filter, setFilter] = useState('');
    const [offset, setOffset] = useState(0);
    const [pageSize, setPageSize] = useState(DEFAULT_PAGE_SIZE);
    const areas = useMemo(() => areasOf(catalogue), [catalogue]);
    const everything = roles.find((role) => role.permissionCodes.includes('*'));
    if (everything !== undefined) {
        return (
            <p className="text-sm text-ink-muted">
                {t('access.everythingBy', { role: roleLabel(t, everything.name) })}
            </p>
        );
    }
    const granted = grantedBy(roles);
    const chosen: readonly Area[] =
        area === '' ? areas : areas.filter((entry) => entry.component === area);
    const rows = permissionRows(chosen, granted, {
        onlyGranted: true,
        filter,
        areaName: (component) => areaLabel(t, component),
    });
    const page = rows.slice(offset, offset + pageSize);
    return (
        <div className="space-y-3">
            <div className="flex flex-wrap items-center gap-3">
                <Input
                    type="search"
                    className="max-w-md"
                    value={filter}
                    onChange={(event) => {
                        setFilter(event.target.value);
                        setOffset(0);
                    }}
                    placeholder={t('access.roles.find')}
                    aria-label={t('access.roles.find')}
                />
                <AreaFilter
                    areas={areas}
                    value={area}
                    onChange={(component) => {
                        setArea(component);
                        setOffset(0);
                    }}
                />
            </div>
            <PermissionAreas
                areas={chosen}
                granted={granted}
                onlyGranted
                filter={filter}
                window={new Set(page)}
                explain={(code) =>
                    rolesGranting(roles, code)
                        .map((name) => roleLabel(t, name))
                        .join(', ')
                }
            />
            {rows.length > 0 && (
                <Pager
                    offset={offset}
                    shown={page.length}
                    total={rows.length}
                    pageSize={pageSize}
                    showing={t('access.permissionsShowing', {
                        from: String(offset + 1),
                        to: String(offset + page.length),
                        total: String(rows.length),
                    })}
                    onMove={setOffset}
                    onPageSize={(size) => {
                        setPageSize(size);
                        setOffset(0);
                    }}
                />
            )}
        </div>
    );
}
