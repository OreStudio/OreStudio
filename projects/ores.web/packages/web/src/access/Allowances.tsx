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

import { keepPreviousData, useQuery } from '@tanstack/react-query';
import { useEffect, useState, type ReactNode } from 'react';
import type { HeldRole, PermissionEntry, PermissionPage } from '@ores/wire-protocol/browser';
import type { PermissionPageQuery } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { DEFAULT_PAGE_SIZE, Pager } from '../ui/Pager.js';
import { Input, Notice } from '../ui/Primitives.js';
import { rolesGranting, search, type Area } from './catalogue.js';
import { AreaFilter, PermissionAreas } from './PermissionAreas.js';
import { roleLabel } from './words.js';

/** How many answers "Can I…?" shows at once. */
const ANSWERS = 8;

/** How long typing pauses before a search is sent to the server. */
const SEARCH_PAUSE_MS = 300;

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
 * What an account's roles let it do, one page of permissions at a time, read
 * from the server a page at a time.
 *
 * The page on the screen is the page the request asks for: the pager, the page
 * size, the area and the search are the request's offset, limit, area and
 * search, and the server answers the rows, the total and the areas the account
 * holds something in. So the combo box offers only those areas, always has one
 * chosen, and has no "all areas": the first area the account holds is chosen
 * until another is. A role that grants everything says so instead.
 */
export function RolesAllow({
    queryKey,
    read,
    everythingBy,
}: {
    /** Names the account the pages are of, so each account's pages are cached apart. */
    readonly queryKey: readonly unknown[];
    readonly read: (query: PermissionPageQuery) => Promise<PermissionPage>;
    /** The role that grants everything, when one does. */
    readonly everythingBy?: string | undefined;
}): ReactNode {
    const { t } = useTranslation();
    const [area, setArea] = useState('');
    const [search, setSearch] = useState('');
    const [typed, setTyped] = useState('');
    const [offset, setOffset] = useState(0);
    const [pageSize, setPageSize] = useState(DEFAULT_PAGE_SIZE);

    // The search is sent when the typing pauses, so a request is not made per key.
    useEffect(() => {
        const timer = setTimeout(() => {
            setSearch(typed);
            setOffset(0);
        }, SEARCH_PAUSE_MS);
        return () => clearTimeout(timer);
    }, [typed]);

    const page = useQuery({
        queryKey: ['permission-page', ...queryKey, area, search, offset, pageSize],
        queryFn: () => read({ area, search, offset, limit: pageSize }),
        placeholderData: keepPreviousData,
        enabled: everythingBy === undefined,
    });

    if (everythingBy !== undefined) {
        return (
            <p className="text-sm text-ink-muted">
                {t('access.everythingBy', { role: roleLabel(t, everythingBy) })}
            </p>
        );
    }
    if (page.isError) {
        return <Notice tone="error">{page.error.message}</Notice>;
    }
    if (page.data === undefined) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }

    const { rows, areas, totalCount } = page.data;
    const chosen = page.data.area;
    const granted = new Set(
        rows.flatMap((row) =>
            row.held.map((action) => `${row.component}::${row.resource}:${action}`),
        ),
    );
    const rolesOf = new Map(rows.map((row) => [`${row.component}::${row.resource}`, row.roles]));
    const shown: readonly Area[] =
        rows.length === 0
            ? []
            : [
                  {
                      component: chosen,
                      size: totalCount,
                      resources: rows.map((row) => ({ name: row.resource, actions: row.actions })),
                  },
              ];

    return (
        <div className="space-y-3">
            <div className="flex flex-wrap items-center gap-3">
                <Input
                    type="search"
                    className="max-w-md"
                    value={typed}
                    onChange={(event) => setTyped(event.target.value)}
                    placeholder={t('access.roles.find')}
                    aria-label={t('access.roles.find')}
                />
                <AreaFilter
                    areas={areas}
                    value={chosen}
                    includeAll={false}
                    onChange={(component) => {
                        setArea(component);
                        setOffset(0);
                    }}
                />
            </div>
            {rows.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('access.nothingMatches')}</p>
            ) : (
                <PermissionAreas
                    areas={shown}
                    granted={granted}
                    onlyGranted
                    summary={t('access.resourcesHeld', { count: String(totalCount) })}
                    explain={(code) => {
                        const resource = code.slice(0, code.lastIndexOf(':'));
                        return (rolesOf.get(resource) ?? [])
                            .map((roleName) => roleLabel(t, roleName))
                            .join(', ');
                    }}
                />
            )}
            {totalCount > 0 && (
                <Pager
                    offset={offset}
                    shown={rows.length}
                    total={totalCount}
                    pageSize={pageSize}
                    showing={t('access.permissionsShowing', {
                        from: String(offset + 1),
                        to: String(offset + rows.length),
                        total: String(totalCount),
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
