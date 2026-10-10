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

import { keepPreviousData, useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useEffect, useState, type ReactNode } from 'react';
import { useNavigate } from 'react-router';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { Button, Dialog, Field, Input, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { DEFAULT_PAGE_SIZE, Pager, pageBounds } from '../ui/Pager.js';
import { AreaFilter } from './PermissionAreas.js';
import { roleLabel } from './words.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { RefreshQueries } from '../ui/RefreshButton.js';

/** How long typing pauses before a search is sent to the server. */
const SEARCH_PAUSE_MS = 300;

/**
 * The tenant's roles and what each one lets people do.
 *
 * Each page is a request for that page: the pager, the page size, the search,
 * the area and the choice to include the platform's own roles are the
 * request's, and the server answers the rows, the total and the areas some
 * role grants something in. The platform's own services sign in with roles of
 * their own, nineteen of them in the seed. They are left out unless asked for,
 * because they are not given to people and are not changed here.
 *
 * The area filter narrows the roles to those that grant something in an area,
 * and offers only areas some role does. Leaving it on its first choice shows
 * every role.
 */
export function RolesPage(): ReactNode {
    const { t, plural } = useTranslation();
    const navigate = useNavigate();
    const [showService, setShowService] = useState(false);
    const [creating, setCreating] = useState(false);
    const [typed, setTyped] = useState('');
    const [search, setSearch] = useState('');
    const [area, setArea] = useState('');
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
        queryKey: ['roles-page', search, area, showService, offset, pageSize],
        queryFn: () =>
            api.rolesPage({
                search,
                area,
                includeService: showService,
                offset,
                limit: pageSize,
            }),
        placeholderData: keepPreviousData,
    });

    if (page.isError) {
        return <Notice tone="error">{page.error.message}</Notice>;
    }
    if (page.data === undefined) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    const { roles, totalCount, serviceHidden, areas } = page.data;
    const { first, last } = pageBounds(offset, roles.length);
    const narrow = (apply: () => void): void => {
        apply();
        setOffset(0);
    };

    return (
        <div>
            <div>
                <AreaTrail area="organisation" screen={t('access.roles.title')} />
                <PageHeader
                    title={t('access.roles.title')}
                    description={t('access.roles.lead')}
                    actions={
                        <div className="flex items-center gap-2">
                            <RefreshQueries keys={[['roles-page']]} />
                            <Button variant="primary" onClick={() => setCreating(true)}>
                                {t('access.roles.new')}
                            </Button>
                        </div>
                    }
                />
            </div>
            <div className="mb-3 flex flex-wrap items-center gap-3">
                <Input
                    type="search"
                    className="max-w-md"
                    value={typed}
                    onChange={(event) => setTyped(event.target.value)}
                    placeholder={t('access.roles.search')}
                    aria-label={t('access.roles.search')}
                />
                {areas.length > 0 && (
                    <AreaFilter
                        areas={areas}
                        value={area}
                        onChange={(component) => narrow(() => setArea(component))}
                    />
                )}
                <label className="flex items-center gap-2 text-sm text-ink-muted">
                    <input
                        type="checkbox"
                        checked={showService}
                        onChange={(event) => narrow(() => setShowService(event.target.checked))}
                    />
                    {t('access.roles.showService')}
                    {!showService && (
                        <span className="text-ink-faint">
                            · {t('access.roles.serviceHidden', { count: String(serviceHidden) })}
                        </span>
                    )}
                </label>
            </div>
            <div className="overflow-x-auto rounded-md border border-line">
                <table className="w-full text-left text-sm">
                    <thead>
                        <tr className="border-b border-line text-xs text-ink-muted">
                            <th className="px-4 py-2 font-medium">{t('access.person.role')}</th>
                            <th className="px-4 py-2 font-medium">
                                {t('access.roles.letsPeople')}
                            </th>
                        </tr>
                    </thead>
                    <tbody>
                        {roles.length === 0 && (
                            <tr>
                                <td colSpan={2} className="px-4 py-3 text-ink-muted">
                                    {t('access.nothingMatches')}
                                </td>
                            </tr>
                        )}
                        {roles.map((role) => (
                            <tr
                                key={role.id}
                                className="cursor-pointer border-b border-line-subtle last:border-b-0 hover:bg-surface-hover"
                                onClick={() =>
                                    void navigate(`/roles/${encodeURIComponent(role.id)}`)
                                }
                            >
                                <td className="px-4 py-2">
                                    <span className="block font-medium">
                                        {roleLabel(t, role.name)}{' '}
                                        {role.service && (
                                            <Tag tone="muted">{t('access.roles.service')}</Tag>
                                        )}
                                    </span>
                                    <span className="block text-xs text-ink-faint">
                                        {role.description}
                                    </span>
                                </td>
                                <td className="px-4 py-2 text-ink-muted">
                                    {role.service
                                        ? '—'
                                        : role.everything
                                          ? t('access.lets.everything')
                                          : t('access.lets.count', {
                                                count: String(role.permissionCount),
                                            })}
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            </div>
            <Pager
                offset={offset}
                shown={roles.length}
                total={totalCount}
                pageSize={pageSize}
                showing={plural('access.roles.showing', totalCount, { first, last })}
                onMove={setOffset}
                onPageSize={(size) => {
                    setPageSize(size);
                    setOffset(0);
                }}
            />
            {creating && <NewRoleDialog onClose={() => setCreating(false)} />}
        </div>
    );
}

function NewRoleDialog({ onClose }: { readonly onClose: () => void }): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const queries = useQueryClient();
    const [name, setName] = useState('');
    const [description, setDescription] = useState('');
    const create = useMutation({
        mutationFn: () => api.createRole({ name: name.trim(), description }),
        onSuccess: async (id) => {
            await queries.invalidateQueries({ queryKey: ['roles'] });
            await queries.invalidateQueries({ queryKey: ['roles-page'] });
            void navigate(`/roles/${encodeURIComponent(id)}`);
        },
    });
    return (
        <Dialog
            title={t('access.roles.new')}
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" onClick={onClose}>
                        {t('entity.cancel')}
                    </Button>
                    <Button
                        variant="primary"
                        disabled={name.trim() === '' || create.isPending}
                        onClick={() => create.mutate()}
                    >
                        {t('access.roles.create')}
                    </Button>
                </>
            }
        >
            <div className="space-y-4">
                <Field label={t('access.roles.name')}>
                    <Input value={name} onChange={(event) => setName(event.target.value)} />
                </Field>
                <Field label={t('access.roles.forWhat')}>
                    <Input
                        value={description}
                        onChange={(event) => setDescription(event.target.value)}
                    />
                </Field>
                <p className="text-sm text-ink-muted">{t('access.roles.startsEmpty')}</p>
                {create.isError && <Notice tone="error">{create.error.message}</Notice>}
            </div>
        </Dialog>
    );
}
