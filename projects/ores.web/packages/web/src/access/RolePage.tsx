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
import { Link, useNavigate, useParams } from 'react-router';
import type { PermissionPage, RoleHolder, RolePageRow } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { Avatar, imageUrl } from '../ui/Images.js';
import { DEFAULT_PAGE_SIZE, Pager } from '../ui/Pager.js';
import { Button, Dialog, Field, Input, Notice, PageHeader } from '../ui/Primitives.js';
import { areaWildcard, type Area } from './catalogue.js';
import { AreaFilter, PermissionAreas } from './PermissionAreas.js';
import { displayName } from './names.js';
import { roleLabel } from './words.js';
import { useHolds } from './holds.js';

/** How many added or removed permissions the save dialog lists before it counts the rest. */
const LISTED = 8;

/** How long typing pauses before a search is sent to the server. */
const SEARCH_PAUSE_MS = 300;

/**
 * One role: who holds it and what it lets people do.
 *
 * The role's header is one request for that role, and the permissions the
 * editor shows are a page at a time from the server: the pager, the page size,
 * the area and the search are the request's. What the person ticks is kept as
 * a set of changes against what the server holds, so a change made on one page
 * survives a move to another, and the save sends only the changes and first
 * shows what it adds, what it takes away and whose access changes. A role that
 * grants everything, or a service's role, is shown and not edited here.
 */
export function RolePage(): ReactNode {
    const { t } = useTranslation();
    const { roleId = '' } = useParams();
    const header = useQuery({
        queryKey: ['roles-page', 'one', roleId],
        queryFn: () =>
            api.rolesPage({
                roleId,
                search: '',
                area: '',
                includeService: true,
                offset: 0,
                limit: 1,
            }),
    });

    if (header.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (header.isError) {
        return <Notice tone="error">{header.error.message}</Notice>;
    }
    const role = header.data.roles[0];
    if (role === undefined) {
        return <Notice tone="warn">{t('access.roles.notFound')}</Notice>;
    }
    return <Role key={role.id + String(role.version)} role={role} />;
}

function Role({ role }: { readonly role: RolePageRow }): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const queries = useQueryClient();
    const [holderOffset, setHolderOffset] = useState(0);
    const [holderPageSize, setHolderPageSize] = useState(DEFAULT_PAGE_SIZE);
    const mayReadRoles = useHolds()('iam::roles:read');
    /*
     * Who holds the role is a page of people from the server, with their names
     * and pictures, so the page reads nobody's access one account at a time. A
     * reader who may not read roles is not shown a list that would be refused.
     */
    const held = useQuery({
        queryKey: ['role-holders', role.id, holderOffset, holderPageSize],
        queryFn: () => api.roleHolders(role.id, { offset: holderOffset, limit: holderPageSize }),
        placeholderData: keepPreviousData,
        enabled: mayReadRoles,
    });
    const holders: readonly RoleHolder[] = held.data?.holders ?? [];
    const holderTotal = held.data?.totalCount ?? 0;
    const [changes, setChanges] = useState<ReadonlyMap<string, boolean>>(new Map());
    const [saving, setSaving] = useState(false);
    const [renaming, setRenaming] = useState(false);
    const remove = useMutation({
        mutationFn: () => api.deleteRole(role.name),
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['roles-page'] });
            await queries.invalidateQueries({ queryKey: ['roles'] });
            void navigate('/roles');
        },
    });

    const added = [...changes].filter(([, on]) => on).map(([code]) => code);
    const removed = [...changes].filter(([, on]) => !on).map(([code]) => code);
    const changed = changes.size > 0;
    const locked = role.service || role.everything;

    return (
        <div className="space-y-6">
            <p className="text-xs text-ink-faint">
                <Link to="/roles" className="text-accent-bright hover:underline">
                    {t('access.roles.title')}
                </Link>{' '}
                / {roleLabel(t, role.name)}
            </p>
            <PageHeader
                title={roleLabel(t, role.name)}
                description={role.description}
                actions={
                    locked ? undefined : (
                        <>
                            <Button onClick={() => setRenaming(true)}>
                                {t('access.roles.rename')}
                            </Button>
                            <Button
                                variant="danger"
                                disabled={holderTotal > 0 || remove.isPending}
                                title={
                                    holderTotal > 0 ? t('access.roles.heldCannotDelete') : undefined
                                }
                                onClick={() => remove.mutate()}
                            >
                                {t('access.roles.delete')}
                            </Button>
                        </>
                    )
                }
            />
            {remove.isError && <Notice tone="error">{remove.error.message}</Notice>}

            <section className="space-y-2 rounded-md border border-line bg-surface-raised p-4">
                <h2 className="text-sm font-semibold">{t('access.roles.heldBy')}</h2>
                {held.isError ? (
                    <Notice tone="error">{held.error.message}</Notice>
                ) : holderTotal === 0 ? (
                    <p className="text-sm text-ink-faint">{t('access.roles.nobody')}</p>
                ) : (
                    <>
                        <div className="flex flex-wrap gap-2">
                            {holders.map((holder) => {
                                const name =
                                    holder.fullName === '' ? holder.username : holder.fullName;
                                return (
                                    <Link
                                        key={holder.accountId}
                                        to={`/people/${encodeURIComponent(holder.username)}`}
                                        className="flex items-center gap-2 rounded-full border border-line py-0.5 pl-0.5 pr-3 text-sm hover:border-line-strong"
                                    >
                                        <Avatar
                                            name={name}
                                            size="sm"
                                            src={
                                                holder.imageId === null
                                                    ? null
                                                    : imageUrl(holder.imageId)
                                            }
                                        />
                                        {name}
                                    </Link>
                                );
                            })}
                        </div>
                        <Pager
                            offset={holderOffset}
                            shown={holders.length}
                            total={holderTotal}
                            pageSize={holderPageSize}
                            showing={t('access.roles.holdersShowing', {
                                from: String(holderOffset + 1),
                                to: String(holderOffset + holders.length),
                                total: String(holderTotal),
                            })}
                            onMove={setHolderOffset}
                            onPageSize={(size) => {
                                setHolderPageSize(size);
                                setHolderOffset(0);
                            }}
                        />
                    </>
                )}
            </section>

            {locked ? (
                <Notice tone="info">
                    {role.service
                        ? t('access.roles.serviceLocked')
                        : t('access.roles.everythingLocked')}
                </Notice>
            ) : (
                <section className="space-y-3">
                    <div>
                        <h2 className="text-sm font-semibold">{t('access.roles.whatItAllows')}</h2>
                        <p className="text-sm text-ink-muted">{t('access.roles.tickHint')}</p>
                    </div>
                    <RoleEditor roleId={role.id} changes={changes} onChange={setChanges} />
                    <div className="sticky bottom-4 flex flex-wrap items-center justify-between gap-3 rounded-md border border-line-strong bg-surface-overlay px-4 py-2.5">
                        <span className="text-sm">
                            {changed ? (
                                <>
                                    <span className="text-up">
                                        {t('access.roles.added', { count: String(added.length) })}
                                    </span>{' '}
                                    <span className="text-down">
                                        {t('access.roles.removed', {
                                            count: String(removed.length),
                                        })}
                                    </span>
                                </>
                            ) : (
                                <span className="text-ink-muted">
                                    {t('access.roles.noChanges')}
                                </span>
                            )}
                        </span>
                        <span className="flex gap-2">
                            <Button
                                variant="ghost"
                                disabled={!changed}
                                onClick={() => setChanges(new Map())}
                            >
                                {t('access.roles.discard')}
                            </Button>
                            <Button
                                variant="primary"
                                disabled={!changed}
                                onClick={() => setSaving(true)}
                            >
                                {t('access.roles.save')}
                            </Button>
                        </span>
                    </div>
                </section>
            )}

            {saving && (
                <SaveDialog
                    role={role}
                    added={added}
                    removed={removed}
                    holders={holders}
                    holderTotal={holderTotal}
                    onSaved={() => setChanges(new Map())}
                    onClose={() => setSaving(false)}
                />
            )}
            {renaming && <RenameDialog role={role} onClose={() => setRenaming(false)} />}
        </div>
    );
}

/**
 * The permissions of a role, one page of the catalogue at a time, to tick.
 *
 * Each page is a request for that page. Ticks are kept as changes against what
 * the server holds, keyed by code, so moving to another page or area keeps them.
 * A tick that puts a permission back as the server holds it removes the change.
 * The first area the catalogue has is chosen until another is; there is no "all
 * areas", because a page is always of one area.
 */
function RoleEditor({
    roleId,
    changes,
    onChange,
}: {
    readonly roleId: string;
    readonly changes: ReadonlyMap<string, boolean>;
    readonly onChange: (changes: ReadonlyMap<string, boolean>) => void;
}): ReactNode {
    const { t } = useTranslation();
    const [area, setArea] = useState('');
    const [typed, setTyped] = useState('');
    const [search, setSearch] = useState('');
    const [onlyAllowed, setOnlyAllowed] = useState(false);
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
        queryKey: ['role-permission-page', roleId, area, search, onlyAllowed, offset, pageSize],
        queryFn: () =>
            api.rolePermissions(roleId, {
                area,
                search,
                offset,
                limit: pageSize,
                includeUnheld: !onlyAllowed,
            }),
        placeholderData: keepPreviousData,
    });

    if (page.isError) {
        return <Notice tone="error">{page.error.message}</Notice>;
    }
    if (page.data === undefined) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    const data: PermissionPage = page.data;
    const chosen = data.area;

    // What the server holds on this page, and the person's changes laid over it.
    const saved = new Set<string>();
    for (const row of data.rows) {
        for (const action of row.held) {
            saved.add(`${row.component}::${row.resource}:${action}`);
        }
    }
    const wildcard = areaWildcard(chosen);
    if (data.areaWhole) saved.add(wildcard);
    const draft = new Set(saved);
    for (const [code, on] of changes) {
        if (on) draft.add(code);
        else draft.delete(code);
    }

    const toggle = (code: string, on: boolean): void => {
        const next = new Map(changes);
        if (on === saved.has(code)) next.delete(code);
        else next.set(code, on);
        onChange(next);
    };

    const shown: readonly Area[] =
        data.rows.length === 0
            ? []
            : [
                  {
                      component: chosen,
                      size: data.totalCount,
                      resources: data.rows.map((row) => ({
                          name: row.resource,
                          actions: row.actions,
                      })),
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
                    areas={data.areas}
                    value={chosen}
                    includeAll={false}
                    onChange={(component) => {
                        setArea(component);
                        setOffset(0);
                    }}
                />
                <label className="flex items-center gap-2 text-sm text-ink-muted">
                    <input
                        type="checkbox"
                        checked={onlyAllowed}
                        onChange={(event) => {
                            setOnlyAllowed(event.target.checked);
                            setOffset(0);
                        }}
                    />
                    {t('access.roles.onlyAllowed')}
                </label>
            </div>
            {data.rows.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('access.nothingMatches')}</p>
            ) : (
                <PermissionAreas
                    areas={shown}
                    granted={draft}
                    summary={t('access.resourcesHeld', { count: String(data.totalCount) })}
                    onToggle={toggle}
                />
            )}
            {data.totalCount > 0 && (
                <Pager
                    offset={offset}
                    shown={data.rows.length}
                    total={data.totalCount}
                    pageSize={pageSize}
                    showing={t('access.permissionsShowing', {
                        from: String(offset + 1),
                        to: String(offset + data.rows.length),
                        total: String(data.totalCount),
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

function SaveDialog({
    role,
    added,
    removed,
    holders,
    holderTotal,
    onSaved,
    onClose,
}: {
    readonly role: RolePageRow;
    readonly added: readonly string[];
    readonly removed: readonly string[];
    readonly holders: readonly RoleHolder[];
    readonly holderTotal: number;
    readonly onSaved: () => void;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const [note, setNote] = useState('');
    const save = useMutation({
        mutationFn: () =>
            api.changeRolePermissions(role.id, { add: added, remove: removed }, note.trim()),
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['roles-page'] });
            await queries.invalidateQueries({ queryKey: ['role-permission-page', role.id] });
            await queries.invalidateQueries({ queryKey: ['account-access'] });
            await queries.invalidateQueries({ queryKey: ['permission-page'] });
            await queries.invalidateQueries({ queryKey: ['role-holders', role.id] });
            onSaved();
            onClose();
        },
    });
    const list = (codes: readonly string[]) => (
        <ul className="mt-1 space-y-0.5 pl-4 text-sm">
            {codes.slice(0, LISTED).map((code) => (
                <li key={code}>
                    <span className="font-mono text-xs">{code}</span>
                </li>
            ))}
            {codes.length > LISTED && (
                <li className="text-ink-faint">
                    {t('access.roles.andMore', { count: String(codes.length - LISTED) })}
                </li>
            )}
        </ul>
    );
    return (
        <Dialog
            title={t('access.roles.saveTitle', { role: roleLabel(t, role.name) })}
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" onClick={onClose}>
                        {t('entity.cancel')}
                    </Button>
                    <Button
                        variant="primary"
                        disabled={note.trim() === '' || save.isPending}
                        onClick={() => save.mutate()}
                    >
                        {t('access.roles.save')}
                    </Button>
                </>
            }
        >
            <div className="space-y-4">
                {holderTotal > 0 && (
                    <p className="text-sm text-ink-muted">
                        {t('access.roles.reaches', {
                            people:
                                holders
                                    .map((holder) =>
                                        holder.fullName === '' ? holder.username : holder.fullName,
                                    )
                                    .join(', ') +
                                (holderTotal > holders.length
                                    ? ` ${t('access.roles.andMore', { count: String(holderTotal - holders.length) })}`
                                    : ''),
                        })}
                    </p>
                )}
                {added.length > 0 && (
                    <div>
                        <p className="text-sm text-up">{t('access.roles.nowAllows')}</p>
                        {list(added)}
                    </div>
                )}
                {removed.length > 0 && (
                    <div>
                        <p className="text-sm text-down">{t('access.roles.noLongerAllows')}</p>
                        {list(removed)}
                    </div>
                )}
                <Field label={t('access.roles.whyChange')}>
                    <Input value={note} onChange={(event) => setNote(event.target.value)} />
                </Field>
                {save.isError && <Notice tone="error">{save.error.message}</Notice>}
            </div>
        </Dialog>
    );
}

function RenameDialog({
    role,
    onClose,
}: {
    readonly role: RolePageRow;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const [name, setName] = useState(role.name);
    const [description, setDescription] = useState(role.description);
    const [requestable, setRequestable] = useState(role.requestable);
    const update = useMutation({
        mutationFn: () =>
            api.updateRole(role.id, {
                name: name.trim(),
                description,
                version: role.version,
                registrationDefault: role.registrationDefault,
                requestable,
            }),
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['roles'] });
            await queries.invalidateQueries({ queryKey: ['roles-page'] });
            onClose();
        },
    });
    return (
        <Dialog
            title={t('access.roles.rename')}
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" onClick={onClose}>
                        {t('entity.cancel')}
                    </Button>
                    <Button
                        variant="primary"
                        disabled={name.trim() === '' || update.isPending}
                        onClick={() => update.mutate()}
                    >
                        {t('entity.save')}
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
                <label className="flex items-center gap-2 text-sm">
                    <input
                        type="checkbox"
                        checked={requestable}
                        onChange={(event) => setRequestable(event.target.checked)}
                    />
                    {t('access.roles.requestable')}
                </label>
                <p className="text-xs text-ink-muted">{t('access.roles.requestableHint')}</p>
                {update.isError && <Notice tone="error">{update.error.message}</Notice>}
            </div>
        </Dialog>
    );
}
