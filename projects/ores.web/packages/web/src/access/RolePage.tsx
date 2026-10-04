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

import { useMutation, useQueries, useQuery, useQueryClient } from '@tanstack/react-query';
import { useMemo, useState, type ReactNode } from 'react';
import { Link, useNavigate, useParams } from 'react-router';
import type { Account, PermissionEntry, RoleSummary } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { Avatar, imageUrl } from '../ui/Images.js';
import { Button, Dialog, Field, Input, Notice, PageHeader } from '../ui/Primitives.js';
import { areasOf, parseCode } from './catalogue.js';
import { PermissionAreas } from './PermissionAreas.js';
import { roleLabel } from './words.js';

/** How many added or removed permissions the save dialog lists before it counts the rest. */
const LISTED = 8;

/**
 * One role: who holds it and what it lets people do.
 *
 * The person ticks what the role allows, an area at a time if they like; the
 * save sends the whole set, and first shows what it adds, what it takes away
 * and whose access changes. A role that grants everything, or a service's
 * role, is shown and not edited here.
 */
export function RolePage(): ReactNode {
    const { t } = useTranslation();
    const { roleId = '' } = useParams();
    const roles = useQuery({ queryKey: ['roles'], queryFn: api.roles });
    const catalogue = useQuery({ queryKey: ['permissions'], queryFn: api.permissions });

    if (roles.isPending || catalogue.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (roles.isError) {
        return <Notice tone="error">{roles.error.message}</Notice>;
    }
    if (catalogue.isError) {
        return <Notice tone="error">{catalogue.error.message}</Notice>;
    }
    const role = roles.data.find((candidate) => candidate.id === roleId);
    if (role === undefined) {
        return <Notice tone="warn">{t('access.roles.notFound')}</Notice>;
    }
    return <Role key={role.id + role.version} role={role} catalogue={catalogue.data} />;
}

/** The people who hold a role, read through each person's access. */
function useHolders(roleId: string): readonly Account[] {
    const people = useQuery({ queryKey: ['accounts'], queryFn: api.accounts });
    const accounts = people.data?.accounts ?? [];
    const access = useQueries({
        queries: accounts.map((account) => ({
            queryKey: ['account-access', account.id],
            queryFn: () => api.accountAccess(account.id),
        })),
    });
    return accounts.filter((_, index) =>
        (access[index]?.data?.roles ?? []).some((held) => held.roleId === roleId),
    );
}

function Role({
    role,
    catalogue,
}: {
    readonly role: RoleSummary;
    readonly catalogue: readonly PermissionEntry[];
}): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const queries = useQueryClient();
    const areas = useMemo(() => areasOf(catalogue), [catalogue]);
    const holders = useHolders(role.id);
    const [draft, setDraft] = useState<ReadonlySet<string>>(() => new Set(role.permissionCodes));
    const [filter, setFilter] = useState('');
    const [onlyAllowed, setOnlyAllowed] = useState(false);
    const [saving, setSaving] = useState(false);
    const [renaming, setRenaming] = useState(false);
    const remove = useMutation({
        mutationFn: () => api.deleteRole(role.name),
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['roles'] });
            void navigate('/roles');
        },
    });

    const stored = new Set(role.permissionCodes);
    const added = [...draft].filter((code) => !stored.has(code));
    const removed = [...stored].filter((code) => !draft.has(code));
    const changed = added.length > 0 || removed.length > 0;
    const locked = role.service || stored.has('*');

    const toggle = (code: string, on: boolean) => {
        const next = new Set(draft);
        if (on) {
            next.add(code);
            const { component, resource } = parseCode(code);
            if (resource === '*') {
                for (const held of draft) {
                    if (held !== code && parseCode(held).component === component) next.delete(held);
                }
            }
        } else {
            next.delete(code);
        }
        setDraft(next);
    };

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
                                disabled={holders.length > 0 || remove.isPending}
                                title={
                                    holders.length > 0
                                        ? t('access.roles.heldCannotDelete')
                                        : undefined
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
                {holders.length === 0 ? (
                    <p className="text-sm text-ink-faint">{t('access.roles.nobody')}</p>
                ) : (
                    <div className="flex flex-wrap gap-2">
                        {holders.map((account) => {
                            const name =
                                account.fullName === '' ? account.username : account.fullName;
                            return (
                                <Link
                                    key={account.id}
                                    to={`/people/${encodeURIComponent(account.username)}`}
                                    className="flex items-center gap-2 rounded-full border border-line py-0.5 pl-0.5 pr-3 text-sm hover:border-line-strong"
                                >
                                    <Avatar
                                        name={name}
                                        size="sm"
                                        src={
                                            account.imageId === null
                                                ? null
                                                : imageUrl(account.imageId)
                                        }
                                    />
                                    {name}
                                </Link>
                            );
                        })}
                    </div>
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
                    <div className="flex flex-wrap items-center gap-3">
                        <Input
                            type="search"
                            className="max-w-md"
                            value={filter}
                            onChange={(event) => setFilter(event.target.value)}
                            placeholder={t('access.roles.find')}
                            aria-label={t('access.roles.find')}
                        />
                        <label className="flex items-center gap-2 text-sm text-ink-muted">
                            <input
                                type="checkbox"
                                checked={onlyAllowed}
                                onChange={(event) => setOnlyAllowed(event.target.checked)}
                            />
                            {t('access.roles.onlyAllowed')}
                        </label>
                    </div>
                    <PermissionAreas
                        areas={areas}
                        granted={draft}
                        filter={filter}
                        onlyGranted={onlyAllowed}
                        onToggle={toggle}
                    />
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
                                onClick={() => setDraft(new Set(role.permissionCodes))}
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
                    draft={draft}
                    added={added}
                    removed={removed}
                    holders={holders}
                    catalogue={catalogue}
                    onClose={() => setSaving(false)}
                />
            )}
            {renaming && <RenameDialog role={role} onClose={() => setRenaming(false)} />}
        </div>
    );
}

function SaveDialog({
    role,
    draft,
    added,
    removed,
    holders,
    catalogue,
    onClose,
}: {
    readonly role: RoleSummary;
    readonly draft: ReadonlySet<string>;
    readonly added: readonly string[];
    readonly removed: readonly string[];
    readonly holders: readonly Account[];
    readonly catalogue: readonly PermissionEntry[];
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const [note, setNote] = useState('');
    const describe = new Map(catalogue.map((entry) => [entry.code, entry.description]));
    const save = useMutation({
        mutationFn: () => api.saveRolePermissions(role.id, [...draft], note.trim()),
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['roles'] });
            await queries.invalidateQueries({ queryKey: ['account-access'] });
            onClose();
        },
    });
    const list = (codes: readonly string[]) => (
        <ul className="mt-1 space-y-0.5 pl-4 text-sm">
            {codes.slice(0, LISTED).map((code) => (
                <li key={code}>
                    <span className="font-mono text-xs">{code}</span>{' '}
                    <span className="text-ink-faint">{describe.get(code) ?? ''}</span>
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
                {holders.length > 0 && (
                    <p className="text-sm text-ink-muted">
                        {t('access.roles.reaches', {
                            people: holders
                                .map((account) =>
                                    account.fullName === '' ? account.username : account.fullName,
                                )
                                .join(', '),
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
    readonly role: RoleSummary;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const [name, setName] = useState(role.name);
    const [description, setDescription] = useState(role.description);
    const update = useMutation({
        mutationFn: () =>
            api.updateRole(role.id, { name: name.trim(), description, version: role.version }),
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['roles'] });
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
                {update.isError && <Notice tone="error">{update.error.message}</Notice>}
            </div>
        </Dialog>
    );
}
