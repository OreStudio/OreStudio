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

import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { Button, Dialog, Field, Notice } from '../ui/Primitives.js';
import { roleLabel } from '../access/words.js';

/**
 * Asking for a role.
 *
 * The picker offers the roles the tenant lets its members ask for, less the
 * ones the person already holds. Whether a role is on offer is the tenant's
 * answer, carried on the role as `requestable`, and the server refuses one
 * that is not: the screen narrows the list and the server decides.
 *
 * A duplicate ask is refused by the server, and its own words are what the
 * dialog shows: what counts as a duplicate is the queue's business, not the
 * screen's, and the server reads the queue.
 */
export function AskForRoleDialog({ onClose }: { readonly onClose: () => void }): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const [roleId, setRoleId] = useState('');
    const [reason, setReason] = useState('');
    const roles = useQuery({ queryKey: ['roles'], queryFn: api.roles });
    const access = useQuery({ queryKey: ['my-access'], queryFn: api.myAccess });

    const held = new Set((access.data?.roles ?? []).map((role) => role.roleId));
    const offered = (roles.data ?? []).filter(
        (role) => role.requestable && !held.has(role.id),
    );

    const ask = useMutation({
        mutationFn: () => api.askForRoles({ roleIds: [roleId], reason: reason.trim() }),
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['my-requests'] });
            onClose();
        },
    });

    return (
        <Dialog
            title={t('inbox.ask.title')}
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" onClick={onClose}>
                        {t('entity.cancel')}
                    </Button>
                    <Button
                        variant="primary"
                        disabled={roleId === '' || reason.trim() === '' || ask.isPending}
                        pending={ask.isPending}
                        onClick={() => ask.mutate()}
                    >
                        {t('inbox.ask.submit')}
                    </Button>
                </>
            }
        >
            <div className="space-y-4">
                <p className="text-sm text-ink-muted">{t('inbox.ask.lead')}</p>
                {ask.isError && <Notice tone="error">{ask.error.message}</Notice>}
                {roles.isError && <Notice tone="error">{roles.error.message}</Notice>}
                {access.isError && <Notice tone="error">{access.error.message}</Notice>}
                {(roles.isPending || access.isPending) && (
                    <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                )}
                {offered.length === 0 && !roles.isPending && !access.isPending && (
                    <p className="text-sm text-ink-muted">{t('inbox.ask.none')}</p>
                )}
                <fieldset className="space-y-1.5">
                    <legend className="mb-1.5 block text-sm font-medium text-ink-muted">
                        {t('inbox.ask.role')}
                    </legend>
                    {offered.map((role) => (
                        <label
                            key={role.id}
                            className="flex cursor-pointer items-start gap-3 rounded-md border border-line px-3 py-2.5 hover:bg-surface-hover"
                        >
                            <input
                                type="radio"
                                name="ask-role"
                                className="mt-1"
                                checked={roleId === role.id}
                                onChange={() => setRoleId(role.id)}
                            />
                            <span className="min-w-0">
                                <span className="block text-sm font-medium">
                                    {roleLabel(t, role.name)}
                                </span>
                                <span className="block text-xs text-ink-muted">
                                    {role.description}
                                </span>
                                <span className="block text-xs text-ink-faint">
                                    {t('access.lets.count', {
                                        count: String(role.permissionCodes.length),
                                    })}
                                </span>
                            </span>
                        </label>
                    ))}
                </fieldset>
                <Field label={t('inbox.ask.why')} hint={t('inbox.ask.whyHint')}>
                    <textarea
                        className="w-full rounded-md border border-line bg-surface-base px-3 py-2 text-sm text-ink placeholder:text-ink-faint hover:border-line-strong focus:border-accent focus:outline-none focus:ring-3 focus:ring-accent/20"
                        rows={4}
                        value={reason}
                        onChange={(event) => setReason(event.target.value)}
                        placeholder={t('inbox.ask.whyPlaceholder')}
                    />
                </Field>
            </div>
        </Dialog>
    );
}
