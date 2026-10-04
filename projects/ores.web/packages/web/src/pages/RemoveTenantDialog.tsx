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

import { useMutation, useQueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { useAccessLifetimeSeconds } from '../session/SessionProvider.js';
import { Button, Dialog, Field, Input, Notice } from '../ui/Primitives.js';

/**
 * Asks before a tenant is removed, and says what removal does.
 *
 * The words state what the server does: sign-in refuses the tenant, token
 * refresh refuses a session already open in it, so those people lose access
 * within one access lifetime, and the tenant's data is kept. The person types
 * the tenant's code, because removal is not undone from here and a click on
 * the wrong row is the mistake this guards against.
 */
export function RemoveTenantDialog({
    tenant,
    onClose,
    onRemoved,
}: {
    readonly tenant: { readonly code: string; readonly name: string };
    readonly onClose: () => void;
    readonly onRemoved: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const lifetime = useAccessLifetimeSeconds();
    const [typed, setTyped] = useState('');
    const removal = useMutation({
        mutationFn: () => api.removeTenant(tenant.code, typed),
        onSuccess: async () => {
            await Promise.all([
                queries.invalidateQueries({ queryKey: ['tenants'] }),
                queries.invalidateQueries({ queryKey: ['overview'] }),
            ]);
            onRemoved();
        },
    });
    const confirmed = typed === tenant.code;

    return (
        <Dialog
            title={t('tenants.remove.title', { name: tenant.name })}
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" onClick={onClose} disabled={removal.isPending}>
                        {t('entity.cancel')}
                    </Button>
                    <Button
                        variant="danger"
                        onClick={() => removal.mutate()}
                        disabled={!confirmed || removal.isPending}
                    >
                        {t('tenants.remove.confirm')}
                    </Button>
                </>
            }
        >
            <div className="space-y-4 text-sm">
                <ul className="list-disc space-y-1 pl-5 text-ink-muted">
                    <li>{t('tenants.remove.noSignIn')}</li>
                    <li>
                        {lifetime === undefined
                            ? t('tenants.remove.signedInSoon')
                            : t('tenants.remove.signedIn', {
                                  minutes: String(Math.max(1, Math.round(lifetime / 60))),
                              })}
                    </li>
                    <li>{t('tenants.remove.dataKept')}</li>
                    <li>{t('tenants.remove.noUndo')}</li>
                </ul>
                <Field label={t('tenants.remove.typeCode', { code: tenant.code })}>
                    <Input
                        value={typed}
                        onChange={(event) => setTyped(event.target.value)}
                        autoComplete="off"
                        spellCheck={false}
                        className="font-mono"
                    />
                </Field>
                {removal.isError && <Notice tone="error">{removal.error.message}</Notice>}
            </div>
        </Dialog>
    );
}
