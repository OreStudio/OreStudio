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
import { useState, type ReactNode } from 'react';
import { listConsumers, writeSecret } from '../api/vault.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Dialog, Field, Input, Notice, Select } from '../ui/Primitives.js';
import { toast } from '../ui/Toasts.js';

/**
 * The dialog that stores one version of a secret.
 *
 * The owner is the consumer the vault seals to and the only one that may ask for
 * the value back, which is why it is chosen here and not guessed: the list of
 * owners is the vault's own list of consumers, narrowed to the ones it enabled.
 *
 * The value is sealed by the gateway to the vault's transport key before it is
 * forwarded, so the bus carries ciphertext and the vault never accepts a plain
 * value. The person's value does cross the gateway, over the session's own
 * connection, which is what makes a browser able to write at all.
 */
export function WriteSecretDialog({
    name: fixedName,
    owner: fixedOwner,
    onClose,
    onWritten,
}: {
    /** The secret being changed, or nothing when a secret is added. */
    readonly name?: string;
    /** The owner the secret has, for a change that keeps it. */
    readonly owner?: string;
    readonly onClose: () => void;
    readonly onWritten: (name: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const changing = fixedName !== undefined;
    const [name, setName] = useState(fixedName ?? '');
    const [owner, setOwner] = useState(fixedOwner ?? '');
    const [value, setValue] = useState('');
    const [saving, setSaving] = useState(false);
    const [error, setError] = useState<string>();
    const consumers = useQuery({
        queryKey: ['vault-consumers', 0],
        queryFn: () => listConsumers(0, 200),
    });
    const owners = (consumers.data?.consumers ?? []).filter((consumer) => consumer.is_enabled);
    const chosen = owner !== '' ? owner : (owners[0]?.name ?? '');

    async function save(): Promise<void> {
        setError(undefined);
        if (name.trim() === '') {
            setError(t('vault.write.nameRequired'));
            return;
        }
        if (chosen === '') {
            setError(t('vault.write.ownerRequired'));
            return;
        }
        if (value === '') {
            setError(t('vault.write.valueRequired'));
            return;
        }
        setSaving(true);
        try {
            const written = await writeSecret({ name: name.trim(), owner: chosen, value });
            toast(t('vault.write.written', { name: written.name }));
            onWritten(written.name);
        } catch (failure) {
            setError(failure instanceof Error ? failure.message : String(failure));
        } finally {
            setSaving(false);
        }
    }

    return (
        <Dialog
            title={changing ? t('vault.write.changeTitle') : t('vault.write.addTitle')}
            name={changing ? 'vault.secret.change' : 'vault.secret.add'}
            onClose={onClose}
            footer={
                <>
                    <Button type="button" size="sm" onClick={onClose}>
                        {t('common.cancel')}
                    </Button>
                    <Button
                        type="button"
                        size="sm"
                        variant="primary"
                        pending={saving}
                        onClick={() => void save()}
                    >
                        {t('vault.write.save')}
                    </Button>
                </>
            }
        >
            <div className="space-y-3">
                {error !== undefined && <Notice tone="error">{error}</Notice>}
                <Field
                    label={t('vault.write.name')}
                    {...(changing ? {} : { hint: t('vault.write.nameHint') })}
                >
                    <Input
                        value={name}
                        readOnly={changing}
                        onChange={(event) => setName(event.target.value)}
                        placeholder="db.marketdata_service.password"
                        autoComplete="off"
                    />
                </Field>
                <Field
                    label={t('vault.write.owner')}
                    hint={t('vault.write.ownerHint')}
                    {...(!consumers.isPending && owners.length === 0
                        ? { error: t('vault.write.noOwner') }
                        : {})}
                >
                    <Select
                        value={chosen}
                        onChange={(event) => setOwner(event.target.value)}
                        disabled={owners.length === 0}
                    >
                        {owners.length === 0 && (
                            <option value="">{t('vault.write.noOwner')}</option>
                        )}
                        {owners.map((consumer) => (
                            <option key={consumer.id} value={consumer.name}>
                                {consumer.name}
                            </option>
                        ))}
                    </Select>
                </Field>
                <Field label={t('vault.write.value')} hint={t('vault.write.valueHint')}>
                    <Input
                        type="password"
                        value={value}
                        onChange={(event) => setValue(event.target.value)}
                        autoComplete="new-password"
                    />
                </Field>
                <p className="text-xs text-ink-faint">{t('vault.write.sealed')}</p>
            </div>
        </Dialog>
    );
}
