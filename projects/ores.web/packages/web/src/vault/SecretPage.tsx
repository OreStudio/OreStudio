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
import { useNavigate, useParams } from 'react-router';
import { describeSecret, removeSecret, revealSecret, type SecretReveal } from '../api/vault.js';
import { PERMISSION } from '../access/permissions.js';
import { useHeld } from '../access/holds.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { VAULT } from '../shell/paths.js';
import { Button, Detail, Dialog, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { RefreshQueries } from '../ui/RefreshButton.js';
import { formatDateTime } from '../ui/Time.js';
import { toast } from '../ui/Toasts.js';
import { WriteSecretDialog } from './WriteSecretDialog.js';

/**
 * One secret: what the vault holds about it, and the ask for its value.
 *
 * The value is not part of the record. It stays masked until somebody asks for
 * it, the ask needs the resolve permission, and it is recorded: the vault
 * records the gateway's resolve and the gateway records the person whose
 * permission let it happen. The screen says so, because a value on a screen is
 * a copy that outlives the ask.
 *
 * Change writes a new version and keeps the owner; Delete removes the value and
 * leaves the use record that describes the delete. Neither is drawn for somebody
 * who does not hold the permission each needs.
 */
export function SecretPage(): ReactNode {
    const { t, language } = useTranslation();
    const { name = '' } = useParams();
    const navigate = useNavigate();
    const holds = useHeld();
    const [revealed, setRevealed] = useState<SecretReveal>();
    const [asking, setAsking] = useState(false);
    const [askError, setAskError] = useState<string>();
    const [writing, setWriting] = useState(false);
    const [deleting, setDeleting] = useState(false);
    const [deleteError, setDeleteError] = useState<string>();

    const secret = useQuery({
        queryKey: ['vault-secret', name],
        queryFn: () => describeSecret(name),
        enabled: name !== '',
    });

    const mayReveal = holds(PERMISSION.vaultSecretsResolve);
    const mayWrite = holds(PERMISSION.vaultSecretsWrite);
    const mayDelete = holds(PERMISSION.vaultSecretsDelete);

    async function ask(): Promise<void> {
        setAskError(undefined);
        setAsking(true);
        try {
            setRevealed(await revealSecret(name));
        } catch (failure) {
            setAskError(failure instanceof Error ? failure.message : String(failure));
        } finally {
            setAsking(false);
        }
    }

    async function remove(): Promise<void> {
        setDeleteError(undefined);
        try {
            await removeSecret(name);
            toast(t('vault.secret.deleted', { name }));
            navigate(VAULT.list);
        } catch (failure) {
            setDeleteError(failure instanceof Error ? failure.message : String(failure));
        }
    }

    // The list is the area's page, so the record's trail is the area and then
    // the secret, and its name is the link back to the list.
    const trail = <AreaTrail area="vault" screen={name} />;
    const actions = (
        <div className="flex items-center gap-2">
            {mayWrite && (
                <Button size="sm" onClick={() => setWriting(true)}>
                    {t('vault.secret.change')}
                </Button>
            )}
            {mayDelete && (
                <Button size="sm" variant="danger" onClick={() => setDeleting(true)}>
                    {t('vault.secret.delete')}
                </Button>
            )}
            {revealed === undefined ? (
                mayReveal && (
                    <Button
                        size="sm"
                        variant="primary"
                        pending={asking}
                        pendingLabel={t('vault.secret.asking')}
                        onClick={() => void ask()}
                    >
                        {t('vault.secret.reveal')}
                    </Button>
                )
            ) : (
                <Button size="sm" onClick={() => setRevealed(undefined)}>
                    {t('vault.secret.hide')}
                </Button>
            )}
            <RefreshQueries keys={[['vault-secret', name]]} />
        </div>
    );

    if (secret.isError) {
        return (
            <div className="space-y-4">
                {trail}
                <PageHeader title={name} description={t('vault.secret.lead')} />
                <Notice tone="error">{secret.error.message}</Notice>
            </div>
        );
    }

    const facts = secret.data;
    const description =
        facts === undefined
            ? t('vault.secret.lead')
            : t('vault.secret.about', {
                  key: String(facts.key_version),
                  owner: facts.owner,
                  written: formatDateTime(facts.written_at, language),
              });

    return (
        <div className="space-y-4">
            <div>
                {trail}
                <PageHeader title={name} description={description} actions={actions} />
            </div>

            <section className="card">
                <header className="space-y-1 border-b border-line-soft px-4 py-3">
                    <h2 className="text-sm font-medium">{t('vault.secret.valueTitle')}</h2>
                    <p className="text-xs text-ink-faint">{t('vault.secret.valueLead')}</p>
                </header>
                <div className="space-y-3 px-4 py-4">
                    {askError !== undefined && <Notice tone="error">{askError}</Notice>}
                    {revealed === undefined ? (
                        <p className="font-mono text-sm tracking-widest text-ink-dim">
                            ••••••••••••
                        </p>
                    ) : (
                        <>
                            <div className="flex flex-wrap items-center gap-2">
                                <code className="break-all rounded-md border border-line bg-surface-base px-2 py-1 font-mono text-sm">
                                    {revealed.value}
                                </code>
                                <Tag tone="warn">{t('vault.secret.revealed')}</Tag>
                            </div>
                            <Notice tone="warn">{t('vault.secret.revealRecorded')}</Notice>
                        </>
                    )}
                    {!mayReveal && (
                        <p className="text-xs text-ink-faint">{t('vault.secret.noReveal')}</p>
                    )}
                </div>
            </section>

            <section className="card">
                <header className="space-y-1 border-b border-line-soft px-4 py-3">
                    <h2 className="text-sm font-medium">{t('vault.secret.recordTitle')}</h2>
                </header>
                <dl className="grid grid-cols-2 gap-4 px-4 py-4 sm:grid-cols-3">
                    <Detail label={t('vault.secret.owner')} value={facts?.owner ?? '—'} mono />
                    <Detail
                        label={t('vault.secret.keyVersion')}
                        value={facts === undefined ? '—' : String(facts.key_version)}
                    />
                    <Detail
                        label={t('vault.secret.written')}
                        value={
                            facts === undefined ? '—' : formatDateTime(facts.written_at, language)
                        }
                    />
                </dl>
            </section>

            {writing && facts !== undefined && (
                <WriteSecretDialog
                    name={name}
                    owner={facts.owner}
                    onClose={() => setWriting(false)}
                    onWritten={() => {
                        setWriting(false);
                        setRevealed(undefined);
                    }}
                />
            )}

            {deleting && (
                <Dialog
                    title={t('vault.secret.deleteTitle')}
                    name="vault.secret.delete"
                    onClose={() => setDeleting(false)}
                    footer={
                        <>
                            <Button size="sm" onClick={() => setDeleting(false)}>
                                {t('common.cancel')}
                            </Button>
                            <Button size="sm" variant="danger" onClick={() => void remove()}>
                                {t('vault.secret.deleteConfirm')}
                            </Button>
                        </>
                    }
                >
                    <div className="space-y-3">
                        {deleteError !== undefined && <Notice tone="error">{deleteError}</Notice>}
                        <p className="text-sm">{t('vault.secret.deleteLead', { name })}</p>
                    </div>
                </Dialog>
            )}
        </div>
    );
}
