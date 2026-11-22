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

import { useCallback, useEffect, useState, type ReactNode } from 'react';
import { agentGrants, isStamped, type AgentGrant, type AgentKey } from '../api/agentGrants.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Field, Input, Notice, Tag } from '../ui/Primitives.js';
import { RefreshNow } from '../ui/RefreshButton.js';

/**
 * The person's own LLM agents: the grants they made, each grant's keys, and the
 * form that makes a grant. A key is shown once, when it is made, and never
 * again; the person copies it into the agent's configuration.
 */
export function AgentGrantsPanel(): ReactNode {
    const { t } = useTranslation();
    const [grants, setGrants] = useState<readonly AgentGrant[]>([]);
    const [keys, setKeys] = useState<readonly AgentKey[]>([]);
    const [failure, setFailure] = useState('');
    const [shownKey, setShownKey] = useState('');
    const [busy, setBusy] = useState(false);
    const [agentName, setAgentName] = useState('');
    const [role, setRole] = useState('AgentReader');
    const [validDays, setValidDays] = useState(30);

    const load = useCallback(async () => {
        try {
            const read = await agentGrants.listOwn();
            setGrants(read.grants);
            setKeys(read.keys);
            setFailure('');
        } catch (error) {
            setFailure(error instanceof Error ? error.message : t('agents.readFailed'));
        }
    }, [t]);

    useEffect(() => {
        void load();
    }, [load]);

    const act = async (work: () => Promise<string | void>): Promise<void> => {
        setBusy(true);
        setShownKey('');
        try {
            const key = await work();
            if (typeof key === 'string') setShownKey(key);
            setFailure('');
            await load();
        } catch (error) {
            setFailure(error instanceof Error ? error.message : t('agents.actFailed'));
        }
        setBusy(false);
    };

    const active = grants.filter((grant) => !isStamped(grant.revoked_at));

    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('agents.title')}</h2>
                <RefreshNow onClick={() => void load()} />
            </header>
            <p className="text-sm text-ink-muted">{t('agents.lead')}</p>
            {failure !== '' && <Notice tone="error">{failure}</Notice>}
            {shownKey !== '' && (
                <Notice tone="warn">
                    <p>{t('agents.keyOnce')}</p>
                    <code className="mt-2 block break-all font-mono text-xs">{shownKey}</code>
                </Notice>
            )}

            {active.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('agents.empty')}</p>
            ) : (
                <ul className="divide-y divide-line-subtle">
                    {active.map((grant) => {
                        const grantKeys = keys.filter(
                            (key) => key.grant_id === grant.id && !isStamped(key.revoked_at),
                        );
                        return (
                            <li key={grant.id} className="space-y-2 py-3">
                                <div className="flex flex-wrap items-center justify-between gap-2">
                                    <div className="flex items-center gap-2">
                                        <Tag>{grant.agent_name}</Tag>
                                        <span className="text-xs text-ink-faint">
                                            {t('agents.until', { at: grant.not_after })}
                                        </span>
                                    </div>
                                    <div className="flex gap-2">
                                        <Button
                                            size="sm"
                                            variant="secondary"
                                            disabled={busy}
                                            onClick={() =>
                                                void act(() => agentGrants.createKey(grant.id))
                                            }
                                        >
                                            {t('agents.newKey')}
                                        </Button>
                                        <Button
                                            size="sm"
                                            variant="secondary"
                                            disabled={busy}
                                            onClick={() =>
                                                window.confirm(t('agents.revokeGrantConfirm')) &&
                                                void act(() =>
                                                    agentGrants.revokeGrant(
                                                        grant.id,
                                                        'Revoked by its person',
                                                    ),
                                                )
                                            }
                                        >
                                            {t('agents.revokeGrant')}
                                        </Button>
                                    </div>
                                </div>
                                <ul className="space-y-1 pl-4 text-xs">
                                    {grantKeys.map((key) => (
                                        <li
                                            key={key.id}
                                            className="flex items-center justify-between gap-2"
                                        >
                                            <span className="font-mono text-ink-muted">
                                                {t('agents.key', {
                                                    id: key.id,
                                                    at: key.recorded_at,
                                                })}
                                            </span>
                                            <Button
                                                size="sm"
                                                variant="secondary"
                                                disabled={busy}
                                                onClick={() =>
                                                    void act(() =>
                                                        agentGrants.revokeKey(
                                                            key.id,
                                                            'Revoked by its person',
                                                        ),
                                                    )
                                                }
                                            >
                                                {t('agents.revokeKey')}
                                            </Button>
                                        </li>
                                    ))}
                                </ul>
                            </li>
                        );
                    })}
                </ul>
            )}

            <form
                className="grid gap-3 sm:grid-cols-4 sm:items-end"
                onSubmit={(event) => {
                    event.preventDefault();
                    void act(() => agentGrants.create(agentName.trim(), role.trim(), validDays));
                }}
            >
                <Field label={t('agents.name')}>
                    <Input
                        value={agentName}
                        maxLength={64}
                        required
                        onChange={(event) => setAgentName(event.target.value)}
                    />
                </Field>
                <Field label={t('agents.role')}>
                    <Input
                        value={role}
                        required
                        onChange={(event) => setRole(event.target.value)}
                    />
                </Field>
                <Field label={t('agents.days')}>
                    <Input
                        type="number"
                        min={1}
                        max={90}
                        value={validDays}
                        onChange={(event) => setValidDays(Number(event.target.value))}
                    />
                </Field>
                <Button type="submit" disabled={busy || agentName.trim() === ''}>
                    {t('agents.create')}
                </Button>
            </form>
        </section>
    );
}
