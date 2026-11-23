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

import { useQuery } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { Link, useSearchParams } from 'react-router';
import { api } from '../api/client.js';
import { agentGrants, isStamped, type AgentGrant, type AgentKey } from '../api/agentGrants.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { PATHS } from '../shell/paths.js';
import { Button, Notice, PageHeader, Select, Tag } from '../ui/Primitives.js';
import { RefreshNow } from '../ui/RefreshButton.js';
import { useHolds } from './holds.js';
import { PERMISSION } from './permissions.js';

const AGENT_GRANT_STATES = ['active', 'ended', 'all'] as const;
type AgentGrantState = (typeof AGENT_GRANT_STATES)[number];

function grantIsActive(grant: AgentGrant, now: number): boolean {
    return !isStamped(grant.revoked_at) && Date.parse(grant.not_after) > now;
}

/**
 * Every LLM agent grant in the administrator's parties, with its keys.
 *
 * Read with iam::agent_grants:read; a holder of iam::agent_grants:revoke may
 * revoke a grant or a key, after a warning that it stops the agent at once. A
 * key's secret and its hash are never read.
 */
export function AgentGrantsAdminPage(): ReactNode {
    const { t } = useTranslation();
    const holds = useHolds();
    const mayRevoke = holds(PERMISSION.agentGrantsRevoke);
    const [state, setState] = useState<AgentGrantState>('active');
    const [person, setPerson] = useState('');
    const [busy, setBusy] = useState(false);
    const [failure, setFailure] = useState('');

    const grants = useQuery({
        queryKey: ['agent-grants-all'],
        queryFn: () => agentGrants.listAll(),
        retry: false,
    });
    const accounts = useQuery({
        queryKey: ['agent-grants-accounts'],
        queryFn: () => api.accounts(),
    });
    const roles = useQuery({ queryKey: ['agent-grants-roles'], queryFn: () => api.roles() });

    const nameOf = new Map<string, string>(
        (accounts.data?.accounts ?? []).map((a) => [a.id, a.username] as const),
    );
    const roleOf = new Map<string, string>((roles.data ?? []).map((r) => [r.id, r.name] as const));
    const now = Date.now();
    const [params] = useSearchParams();
    const only = params.get('grant') ?? '';
    const rows = (grants.data?.grants ?? [])
        .filter((g) => only === '' || g.id === only)
        .filter((g) => person === '' || g.grantor_account_id === person)
        .filter((g) => (state === 'all' ? true : grantIsActive(g, now) === (state === 'active')));
    const people = [...new Set((grants.data?.grants ?? []).map((g) => g.grantor_account_id))];

    const revoke = async (work: () => Promise<void>): Promise<void> => {
        if (!window.confirm(t('agentGrantsAdmin.revokeConfirm'))) return;
        setBusy(true);
        try {
            await work();
            setFailure('');
        } catch (error) {
            setFailure(error instanceof Error ? error.message : t('agentGrantsAdmin.revokeFailed'));
        } finally {
            setBusy(false);
        }
        await grants.refetch();
    };

    return (
        <div className="space-y-6">
            <AreaTrail area="operations" screen={t('agentGrantsAdmin.title')} />
            <PageHeader
                title={t('agentGrantsAdmin.title')}
                description={t('agentGrantsAdmin.description')}
                actions={<RefreshNow onClick={() => void grants.refetch()} />}
            />
            <section className="card space-y-4 p-6">
                <div className="flex flex-wrap gap-3">
                    <label className="flex flex-col gap-1 text-xs text-ink-muted">
                        {t('agentGrantsAdmin.person')}
                        <Select value={person} onChange={(e) => setPerson(e.target.value)}>
                            <option value="">{t('agentGrantsAdmin.everyone')}</option>
                            {people.map((id) => (
                                <option key={id} value={id}>
                                    {nameOf.get(id) ?? id}
                                </option>
                            ))}
                        </Select>
                    </label>
                    <label className="flex flex-col gap-1 text-xs text-ink-muted">
                        {t('agentGrantsAdmin.state')}
                        <Select
                            value={state}
                            onChange={(e) => setState(e.target.value as AgentGrantState)}
                        >
                            {AGENT_GRANT_STATES.map((option) => (
                                <option key={option} value={option}>
                                    {t(`agentGrantsAdmin.states.${option}`)}
                                </option>
                            ))}
                        </Select>
                    </label>
                </div>
                {failure !== '' && <Notice tone="error">{failure}</Notice>}
                {grants.isPending ? (
                    <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                ) : grants.isError ? (
                    <Notice tone="error">{grants.error.message}</Notice>
                ) : rows.length === 0 ? (
                    <p className="text-sm text-ink-muted">{t('agentGrantsAdmin.empty')}</p>
                ) : (
                    <table className="w-full text-left text-sm">
                        <thead className="text-xs text-ink-faint">
                            <tr>
                                <th className="py-1 font-normal">
                                    {t('agentGrantsAdmin.column.person')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('agentGrantsAdmin.column.agent')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('agentGrantsAdmin.column.role')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('agentGrantsAdmin.column.until')}
                                </th>
                                <th className="py-1 font-normal">
                                    {t('agentGrantsAdmin.column.keys')}
                                </th>
                                <th className="py-1 font-normal" />
                            </tr>
                        </thead>
                        <tbody className="divide-y divide-line-subtle">
                            {rows.map((grant) => (
                                <GrantRow
                                    key={grant.id}
                                    grant={grant}
                                    keys={(grants.data?.keys ?? []).filter(
                                        (k) => k.grant_id === grant.id,
                                    )}
                                    person={
                                        nameOf.get(grant.grantor_account_id) ??
                                        grant.grantor_account_id
                                    }
                                    role={roleOf.get(grant.role_id) ?? grant.role_id}
                                    active={grantIsActive(grant, now)}
                                    mayRevoke={mayRevoke}
                                    busy={busy}
                                    onRevokeGrant={() =>
                                        void revoke(() =>
                                            agentGrants.revokeGrant(
                                                grant.id,
                                                'Revoked by an administrator',
                                            ),
                                        )
                                    }
                                    onRevokeKey={(id) =>
                                        void revoke(() =>
                                            agentGrants.revokeKey(
                                                id,
                                                'Revoked by an administrator',
                                            ),
                                        )
                                    }
                                />
                            ))}
                        </tbody>
                    </table>
                )}
            </section>
        </div>
    );
}

function GrantRow({
    grant,
    keys,
    person,
    role,
    active,
    mayRevoke,
    busy,
    onRevokeGrant,
    onRevokeKey,
}: {
    readonly grant: AgentGrant;
    readonly keys: readonly AgentKey[];
    readonly person: string;
    readonly role: string;
    readonly active: boolean;
    readonly mayRevoke: boolean;
    readonly busy: boolean;
    readonly onRevokeGrant: () => void;
    readonly onRevokeKey: (keyId: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const liveKeys = keys.filter((k) => !isStamped(k.revoked_at));
    return (
        <tr className="align-top">
            <td className="py-2">{person}</td>
            <td className="py-2">
                <Tag>{grant.agent_name}</Tag>
                {!active && (
                    <span className="ml-2 text-xs text-ink-faint">
                        {t('agentGrantsAdmin.ended')}
                    </span>
                )}
            </td>
            <td className="py-2 font-mono text-xs">{role}</td>
            <td className="py-2 font-mono text-xs">{grant.not_after}</td>
            <td className="py-2">
                <ul className="space-y-1 text-xs">
                    {liveKeys.map((key) => (
                        <li key={key.id} className="flex items-center gap-2">
                            <span className="font-mono text-ink-muted">{key.id}</span>
                            {mayRevoke && active && (
                                <Button
                                    size="sm"
                                    variant="secondary"
                                    disabled={busy}
                                    onClick={() => onRevokeKey(key.id)}
                                >
                                    {t('agentGrantsAdmin.revokeKey')}
                                </Button>
                            )}
                        </li>
                    ))}
                </ul>
            </td>
            <td className="space-y-1 py-2 text-right">
                <Link
                    className="block text-xs text-accent"
                    to={`${PATHS.signIns}?grant=${encodeURIComponent(grant.id)}`}
                >
                    {t('agentGrantsAdmin.sessions')}
                </Link>
                {mayRevoke && active && (
                    <Button size="sm" variant="secondary" disabled={busy} onClick={onRevokeGrant}>
                        {t('agentGrantsAdmin.revokeGrant')}
                    </Button>
                )}
            </td>
        </tr>
    );
}
