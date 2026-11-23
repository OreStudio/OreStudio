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
import type { ReactNode } from 'react';
import { useSearchParams } from 'react-router';
import { agentGrants } from '../api/agentGrants.js';
import { useTranslation } from '../i18n/Provider.js';
import { Notice, Select, Tag } from '../ui/Primitives.js';
import { RefreshNow } from '../ui/RefreshButton.js';

/**
 * What the reader's own LLM agents did: every call, reads and writes, newest
 * first, with the agent that made it and how it was answered. A record an
 * agent changed also says so in its own history.
 */
export function AgentActivityPanel(): ReactNode {
    const { t } = useTranslation();
    const [params, setParams] = useSearchParams();
    const agent = params.get('agent') ?? '';
    const calls = useQuery({
        queryKey: ['agent-activity'],
        queryFn: () => agentGrants.listActivity(),
        retry: false,
    });
    const grants = useQuery({
        queryKey: ['agent-grants-own'],
        queryFn: () => agentGrants.listOwn(),
    });
    const nameOf = new Map((grants.data?.grants ?? []).map((g) => [g.id, g.agent_name] as const));
    const shown = (calls.data ?? []).filter((c) => agent === '' || c.grant_id === agent);

    return (
        <section id="agent-activity" className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('agents.activity.title')}</h2>
                <div className="flex items-center gap-3">
                    <label className="flex items-center gap-2 text-xs text-ink-muted">
                        {t('agents.activity.agent')}
                        <Select
                            value={agent}
                            onChange={(e) => {
                                const next = new URLSearchParams(params);
                                if (e.target.value === '') next.delete('agent');
                                else next.set('agent', e.target.value);
                                setParams(next);
                            }}
                        >
                            <option value="">{t('agents.activity.every')}</option>
                            {[...nameOf].map(([id, name]) => (
                                <option key={id} value={id}>
                                    {name}
                                </option>
                            ))}
                        </Select>
                    </label>
                    <RefreshNow onClick={() => void calls.refetch()} />
                </div>
            </header>
            {calls.isPending ? (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            ) : calls.isError ? (
                <Notice tone="error">{calls.error.message}</Notice>
            ) : shown.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('agents.activity.empty')}</p>
            ) : (
                <table className="w-full text-left text-sm">
                    <thead className="text-xs text-ink-faint">
                        <tr>
                            <th className="py-1 font-normal">{t('agents.activity.when')}</th>
                            <th className="py-1 font-normal">{t('agents.activity.agent')}</th>
                            <th className="py-1 font-normal">{t('agents.activity.call')}</th>
                            <th className="py-1 font-normal">{t('agents.activity.answer')}</th>
                            <th className="py-1 text-right font-normal">
                                {t('agents.activity.took')}
                            </th>
                        </tr>
                    </thead>
                    <tbody className="divide-y divide-line-subtle">
                        {shown.map((call) => (
                            <tr key={call.id}>
                                <td className="py-2 font-mono text-xs whitespace-nowrap">
                                    {call.called_at}
                                </td>
                                <td className="py-2">
                                    <Tag>{nameOf.get(call.grant_id) ?? call.grant_id}</Tag>
                                </td>
                                <td className="py-2 font-mono text-xs">
                                    {call.method} {call.route}
                                </td>
                                <td className="py-2 font-mono text-xs">{String(call.status)}</td>
                                <td className="py-2 text-right font-mono text-xs text-ink-faint">
                                    {t('agents.activity.ms', { ms: String(call.duration_ms) })}
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            )}
        </section>
    );
}
