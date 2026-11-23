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
import type { InboxRequestView } from '@ores/wire-protocol/browser';
import { agentGrants } from '../api/agentGrants.js';
import { useTranslation } from '../i18n/Provider.js';
import { Detail, Notice, Tag } from '../ui/Primitives.js';

/**
 * What only an agent's write shows: the agent that made it, the call it made
 * for the person, and what the call answered once it was replayed.
 *
 * The person decides on what their agent asked to do, so the call is drawn as
 * it was made: its method, its address and its body. Approving replays it as
 * the person, and the write then meets whatever approval their own would.
 */
export function AgentWriteExtras({ request }: { readonly request: InboxRequestView }): ReactNode {
    const { t } = useTranslation();
    const held = useQuery({
        queryKey: ['agent-held-write', request.id],
        queryFn: () => agentGrants.heldWrite(request.id),
        retry: false,
    });
    if (held.isPending) return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    if (held.isError) return <Notice tone="error">{held.error.message}</Notice>;
    const { write, agent_name } = held.data;
    let body = write.body;
    try {
        body = JSON.stringify(JSON.parse(write.body), null, 2);
    } catch {
        // A body that is not JSON is drawn as the agent sent it.
    }
    return (
        <section className="space-y-3">
            <Notice tone="warn">{t('inbox.agentWrite.lead', { agent: agent_name })}</Notice>
            <dl className="grid gap-2 sm:grid-cols-2">
                <Detail label={t('inbox.agentWrite.agent')} value={<Tag>{agent_name}</Tag>} />
                <Detail
                    label={t('inbox.agentWrite.call')}
                    value={`${write.method} ${write.target}`}
                    mono
                />
                <Detail
                    label={t('inbox.agentWrite.state')}
                    value={t(`inbox.agentWrite.states.${write.state}`)}
                />
                {write.outcome !== '' && (
                    <Detail label={t('inbox.agentWrite.outcome')} value={write.outcome} />
                )}
            </dl>
            {body !== '' && (
                <pre className="max-h-80 overflow-auto rounded-md bg-surface-sunken p-3 font-mono text-xs">
                    {body}
                </pre>
            )}
        </section>
    );
}
