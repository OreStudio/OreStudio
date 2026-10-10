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

import { useQueries, useQuery } from '@tanstack/react-query';
import { useMemo, type ReactNode } from 'react';
import type { ReportingTreeNode } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { RelativeTime } from '../ui/Time.js';
import { lineChanges, type LineChange } from './lineHistory.js';
import { NodeAvatar, nameOf } from './NodeParts.js';

/** How many recently changed accounts are looked into for a change of their line. */
const CANDIDATES = 25;

interface Changed {
    readonly node: ReportingTreeNode;
    readonly latest: LineChange;
}

/**
 * The people whose reporting line changed recently, newest change first.
 *
 * The tenant has no read of changes across accounts, so the accounts changed
 * most recently are looked into, a few at a time, and kept when one of their
 * versions moved the manager. Choosing a person shows their line's history.
 * The accounts are read already by the screens around, and each person's story
 * is the same read their own page makes, so nothing is read twice.
 */
export function RecentLineChanges({
    nodes,
    nameFor,
    selectedId,
    onSelect,
}: {
    readonly nodes: readonly ReportingTreeNode[];
    readonly nameFor: (accountId: string | null) => string;
    readonly selectedId: string;
    readonly onSelect: (accountId: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const accounts = useQuery({
        queryKey: ['accounts'],
        queryFn: api.accounts,
        meta: { quiet: true },
    });
    const candidates = useMemo(
        () =>
            [...(accounts.data?.accounts ?? [])]
                .filter((account) => account.version > 1)
                .sort((a, b) => b.recordedAt.localeCompare(a.recordedAt))
                .slice(0, CANDIDATES),
        [accounts.data],
    );
    const stories = useQueries({
        queries: candidates.map((account) => ({
            queryKey: ['timeline', 'person', account.username],
            queryFn: () => api.timeline('person', account.username),
            retry: false,
            meta: { quiet: true },
        })),
    });
    const changed: Changed[] = candidates.flatMap((account, index) => {
        const node = nodes.find((candidate) => candidate.accountId === account.id);
        const latest = lineChanges(stories[index]?.data, nameFor)[0];
        return node === undefined || latest === undefined ? [] : [{ node, latest }];
    });
    changed.sort((a, b) => b.latest.at.localeCompare(a.latest.at));
    const reading = accounts.isPending || stories.some((story) => story.isPending);

    return (
        <section className="card">
            <h2 className="border-b border-line px-4 py-3 text-sm font-semibold">
                {t('membership.reporting.recentChanges')}
            </h2>
            {changed.length === 0 ? (
                <p className="px-4 py-3 text-sm text-ink-muted">
                    {reading ? t('common.loading') : t('membership.reporting.noRecentChanges')}
                </p>
            ) : (
                <ul>
                    {changed.map(({ node, latest }) => (
                        <li
                            key={node.accountId}
                            className="border-t border-line-subtle first:border-t-0"
                        >
                            <button
                                type="button"
                                aria-pressed={node.accountId === selectedId}
                                onClick={() => onSelect(node.accountId)}
                                className={`flex w-full items-center gap-3 px-4 py-2 text-left hover:bg-surface-overlay ${
                                    node.accountId === selectedId ? 'bg-accent/10' : ''
                                }`}
                            >
                                <NodeAvatar node={node} size="sm" />
                                <span className="min-w-0 flex-1">
                                    <span className="block text-sm font-medium">
                                        {nameOf(node)}
                                    </span>
                                    <span className="block truncate text-xs text-ink-muted">
                                        {latest.from} → {latest.to}
                                    </span>
                                </span>
                                <span className="text-xs text-ink-faint">
                                    <RelativeTime at={latest.at} />
                                </span>
                            </button>
                        </li>
                    ))}
                </ul>
            )}
        </section>
    );
}
