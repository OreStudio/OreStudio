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
import type { Backlog } from '@ores/contracts';
import { useTranslation } from '../i18n/Provider.js';
import { Notice, Tag } from '../ui/Primitives.js';
import { DocPanel, docRoute } from './DocPanel.js';
import { openDoc } from './docStack.js';
import { agileQueries } from './queries.js';

/**
 * The backlog buckets, each with its captures and stories, newest first. A row
 * opens its doc inside the tab, and a link in that doc opens the next, with a
 * Back button that walks the same way back to the list. The tab never leaves
 * the page, so a list that was read stays on the screen.
 *
 * The tab names this screen, so it carries no heading of its own.
 */
export function BacklogTab(): ReactNode {
    const { t } = useTranslation();
    const backlog = useQuery(agileQueries.backlog());
    const [stack, setStack] = useState<readonly string[]>([]);

    if (backlog.isError) return <Notice tone="error">{backlog.error.message}</Notice>;
    if (backlog.isPending) return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;

    const shown = stack[stack.length - 1];
    if (shown !== undefined) {
        return (
            <DocPanel
                id={shown}
                onDocLink={(id) => setStack(openDoc(stack, id))}
                onBack={() => setStack(stack.slice(0, -1))}
            />
        );
    }
    return <Buckets backlog={backlog.data} onOpen={(id) => setStack([id])} />;
}

export function Buckets({
    backlog,
    onOpen,
}: {
    readonly backlog: Backlog;
    readonly onOpen: (id: string) => void;
}): ReactNode {
    const { t, plural } = useTranslation();
    return (
        <div className="space-y-4">
            {backlog.buckets.map((bucket) => (
                <section
                    key={bucket.id}
                    className="card"
                    aria-label={t(`development.backlog.buckets.${bucket.id}`)}
                >
                    <div className="flex items-center gap-2 px-4 py-3">
                        <h2 className="text-base font-semibold text-ink">
                            {t(`development.backlog.buckets.${bucket.id}`)}
                        </h2>
                        <Tag small>{bucket.items.length}</Tag>
                    </div>
                    {bucket.items.length === 0 ? (
                        <p className="px-4 pb-3 text-sm text-ink-muted">
                            {t('development.backlog.empty')}
                        </p>
                    ) : (
                        <table className="w-full text-sm">
                            <thead>
                                <tr className="border-y border-line text-left text-xs text-ink-faint">
                                    <th className="px-4 py-1.5 font-normal">
                                        {t('development.backlog.item')}
                                    </th>
                                    <th className="px-4 py-1.5 font-normal">
                                        {t('development.backlog.kind')}
                                    </th>
                                    <th className="px-4 py-1.5 font-normal">
                                        {t('development.backlog.created')}
                                    </th>
                                </tr>
                            </thead>
                            <tbody>
                                {bucket.items.map((item) => (
                                    <tr key={item.path} className="border-b border-line-subtle">
                                        <td className="px-4 py-1.5">
                                            {item.id === '' ? (
                                                item.title
                                            ) : (
                                                <a
                                                    className="text-accent-bright hover:underline"
                                                    href={docRoute(item.id)}
                                                    onClick={(event) => {
                                                        if (
                                                            event.metaKey ||
                                                            event.ctrlKey ||
                                                            event.shiftKey ||
                                                            event.button !== 0
                                                        ) {
                                                            return;
                                                        }
                                                        event.preventDefault();
                                                        onOpen(item.id);
                                                    }}
                                                >
                                                    {item.title}
                                                </a>
                                            )}
                                        </td>
                                        <td className="px-4 py-1.5 text-ink-muted">
                                            {item.kind === 'story'
                                                ? plural('development.backlog.story', item.tasks)
                                                : t('development.backlog.capture')}
                                        </td>
                                        <td className="px-4 py-1.5 text-ink-muted">
                                            {item.created}
                                        </td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    )}
                </section>
            ))}
        </div>
    );
}
