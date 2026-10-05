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
import { Link } from 'react-router';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { Input, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { Crumbs, TOPICS, classificationsPath } from './shared.js';

/**
 * The classification lists, found by topic or by name.
 *
 * Each topic is a card; each list in it shows how many rows it holds, and the
 * lists of ORE spellings say they are read-only before anyone opens them.
 */
export function ClassificationsPage(): ReactNode {
    const { t } = useTranslation();
    const [search, setSearch] = useState('');
    const lists = useQuery({ queryKey: ['classifications'], queryFn: api.classificationLists });

    if (lists.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (lists.isError) {
        return <Notice tone="error">{lists.error.message}</Notice>;
    }
    const wanted = search.trim().toLowerCase();
    const named = lists.data.map((list) => ({
        ...list,
        label: t(`refdata.classifications.lists.${list.key}`),
    }));
    const cards = TOPICS.map((topic) => ({
        topic,
        lists: named.filter(
            (list) =>
                list.topic === topic &&
                (wanted === '' || list.label.toLowerCase().includes(wanted)),
        ),
    })).filter((card) => card.lists.length > 0);

    return (
        <div className="space-y-6">
            <div>
                <Crumbs
                    parts={[
                        { label: t('refdata.area.title'), to: '/refdata' },
                        { label: t('refdata.classifications.title') },
                    ]}
                />
                <PageHeader
                    title={t('refdata.classifications.title')}
                    description={t('refdata.classifications.lead')}
                />
            </div>
            <Input
                type="search"
                value={search}
                placeholder={t('refdata.classifications.find')}
                aria-label={t('refdata.classifications.find')}
                onChange={(event) => setSearch(event.target.value)}
            />
            {cards.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('refdata.classifications.noneFound')}</p>
            ) : (
                <div className="grid items-start gap-4 sm:grid-cols-2 lg:grid-cols-3">
                    {cards.map((card) => (
                        <section key={card.topic} className="card overflow-hidden">
                            <h2 className="px-4 pt-3 pb-1 text-xs font-semibold tracking-wide text-ink-faint uppercase">
                                {t(`refdata.classifications.topics.${card.topic}`)}
                            </h2>
                            <ul>
                                {card.lists.map((list) => (
                                    <li
                                        key={list.key}
                                        className="border-t border-line-subtle first:border-t-0"
                                    >
                                        <Link
                                            to={classificationsPath(list.key)}
                                            className="flex items-center gap-2 px-4 py-2 text-sm hover:bg-surface-hover"
                                        >
                                            <span className="flex-1">{list.label}</span>
                                            {!list.editable && (
                                                <Tag tone="muted">
                                                    {t('refdata.classifications.readOnlyTag')}
                                                </Tag>
                                            )}
                                            <span className="text-xs text-ink-faint tabular-nums">
                                                {list.count ?? '—'}
                                            </span>
                                        </Link>
                                    </li>
                                ))}
                            </ul>
                        </section>
                    ))}
                </div>
            )}
        </div>
    );
}
