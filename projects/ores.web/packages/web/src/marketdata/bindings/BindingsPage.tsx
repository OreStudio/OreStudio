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

import { type ReactNode, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { useNavigate } from 'react-router';
import { useTranslation } from '../../i18n/Provider.js';
import { bindings } from '../../api/bindings.js';
import { RecordList, useRecordSource, type ListFilter } from '../../refdata/RecordList.js';
import { RecordDialog, show } from '../../refdata/records.js';
import { RecordStoreProvider, syntheticStore } from '../../refdata/store.js';
import { useSeriesTitle } from '../../oresmd/useSeriesTitle.js';
import { useAreaRoot } from '../../shell/AreaTrail.js';
import { Tag } from '../../ui/Primitives.js';
import { RelativeTime } from '../../ui/Time.js';
import { SYNTHETIC, heldBy, isSynthetic } from './bindingView.js';
import { BINDINGS, BINDING_WATCHES, VENDOR, bindingPath } from './bindingPaths.js';
import { BINDING_DIALOG_FIELDS } from './BindingPage.js';
import { StatusTag } from './StatusTag.js';
import { useBindingFacts } from './useBindings.js';

/** The party's feed bindings: one page at a time, searched, filtered by kind and sorted on the server. */
export function BindingsPage(): ReactNode {
    return (
        <RecordStoreProvider store={syntheticStore}>
            <BindingsList />
        </RecordStoreProvider>
    );
}

function BindingsList(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const titled = useSeriesTitle();
    const facts = useBindingFacts();
    const [adding, setAdding] = useState(false);
    const candidates = useQuery({
        queryKey: ['bindings', 'sources'],
        queryFn: bindings.sources,
    });
    const base = useRecordSource(BINDINGS);
    const filters: readonly ListFilter[] = [
        {
            kind: 'choice',
            id: 'producer_kind',
            label: t('refdata.fields.producer_kind'),
            all: t('marketdata.bindings.allKinds'),
            choices: [SYNTHETIC, VENDOR].map((value) => ({
                value,
                label: t(`refdata.choices.producer_kind.${value}`),
            })),
        },
    ];
    return (
        <>
            <RecordList
                source={{
                    ...base,
                    filters,
                    watches: BINDING_WATCHES,
                    recordKey: (row) => ({ source_name: show(row['source_name']) }),
                }}
                title={t('marketdata.bindings.title')}
                lead={t('marketdata.bindings.lead')}
                crumbs={[...useAreaRoot('marketdata'), { label: t('marketdata.bindings.title') }]}
                pathOf={(row) => bindingPath(show(row['id']))}
                addLabel={t('marketdata.bindings.add')}
                onAdd={() => setAdding(true)}
                // The output the binding writes leads; the source it consumes follows.
                columns={[
                    {
                        id: 'series',
                        header: t('marketdata.bindings.series'),
                        cell: (row) => {
                            const listed = facts.seriesOf(show(row['source_name']));
                            return listed === undefined ? (
                                <span className="text-ink-faint">{'\u2014'}</span>
                            ) : (
                                titled(listed.identity)
                            );
                        },
                    },
                    {
                        id: 'source_name',
                        header: t('refdata.fields.source_name'),
                        cell: (row) => show(row['source_name']),
                        mono: true,
                        sort: 'source_name',
                    },
                    {
                        id: 'producer_kind',
                        header: t('refdata.fields.producer_kind'),
                        cell: (row) => (
                            <Tag tone={isSynthetic(show(row['producer_kind'])) ? 'accent' : 'warn'}>
                                {t(`refdata.choices.producer_kind.${show(row['producer_kind'])}`)}
                            </Tag>
                        ),
                    },
                    {
                        id: 'enabled',
                        header: t('refdata.fields.enabled'),
                        cell: (row) =>
                            row['enabled'] === true ? (
                                <Tag tone="up">{t('refdata.records.yes')}</Tag>
                            ) : (
                                <Tag tone="muted">{t('refdata.records.no')}</Tag>
                            ),
                        sort: 'enabled',
                    },
                    {
                        id: 'status',
                        header: t('marketdata.bindings.status'),
                        cell: (row) => (
                            <StatusTag
                                status={facts.statusOf({
                                    source_name: show(row['source_name']),
                                    producer_kind: show(row['producer_kind']),
                                    enabled: row['enabled'] === true,
                                })}
                            />
                        ),
                    },
                    {
                        id: 'last_tick',
                        header: t('marketdata.bindings.lastTick'),
                        cell: (row) => {
                            const at = facts.lastTickOf(show(row['source_name']));
                            return at === null ? (
                                <span className="text-ink-faint">{'\u2014'}</span>
                            ) : (
                                <RelativeTime at={at} />
                            );
                        },
                    },
                ]}
            />
            {adding && (
                <RecordDialog
                    title={t('marketdata.bindings.addTitle')}
                    resource={BINDINGS}
                    specs={BINDING_DIALOG_FIELDS}
                    row={undefined}
                    given={{ id: crypto.randomUUID() }}
                    check={(values) => {
                        const held = heldBy(candidates.data, values['source_name'] ?? '');
                        return held === undefined
                            ? []
                            : [t('marketdata.bindings.held', { source: held })];
                    }}
                    onClose={() => setAdding(false)}
                    onSaved={(write) => void navigate(bindingPath(show(write['id'])))}
                />
            )}
        </>
    );
}
