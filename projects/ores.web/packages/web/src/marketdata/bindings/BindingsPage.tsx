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

import { type ReactNode } from 'react';
import { Link } from 'react-router';
import { useTranslation } from '../../i18n/Provider.js';
import { RecordList, useRecordSource, type ListFilter } from '../../refdata/RecordList.js';
import { RecordStoreProvider, syntheticStore } from '../../refdata/store.js';
import { show } from '../../refdata/records.js';
import { seriesPageOf } from '../../oresmd/identity.js';
import { useSeriesTitle } from '../../oresmd/useSeriesTitle.js';
import { useAreaRoot } from '../../shell/AreaTrail.js';
import { RelativeTime } from '../../ui/Time.js';
import { SYNTHETIC } from './bindingView.js';
import { BINDINGS, VENDOR, bindingPath } from './bindingPaths.js';
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
    const titled = useSeriesTitle();
    const facts = useBindingFacts();
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
        <RecordList
            source={{ ...base, filters, mayAdd: false }}
            title={t('marketdata.bindings.title')}
            lead={t('marketdata.bindings.lead')}
            crumbs={[...useAreaRoot('marketdata'), { label: t('marketdata.bindings.title') }]}
            pathOf={(row) => bindingPath(show(row['id']))}
            columns={[
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
                    cell: (row) => t(`refdata.choices.producer_kind.${show(row['producer_kind'])}`),
                },
                {
                    id: 'series',
                    header: t('marketdata.bindings.series'),
                    cell: (row) => {
                        const listed = facts.seriesOf(show(row['source_name']));
                        if (listed === undefined) {
                            return <span className="text-ink-faint">{'\u2014'}</span>;
                        }
                        return (
                            <Link
                                className="text-accent hover:underline"
                                to={seriesPageOf(listed)}
                                onClick={(event) => event.stopPropagation()}
                            >
                                {titled(listed.identity)}
                            </Link>
                        );
                    },
                },
                {
                    id: 'enabled',
                    header: t('refdata.fields.enabled'),
                    cell: (row) =>
                        row['enabled'] === true
                            ? t('refdata.records.yes')
                            : t('refdata.records.no'),
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
                        return at === '' ? (
                            <span className="text-ink-faint">{'\u2014'}</span>
                        ) : (
                            <RelativeTime at={at} />
                        );
                    },
                },
            ]}
        />
    );
}
