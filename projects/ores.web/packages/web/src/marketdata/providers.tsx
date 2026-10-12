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

import { useMemo, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { RecordList, useRecordSource } from '../refdata/RecordList.js';
import { show, useRecords } from '../refdata/records.js';
import { RecordStoreProvider, syntheticStore } from '../refdata/store.js';
import { useAreaParts } from '../shell/AreaTrail.js';

/**
 * The providers whose market data the synthetic feeds simulate: Bloomberg,
 * Refinitiv, the exchanges and the brokers. The catalogue belongs to the
 * installation and every tenant reads it. Each provider is shown with the
 * synthetic mark, so a simulated Bloomberg is never read as the real one.
 */
export function ProvidersPage(): ReactNode {
    return (
        <RecordStoreProvider store={syntheticStore}>
            <ProvidersList />
        </RecordStoreProvider>
    );
}

function ProvidersList(): ReactNode {
    const { t } = useTranslation();
    const source = useRecordSource('providers');
    const collections = useRecords('collections');
    const trail = useAreaParts('marketdata', t('marketdata.providers.title'));
    // How many collections stand for each provider, read once for the table.
    const counts = useMemo(() => {
        const out = new Map<string, number>();
        for (const collection of collections.data ?? []) {
            const code = show(collection['provider_code']);
            out.set(code, (out.get(code) ?? 0) + 1);
        }
        return out;
    }, [collections.data]);
    // A failed read is a dash, so it is not mistaken for none.
    const countOf = (code: string): string => {
        if (collections.isError) {
            return '—';
        }
        return collections.data === undefined ? '' : String(counts.get(code) ?? 0);
    };
    return (
        <RecordList
            source={source}
            title={t('marketdata.providers.title')}
            lead={t('marketdata.providers.lead')}
            crumbs={trail}
            keyOf={(row) => show(row['code'])}
            columns={[
                {
                    id: 'name',
                    header: t('refdata.fields.name'),
                    cell: (row) => t('marketdata.providers.named', { name: show(row['name']) }),
                },
                {
                    id: 'code',
                    header: t('refdata.fields.code'),
                    cell: (row) => show(row['code']),
                    mono: true,
                },
                {
                    id: 'kind',
                    header: t('marketdata.providers.kind'),
                    cell: (row) => t(`marketdata.providers.kinds.${show(row['kind'])}`),
                },
                {
                    id: 'collections',
                    header: t('marketdata.providers.collections'),
                    cell: (row) => countOf(show(row['code'])),
                    numeric: true,
                },
                {
                    id: 'description',
                    header: t('refdata.fields.description'),
                    cell: (row) => show(row['description']),
                },
            ]}
        />
    );
}
