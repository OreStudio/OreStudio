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

/**
 * The kinds of cut, which Reference data draws.
 *
 * A kind is not a sentence in a knowledge page: it is a row a person maintains,
 * because four of the facts it carries are decisions a firm changes and reads
 * back. The list states the code and its name, the family the kind belongs to,
 * who signs it, whether its values are indicative, the retention it carries and
 * how many cuts the party holds of it.
 *
 * Nothing a screen prints about authorisation is stored. The signed-or-not a
 * person reads is derived from the approval kind the type points at, so a firm
 * that gives the close to another function changes one approval kind and no
 * type, and every screen reads the new words.
 *
 * A type's cuts belong to the type's own page: the count is here and the bodies
 * are there.
 */

import type { ReactNode } from 'react';
import type { CutKindRow } from '@ores/wire-protocol/browser';
import { api } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import {
    RecordList,
    type ListColumn,
    type ListPage,
    type ListSource,
} from '../../refdata/RecordList.js';
import { RecordStoreProvider } from '../../refdata/store.js';
import { useAreaRoot } from '../../shell/AreaTrail.js';
import { cutTypePath } from '../../shell/paths.js';
import { CutAuthorisationLabel, CutFamilyLabel, CutValuesLabel } from './shared.js';
import { CUT_KIND_RESOURCE, cutKindStore } from './cutKindStore.js';

/**
 * The kinds, one page at a time, searched and ordered on the server.
 *
 * A count is a read of the cuts, so the list says when a kind holds more than
 * the read could count rather than stating a number that is quietly short.
 */
function useCutTypeSource(): ListSource<CutKindRow> {
    const { t } = useTranslation();
    return {
        key: CUT_KIND_RESOURCE,
        read: async (request): Promise<ListPage<CutKindRow>> => {
            const answer = await api.cutKinds(request);
            return {
                rows: answer.rows,
                total: answer.total,
                ...(answer.partial
                    ? { notes: [{ tone: 'warn' as const, text: t('cuts.types.partialCounts') }] }
                    : {}),
            };
        },
        search: true,
        sortable: ['code', 'name', 'family'],
        mayAdd: false,
    };
}

/** Who signs a cut of this type, or the fact that nobody does. */
function Authorisation({ row }: { readonly row: CutKindRow }): ReactNode {
    if (!row.signed) {
        return <CutAuthorisationLabel />;
    }
    return (
        <span className="flex flex-col gap-0.5">
            <span className="text-sm">{row.approvalKindName ?? row.approvalKindCode ?? ''}</span>
            {row.decidePermissionCode !== null && row.decidePermissionCode !== '' && (
                <span className="font-mono text-[11px] text-ink-faint">
                    {row.decidePermissionCode}
                </span>
            )}
        </span>
    );
}

/** How long a cut of this type is kept, and where it goes when it leaves the warm tier. */
function Retention({ row }: { readonly row: CutKindRow }): ReactNode {
    const { t } = useTranslation();
    return (
        <span className="flex flex-col gap-0.5">
            <span className="text-sm">
                {t('cuts.types.months', { count: String(row.retentionMonths) })}
            </span>
            <span className="text-[11px] text-ink-faint">
                {t('cuts.types.destination', { destination: row.retentionDestinationCode })}
            </span>
        </span>
    );
}

/** The columns of the types, in the order the record screen standard sets them. */
function useColumns(): readonly ListColumn<CutKindRow>[] {
    const { t } = useTranslation();
    return [
        {
            id: 'code',
            sort: 'code',
            header: t('cuts.columns.type'),
            cell: (row) => <span className="font-mono text-xs">{row.code}</span>,
            mono: true,
        },
        {
            id: 'name',
            sort: 'name',
            header: t('cuts.columns.name'),
            cell: (row) => row.name,
        },
        {
            id: 'family',
            sort: 'family',
            header: t('cuts.columns.family'),
            cell: (row) => <CutFamilyLabel code={row.familyCode} />,
        },
        {
            id: 'signed',
            header: t('cuts.columns.signed'),
            cell: (row) => <Authorisation row={row} />,
        },
        {
            id: 'indicative',
            header: t('cuts.columns.indicative'),
            cell: (row) => <CutValuesLabel indicative={row.outputIsIndicative} />,
        },
        {
            id: 'retention',
            header: t('cuts.columns.retention'),
            cell: (row) => <Retention row={row} />,
        },
        {
            id: 'cuts',
            header: t('cuts.columns.cutsHeld'),
            cell: (row) => <span className="font-mono tabular-nums">{row.cutsCount}</span>,
            mono: true,
            numeric: true,
        },
    ];
}

/** The kinds of cut a person maintains, each with the party's cut count. */
export function CutTypesPage(): ReactNode {
    return (
        <RecordStoreProvider store={cutKindStore}>
            <CutTypesList />
        </RecordStoreProvider>
    );
}

/**
 * The list itself.
 *
 * The kinds are read one page at a time from the server, which is the page the
 * shared list asks for: the count under the table is the server's total, so a
 * firm that seeds more kinds than fit on a page reads them by paging rather
 * than by scrolling a list that was read whole.
 */
function CutTypesList(): ReactNode {
    const { t } = useTranslation();
    const trail = [...useAreaRoot('refdata'), { label: t('cuts.types.title') }];
    const source = useCutTypeSource();
    return (
        <RecordList
            source={source}
            title={t('cuts.types.title')}
            lead={t('cuts.types.lead')}
            crumbs={trail}
            columns={useColumns()}
            pathOf={(row) => cutTypePath(row.code)}
            keyOf={(row) => row.code}
            notes={[{ tone: 'info', text: t('cuts.types.readerNote') }]}
        />
    );
}
