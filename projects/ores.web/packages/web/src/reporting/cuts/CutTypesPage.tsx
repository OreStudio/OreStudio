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
import { useQuery } from '@tanstack/react-query';
import type { CutKindRow } from '@ores/wire-protocol/browser';
import { api } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { Crumbs } from '../../refdata/shared.js';
import {
    RecordList,
    type ListColumn,
    type ListPage,
    type ListSource,
} from '../../refdata/RecordList.js';
import { ScreenFrame } from '../../shell/ScreenFrame.js';
import { useAreaRoot } from '../../shell/AreaTrail.js';
import { cutTypePath } from '../../shell/paths.js';
import { Notice } from '../../ui/Primitives.js';
import { IndicativeTag } from './shared.js';

/** The key the types are cached under. */
export const CUT_TYPES_QUERY_KEY = ['cuts', 'types'] as const;

/**
 * The types, read whole and searched and ordered on what the server answered.
 *
 * A firm holds a handful of kinds, so the list reads them in one call and pages
 * them in the browser. The count of each kind's cuts is the server's, because
 * the reads that answer it are the cut reads.
 */
function useCutTypeSource(partial: boolean): ListSource<CutKindRow> {
    const { t } = useTranslation();
    return {
        key: 'cut-types',
        read: async (request): Promise<ListPage<CutKindRow>> => {
            const answer = await api.cutKinds();
            const needle = request.search.toLowerCase();
            const matched =
                needle === ''
                    ? answer.rows
                    : answer.rows.filter(
                          (row) =>
                              row.code.toLowerCase().includes(needle) ||
                              row.name.toLowerCase().includes(needle) ||
                              row.familyName.toLowerCase().includes(needle),
                      );
            const ordered = [...matched].sort((left, right) => {
                switch (request.sort) {
                    case 'code':
                        return left.code.localeCompare(right.code);
                    case 'name':
                        return left.name.localeCompare(right.name);
                    case 'family':
                        return left.familyName.localeCompare(right.familyName);
                    default:
                        return (
                            left.displayOrder - right.displayOrder ||
                            left.code.localeCompare(right.code)
                        );
                }
            });
            const directed = request.descending ? ordered.reverse() : ordered;
            return {
                rows: directed.slice(request.offset, request.offset + request.limit),
                total: directed.length,
                ...(partial
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
    const { t } = useTranslation();
    if (!row.signed) {
        return <span className="text-ink-muted">{t('cuts.types.unsigned')}</span>;
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
            cell: (row) => row.familyName,
        },
        {
            id: 'signed',
            header: t('cuts.columns.signed'),
            cell: (row) => <Authorisation row={row} />,
        },
        {
            id: 'indicative',
            header: t('cuts.columns.indicative'),
            cell: (row) =>
                row.outputIsIndicative ? (
                    <IndicativeTag />
                ) : (
                    <span className="text-ink-muted">{t('cuts.types.official')}</span>
                ),
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
    const { t } = useTranslation();
    const trail = [...useAreaRoot('refdata'), { label: t('cuts.types.title') }];
    const kinds = useQuery({ queryKey: CUT_TYPES_QUERY_KEY, queryFn: () => api.cutKinds() });

    if (kinds.isPending) {
        return (
            <ScreenFrame
                trail={<Crumbs parts={trail} />}
                title={t('cuts.types.title')}
                description={t('cuts.types.lead')}
            >
                <p className="text-sm text-ink-muted" role="status">
                    {t('common.loading')}
                </p>
            </ScreenFrame>
        );
    }
    if (kinds.isError) {
        return (
            <ScreenFrame
                trail={<Crumbs parts={trail} />}
                title={t('cuts.types.title')}
                description={t('cuts.types.lead')}
            >
                <Notice tone="error">{kinds.error.message}</Notice>
            </ScreenFrame>
        );
    }
    return <CutTypesList trail={trail} partial={kinds.data.partial} />;
}

/** The list itself, drawn once the types are read. */
function CutTypesList({
    trail,
    partial,
}: {
    readonly trail: readonly { readonly label: string; readonly to?: string }[];
    readonly partial: boolean;
}): ReactNode {
    const { t } = useTranslation();
    const source = useCutTypeSource(partial);
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
