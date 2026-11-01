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
 * The cuts a party has taken, which Reporting draws.
 *
 * A cut exists for one purpose, the valuation a run computes, so a person who
 * asks what a number was computed from looks where the reports are. The row
 * states the label, the kind, the instant, the derived state with its basis,
 * the taker, the authoriser and the number of runs that priced from the body.
 *
 * The state and the label are functions of the facts a cut records, and the
 * reporting service derives both. The screen therefore prints what it is given:
 * the one rule it holds is the sentence under a state, which says which fact
 * admitted the body.
 *
 * A superseded cut stays in the list. The runs that priced from it still
 * reproduce from it, so hiding it would hide the provenance of a result.
 */

import type { ReactNode } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Link, useNavigate } from 'react-router';
import { CUT_STATE_WORDS, type CutRow } from '@ores/wire-protocol/browser';
import { api } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { Crumbs } from '../../refdata/shared.js';
import {
    RecordList,
    type ListColumn,
    type ListFilter,
    type ListPage,
    type ListSource,
} from '../../refdata/RecordList.js';
import { ScreenFrame } from '../../shell/ScreenFrame.js';
import { useAreaRoot } from '../../shell/AreaTrail.js';
import { PATHS, cutPath, takeCutPath } from '../../shell/paths.js';
import { Button, Notice } from '../../ui/Primitives.js';
import { usePermissions } from '../../access/holds.js';
import { PERMISSION } from '../../access/permissions.js';
import { RelativeTime } from '../../ui/Time.js';
import { Absent, StateCell, useStateName } from './shared.js';

/**
 * The kinds the kind filter offers, read once from the first page of the kinds.
 *
 * A filter's choices are the classification a person narrows by, and the kinds
 * are a closed set a firm seeds, so one page of them names every kind a cut can
 * be of.
 */
function useKindChoices(): readonly { readonly value: string; readonly label: string }[] {
    const kinds = useQuery({
        queryKey: ['cuts', 'types'],
        queryFn: () => api.cutKinds(KIND_CHOICES_PAGE),
    });
    return (kinds.data?.rows ?? []).map((row) => ({ value: row.code, label: row.name }));
}

/** How many kinds the filter's choices read, which is one page of the list's own sizes. */
const KIND_CHOICES_PAGE = {
    offset: 0,
    limit: 200,
    search: '',
    sort: '',
    descending: false,
};

/** The two filters the list offers: the kind of cut, and the state it is in. */
function useFilters(): readonly ListFilter[] {
    const { t } = useTranslation();
    const kinds = useKindChoices();
    return [
        {
            kind: 'choice',
            id: 'kind',
            label: t('cuts.columns.kind'),
            all: t('cuts.filter.allKinds'),
            choices: kinds,
        },
        {
            kind: 'choice',
            id: 'state',
            label: t('cuts.columns.state'),
            all: t('cuts.filter.allStates'),
            choices: CUT_STATE_WORDS.map((word) => ({
                value: word,
                label: t(`cuts.states.${word}`),
            })),
        },
    ];
}

/**
 * The cuts the filters name, one page at a time from the server.
 *
 * The server narrows by the kind, the state and the text, because the label and
 * the state are derived there, and it answers one page with the count of every
 * row the query matched. The list therefore never reads a large fixed slice and
 * never pages to the end to show everything.
 */
function useCutSource(filters: readonly ListFilter[]): ListSource<CutRow> {
    const { t } = useTranslation();
    return {
        key: 'cuts',
        read: async (request): Promise<ListPage<CutRow>> => {
            const answer = await api.cuts({
                offset: request.offset,
                limit: request.limit,
                kindCode: request.filters['kind'] ?? '',
                state: request.filters['state'] ?? '',
                search: request.search,
            });
            return {
                rows: answer.rows,
                total: answer.total,
                summary: <PageSummary cuts={answer.rows} />,
                ...(answer.partial
                    ? { notes: [{ tone: 'warn' as const, text: t('cuts.partialPage') }] }
                    : {}),
            };
        },
        search: true,
        sortable: [],
        filters,
        mayAdd: false,
    };
}

/** The taker of the body, or an absence. */
function Taker({ row }: { readonly row: CutRow }): ReactNode {
    return row.takenBy === '' ? <Absent /> : <span className="text-sm">{row.takenBy}</span>;
}

/**
 * The person whose authorisation released the body.
 *
 * A cut whose kind requires none has no authoriser, and the dash says so; the
 * state beside it carries the reason, so the column never reads as a gap.
 */
function Authoriser({ row }: { readonly row: CutRow }): ReactNode {
    return row.approvedBy === '' ? <Absent /> : <span className="text-sm">{row.approvedBy}</span>;
}

/** The columns of the cuts, in the order the record screen standard sets them. */
function useColumns(): readonly ListColumn<CutRow>[] {
    const { t } = useTranslation();
    return [
        {
            id: 'label',
            header: t('cuts.columns.cut'),
            cell: (row) => (
                <Link to={cutPath(row.id)} className="font-mono text-xs text-accent">
                    {row.label === '' ? t('cuts.labelMissing') : row.label}
                </Link>
            ),
            mono: true,
        },
        {
            id: 'kind',
            header: t('cuts.columns.kind'),
            cell: (row) => <span className="text-sm">{row.kindName || row.kindCode}</span>,
        },
        {
            id: 'timepoint',
            header: t('cuts.columns.timepoint'),
            cell: (row) => <RelativeTime at={row.timepoint} />,
            mono: true,
        },
        {
            id: 'state',
            header: t('cuts.columns.state'),
            cell: (row) => <StateCell row={row} />,
        },
        {
            id: 'taker',
            header: t('cuts.columns.taker'),
            cell: (row) => <Taker row={row} />,
        },
        {
            id: 'authoriser',
            header: t('cuts.columns.authoriser'),
            cell: (row) => <Authoriser row={row} />,
        },
        {
            id: 'runs',
            header: t('cuts.columns.runs'),
            cell: (row) => <span className="font-mono tabular-nums">{row.runCount}</span>,
            mono: true,
            numeric: true,
        },
    ];
}

/**
 * What the page says about the rows its filters match, before them.
 *
 * Two facts a person needs before running a report: which cuts a run may not
 * price from, and why a body nobody signed is admitted. Both are read from the
 * rows the server answered and never from a stored state.
 */
export function PageSummary({ cuts }: { readonly cuts: readonly CutRow[] }): ReactNode {
    const { t } = useTranslation();
    const stateName = useStateName();
    // A draft is not a cut yet, so it is not one of the bodies a run refuses: the
    // set is a body that is taken, one that is incomplete and one that is refused.
    const blocked = cuts.filter(
        (row) => row.state === 'incomplete' || row.state === 'taken' || row.state === 'refused',
    );
    const unsigned = cuts.filter((row) => row.state === 'in_use' && !row.requiresAuthorisation);
    if (blocked.length === 0 && unsigned.length === 0) {
        return null;
    }
    return (
        <div className="space-y-2">
            {blocked.length > 0 && (
                <Notice tone="warn">
                    {t('cuts.notice.blocked', {
                        count: String(blocked.length),
                        states: [...new Set(blocked.map((row) => stateName(row.state)))]
                            .sort()
                            .join(', '),
                    })}
                </Notice>
            )}
            {unsigned.length > 0 && (
                <Notice tone="info">
                    {t('cuts.notice.noSignature', { count: String(unsigned.length) })}
                </Notice>
            )}
        </div>
    );
}

/** The filters the list offers, drawn once and read by both the source and the table. */
export function CutsPage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const trail = [...useAreaRoot('reporting'), { label: t('cuts.title') }];
    const filters = useFilters();
    // The take is drawn only for a person who may press it: the standard's rule
    // is that a person who may not take a cut sees no button, and the server
    // checks the permission again on the call.
    const { ready, can } = usePermissions();
    const mayTake = ready && can({ all: [PERMISSION.cutsTake] });
    const source = useCutSource(filters);
    const kinds = useQuery({
        queryKey: ['cuts', 'types'],
        queryFn: () => api.cutKinds(KIND_CHOICES_PAGE),
    });

    if (kinds.isError) {
        return (
            <ScreenFrame
                trail={<Crumbs parts={trail} />}
                title={t('cuts.title')}
                description={t('cuts.lead')}
            >
                <Notice tone="error">{kinds.error.message}</Notice>
            </ScreenFrame>
        );
    }
    return (
        <RecordList
            source={source}
            title={t('cuts.title')}
            lead={t('cuts.lead')}
            crumbs={trail}
            columns={useColumns()}
            pathOf={(row) => cutPath(row.id)}
            keyOf={(row) => row.id}
            notes={[{ tone: 'info', text: t('cuts.readerNote') }]}
            actions={
                mayTake ? (
                    <Button
                        variant="primary"
                        icon="add"
                        onClick={() => void navigate(takeCutPath())}
                    >
                        {t('cuts.take.title')}
                    </Button>
                ) : null
            }
        />
    );
}
