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

import type { ReactNode } from 'react';
import { Link } from 'react-router';
import { api, type PublishedCurveSet } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import type { Translate } from '../inbox/kinds.js';
import { PARTY_FILTER_ID, usePartyScope } from '../refdata/partyScope.js';
import {
    RecordList,
    type ListColumn,
    type ListFilter,
    type ListNote,
    type ListSource,
} from '../refdata/RecordList.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { Crumbs } from '../refdata/shared.js';
import { publishedCurvePath } from '../shell/paths.js';
import { Notice, Tag } from '../ui/Primitives.js';
import { RelativeTime } from '../ui/Time.js';
import { stateOf, type SetState } from './publishedCurves.js';

/**
 * The tenant's published curves, from
 * doc/prototypes/market_data/published-curves.org.
 *
 * One row per publish record: the curve, the party it belongs to, the as-of the
 * set was built at, the account that applied the approval, the instant it moved,
 * how many pillars it holds and the state it is in. The filters narrow the list
 * to one party, to the candidates nobody has approved, and to the sets that
 * ended, because a candidate read as the set a desk prices from would be a lie
 * and an ended set is reached on purpose.
 *
 * The set is read only. The gate that applies an approved request writes it and
 * no screen does, so the list offers no Add, no Edit and no Delete, and says so
 * in one line. The row opens the record, which holds the comparison and the
 * history.
 */

/** The filter that draws the candidates under the sets in use. */
const CANDIDATES_FILTER = 'candidates';

/** The filter that draws the withdrawn and superseded sets. */
const ENDED_FILTER = 'ended';

/** How many rows one read asks for. */
const PAGE = 50;

/** The tone a state's marker carries: the set in use is the only strong one. */
function toneOf(state: SetState): 'up' | 'accent' | 'muted' | 'neutral' {
    if (state === 'published') return 'up';
    if (state === 'candidate') return 'accent';
    if (state === 'withdrawn') return 'muted';
    return 'neutral';
}

/** The word a state reads as. */
export function stateLabelKey(state: SetState): string {
    return `marketdata.published.state.${state}`;
}

function filtersOf(t: Translate): readonly ListFilter[] {
    return [
        {
            kind: 'toggle',
            id: CANDIDATES_FILTER,
            label: t('marketdata.published.filter.candidates'),
        },
        { kind: 'toggle', id: ENDED_FILTER, label: t('marketdata.published.filter.ended') },
    ];
}

/**
 * Where the rows come from: the server reads the page, with the filters and the
 * search it names. The screen filters nothing it was given, because a page that
 * was filtered after paging would count the wrong rows.
 */
function sourceOf(
    partyFilter: ListFilter | undefined,
    t: Translate,
): ListSource<PublishedCurveSet> {
    return {
        key: 'published-curves',
        read: async (page) => {
            const partyId = page.filters[PARTY_FILTER_ID] ?? '';
            const answer = await api.publishedCurves({
                ...(partyId === '' ? {} : { partyId }),
                search: page.search,
                includeCandidates: page.filters[CANDIDATES_FILTER] === '1',
                includeEnded: page.filters[ENDED_FILTER] === '1',
                offset: page.offset,
                limit: page.limit,
            });
            return { rows: answer.sets, total: answer.total };
        },
        search: true,
        sortable: [],
        filters: [...(partyFilter === undefined ? [] : [partyFilter]), ...filtersOf(t)],
        mayAdd: false,
    };
}

function columnsOf(t: Translate): readonly ListColumn<PublishedCurveSet>[] {
    return [
        {
            id: 'curve',
            header: t('marketdata.published.columns.curve'),
            mono: true,
            cell: (row) => (
                <span className="block">
                    <Link to={publishedCurvePath(row.curve, row.party)} className="text-accent">
                        {row.curve}
                    </Link>
                    {stateOf(row) === 'withdrawn' && row.withdrawn_at !== null && (
                        <span className="block text-xs text-ink-faint">
                            {t('marketdata.published.leftUse', { at: row.withdrawn_at })}
                        </span>
                    )}
                    {stateOf(row) === 'superseded' && row.superseded_at !== null && (
                        <span className="block text-xs text-ink-faint">
                            {t('marketdata.published.leftUse', { at: row.superseded_at })}
                        </span>
                    )}
                </span>
            ),
        },
        {
            id: 'party',
            header: t('marketdata.published.columns.party'),
            cell: (row) => row.party,
        },
        {
            id: 'asOf',
            header: t('marketdata.published.columns.asOf'),
            mono: true,
            cell: (row) => row.as_of,
        },
        {
            id: 'publisher',
            header: t('marketdata.published.columns.publisher'),
            cell: (row) =>
                row.published_by === '' ? (
                    <span className="text-ink-faint">—</span>
                ) : (
                    row.published_by
                ),
        },
        {
            id: 'published',
            header: t('marketdata.published.columns.published'),
            cell: (row) =>
                row.published_at === null ? (
                    <span className="text-ink-faint">—</span>
                ) : (
                    <span title={row.published_at}>
                        <RelativeTime at={row.published_at} />
                    </span>
                ),
        },
        {
            id: 'points',
            header: t('marketdata.published.columns.points'),
            mono: true,
            numeric: true,
            cell: (row) => String(row.point_count),
        },
        {
            id: 'state',
            header: t('marketdata.published.columns.state'),
            cell: (row) => {
                const state = stateOf(row);
                return <Tag tone={toneOf(state)}>{t(stateLabelKey(state))}</Tag>;
            },
        },
    ];
}

/** The line that says what the list is and why it has no buttons. */
function notesOf(t: Translate): readonly ListNote[] {
    return [{ tone: 'info', text: t('marketdata.published.readOnly') }];
}

export function PublishedCurvesPage(): ReactNode {
    const { t } = useTranslation();
    const scope = usePartyScope({
        label: t('marketdata.published.columns.party'),
        all: t('marketdata.published.allParties'),
    });
    const trail = [...useAreaRoot('marketdata'), { label: t('marketdata.published.title') }];

    if (scope.pending || scope.error !== null) {
        return (
            <ScreenFrame
                trail={<Crumbs parts={trail} />}
                title={t('marketdata.published.title')}
                description={t('marketdata.published.lead')}
            >
                {scope.error === null ? (
                    <p className="text-sm text-ink-muted" role="status">
                        {t('common.loading')}
                    </p>
                ) : (
                    <Notice tone="error">{scope.error}</Notice>
                )}
            </ScreenFrame>
        );
    }

    return (
        <RecordList
            source={sourceOf(scope.filter, t)}
            title={t('marketdata.published.title')}
            lead={t('marketdata.published.lead')}
            crumbs={trail}
            columns={columnsOf(t)}
            notes={notesOf(t)}
            pathOf={(row) => publishedCurvePath(row.curve, row.party)}
        />
    );
}
