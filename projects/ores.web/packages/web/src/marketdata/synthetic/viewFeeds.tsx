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
import type { RecordRow } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { show, useRecord, type FieldSpec } from '../../refdata/records.js';
import { syntheticStore } from '../../refdata/store.js';
import { PATHS } from '../../shell/paths.js';
import { Card } from '../../ui/Card.js';
import { FeedPage } from './FeedScreens.js';
import { feedPath, type FeedKind } from './feedKind.js';

const VIEWS = 'fx-vol-views';

/** The pair a view quotes, from the last segment of its scheme name, such as audusd. */
export function viewPair(feed: Readonly<Record<string, unknown>>): string {
    const pair = show(feed['source_name']).split('.').pop() ?? '';
    return pair.length === 6 ? `${pair.slice(0, 3)}/${pair.slice(3)}`.toUpperCase() : pair;
}

const VIEW_FIELDS: readonly FieldSpec[] = [
    {
        field: 'config_id',
        history: 'Config ID',
        kind: {
            kind: 'record',
            resource: 'collections',
            value: 'id',
            label: 'name',
            store: syntheticStore,
        },
        fixed: true,
    },
    {
        field: 'feed_id',
        history: 'Feed ID',
        kind: {
            kind: 'record',
            resource: 'fx-vol-surface-feeds',
            value: 'id',
            label: 'source_name',
            store: syntheticStore,
        },
        fixed: true,
        help: 'marketdata.volViews.help.feed_id',
    },
    {
        field: 'contributor_id',
        history: 'Contributor ID',
        kind: {
            kind: 'record',
            resource: 'fx-vol-contributors',
            value: 'id',
            label: 'provider_code',
            store: syntheticStore,
        },
        fixed: true,
        help: 'marketdata.volViews.help.contributor_id',
    },
    { field: 'source_name', history: 'Source Name', kind: { kind: 'text', max: 200 } },
    { field: 'enabled', history: 'Enabled', kind: { kind: 'bool' } },
    { field: 'auto_start', history: 'Auto Start', kind: { kind: 'bool' } },
];

const VIEW_EDIT_FIELDS: readonly FieldSpec[] = VIEW_FIELDS.filter(
    (spec) => spec.field === 'enabled' || spec.field === 'auto_start',
);

/**
 * What a view is, said plainly: one vendor's noise over a latent surface, with the surface it
 * reads and the noise profile it applies, each linked.
 */
function ViewInputs({ feed }: { readonly feed: RecordRow }): ReactNode {
    const { t } = useTranslation();
    const surface = useRecord('fx-vol-surface-feeds', show(feed['feed_id']));
    const noise = useRecord('fx-vol-contributors', show(feed['contributor_id']));
    const vendor = show(noise.data?.['provider_code']);
    return (
        <div className="space-y-4">
            <p className="text-sm text-ink-muted">
                {t('marketdata.volViews.lead', { vendor, pair: viewPair(feed) })}
            </p>
            <Card title={t('marketdata.volViews.surface')}>
                <Link
                    className="font-mono text-xs text-accent hover:text-accent-bright"
                    to={`${PATHS.volFeed}/${encodeURIComponent(show(feed['feed_id']))}`}
                >
                    {show(surface.data?.['source_name']) || show(feed['feed_id'])}
                </Link>
            </Card>
            <Card title={t('marketdata.volViews.noise')}>
                <p className="text-sm">
                    <Link
                        className="text-accent hover:text-accent-bright"
                        to={`${PATHS.volContributor}/${encodeURIComponent(show(feed['contributor_id']))}`}
                    >
                        {t('marketdata.volViews.noiseOf', { vendor })}
                    </Link>
                </p>
                <p className="text-xs text-ink-muted">{t('marketdata.volViews.noiseShared')}</p>
            </Card>
        </div>
    );
}

/** A vendor's view of a latent surface, as the shared feed screens draw it. */
export const VIEW_FEED_KIND: FeedKind = {
    resource: VIEWS,
    entity: 'ores.synthetic.fx_vol_view',
    historySubject: 'vol_view',
    path: PATHS.volView,
    words: 'volViews',
    fields: VIEW_FIELDS,
    editFields: VIEW_EDIT_FIELDS,
    kept: ['folder_id', 'feed_id', 'contributor_id', 'config_id', 'source_name'],
    initial: {},
    complete: (write) => ({ ...write }),
    useCheck: () => () => [],
    nameOf: viewPair,
    flag: 'pair',
    detailOf: (_feed, t) => t('marketdata.volViews.engine'),
    extraDetails: () => [],
    parts: [],
    tabs: [],
    afterAdd: 'details',
    heldByProvider: true,
    calibration: ({ feed }) => <ViewInputs feed={feed} />,
};

export function volViewPath(id?: string): string {
    return feedPath(VIEW_FEED_KIND, id);
}

/** The page of one vendor's view of a latent surface. */
export function VolViewPage(): ReactNode {
    return <FeedPage kind={VIEW_FEED_KIND} />;
}
