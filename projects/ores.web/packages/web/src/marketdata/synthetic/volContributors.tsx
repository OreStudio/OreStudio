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
import { useState, type ReactNode } from 'react';
import { Link, useParams } from 'react-router';
import type { HistoryVersion } from '@ores/wire-protocol/browser';
import type { RecordRow } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { RecordTimeline } from '../../refdata/RecordTimeline.js';
import {
    RecordDetails,
    RecordDialog,
    RecordGate,
    RecordHeader,
    RevertDialog,
    show,
    useRecord,
    useRecordPermissions,
    useRecordTabs,
    useRecords,
} from '../../refdata/records.js';
import { RecordStoreProvider, syntheticStore } from '../../refdata/store.js';
import { useAreaRoot } from '../../shell/AreaTrail.js';
import { PATHS } from '../../shell/paths.js';
import { useSyntheticStep } from './syntheticTrail.js';
import { NOISE_FIELDS } from './volFeeds.js';

const CONTRIBUTORS = 'fx-vol-contributors';
const LINKS = 'fx-vol-views';
const SURFACES = 'fx-vol-surface-feeds';
const ENTITY = 'ores.synthetic.fx_vol_contributor';

/** The surfaces a contributor quotes, in pair order. */
export function surfacesOf(
    contributor: string,
    links: readonly RecordRow[],
    surfaces: readonly RecordRow[],
): readonly RecordRow[] {
    const linked = new Set(
        links
            .filter((link) => show(link['contributor_id']) === contributor)
            .map((link) => show(link['feed_id'])),
    );
    return surfaces
        .filter((surface) => linked.has(show(surface['id'])))
        .sort((left, right) => show(left['source_name']).localeCompare(show(right['source_name'])));
}

/**
 * One contributor of the synthetic vol surfaces: its noise profile, which every surface it quotes
 * shares, the surfaces it quotes, and its history.
 */
export function VolContributorPage(): ReactNode {
    const { id } = useParams();
    return (
        <RecordStoreProvider store={syntheticStore}>
            <RecordGate
                resource={CONTRIBUTORS}
                recordKey={id ?? ''}
                listPath={`${PATHS.synthetic}?tab=provider`}
            >
                {(row) => <ContributorBody key={show(row['id'])} row={row} />}
            </RecordGate>
        </RecordStoreProvider>
    );
}

function ContributorBody({ row }: { readonly row: RecordRow }): ReactNode {
    const { t } = useTranslation();
    const id = show(row['id']);
    const provider = show(row['provider_code']);
    const may = useRecordPermissions(CONTRIBUTORS);
    const record = useRecord(CONTRIBUTORS, id);
    const links = useRecords(LINKS);
    const surfaces = useRecords(SURFACES);
    const { tab, bar } = useRecordTabs({
        label: provider,
        tabs: ['details', 'surfaces', 'history'],
    });
    const [editing, setEditing] = useState(false);
    const [reverting, setReverting] = useState<HistoryVersion | null>(null);
    const quoted = surfacesOf(id, links.data ?? [], surfaces.data ?? []);
    const title = t('marketdata.volFeeds.contributors.noise', { provider });

    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[
                    ...useAreaRoot('marketdata'),
                    useSyntheticStep(),
                    {
                        label: t('marketdata.synthetic.tabs.provider'),
                        to: `${PATHS.synthetic}?tab=provider`,
                    },
                    { label: t('marketdata.volFeeds.contributors.title') },
                    { label: provider },
                ]}
                title={title}
                recordKey={provider}
                version={row.version}
                onEdit={may.write ? () => setEditing(true) : undefined}
            />
            {bar}
            {tab === 'details' && <RecordDetails specs={NOISE_FIELDS} row={row} />}
            {tab === 'surfaces' && (
                <section className="space-y-3">
                    <p className="text-sm text-ink-muted">
                        {t('marketdata.volFeeds.contributors.surfacesLead')}
                    </p>
                    {quoted.length === 0 ? (
                        <p className="text-sm text-ink-muted">
                            {t('marketdata.volFeeds.contributors.noSurfaces')}
                        </p>
                    ) : (
                        <ul className="space-y-1 text-sm">
                            {quoted.map((surface) => (
                                <li key={show(surface['id'])}>
                                    <Link
                                        to={`${PATHS.volFeed}/${encodeURIComponent(show(surface['id']))}`}
                                        className="text-accent hover:underline"
                                    >
                                        {show(surface['source_name'])}
                                    </Link>
                                </li>
                            ))}
                        </ul>
                    )}
                </section>
            )}
            {tab === 'history' && (
                <RecordTimeline
                    entityType={ENTITY}
                    entityId={id}
                    {...(may.write ? { onRevert: setReverting } : {})}
                />
            )}
            {editing && (
                <RecordDialog
                    title={title}
                    resource={CONTRIBUTORS}
                    specs={NOISE_FIELDS}
                    row={row}
                    loadedAt={record.dataUpdatedAt}
                    keep={['id', 'provider_code']}
                    onClose={() => setEditing(false)}
                />
            )}
            {reverting !== null && (
                <RevertDialog
                    resource={CONTRIBUTORS}
                    specs={NOISE_FIELDS}
                    row={row}
                    version={reverting}
                    onClose={() => setReverting(null)}
                />
            )}
        </div>
    );
}
