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
import { type ReactNode, useState } from 'react';
import { Link, useNavigate, useParams } from 'react-router';
import { api } from '../../api/client.js';
import { bindings } from '../../api/bindings.js';
import { useTranslation } from '../../i18n/Provider.js';
import { seriesPageOf } from '../../oresmd/identity.js';
import { OresmdUrl } from '../../oresmd/OresmdUrl.js';
import { useSeriesTitle } from '../../oresmd/useSeriesTitle.js';
import {
    RecordDetails,
    RecordDialog,
    RecordGate,
    RecordHeader,
    RemoveRecordDialog,
    show,
    useRecordPermissions,
    useRecordTabs,
    useRecords,
    type FieldSpec,
} from '../../refdata/records.js';
import { RecordStoreProvider, syntheticStore } from '../../refdata/store.js';
import { useAreaRoot } from '../../shell/AreaTrail.js';
import { Notice } from '../../ui/Primitives.js';
import { RelativeTime } from '../../ui/Time.js';
import { BindingHistory } from './BindingHistory.js';
import { heldBy, isSynthetic } from './bindingView.js';
import { BINDINGS, VENDOR, bindingPath } from './bindingPaths.js';
import { BINDABLE, bindableSourcesStore } from './bindableSourcesStore.js';
import { StatusTag } from './StatusTag.js';
import { useBindingFacts } from './useBindings.js';
import { feedPageOf, folderChain } from '../synthetic/feedTree.js';

/** The fields of the record, as the details tab and the history draw them. */
const BINDING_FIELDS: readonly FieldSpec[] = [
    { field: 'source_name', history: 'Source Name', kind: { kind: 'text' }, fixed: true },
    {
        field: 'producer_kind',
        history: 'Producer Kind',
        kind: { kind: 'choice', options: ['SYNTHETIC', VENDOR] },
        fixed: true,
    },
    { field: 'enabled', history: 'Enabled', kind: { kind: 'bool' } },
];

/**
 * The fields the add and edit dialog asks for: the source the party consumes, and
 * whether ingestion is enabled. The source is a picker, not a choice, because the
 * choices are the party's own sources rather than a fixed enumeration.
 */
export const BINDING_DIALOG_FIELDS: readonly FieldSpec[] = [
    {
        field: 'source_name',
        history: 'Source Name',
        kind: {
            kind: 'record',
            resource: BINDABLE,
            value: 'source_name',
            label: 'label',
            store: bindableSourcesStore,
        },
        help: 'marketdata.bindings.sourceHelp',
    },
    {
        field: 'enabled',
        history: 'Enabled',
        kind: { kind: 'bool' },
        help: 'marketdata.bindings.enabledHelp',
    },
];

/** One binding: its fields, what its feed does, and the series it writes. */
export function BindingPage(): ReactNode {
    const { id } = useParams();
    return (
        <RecordStoreProvider store={syntheticStore}>
            <RecordGate resource={BINDINGS} recordKey={id ?? ''} listPath={bindingPath()}>
                {(binding) => <BindingBody key={show(binding['id'])} binding={binding} />}
            </RecordGate>
        </RecordStoreProvider>
    );
}

function BindingBody({
    binding,
}: {
    readonly binding: Readonly<Record<string, unknown>> & { version: number };
}): ReactNode {
    const { t } = useTranslation();
    const titled = useSeriesTitle();
    const navigate = useNavigate();
    const sourceName = show(binding['source_name']);
    const kind = show(binding['producer_kind']);
    const may = useRecordPermissions(BINDINGS);
    const facts = useBindingFacts();
    const [editing, setEditing] = useState(false);
    const [removing, setRemoving] = useState(false);
    // The sources the party may bind, so the dialog can refuse one whose series a
    // running feed already writes, before the write rather than after it.
    const candidates = useQuery({
        queryKey: ['bindings', 'sources'],
        queryFn: bindings.sources,
        enabled: isSynthetic(kind) && may.write,
    });
    const refusal = (values: Readonly<Record<string, string>>): readonly string[] => {
        const held = heldBy(candidates.data, values['source_name'] ?? '');
        return held === undefined ? [] : [t('marketdata.bindings.held', { source: held })];
    };
    const enabled = binding['enabled'] === true;
    const feeds = useQuery({
        queryKey: ['bindings', 'feed', sourceName],
        queryFn: () => api.folderFeeds({ search: sourceName }, { offset: 0, limit: 20 }),
        enabled: isSynthetic(kind),
    });
    const feed = feeds.data?.feeds.find((one) => one.source_name === sourceName);
    const folders = useRecords('folders');
    const folder = folderChain(
        (folders.data ?? []).map((row) => ({
            id: show(row['id']),
            parent_id: show(row['parent_id']),
            name: show(row['name']),
            kind: show(row['kind']),
        })),
        feed?.folder_id ?? '',
    )
        .map((row) => row.name)
        .join(' \u203a ');
    const listed = facts.seriesOf(sourceName);
    const lastTick = facts.lastTickOf(sourceName);
    const status = facts.statusOf({ source_name: sourceName, producer_kind: kind, enabled });
    const none = <span className="text-ink-faint">{'\u2014'}</span>;
    // The page is named for what the binding writes, not the source it consumes.
    const target = listed === undefined ? sourceName : titled(listed.identity);
    const { tab, bar } = useRecordTabs({
        label: target,
        tabs: ['details', 'history'],
    });

    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[
                    ...useAreaRoot('marketdata'),
                    { label: t('marketdata.bindings.title'), to: bindingPath() },
                    { label: target },
                ]}
                title={target}
                recordKey={sourceName}
                version={binding.version}
                subdued
                onEdit={may.write && isSynthetic(kind) ? () => setEditing(true) : undefined}
                onDelete={may.remove ? () => setRemoving(true) : undefined}
            />
            {bar}
            {tab === 'details' && (
                <RecordDetails
                    specs={BINDING_FIELDS}
                    row={binding as never}
                    extra={[
                        [t('marketdata.bindings.status'), <StatusTag status={status} />],
                        [
                            t('marketdata.bindings.lastTick'),
                            lastTick === '' ? none : <RelativeTime at={lastTick} />,
                        ],
                        [
                            t('marketdata.bindings.feed'),
                            feed === undefined ? (
                                none
                            ) : (
                                <Link className="text-accent hover:underline" to={feedPageOf(feed)}>
                                    {feed.label}
                                </Link>
                            ),
                        ],
                        [t('marketdata.bindings.folder'), folder === '' ? none : folder],
                        [
                            t('marketdata.bindings.ticksPerHour'),
                            feed === undefined ? none : String(feed.ticks_per_hour),
                        ],
                        [
                            t('marketdata.bindings.series'),
                            listed === undefined && feed === undefined ? (
                                none
                            ) : (
                                <span className="flex flex-col gap-1">
                                    {listed !== undefined && (
                                        <Link
                                            className="text-accent hover:underline"
                                            to={seriesPageOf(listed)}
                                        >
                                            {titled(listed.identity)}
                                        </Link>
                                    )}
                                    {feed !== undefined && <OresmdUrl uri={feed.oresmd_uri} />}
                                </span>
                            ),
                            t('marketdata.bindings.seriesHint'),
                        ],
                    ]}
                />
            )}
            {tab === 'history' && (
                <BindingHistory
                    row={binding as never}
                    specs={BINDING_FIELDS}
                    kind={kind}
                    mayWrite={may.write}
                />
            )}
            {!isSynthetic(kind) && (
                <Notice tone="info">{t('marketdata.bindings.vendorNote')}</Notice>
            )}
            {editing && (
                <RecordDialog
                    title={t('refdata.records.editTitle', { code: sourceName })}
                    resource={BINDINGS}
                    specs={BINDING_DIALOG_FIELDS}
                    row={binding as never}
                    keep={['id']}
                    check={refusal}
                    onClose={() => setEditing(false)}
                />
            )}
            {removing && (
                <RemoveRecordDialog
                    resource={BINDINGS}
                    title={t('refdata.records.deleteTitle', { code: sourceName })}
                    warning={t('marketdata.bindings.removeWarning')}
                    recordKey={{ id: show(binding['id']) }}
                    version={binding.version}
                    onClose={() => setRemoving(false)}
                    onRemoved={() => void navigate(bindingPath())}
                />
            )}
        </div>
    );
}
