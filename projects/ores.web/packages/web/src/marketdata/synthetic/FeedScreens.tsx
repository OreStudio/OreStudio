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
import { useNavigate, useParams } from 'react-router';
import { api, type RecordRow } from '../../api/client.js';
import { useHeld } from '../../access/holds.js';
import { PERMISSION } from '../../access/permissions.js';
import { useTranslation } from '../../i18n/Provider.js';
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
} from '../../refdata/records.js';
import { RecordStoreProvider, syntheticStore } from '../../refdata/store.js';
import { useAreaRoot } from '../../shell/AreaTrail.js';
import { PATHS } from '../../shell/paths.js';
import { Button, Notice } from '../../ui/Primitives.js';
import { FeedStatus, RunOutcomeText, useFeedRuns, useRunningFeeds } from './feedRuns.js';
import { FeedHistory, type FeedPart } from './FeedHistory.js';
import type { FeedKind } from './feedKind.js';
import { folderChain, folderKey } from './feedTree.js';
import { useSyntheticStep } from './syntheticTrail.js';

/**
 * One feed: its fields, the kind's own tabs, and its history. Every asset
 * class draws this; the kind states the tabs and the rows the feed owns.
 */
export function FeedPage({ kind }: { readonly kind: FeedKind }): ReactNode {
    const { id } = useParams();
    return (
        <RecordStoreProvider store={syntheticStore}>
            <RecordGate
                resource={kind.resource}
                recordKey={id ?? ''}
                listPath={`${PATHS.synthetic}?tab=tree`}
            >
                {(feed) => <FeedBody key={show(feed['id'])} kind={kind} feed={feed} />}
            </RecordGate>
        </RecordStoreProvider>
    );
}

/** The rows each part of a feed holds, and whether the person may write them. */
function useFeedParts(
    kind: FeedKind,
    id: string,
): readonly (FeedPart & { readonly mayRemove: boolean })[] {
    // The parts of a kind never change, so the hooks run in the same order each time.
    return kind.parts.map((part) => {
        const rows = useRecords(part.resource, id);
        const may = useRecordPermissions(part.resource);
        return {
            entity: part.entity,
            resource: part.resource,
            specs: part.specs,
            keep: part.keep,
            rows: rows.data ?? [],
            mayWrite: may.write,
            mayRemove: may.remove,
        };
    });
}

function FeedBody({
    kind,
    feed,
}: {
    readonly kind: FeedKind;
    readonly feed: RecordRow;
}): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const id = show(feed['id']);
    const name = kind.nameOf(feed);
    const words = `marketdata.${kind.words}`;
    const root = [...useAreaRoot('marketdata'), useSyntheticStep()];
    const folders = useRecords('folders');
    const chain = folderChain(
        (folders.data ?? []).map((row) => ({
            id: show(row['id']),
            parent_id: show(row['parent_id']),
            name: show(row['name']),
            kind: show(row['kind']),
        })),
        show(feed['folder_id']),
    );
    const may = useRecordPermissions(kind.resource);
    const mayRun = useHeld()(PERMISSION.feedsRun);
    const check = kind.useCheck();
    const parts = useFeedParts(kind, id);
    const { tab, bar } = useRecordTabs({
        label: name,
        tabs: ['details', ...kind.tabs.map((one) => one.name), 'history'],
    });
    const running = useRunningFeeds()?.has(show(feed['source_name']));
    const runs = useFeedRuns();
    const [editing, setEditing] = useState(false);
    const [removing, setRemoving] = useState(false);
    const own = kind.tabs.find((one) => one.name === tab);

    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[
                    ...root,
                    {
                        label: t('marketdata.synthetic.tabs.tree'),
                        to: `${PATHS.synthetic}?tab=tree`,
                    },
                    ...chain.map((row) => ({
                        label: row.name,
                        to: `${PATHS.synthetic}?tab=tree&node=${encodeURIComponent(folderKey(row.id))}`,
                    })),
                    { label: name },
                ]}
                title={show(feed['source_name'])}
                recordKey={name}
                version={feed.version}
                detail={kind.detailOf(feed, t)}
                {...(kind.flag === undefined ? {} : { flag: kind.flag })}
                access={null}
                own={
                    <>
                        <FeedStatus running={running} />
                        {mayRun && running !== undefined && (
                            <Button
                                size="sm"
                                pending={runs.pending}
                                onClick={() =>
                                    runs.run(running ? 'stop' : 'start', [
                                        { id, label: show(feed['source_name']) },
                                    ])
                                }
                            >
                                {running ? t('marketdata.runs.stop') : t('marketdata.runs.start')}
                            </Button>
                        )}
                    </>
                }
                onEdit={may.write ? () => setEditing(true) : undefined}
                onDelete={may.remove ? () => setRemoving(true) : undefined}
            />
            {runs.outcome !== undefined && runs.outcome.refused.length > 0 && (
                <Notice tone="warn">
                    <RunOutcomeText outcome={runs.outcome} />
                </Notice>
            )}
            {bar}
            {tab === 'details' && (
                <RecordDetails specs={kind.fields} row={feed} extra={kind.extraDetails(feed, t)} />
            )}
            {own?.render({
                feed,
                running,
                mayWrite: may.write,
                parts: parts.map((part) => part.rows),
            })}
            {tab === 'history' && (
                <FeedHistory
                    feed={feed}
                    feedEntity={kind.entity}
                    subject={kind.historySubject}
                    parts={parts}
                    feedSpecs={kind.fields}
                    feedResource={kind.resource}
                    complete={kind.complete}
                    keep={['id', ...kind.kept]}
                    mayWrite={may.write}
                />
            )}
            {editing && (
                <RecordDialog
                    title={t('refdata.records.editTitle', { code: name })}
                    resource={kind.resource}
                    specs={kind.editFields}
                    row={feed}
                    keep={[
                        'id',
                        ...kind.fields
                            .filter((spec) => !kind.editFields.includes(spec))
                            .map((spec) => spec.field),
                        ...kind.kept,
                    ]}
                    complete={kind.complete}
                    check={check}
                    onClose={() => setEditing(false)}
                />
            )}
            {removing && (
                <RemoveRecordDialog
                    resource={kind.resource}
                    title={t('refdata.records.deleteTitle', { code: name })}
                    warning={t(`${words}.removeWarning`)}
                    recordKey={{ id }}
                    version={feed.version}
                    before={async (intent) => {
                        await api.stopFeed(id);
                        for (const part of parts) {
                            if (part.rows.length > 0) {
                                await api.removeSynthetic(
                                    part.resource,
                                    part.rows.map((row) => ({
                                        key: show(row['id']),
                                        version: part.rows.length === 1 ? row.version : null,
                                    })),
                                    intent,
                                );
                            }
                        }
                    }}
                    onClose={() => setRemoving(false)}
                    onRemoved={() => void navigate(`${PATHS.synthetic}?tab=tree`)}
                />
            )}
        </div>
    );
}
