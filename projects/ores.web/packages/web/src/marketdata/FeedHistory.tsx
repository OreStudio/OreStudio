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

import { useQueries } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import {
    orderTimeline,
    recordEvents,
    type HistoryVersion,
    type Timeline as Stream,
    type TimelineEvent,
} from '@ores/wire-protocol/browser';
import { api, type RecordRow } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { useHolds } from '../access/holds.js';
import { actorPathFor } from '../access/PeoplePage.js';
import { useActorPictures } from '../access/PersonRef.js';
import { RevertDialog, show, type FieldSpec } from '../refdata/records.js';
import { Timeline } from '../timeline/Timeline.js';
import { Button, Notice } from '../ui/Primitives.js';

export const FEED_ENTITY = 'ores.synthetic.fx_spot_generation_config';
export const COMPONENT_ENTITY = 'ores.synthetic.gmm_component';

/** A component's fields, for its revert; the mixture table edits the rest. */
const COMPONENT_FIELDS: readonly FieldSpec[] = [
    { field: 'component_index', history: 'Component Index', kind: { kind: 'int', min: 0 } },
    { field: 'description', history: 'Description', kind: { kind: 'text' }, blank: true },
    { field: 'mean', history: 'Mean', kind: { kind: 'decimal' } },
    { field: 'stdev', history: 'Stdev', kind: { kind: 'decimal' } },
    { field: 'weight', history: 'Weight', kind: { kind: 'decimal' } },
];

function keyOf(entityType: string, entityId: string, version: number): string {
    return `${entityType}\u0000${entityId}\u0000${String(version)}`;
}

/**
 * The feed's story as one stream: the feed's versions and the versions of
 * each component of its mixture, newest first, read as one history is read on
 * a person's page. Reads the history of the feed and of each component it
 * holds now.
 */
export function useFeedStory(
    feed: RecordRow,
    components: readonly RecordRow[],
): {
    readonly stream: Stream | undefined;
    readonly versions: ReadonlyMap<string, HistoryVersion>;
    readonly error: Error | null;
} {
    const feedId = show(feed['id']);
    const reads = [
        { entityType: FEED_ENTITY, entityId: feedId },
        ...components.map((row) => ({
            entityType: COMPONENT_ENTITY,
            entityId: show(row['id']),
        })),
    ];
    const results = useQueries({
        queries: reads.map((read) => ({
            queryKey: ['history', read.entityType, read.entityId],
            queryFn: () => api.history(read.entityType, read.entityId),
        })),
    });
    const versions = new Map<string, HistoryVersion>();
    const events: TimelineEvent[] = [];
    reads.forEach((read, index) => {
        for (const version of results[index]?.data ?? []) {
            versions.set(keyOf(read.entityType, read.entityId, version.version), version);
        }
        events.push(...recordEvents(read.entityType, read.entityId, results[index]?.data ?? []));
    });
    const error = results.find((result) => result.error !== null)?.error ?? null;
    const pending = results.some((result) => result.isPending);
    return {
        stream: pending
            ? undefined
            : { subject: 'fx_feed', id: feedId, events: orderTimeline(events), gaps: [] },
        versions,
        error,
    };
}

/**
 * The History tab of a feed: one timeline for the feed and its mixture. A
 * version older than its record's newest offers Revert to a person who may
 * write it, which writes its values back as a new version.
 */
export function FeedHistory({
    feed,
    components,
    feedSpecs,
    feedResource,
    componentResource,
    complete,
    keep,
    mayWrite,
}: {
    readonly feed: RecordRow;
    readonly components: readonly RecordRow[];
    readonly feedSpecs: readonly FieldSpec[];
    readonly feedResource: string;
    readonly componentResource: string;
    readonly complete: (write: Readonly<Record<string, unknown>>) => Record<string, unknown>;
    readonly keep: readonly string[];
    readonly mayWrite: { readonly feed: boolean; readonly components: boolean };
}): ReactNode {
    const { t } = useTranslation();
    const holds = useHolds();
    const actorPicture = useActorPictures();
    const story = useFeedStory(feed, components);
    const [reverting, setReverting] = useState<TimelineEvent | null>(null);
    const rowOf = (event: TimelineEvent): RecordRow | undefined =>
        event.entityType === FEED_ENTITY
            ? feed
            : components.find((row) => show(row['id']) === event.entityId);
    const newest = (event: TimelineEvent): number =>
        Math.max(
            ...(story.stream?.events ?? [])
                .filter(
                    (one) => one.entityType === event.entityType && one.entityId === event.entityId,
                )
                .map((one) => one.version),
        );
    const revertedVersion =
        reverting === null
            ? undefined
            : story.versions.get(
                  keyOf(reverting.entityType, reverting.entityId, reverting.version),
              );
    const revertedRow = reverting === null ? undefined : rowOf(reverting);

    return (
        <div className="space-y-3">
            <p className="text-sm text-ink-muted">{t('marketdata.history.lead')}</p>
            {story.error !== null && <Notice tone="error">{story.error.message}</Notice>}
            {story.stream === undefined ? (
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            ) : (
                <Timeline
                    timeline={story.stream}
                    actorPath={actorPathFor(holds('iam::accounts:read'))}
                    actorPicture={actorPicture}
                    renderActions={(event) => {
                        const may =
                            event.entityType === FEED_ENTITY ? mayWrite.feed : mayWrite.components;
                        if (!may || event.version >= newest(event)) {
                            return undefined;
                        }
                        return (
                            <div className="mt-2 flex justify-end">
                                <Button size="sm" icon="revert" onClick={() => setReverting(event)}>
                                    {t('history.revert')}
                                </Button>
                            </div>
                        );
                    }}
                />
            )}
            <p className="text-xs text-ink-faint">{t('marketdata.history.gap')}</p>
            {reverting !== null &&
                revertedVersion !== undefined &&
                revertedRow !== undefined &&
                (reverting.entityType === FEED_ENTITY ? (
                    <RevertDialog
                        resource={feedResource}
                        specs={feedSpecs}
                        row={revertedRow}
                        version={revertedVersion}
                        keep={keep}
                        complete={complete}
                        onClose={() => setReverting(null)}
                    />
                ) : (
                    <RevertDialog
                        resource={componentResource}
                        specs={COMPONENT_FIELDS}
                        row={revertedRow}
                        version={revertedVersion}
                        keep={['id', 'fx_spot_config_id']}
                        onClose={() => setReverting(null)}
                    />
                ))}
        </div>
    );
}
