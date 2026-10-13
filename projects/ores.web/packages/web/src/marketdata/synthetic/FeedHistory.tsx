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
import { api, type RecordRow } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { useHolds } from '../../access/holds.js';
import { actorPathFor } from '../../access/PeoplePage.js';
import { useActorPictures } from '../../access/PersonRef.js';
import { RevertDialog, show, type FieldSpec } from '../../refdata/records.js';
import { Timeline } from '../../timeline/Timeline.js';
import { Button, Notice } from '../../ui/Primitives.js';

/**
 * One kind of child row a feed owns, such as the components of an FX feed's
 * mixture or the template entries of an IR curve feed. The history of a feed
 * is the feed's own and each of its children's, read as one stream.
 */
export interface FeedPart {
    /** The entity type the history is read by. */
    readonly entity: string;
    /** The synthetic resource a revert writes to. */
    readonly resource: string;
    /** The fields a revert offers and restores. */
    readonly specs: readonly FieldSpec[];
    /** The fields a revert carries over from the row as it stands. */
    readonly keep: readonly string[];
    /** The rows the feed holds now. */
    readonly rows: readonly RecordRow[];
    /** Whether the person may write this kind of row, so Revert is offered. */
    readonly mayWrite: boolean;
}

function keyOf(entityType: string, entityId: string, version: number): string {
    return `${entityType}\u0000${entityId}\u0000${String(version)}`;
}

/**
 * The feed's story as one stream: the feed's versions and the versions of
 * each child row it holds now, newest first, read as one history is read on
 * a person's page. Reads the history of the feed and of each child.
 */
export function useFeedStory(
    feed: RecordRow,
    feedEntity: string,
    parts: readonly FeedPart[],
    subject: string,
): {
    readonly stream: Stream | undefined;
    readonly versions: ReadonlyMap<string, HistoryVersion>;
    readonly error: Error | null;
} {
    const feedId = show(feed['id']);
    const reads = [
        { entityType: feedEntity, entityId: feedId },
        ...parts.flatMap((part) =>
            part.rows.map((row) => ({ entityType: part.entity, entityId: show(row['id']) })),
        ),
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
            : { subject, id: feedId, events: orderTimeline(events), gaps: [] },
        versions,
        error,
    };
}

/**
 * The History tab of a feed: one timeline for the feed and its child rows. A
 * version older than its record's newest offers Revert to a person who may
 * write it, which writes its values back as a new version.
 */
export function FeedHistory({
    feed,
    feedEntity,
    subject,
    parts,
    feedSpecs,
    feedResource,
    complete,
    keep,
    mayWrite,
}: {
    readonly feed: RecordRow;
    readonly feedEntity: string;
    /** What the timeline calls the stream, such as `fx_feed`. */
    readonly subject: string;
    readonly parts: readonly FeedPart[];
    readonly feedSpecs: readonly FieldSpec[];
    readonly feedResource: string;
    readonly complete: (write: Readonly<Record<string, unknown>>) => Record<string, unknown>;
    readonly keep: readonly string[];
    readonly mayWrite: boolean;
}): ReactNode {
    const { t } = useTranslation();
    const holds = useHolds();
    const actorPicture = useActorPictures();
    const story = useFeedStory(feed, feedEntity, parts, subject);
    const [reverting, setReverting] = useState<TimelineEvent | null>(null);
    const partOf = (event: TimelineEvent): FeedPart | undefined =>
        parts.find((part) => part.entity === event.entityType);
    const rowOf = (event: TimelineEvent): RecordRow | undefined =>
        event.entityType === feedEntity
            ? feed
            : partOf(event)?.rows.find((row) => show(row['id']) === event.entityId);
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
    const revertedPart = reverting === null ? undefined : partOf(reverting);

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
                            event.entityType === feedEntity
                                ? mayWrite
                                : (partOf(event)?.mayWrite ?? false);
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
                (reverting.entityType === feedEntity ? (
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
                    revertedPart !== undefined && (
                        <RevertDialog
                            resource={revertedPart.resource}
                            specs={revertedPart.specs}
                            row={revertedRow}
                            version={revertedVersion}
                            keep={revertedPart.keep}
                            onClose={() => setReverting(null)}
                        />
                    )
                ))}
        </div>
    );
}
