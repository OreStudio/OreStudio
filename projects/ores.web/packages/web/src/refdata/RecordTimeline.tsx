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
import type { ReactNode } from 'react';
import {
    orderTimeline,
    recordEvents,
    type HistoryVersion,
    type TimelineEvent,
} from '@ores/wire-protocol/browser';
import { api, type RecordRow } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { actorPathFor } from '../access/PeoplePage.js';
import { useHolds } from '../access/holds.js';
import { useActorPictures, usePeopleDirectory } from '../access/PersonRef.js';
import { Timeline } from '../timeline/Timeline.js';
import { Button, Notice } from '../ui/Primitives.js';
import { show } from './records.js';

/**
 * The parts of a record whose versions the record's history carries: the
 * rows it holds now, and how one of them is named on an entry.
 */
export interface TimelinePart {
    readonly entityType: string;
    readonly rows: readonly RecordRow[];
    readonly name: (row: RecordRow) => string;
}

/**
 * The history of one record, drawn as the shared timeline: the event chart,
 * then one line per version with the note below it and the detail on demand.
 * This is the History tab of every record page and the history dialog of every
 * part, as the record screen standard sets it (RS-HIST-01). A record page gives
 * its parts too, and the stream is then the record's story: its own versions
 * and those of every part it holds now, newest first. A part deleted from the
 * record is not read, and the stream says so. Reverting is the caller's,
 * because it is a write of the record the caller owns; it is offered on every
 * version of the record but the newest, which has nothing to revert to.
 */
export function RecordTimeline({
    entityType,
    entityId,
    parts = [],
    onRevert,
}: {
    readonly entityType: string;
    readonly entityId: string;
    readonly parts?: readonly TimelinePart[];
    readonly onRevert?: (version: HistoryVersion) => void;
}): ReactNode {
    const { t } = useTranslation();
    const holds = useHolds();
    const actorPicture = useActorPictures();
    const { isService } = usePeopleDirectory();
    const reads = [
        { entityType, entityId, name: undefined as string | undefined },
        ...parts.flatMap((part) =>
            part.rows.map((row) => ({
                entityType: part.entityType,
                entityId: show(row['id']),
                name: part.name(row),
            })),
        ),
    ];
    const results = useQueries({
        queries: reads.map((read) => ({
            queryKey: ['history', read.entityType, read.entityId],
            queryFn: () => api.history(read.entityType, read.entityId),
        })),
    });

    if (results.some((result) => result.isPending)) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    const failed = results.find((result) => result.error !== null);
    if (failed?.error) {
        return <Notice tone="error">{failed.error.message}</Notice>;
    }
    const own = results[0]?.data ?? [];
    const newest = own[0]?.version ?? 0;
    const names = new Map<string, string>();
    const events: TimelineEvent[] = [];
    reads.forEach((read, index) => {
        if (read.name !== undefined) names.set(`${read.entityType}:${read.entityId}`, read.name);
        events.push(...recordEvents(read.entityType, read.entityId, results[index]?.data ?? []));
    });
    const versionOf = (event: TimelineEvent): HistoryVersion | undefined =>
        own.find((version) => version.version === event.version);
    return (
        <Timeline
            timeline={{
                subject: 'record',
                id: entityId,
                events: orderTimeline(events),
                gaps: parts.length === 0 ? [] : [{ entity: 'record_parts_removed', reason: '' }],
            }}
            actorPath={actorPathFor(holds('iam::accounts:read'))}
            actorPicture={actorPicture}
            isService={isService}
            recordName={(event) => names.get(`${event.entityType}:${event.entityId}`)}
            renderActions={(event) => {
                if (onRevert === undefined || event.entityType !== entityType) return undefined;
                if (event.version >= newest) return undefined;
                const version = versionOf(event);
                if (version === undefined) return undefined;
                return (
                    <div className="mt-2 flex justify-end">
                        <Button size="sm" icon="revert" onClick={() => onRevert(version)}>
                            {t('history.revert')}
                        </Button>
                    </div>
                );
            }}
        />
    );
}
