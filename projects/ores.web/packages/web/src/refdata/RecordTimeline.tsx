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
import type { ReactNode } from 'react';
import { recordEvents, type HistoryVersion } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { actorPathFor } from '../access/PeoplePage.js';
import { useHolds } from '../access/holds.js';
import { useActorPictures, usePeopleDirectory } from '../access/PersonRef.js';
import { Timeline } from '../timeline/Timeline.js';
import { Button, Notice } from '../ui/Primitives.js';

/**
 * The history of one record, drawn as the shared timeline: the event chart,
 * then one line per version with the note below it and the detail on demand.
 * This is the History tab of every record page and the history dialog of every
 * part, as the record screen standard sets it (RS-HIST-01). Reverting is the
 * caller's, because it is a write of the record the caller owns; it is offered
 * on every version but the newest, which has nothing to revert to.
 */
export function RecordTimeline({
    entityType,
    entityId,
    onRevert,
}: {
    readonly entityType: string;
    readonly entityId: string;
    readonly onRevert?: (version: HistoryVersion) => void;
}): ReactNode {
    const { t } = useTranslation();
    const holds = useHolds();
    const actorPicture = useActorPictures();
    const { isService } = usePeopleDirectory();
    const history = useQuery({
        queryKey: ['history', entityType, entityId],
        queryFn: () => api.history(entityType, entityId),
    });

    if (history.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (history.isError) {
        return <Notice tone="error">{history.error.message}</Notice>;
    }
    const versions = history.data;
    const newest = versions[0]?.version ?? 0;
    return (
        <Timeline
            timeline={{
                subject: 'record',
                id: entityId,
                events: [...recordEvents(entityType, entityId, versions)],
                gaps: [],
            }}
            actorPath={actorPathFor(holds('iam::accounts:read'))}
            actorPicture={actorPicture}
            isService={isService}
            renderActions={(event) => {
                if (onRevert === undefined || event.version >= newest) return undefined;
                const version = versions.find((v) => v.version === event.version);
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
