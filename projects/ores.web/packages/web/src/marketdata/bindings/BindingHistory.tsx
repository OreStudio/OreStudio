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
import { useState, type ReactNode } from 'react';
import { orderTimeline, recordEvents, type HistoryVersion } from '@ores/wire-protocol/browser';
import { api, type RecordRow } from '../../api/client.js';
import { bindings } from '../../api/bindings.js';
import { useTranslation } from '../../i18n/Provider.js';
import { useHolds } from '../../access/holds.js';
import { actorPathFor } from '../../access/PeoplePage.js';
import { useActorPictures, usePeopleDirectory } from '../../access/PersonRef.js';
import { RevertDialog, show, type FieldSpec } from '../../refdata/records.js';
import { Timeline } from '../../timeline/Timeline.js';
import { Button, Notice } from '../../ui/Primitives.js';
import { BINDING_ENTITY, BINDINGS } from './bindingPaths.js';
import { heldBy, revertOffer } from './bindingView.js';

/**
 * The story of a binding: one stream of its own versions under a chart of the
 * events over time, newest first, with the author of each entry. A version
 * older than the newest offers Revert to a person who may write a synthetic
 * binding, unless the series the binding writes is held by a running feed.
 */
export function BindingHistory({
    row,
    specs,
    kind,
    mayWrite,
}: {
    readonly row: RecordRow;
    readonly specs: readonly FieldSpec[];
    /** The binding's producer kind, which decides whether Revert is offered. */
    readonly kind: string;
    /** Whether the person may write a binding. */
    readonly mayWrite: boolean;
}): ReactNode {
    const { t } = useTranslation();
    const holds = useHolds();
    const pictureOf = useActorPictures();
    const { isService } = usePeopleDirectory();
    const id = show(row['id']);
    const sourceName = show(row['source_name']);
    const [reverting, setReverting] = useState<number | null>(null);
    const history = useQuery({
        queryKey: ['history', BINDING_ENTITY, sourceName],
        queryFn: () => api.history(BINDING_ENTITY, sourceName),
    });
    // A source a running feed holds may not be written back: a revert would take
    // a series another feed is publishing.
    const sources = useQuery({
        queryKey: ['bindings', 'sources'],
        queryFn: () => bindings.sources(),
    });

    if (history.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (history.isError) {
        return <Notice tone="error">{history.error.message}</Notice>;
    }

    const versions: readonly HistoryVersion[] = history.data;
    const events = orderTimeline(recordEvents(BINDING_ENTITY, id, versions));
    const newest = events.reduce((top, event) => Math.max(top, event.version), 0);
    const held = heldBy(sources.data, sourceName);
    const reverted = versions.find((version) => version.version === reverting);

    return (
        <div className="space-y-3">
            <p className="text-sm text-ink-muted">{t('marketdata.bindings.historyLead')}</p>
            <Timeline
                timeline={{ subject: 'feed_binding', id, events, gaps: [] }}
                actorPath={actorPathFor(holds('iam::accounts:read'))}
                actorPicture={pictureOf}
                isService={isService}
                renderActions={(event) =>
                    revertOffer({
                        kind,
                        mayWrite,
                        version: event.version,
                        newest,
                        heldBy: held,
                    }) ? (
                        <div className="mt-2 flex justify-end">
                            <Button
                                size="sm"
                                icon="revert"
                                onClick={() => setReverting(event.version)}
                            >
                                {t('history.revert')}
                            </Button>
                        </div>
                    ) : undefined
                }
            />
            <p className="text-xs text-ink-faint">{t('marketdata.bindings.historyGap')}</p>
            {reverted !== undefined && (
                <RevertDialog
                    resource={BINDINGS}
                    specs={specs}
                    row={row}
                    version={reverted}
                    keep={['id']}
                    onClose={() => setReverting(null)}
                />
            )}
        </div>
    );
}
