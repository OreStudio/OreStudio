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

import { useQueryClient } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import type { LoadState } from '../events/useEntityChanges.js';
import { useLoadState } from '../events/useLoadState.js';
import { useTranslation } from '../i18n/Provider.js';
import { formatDateTime } from './Time.js';
import { LoadedAt } from './LoadedAt.js';
import { Button } from './Primitives.js';

/**
 * The one Refresh every list, tree and record page offers.
 *
 * It reads the data again and nothing else. It takes the screen's load state
 * and the news about it, the same two values the stale logic works from, so a
 * screen cannot show the button and forget to say how old its data is. The
 * time of the load stands under the button, quietly.
 *
 * When the server has said the data changed, the button turns gold, pulses a
 * few times and explains why in its tooltip, with the time of the change when
 * the screen knows it. The gold stays until the screen loads again. It does not
 * refresh by itself, because a screen that moves while somebody reads it is
 * worse than one that is briefly out of date.
 */
export function RefreshButton({
    onClick,
    load,
    news,
}: {
    readonly onClick: () => void;
    readonly load: LoadState;
    readonly news: {
        readonly stale: boolean;
        /** The server's time of the newest change, for the tooltip. */
        readonly changedAt: string | undefined;
    };
}): ReactNode {
    const { t, language } = useTranslation();
    const { stale, changedAt } = news;
    return (
        <div className="flex flex-col items-end gap-1">
            <Button
                icon="refresh"
                onClick={onClick}
                pending={load.isFetching}
                {...(stale
                    ? {
                          className: 'stale-mark stale-pulse',
                          title:
                              changedAt === undefined || changedAt === ''
                                  ? t('common.refreshStale')
                                  : t('common.refreshStaleAt', {
                                        time: formatDateTime(changedAt, language),
                                    }),
                      }
                    : {})}
            >
                {t('common.refresh')}
            </Button>
            <LoadedAt at={load.oldestUpdatedAt ?? load.dataUpdatedAt} />
        </div>
    );
}

/**
 * Refresh for a screen that is not told when its data changes: it reads again
 * and shows that it is reading. It has no stale state, because nothing says
 * the data went stale.
 */
export function RefreshNow({
    onClick,
    pending = false,
}: {
    readonly onClick: () => void;
    readonly pending?: boolean;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <Button icon="refresh" onClick={onClick} pending={pending}>
            {t('common.refresh')}
        </Button>
    );
}

/**
 * Refresh for a screen whose rows come from queries: it reads again every query
 * that starts with one of the given keys, shows that it is reading, and states
 * when the data arrived.
 *
 * It carries the same load label as RefreshButton, because a screen that offers
 * the button must also say how old what it shows is. It never turns gold: these
 * screens are not told that their data changed.
 */
export function RefreshQueries({
    keys,
}: {
    readonly keys: readonly (readonly unknown[])[];
}): ReactNode {
    const client = useQueryClient();
    const load = useLoadState(keys);
    return (
        <div className="flex flex-col items-end gap-1">
            <RefreshNow
                pending={load.isFetching}
                onClick={() => {
                    for (const queryKey of keys) {
                        void client.invalidateQueries({ queryKey });
                    }
                }}
            />
            <LoadedAt at={load.oldestUpdatedAt ?? load.dataUpdatedAt} />
        </div>
    );
}
