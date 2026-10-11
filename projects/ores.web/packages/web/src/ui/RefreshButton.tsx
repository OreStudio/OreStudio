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
import { useTranslation } from '../i18n/Provider.js';
import { Button } from './Primitives.js';

/**
 * The one Refresh every list, tree and record page offers.
 *
 * It reads the data again and nothing else. When the server has said the data
 * changed, `stale` makes it pulse and explains why in its tooltip; it does not
 * refresh by itself, because a screen that moves while somebody reads it is
 * worse than one that is briefly out of date.
 */
export function RefreshButton({
    onClick,
    pending = false,
    stale = false,
}: {
    readonly onClick: () => void;
    readonly pending?: boolean;
    readonly stale?: boolean;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <Button
            icon="refresh"
            onClick={onClick}
            pending={pending}
            {...(stale ? { className: 'stale-pulse', title: t('common.refreshStale') } : {})}
        >
            {t('common.refresh')}
        </Button>
    );
}

/**
 * Refresh for a screen whose rows come from queries: it reads again every query
 * that starts with one of the given keys, and shows that it is reading.
 */
export function RefreshQueries({
    keys,
}: {
    readonly keys: readonly (readonly unknown[])[];
}): ReactNode {
    const client = useQueryClient();
    const reading = keys.some((queryKey) => client.isFetching({ queryKey }) > 0);
    return (
        <RefreshButton
            pending={reading}
            onClick={() => {
                for (const queryKey of keys) {
                    void client.invalidateQueries({ queryKey });
                }
            }}
        />
    );
}
