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
import { useEffect, useSyncExternalStore, type ReactNode } from 'react';
import { Link, useLocation } from 'react-router';
import { clearRefusal, currentRefusal, subscribeRefusal } from '../api/errors.js';
import { useTranslation } from '../i18n/Provider.js';
import { trail } from '../log/clientLog.js';
import { Button, Notice } from '../ui/Primitives.js';

const access = trail('access');

/**
 * Stands in for a screen whose read the server refused after it was drawn.
 *
 * A screen is drawn only for somebody who can use it, so a refusal that follows
 * is an inconsistency, and it is ours, not the person's. Two things can be true:
 * their access changed since the roles were read, or the screen asked for
 * something it did not declare. The roles are read again, and if they changed
 * the screen's own guard takes over and says what is missing. If they did not,
 * the person is told plainly that it should have worked, and given the reference
 * that finds the request in the log.
 */
export function RefusalGuard({ children }: { readonly children: ReactNode }): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const { pathname } = useLocation();
    const refusal = useSyncExternalStore(subscribeRefusal, currentRefusal, currentRefusal);

    useEffect(() => {
        clearRefusal();
    }, [pathname]);

    useEffect(() => {
        if (refusal === undefined) return;
        const before = JSON.stringify(queries.getQueryData(['my-access']));
        access.error('access inconsistent', {
            operation: refusal.operation,
            request_id: refusal.requestId,
            route: pathname,
        });
        void queries.refetchQueries({ queryKey: ['my-access'] }).then(() => {
            if (JSON.stringify(queries.getQueryData(['my-access'])) !== before) {
                access.info('access changed', { route: pathname });
                clearRefusal();
            }
        });
    }, [refusal, queries, pathname]);

    if (refusal === undefined) return children;
    return (
        <div className="max-w-xl space-y-3">
            <Notice tone="error">
                <div className="space-y-2">
                    <p className="font-medium">{t('unavailable.failedTitle')}</p>
                    <p>{t('unavailable.failed')}</p>
                    <p className="font-mono text-xs">
                        {t('unavailable.reference', { id: refusal.requestId.slice(0, 8) })}
                        {refusal.operation === '' ? '' : ` · ${refusal.operation}`}
                    </p>
                    <div className="flex gap-3">
                        <Button
                            size="sm"
                            onClick={() => {
                                clearRefusal();
                                void queries.invalidateQueries();
                            }}
                        >
                            {t('unavailable.retry')}
                        </Button>
                        <Link className="text-sm underline" to="/">
                            {t('unavailable.home')}
                        </Link>
                    </div>
                </div>
            </Notice>
        </div>
    );
}
