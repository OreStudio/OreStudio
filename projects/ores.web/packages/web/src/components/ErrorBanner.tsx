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

import { useSyncExternalStore, type ReactNode } from 'react';
import { current, dismiss, dismissAll, subscribe } from '../api/errors.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Notice } from '../ui/Primitives.js';

/**
 * The one place a failed request is stated for every screen.
 *
 * A screen that renders its own notice beside the thing that failed still does,
 * because a message next to the control that did not answer is better than a
 * banner far from it. This is the safety net for the screens that show nothing,
 * and it sits above the route table so the rails, the shell and the public
 * screens all carry it.
 *
 * The reports are module state rather than a query, so the store is read
 * through `useSyncExternalStore` and the same snapshot is served on the server
 * where there is nothing to subscribe to.
 */
export function ErrorBanner(): ReactNode {
    const { t } = useTranslation();
    const reports = useSyncExternalStore(subscribe, current, current);
    if (reports.length === 0) {
        return null;
    }
    return (
        <div className="mx-auto w-full max-w-[680px] px-5 pt-5">
            <Notice tone="error">
                <div className="flex items-start justify-between gap-4">
                    <p className="font-semibold">{t('error.heading')}</p>
                    {reports.length > 1 && (
                        <Button variant="ghost" size="sm" onClick={dismissAll}>
                            {t('error.dismissAll')}
                        </Button>
                    )}
                </div>
                <ul className="mt-2 space-y-2">
                    {reports.map((report) => (
                        <li key={report.id} className="flex items-start justify-between gap-4">
                            <p className="font-mono break-words">{report.message}</p>
                            <Button
                                variant="ghost"
                                size="sm"
                                className="shrink-0"
                                onClick={() => dismiss(report.id)}
                            >
                                {t('error.dismiss')}
                            </Button>
                        </li>
                    ))}
                </ul>
            </Notice>
        </div>
    );
}
