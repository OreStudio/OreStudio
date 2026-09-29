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

/**
 * What the browser is running, and what the deployment is running.
 *
 * Both are stated, because either can be wrong on its own. The client version
 * is stamped into the bundle when it is built and never changes afterwards;
 * the server version arrives with the first read the browser makes. A person
 * looking at a screen that behaves like yesterday's build can settle it here:
 * two versions that disagree mean the bundle is stale, and the refresh that
 * fixes it is not a guess.
 *
 * The server's version is absent while the deployment has not answered yet,
 * and the line says so rather than inventing one.
 */

import type { ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';

export function VersionFooter({
    serverVersion,
}: {
    readonly serverVersion: string | undefined;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <footer className="border-t border-line px-5 py-2 text-xs text-ink-faint">
            <div className="flex flex-wrap items-center gap-x-4 gap-y-1">
                <span>{t('version.client', { version: __BUILD_VERSION__ })}</span>
                <span>
                    {serverVersion === undefined || serverVersion === ''
                        ? t('version.serverUnknown')
                        : t('version.server', { version: serverVersion })}
                </span>
            </div>
        </footer>
    );
}
