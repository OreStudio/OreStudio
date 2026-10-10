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
 * What the browser is running, what the deployment is running, and which
 * environment it serves.
 *
 * All three are stated, because any can be wrong on its own. The client version
 * is stamped into the bundle when it is built and never changes afterwards;
 * the server version arrives with the first read the browser makes. A person
 * looking at a screen that behaves like yesterday's build can settle it here:
 * two versions that disagree mean the bundle is stale, and the refresh that
 * fixes it is not a guess.
 *
 * The environment settles the other question a person at a screen must be able
 * to answer without asking anybody: which checkout they are pointed at, and
 * whether it is production. A non-production environment says so in words,
 * because mistaking one environment for another is the mistake that costs the
 * most.
 *
 * The server's version and the environment are absent while the deployment has
 * not answered yet, and the lines say so rather than inventing values.
 */

import type { ReactNode } from 'react';
import type { EnvironmentView } from '@ores/contracts';
import { useTranslation } from '../i18n/Provider.js';
import { Tag } from '../ui/Primitives.js';

export function VersionFooter({
    serverVersion,
    environment,
    tenantName,
    partyName,
}: {
    readonly serverVersion: string | undefined;
    readonly environment: EnvironmentView | undefined;
    /** The tenant the session works in, stated on every screen once somebody is signed in. */
    readonly tenantName?: string;
    /** The party the session acts for. */
    readonly partyName?: string | undefined;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <footer className="border-t border-line px-5 py-2">
            <div className="flex flex-wrap items-center gap-x-2 gap-y-1">
                {/* Where the person is working is the fact most worth seeing. */}
                {tenantName !== undefined && tenantName !== '' && (
                    <Tag small tone="accent">
                        {t('version.tenant', { name: tenantName })}
                    </Tag>
                )}
                {partyName !== undefined && partyName !== '' && (
                    <Tag small tone="accent">
                        {t('version.party', { name: partyName })}
                    </Tag>
                )}
                <Tag small tone="neutral">
                    {t('version.client', { version: __BUILD_VERSION__ })}
                </Tag>
                <Tag
                    small
                    tone={serverVersion === undefined || serverVersion === '' ? 'muted' : 'up'}
                >
                    {serverVersion === undefined || serverVersion === ''
                        ? t('version.serverUnknown')
                        : t('version.server', { version: serverVersion })}
                </Tag>
                {environment === undefined ? (
                    <Tag small tone="muted">
                        {t('version.environmentUnknown')}
                    </Tag>
                ) : (
                    <>
                        <Tag small tone="neutral">
                            {t('version.environment', { name: environment.displayName })}
                        </Tag>
                        {/* Production is the environment where a mistake costs the most. */}
                        <Tag small tone={environment.nonProduction ? 'warn' : 'down'}>
                            {environment.nonProduction
                                ? t('version.kindDev')
                                : t('version.kindProduction')}
                        </Tag>
                    </>
                )}
            </div>
        </footer>
    );
}
