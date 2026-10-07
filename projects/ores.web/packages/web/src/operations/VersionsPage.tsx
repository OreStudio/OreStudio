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
 * Check the versions and the database, from
 * doc/knowledge/journeys/operations/journey_check_the_versions_and_the_database.org.
 *
 * The client comes from the bundle stamp. The server build and the database row
 * arrive with the login answer and the session keeps both, so this screen reads
 * the session and makes no request of its own. A session whose answer carried no
 * database row states the panel as unknown rather than inventing values.
 */

import type { ReactNode } from 'react';
import type { SessionView } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { Detail, PageHeader } from '../ui/Primitives.js';
import { GapPanel, OperationsBack, type ScreenGap } from './OperationsParts.js';
import { RelatedJourneys, type JourneyId } from './RelatedJourneys.js';

/** The journeys that carry on from this one, in the order its page names them. */
const JOURNEYS: readonly JourneyId[] = [
    'ED949529-0A80-4661-BBAC-E5DB7A6E5828',
    '57C4B9A6-DA79-403E-984E-50D2561352B3',
    '7B820710-161C-4926-B5AA-5EF2772A3652',
    '22AC8DD8-A440-4330-9992-47A5E9985473',
];

/** The address the browser is talking to, or nothing before it has one. */
function browserAddress(): string {
    return typeof window === 'undefined' ? '' : window.location.origin;
}

export function VersionsPage({
    session,
    serverVersion,
}: {
    readonly session: SessionView;
    /**
     * The deployment's build, resolved the way the footer resolves it: the
     * session's own value, or the bootstrap answer's when the login answer
     * carried none. The panel and the footer therefore never disagree.
     */
    readonly serverVersion: string;
}): ReactNode {
    const { t } = useTranslation();
    const { database } = session;
    const address = browserAddress();
    const gaps: readonly ScreenGap[] = [
        {
            title: t('operations.versions.gap.shape.title'),
            body: t('operations.versions.gap.shape.body'),
            journey: t('operations.journeys.versions'),
        },
        {
            title: t('operations.versions.gap.releases.title'),
            body: t('operations.versions.gap.releases.body'),
            journey: t('operations.journeys.services'),
        },
    ];

    return (
        <div className="space-y-6">
            <PageHeader
                title={t('operations.versions.title')}
                description={t('operations.versions.description')}
                actions={<OperationsBack />}
            />

            <section className="card space-y-4 p-6">
                <header className="flex flex-wrap items-baseline justify-between gap-2">
                    <h2 className="text-lg font-medium">{t('operations.versions.client.title')}</h2>
                    <span className="text-xs text-ink-faint">
                        {t('operations.versions.client.lead')}
                    </span>
                </header>
                <div className="grid gap-x-6 gap-y-3 sm:grid-cols-3">
                    <Detail
                        label={t('operations.versions.client.version')}
                        value={__BUILD_RELEASE__}
                        mono
                    />
                    <Detail
                        label={t('operations.versions.client.commit')}
                        value={
                            __BUILD_COMMIT__ === ''
                                ? t('operations.versions.unknown')
                                : __BUILD_COMMIT__
                        }
                        mono={__BUILD_COMMIT__ !== ''}
                    />
                    <Detail
                        label={t('operations.versions.client.checkout')}
                        value={
                            __BUILD_COMMIT__ === ''
                                ? t('operations.versions.client.checkoutUnknown')
                                : __BUILD_DIRTY__
                                  ? t('operations.versions.client.dirty')
                                  : t('operations.versions.client.clean')
                        }
                    />
                </div>
                <p className="text-xs text-ink-faint">{t('operations.versions.client.hint')}</p>
            </section>

            <section className="card space-y-4 p-6">
                <header className="flex flex-wrap items-baseline justify-between gap-2">
                    <h2 className="text-lg font-medium">{t('operations.versions.server.title')}</h2>
                    <span className="text-xs text-ink-faint">
                        {t('operations.versions.server.lead')}
                    </span>
                </header>
                <div className="grid gap-x-6 gap-y-3 sm:grid-cols-2">
                    <Detail
                        label={t('operations.versions.server.version')}
                        value={
                            serverVersion === '' ? t('operations.versions.unknown') : serverVersion
                        }
                        mono={serverVersion !== ''}
                    />
                    <Detail
                        label={t('operations.versions.server.address')}
                        value={address === '' ? t('operations.versions.unknown') : address}
                        mono={address !== ''}
                    />
                </div>
                <p className="text-xs text-ink-faint">{t('operations.versions.server.hint')}</p>
            </section>

            <section className="card space-y-4 p-6">
                <header className="flex flex-wrap items-baseline justify-between gap-2">
                    <h2 className="text-lg font-medium">
                        {t('operations.versions.database.title')}
                    </h2>
                    <span className="text-xs text-ink-faint">
                        {t('operations.versions.database.lead')}
                    </span>
                </header>
                <div className="grid gap-x-6 gap-y-3 sm:grid-cols-4">
                    <Detail
                        label={t('operations.versions.database.fingerprint')}
                        value={
                            database.fingerprint === ''
                                ? t('operations.versions.unknown')
                                : database.fingerprint
                        }
                        mono={database.fingerprint !== ''}
                    />
                    <Detail
                        label={t('operations.versions.database.environment')}
                        value={
                            database.environment === ''
                                ? t('operations.versions.unknown')
                                : database.environment
                        }
                    />
                    <Detail
                        label={t('operations.versions.database.commit')}
                        value={
                            database.commit === ''
                                ? t('operations.versions.unknown')
                                : database.commit
                        }
                        mono={database.commit !== ''}
                    />
                    <Detail
                        label={t('operations.versions.database.created')}
                        value={
                            database.created === ''
                                ? t('operations.versions.unknown')
                                : database.created
                        }
                    />
                </div>
                <p className="text-xs text-ink-faint">{t('operations.versions.database.hint')}</p>
            </section>

            <GapPanel gaps={gaps} />
            <RelatedJourneys ids={JOURNEYS} />
        </div>
    );
}
