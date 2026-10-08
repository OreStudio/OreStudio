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
 * The journeys that carry on from a screen.
 *
 * A journey is named by the identifier its page carries under
 * doc/knowledge/journeys, so a screen states which journeys follow it without
 * holding a route of its own. A journey whose screen is not built yet is stated
 * rather than linked: a link that leads nowhere reads as a fault, and the
 * journey is still worth naming.
 */

import type { ReactNode } from 'react';
import { Link } from 'react-router';
import { useTranslation } from '../i18n/Provider.js';
import { Tag } from '../ui/Primitives.js';

export interface JourneyTarget {
    readonly titleKey: string;
    /** Where the journey's screen is, once one exists. */
    readonly to?: string;
}

/**
 * Every journey a screen may name, by the identifier its page carries.
 *
 * The tuple is the union a screen's id list is typed against, so a mistyped
 * identifier is a compile error rather than a section that renders empty.
 */
export const JOURNEY_IDS = [
    'ED949529-0A80-4661-BBAC-E5DB7A6E5828',
    '57C4B9A6-DA79-403E-984E-50D2561352B3',
    '7B820710-161C-4926-B5AA-5EF2772A3652',
    'EE26E58C-F389-4E82-BF96-EC5324F9F795',
    'C3D59907-9D6A-448C-9A61-9E750755BBFB',
    '22AC8DD8-A440-4330-9992-47A5E9985473',
] as const;
export type JourneyId = (typeof JOURNEY_IDS)[number];

export const JOURNEY_TARGETS: Readonly<Record<JourneyId, JourneyTarget>> = {
    // See the running services.
    'ED949529-0A80-4661-BBAC-E5DB7A6E5828': {
        titleKey: 'operations.journeys.services',
        to: '/operations/services',
    },
    // Read the telemetry logs.
    '57C4B9A6-DA79-403E-984E-50D2561352B3': {
        titleKey: 'operations.journeys.logs',
        to: '/operations/logs',
    },
    // Watch the compute grid.
    '7B820710-161C-4926-B5AA-5EF2772A3652': {
        titleKey: 'operations.journeys.grid',
        to: '/operations/grid',
    },
    // Watch the message bus.
    'EE26E58C-F389-4E82-BF96-EC5324F9F795': {
        titleKey: 'operations.journeys.bus',
        to: '/operations/bus',
    },
    // Check the versions and the database.
    'C3D59907-9D6A-448C-9A61-9E750755BBFB': {
        titleKey: 'operations.journeys.versions',
        to: '/operations/versions',
    },
    // Audit sign-ins.
    '22AC8DD8-A440-4330-9992-47A5E9985473': {
        titleKey: 'operations.journeys.audit',
        to: '/audit',
    },
};

export function RelatedJourneys({ ids }: { readonly ids: readonly JourneyId[] }): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card space-y-3 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('operations.related.title')}</h2>
                <span className="text-xs text-ink-faint">{t('operations.related.lead')}</span>
            </header>
            <ul className="space-y-2 text-sm">
                {ids.map((id) => {
                    const target = JOURNEY_TARGETS[id];
                    return (
                        <li key={id} className="flex flex-wrap items-center gap-2">
                            {target.to === undefined ? (
                                <>
                                    <span className="text-ink-muted">{t(target.titleKey)}</span>
                                    <Tag tone="muted">{t('operations.related.notBuilt')}</Tag>
                                </>
                            ) : (
                                <Link to={target.to} className="text-accent-bright hover:underline">
                                    {t(target.titleKey)}
                                </Link>
                            )}
                        </li>
                    );
                })}
            </ul>
        </section>
    );
}
