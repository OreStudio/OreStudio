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

import type { ReactNode } from 'react';
import { Link } from 'react-router';
import type { SessionMode } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { Detail, LinkButton, PageHeader } from '../ui/Primitives.js';
import { areasFor, modeKey, type ShellArea, type ShellJourney } from '../shell/areas.js';

/**
 * Where a signed-in person lands.
 *
 * The mode is what decides the shape of this page. A mode whose journeys have
 * been implemented lands on that mode's areas, each holding its journeys as
 * cards: that is the shell's menu, drawn in full rather than only in the
 * header, because a person who has signed in should be able to see what they
 * can do without opening anything.
 *
 * A mode no group has implemented yet lands on the session instead. That is not
 * a placeholder for a landing that is coming — it is the honest content: the
 * tenant and the party the person is working in are facts, and a screen that
 * shows real state is readable where a hero that promises features is not.
 */
export interface HomePageProps {
    readonly username: string;
    readonly email: string;
    readonly tenantName: string;
    readonly partyName: string;
    /** The context the session runs in, which decides what this page shows. */
    readonly mode: SessionMode;
    /**
     * Whether the session only reads, as it does inside a tenant a system
     * administrator entered. A read-only session is offered no change.
     */
    readonly readOnly?: boolean;
}

export function HomePage({
    username,
    email,
    tenantName,
    partyName,
    mode,
    readOnly = false,
}: HomePageProps): ReactNode {
    const areas = areasFor(mode);

    if (areas.length > 0) {
        return <ModeLanding mode={mode} areas={areas} />;
    }

    return (
        <SessionCard
            username={username}
            email={email}
            tenantName={tenantName}
            partyName={partyName}
            readOnly={readOnly}
        />
    );
}

/** The landing for a mode whose journeys exist: its areas, and their cards. */
function ModeLanding({
    mode,
    areas,
}: {
    readonly mode: SessionMode;
    readonly areas: readonly ShellArea[];
}): ReactNode {
    const { t, plural } = useTranslation();
    const count = areas.reduce((total, area) => total + area.journeys.length, 0);

    return (
        <div className="space-y-8">
            <div className="card p-6">
                <PageHeader
                    title={t(modeKey(mode))}
                    description={plural('shell.journeyCount', count)}
                />
            </div>
            {areas.map((area) => (
                <section key={area.nameKey} id={area.nameKey} className="space-y-4">
                    <h2 className="text-sm font-semibold tracking-tight text-ink">
                        {t(area.nameKey)}
                    </h2>
                    <div className="grid gap-4 sm:grid-cols-2 lg:grid-cols-3">
                        {area.journeys.map((journey) => (
                            <JourneyCard key={journey.nameKey} journey={journey} />
                        ))}
                    </div>
                </section>
            ))}
        </div>
    );
}

/**
 * One journey, as a card.
 *
 * A journey the tree has not built is drawn as a card that names itself and
 * says so, so the area reads as work that is partly done rather than as an area
 * nobody has looked at. It is not a link, because there is nothing to open.
 */
function JourneyCard({ journey }: { readonly journey: ShellJourney }): ReactNode {
    const { t } = useTranslation();
    const name = t(journey.nameKey);

    if (journey.to === undefined) {
        return (
            <div className="card flex items-start justify-between gap-3 p-4">
                <span className="text-sm text-ink-muted">{name}</span>
                <span className="shrink-0 rounded-full border border-line px-2 py-0.5 text-[11px] text-ink-faint">
                    {t('shell.notBuilt')}
                </span>
            </div>
        );
    }

    return (
        <Link
            to={journey.to}
            className="card block p-4 text-sm text-ink hover:border-accent/60 focus-visible:outline-accent"
        >
            {name}
        </Link>
    );
}

/** What a mode with no journeys yet shows: the session it was opened with. */
function SessionCard({
    username,
    email,
    tenantName,
    partyName,
    readOnly,
}: Omit<HomePageProps, 'mode' | 'readOnly'> & { readonly readOnly: boolean }): ReactNode {
    const { t } = useTranslation();

    return (
        <div className="card p-6">
            <PageHeader title={t('home.title')} description={t('home.next')} />
            <dl className="grid gap-4 sm:grid-cols-2">
                <Detail label={t('home.username')} value={username} />
                <Detail label={t('home.email')} value={email} />
                <Detail label={t('home.tenant')} value={tenantName} />
                <Detail label={t('home.party')} value={partyName} />
            </dl>
            {/*
             * A party is added from here until the Parties page is its home. A
             * tenant is not: the Tenants area of system administration is where
             * one is created, and this card is shown to sessions that cannot.
             */}
            <div className="mt-6 flex flex-wrap gap-3">
                <LinkButton to="/parties" variant="secondary">
                    {t('home.parties')}
                </LinkButton>
                {!readOnly && (
                    <LinkButton to="/parties/new" variant="secondary">
                        {t('home.newParty')}
                    </LinkButton>
                )}
            </div>
        </div>
    );
}
