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
import { headerMark } from '../assets/brand.js';
import { Button } from '../ui/Primitives.js';
import { areasFor, modeKey } from '../shell/areas.js';
import { SHELL_WIDTHS, type ShellWidth } from '../shell/layout.js';
import { VersionFooter } from './VersionFooter.js';

/**
 * The shell a signed-in person gets.
 *
 * The session is passed in rather than read here, so the shell renders from its
 * props and a test can render it without a server or a session.
 *
 * The header states the mode the session runs in and offers the areas that mode
 * shows. Both come from the server's answer, so the menu cannot disagree with
 * what the session may do: the areas are the client's structure, and whether a
 * person is in this mode at all is not the client's to decide.
 *
 * The mode is stated rather than chosen. A person does not switch between
 * contexts, because the context is a fact about the account they signed in
 * with; a control that offered a switch would be a control that could lie.
 *
 * Signing out is the one action that belongs on every screen.
 */
export interface AppShellProps {
    readonly username: string;
    readonly tenantName: string;
    readonly partyName: string | undefined;
    /** The context the session runs in, as the server stated it. */
    readonly mode: SessionMode;
    /**
     * How wide the screen may be.
     *
     * A journey is a column and a list is a workspace. The default is the
     * column, because a screen that has not said is more often a form than a
     * table, and a form that runs the width of a wide display is the harder
     * mistake to read.
     */
    readonly width?: ShellWidth;
    readonly onSignOut: () => void;
    /**
     * The tenant a system administrator entered, or `null`.
     *
     * While set, every screen says so above its content and offers the way
     * out, because the person reads another tenant's data and must not lose
     * track of that.
     */
    readonly actingIn?: { readonly tenantName: string } | null;
    readonly onLeaveTenant?: () => void;
    /** The build the deployment answers with, or nothing before it answers. */
    readonly serverVersion?: string;
    readonly children: ReactNode;
}

export function AppShell({
    username,
    tenantName,
    partyName,
    mode,
    width = 'column',
    onSignOut,
    actingIn = null,
    onLeaveTenant,
    serverVersion,
    children,
}: AppShellProps): ReactNode {
    const { t } = useTranslation();
    /*
     * The three parts are joined rather than printed one after another,
     * because a part the session does not carry would otherwise leave its
     * separator behind and read as a dot with nothing beside it.
     */
    const session = [username, tenantName, partyName]
        .filter((part) => part !== undefined && part !== '')
        .join(' · ');
    const areas = areasFor(mode);

    return (
        <div className="flex min-h-full flex-col bg-bg-primary">
            <header className="border-b border-line">
                <div className="flex items-center gap-3 px-5 py-4">
                    <Link to="/" className="flex items-center gap-3">
                        <img src={headerMark} alt="" className="h-7 w-auto" />
                        <span className="text-sm font-semibold tracking-tight text-ink">
                            {t('app.name')}
                        </span>
                    </Link>
                    <span
                        className="rounded-full border border-line px-2 py-0.5 text-[11px] text-ink-muted"
                        title={t('nav.mode')}
                    >
                        {t(modeKey(mode))}
                    </span>
                    {/*
                     * The menu holds the areas this mode shows. An area that
                     * belongs to another mode is absent rather than disabled,
                     * because it is not a door this person may open later.
                     */}
                    {areas.length > 0 && (
                        <nav aria-label={t('nav.areas')} className="flex items-center gap-1">
                            {areas.map((area) => (
                                <a
                                    key={area.nameKey}
                                    href={`#${area.nameKey}`}
                                    className="rounded-md px-2 py-1 text-xs text-ink-muted hover:text-ink"
                                >
                                    {t(area.nameKey)}
                                </a>
                            ))}
                        </nav>
                    )}
                    <div className="ml-auto flex items-center gap-3 text-xs text-ink-muted">
                        <span>{session}</span>
                        <Button variant="ghost" size="sm" onClick={onSignOut}>
                            {t('nav.signOut')}
                        </Button>
                    </div>
                </div>
            </header>
            {actingIn !== null && (
                <div
                    role="status"
                    className="flex items-center gap-3 border-b border-warn/50 bg-warn/10 px-5 py-2 text-sm text-ink"
                >
                    <span>{t('nav.actingIn', { tenant: actingIn.tenantName, username })}</span>
                    <span className="text-xs text-ink-muted">{t('nav.readOnly')}</span>
                    <Button
                        variant="secondary"
                        size="sm"
                        className="ml-auto"
                        onClick={onLeaveTenant}
                    >
                        {t('nav.leaveTenant')}
                    </Button>
                </div>
            )}
            {/*
             * The screen is bounded and centred, and the scroll area is not.
             * A journey that stands something up draws the same banner the
             * public shell draws, and that banner fills the width it is given:
             * an unbounded column turns it into a wall on a wide display. The
             * bound belongs to the screen, because a form and a table want
             * different ones, and both bounds are the public shell's so a
             * screen looks the same either side of the door.
             */}
            <main className="min-w-0 flex-1 overflow-y-auto px-5 py-8">
                <div className={`mx-auto w-full ${SHELL_WIDTHS[width]}`}>{children}</div>
            </main>
            <VersionFooter serverVersion={serverVersion} />
        </div>
    );
}
