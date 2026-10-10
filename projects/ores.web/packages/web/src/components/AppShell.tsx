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

import { useEffect, useRef, useState, type ReactNode } from 'react';
import { Link, NavLink } from 'react-router';
import type { Account, SessionMode } from '@ores/wire-protocol/browser';
import type { EnvironmentView } from '@ores/contracts';
import { useTranslation } from '../i18n/Provider.js';
import { headerMark } from '../assets/brand.js';
import { AccountPicture, Avatar, imageUrl } from '../ui/Images.js';
import { Button } from '../ui/Primitives.js';
import { useHolds } from '../access/holds.js';
import { displayName } from '../access/names.js';
import { NotificationBell } from '../inbox/NotificationBell.js';
import { MENU, modeKey, offered } from '../shell/areas.js';
import { SHELL_WIDTHS, type ShellWidth } from '../shell/layout.js';
import { VersionFooter } from './VersionFooter.js';

/**
 * The shell a signed-in person gets.
 *
 * The session is passed in rather than read here, so the shell renders from its
 * props and a test can render it without a server or a session.
 *
 * The header states the mode the session runs in and offers the screens that
 * mode's menu holds. The mode comes from the server's answer, so the menu cannot
 * disagree with what the session may do: the menu is the client's structure,
 * and whether a person is in this mode at all is not the client's to decide.
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
     * The signed-in person's own account, when the shell could read it.
     *
     * The menu's trigger shows their name and their picture from it. A member
     * may not hold the read that names the image, so the prop is optional and
     * the trigger falls back to the username and the initials.
     */
    readonly self?: Account | null;
    /** The build the deployment answers with, or nothing before it answers. */
    readonly serverVersion?: string;
    /** The environment the deployment serves, or nothing before it answers. */
    readonly environment?: EnvironmentView | undefined;
    readonly children: ReactNode;
}

export function AppShell({
    username,
    tenantName,
    partyName,
    mode,
    width = 'column',
    onSignOut,
    self,
    serverVersion,
    environment,
    children,
}: AppShellProps): ReactNode {
    const { t } = useTranslation();
    const holds = useHolds();
    const menu = MENU.filter((item) => offered(item, holds, mode));

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
                    {/*
                     * Where the person is working stays in view beside the
                     * brand, as an organisation or a workspace does: the
                     * tenant decides what every screen shows.
                     */}
                    {tenantName !== '' && (
                        <span className="text-sm text-ink-muted">{tenantName}</span>
                    )}
                    {/*
                     * Only an administrator's mode is worth a pill: it changes
                     * what every screen does. An ordinary member's session has
                     * no mode to announce, and the tenant and party are in the
                     * footer.
                     */}
                    {mode !== 'application' && (
                        <span
                            className="rounded-full border border-line px-2 py-0.5 text-[11px] text-ink-muted"
                            title={t('nav.mode')}
                        >
                            {t(modeKey(mode))}
                        </span>
                    )}
                    <nav aria-label={t('nav.areas')} className="flex items-center gap-1">
                        {menu.map((item) => (
                            <NavLink
                                key={item.to}
                                to={item.to}
                                end={item.to === '/'}
                                className={({ isActive }) =>
                                    `rounded-md px-2 py-1 text-xs hover:text-ink ${
                                        isActive ? 'bg-surface-overlay text-ink' : 'text-ink-muted'
                                    }`
                                }
                            >
                                {t(item.nameKey)}
                            </NavLink>
                        ))}
                    </nav>
                    <div className="ml-auto flex items-center gap-2">
                        <NotificationBell />
                        <AccountMenu
                            username={username}
                            tenantName={tenantName}
                            partyName={partyName}
                            onSignOut={onSignOut}
                            self={self}
                        />
                    </div>
                </div>
            </header>
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
            <VersionFooter
                serverVersion={serverVersion}
                environment={environment}
                tenantName={tenantName}
                partyName={partyName}
            />
        </div>
    );
}

/**
 * The signed-in person's picture, and the menu behind it.
 *
 * The header holds the picture alone, as most applications do; who the person
 * is, where they work and their own pages sit in the menu it opens, with
 * signing out last. The menu is always in the page and hidden while closed,
 * so it is one element whether open or not, and Escape or a click elsewhere
 * closes it.
 *
 * The header's trigger shows the person's name and picture once the shell has
 * read their own account, and the username and the initials until then.
 */
function AccountMenu({
    username,
    tenantName,
    partyName,
    onSignOut,
    self,
}: {
    readonly username: string;
    readonly tenantName: string;
    readonly partyName: string | undefined;
    readonly onSignOut: () => void;
    readonly self: Account | null | undefined;
}): ReactNode {
    const { t } = useTranslation();
    const [open, setOpen] = useState(false);
    const root = useRef<HTMLDivElement>(null);
    const name = displayName(self, username);
    const photo = self == null ? undefined : self.imageId === null ? null : imageUrl(self.imageId);

    useEffect(() => {
        if (!open) return undefined;
        const outside = (event: MouseEvent) => {
            if (root.current !== null && !root.current.contains(event.target as Node)) {
                setOpen(false);
            }
        };
        const escape = (event: KeyboardEvent) => {
            if (event.key === 'Escape') setOpen(false);
        };
        document.addEventListener('mousedown', outside);
        document.addEventListener('keydown', escape);
        return () => {
            document.removeEventListener('mousedown', outside);
            document.removeEventListener('keydown', escape);
        };
    }, [open]);

    const close = () => setOpen(false);
    const item = 'block rounded px-2 py-1.5 text-sm text-ink hover:bg-surface-hover';

    return (
        <div ref={root} className="relative">
            <button
                type="button"
                aria-haspopup="menu"
                aria-expanded={open}
                aria-label={t('nav.accountMenu')}
                title={name}
                onClick={() => setOpen(!open)}
                className="flex rounded-full focus-visible:outline focus-visible:outline-2 focus-visible:outline-accent"
            >
                <PersonPicture username={username} name={name} photo={photo} />
            </button>
            <div
                role="menu"
                hidden={!open}
                className="absolute right-0 z-30 mt-2 w-64 rounded-md border border-line bg-surface-overlay p-2 shadow-xl"
            >
                <div className="flex items-center gap-3 border-b border-line px-2 pb-3 pt-1">
                    <PersonPicture username={username} name={name} photo={photo} />
                    <div className="min-w-0">
                        <div className="truncate text-sm font-medium text-ink">{name}</div>
                        {name !== username && (
                            <div className="truncate text-xs text-ink-faint">@{username}</div>
                        )}
                        <div className="truncate text-xs text-ink-muted">{tenantName}</div>
                        {partyName !== undefined && partyName !== '' && (
                            <div className="truncate text-xs text-ink-faint">{partyName}</div>
                        )}
                    </div>
                </div>
                <div className="space-y-0.5 border-b border-line py-1.5">
                    <Link to="/profile" role="menuitem" className={item} onClick={close}>
                        {t('shell.menu.profile')}
                    </Link>
                    <Link to="/access" role="menuitem" className={item} onClick={close}>
                        {t('shell.menu.access')}
                    </Link>
                    <Link to="/where-i-work" role="menuitem" className={item} onClick={close}>
                        {t('shell.menu.whereIWork')}
                    </Link>
                    <Link to="/security" role="menuitem" className={item} onClick={close}>
                        {t('nav.security')}
                    </Link>
                </div>
                <div className="pt-1.5">
                    <Button
                        variant="ghost"
                        size="sm"
                        role="menuitem"
                        className="w-full justify-start"
                        onClick={() => {
                            close();
                            onSignOut();
                        }}
                    >
                        {t('nav.signOut')}
                    </Button>
                </div>
            </div>
        </div>
    );
}

/**
 * The person's picture: their own account's image when the shell read it, and
 * the picture route by username otherwise.
 *
 * The two are not the same read. The picture route asks for the account read
 * too, so a person who may not hold it reads their own initials either way.
 */
function PersonPicture({
    username,
    name,
    photo,
}: {
    readonly username: string;
    readonly name: string;
    /** The image of the account the shell read, null for no image, undefined for no read. */
    readonly photo: string | null | undefined;
}): ReactNode {
    return photo === undefined ? (
        <AccountPicture username={username} name={name} />
    ) : (
        <Avatar name={name} src={photo} />
    );
}
