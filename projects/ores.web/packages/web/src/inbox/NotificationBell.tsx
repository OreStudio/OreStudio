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

import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useEffect, useRef, useState, type ReactNode } from 'react';
import { useNavigate } from 'react-router';
import type { InboxNotificationView } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { api } from '../api/client.js';
import { Icon } from '../ui/Icon.js';
import { RelativeTime } from '../ui/Time.js';

/** How many notifications the bell lists at once. */
const LISTED = 20;

/** How often the unread count is read again while a screen is open. */
const POLL_MS = 30_000;

/**
 * The routes that name one thing, and so take the notification's id.
 *
 * A notification about a screen as a whole carries an empty id, and one about
 * a row carries the row's: the difference is the route, not the notification,
 * because only the screen knows whether it is showing a list or a record.
 */
const ID_ROUTES = new Set(['/requests', '/tenants', '/people']);

/** Where a notification points, with the id when the route takes one. */
export function notificationRoute(notification: InboxNotificationView): string {
    const route = notification.linkRoute.startsWith('/')
        ? notification.linkRoute
        : `/${notification.linkRoute}`;
    if (notification.linkId === '' || !ID_ROUTES.has(route)) {
        return route;
    }
    return `${route}/${encodeURIComponent(notification.linkId)}`;
}

/**
 * The bell: what has happened, and how much of it is unread.
 *
 * The count sits on every screen, so it is read on a timer rather than pushed:
 * the client's own default already reads it again when the window takes focus,
 * and a person who leaves a screen open is told within half a minute.
 *
 * Clicking a notification marks it read and goes where it points. The
 * notification never asks the person to act inside it; the screen it links to
 * is where the work is.
 */
export function NotificationBell(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const queries = useQueryClient();
    const [open, setOpen] = useState(false);
    const root = useRef<HTMLDivElement>(null);

    const unread = useQuery({
        queryKey: ['unread-notifications'],
        queryFn: api.unreadNotificationCount,
        refetchInterval: POLL_MS,
    });
    const list = useQuery({
        queryKey: ['notifications'],
        queryFn: () => api.myNotifications({ unreadOnly: false, offset: 0, limit: LISTED }),
    });

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

    const settled = async () => {
        await queries.invalidateQueries({ queryKey: ['notifications'] });
        await queries.invalidateQueries({ queryKey: ['unread-notifications'] });
    };
    const read = useMutation({
        mutationFn: (ids: readonly string[]) => api.markNotificationsRead(ids),
        onSuccess: settled,
    });

    const items = list.data?.items ?? [];
    const count = unread.data ?? 0;

    const openOne = (notification: InboxNotificationView) => {
        if (notification.readAt === '') {
            read.mutate([notification.id]);
        }
        setOpen(false);
        void navigate(notificationRoute(notification));
    };

    return (
        <div ref={root} className="relative">
            <button
                type="button"
                aria-haspopup="menu"
                aria-expanded={open}
                aria-label={t('inbox.bell.label', { count: String(count) })}
                onClick={() => setOpen(!open)}
                className="relative flex size-9 items-center justify-center rounded-full text-ink-muted hover:bg-surface-hover hover:text-ink focus-visible:outline focus-visible:outline-2 focus-visible:outline-accent"
            >
                <Icon name="alert" />
                {count > 0 && (
                    <span className="absolute -right-0.5 -top-0.5 min-w-4 rounded-full bg-accent px-1 text-center text-[10px] font-semibold leading-4 text-ink-inverse">
                        {count}
                    </span>
                )}
            </button>
            <div
                role="menu"
                hidden={!open}
                className="absolute right-0 z-30 mt-2 w-80 rounded-md border border-line bg-surface-overlay p-2 shadow-xl"
            >
                <div className="flex items-center justify-between border-b border-line px-2 pb-2 pt-1">
                    <span className="text-sm font-medium">{t('inbox.bell.title')}</span>
                    <button
                        type="button"
                        role="menuitem"
                        disabled={count === 0 || read.isPending}
                        onClick={() => read.mutate([])}
                        className="rounded px-2 py-1 text-xs text-ink-muted hover:bg-surface-hover hover:text-ink disabled:opacity-45 disabled:hover:bg-transparent"
                    >
                        {t('inbox.bell.markAll')}
                    </button>
                </div>
                {items.length === 0 ? (
                    <p className="px-2 py-3 text-sm text-ink-muted">{t('inbox.bell.empty')}</p>
                ) : (
                    <ul className="max-h-80 overflow-y-auto">
                        {items.map((notification) => (
                            <li key={notification.id}>
                                <button
                                    type="button"
                                    role="menuitem"
                                    onClick={() => openOne(notification)}
                                    className="block w-full rounded px-2 py-2 text-left text-sm hover:bg-surface-hover"
                                >
                                    <span className="flex items-start gap-2">
                                        <span
                                            aria-hidden
                                            className={`mt-1.5 size-2 shrink-0 rounded-full ${notification.readAt === '' ? 'bg-accent' : 'bg-transparent'}`}
                                        />
                                        <span className="min-w-0 flex-1">
                                            <span className="block text-ink">
                                                {t(notification.messageKey, valuesOf(notification))}
                                            </span>
                                            <span className="block text-xs text-ink-faint">
                                                <RelativeTime at={notification.raisedAt} />
                                            </span>
                                        </span>
                                    </span>
                                </button>
                            </li>
                        ))}
                    </ul>
                )}
            </div>
        </div>
    );
}

/** The values a notification's message names, as the screen renders them. */
function valuesOf(notification: InboxNotificationView): Record<string, string> {
    return Object.fromEntries(
        notification.arguments.map((argument) => [argument.name, argument.value]),
    );
}
