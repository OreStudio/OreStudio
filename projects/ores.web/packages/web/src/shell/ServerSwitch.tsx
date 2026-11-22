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
import { useTranslation } from '../i18n/Provider.js';
import { peerOf, servingDeployment, type deployment, type page } from '../api/servers.js';

/**
 * The switch between the two servers of the port.
 *
 * The app is one bundle served on both, so the switch is a pair of buttons: the server
 * serving this page is pressed, and the other is a link to the same screen on its
 * origin, which moves the page rather than a prefix. The same pair stands on the
 * sign-in card, where a person chooses, and in the header of a signed-in screen, so a
 * screen that is not served here is one click from the server that serves it.
 *
 * A page on neither port has no peer, and one whose peer the build names no port for
 * has nowhere to go; both state nothing.
 */

/** The name a person reads for a deployment. */
export function serverKey(one: deployment): string {
    return one === 'cpp' ? 'signIn.serverCpp' : 'signIn.serverBff';
}

export function ServerSwitch({
    at,
    compact = false,
}: {
    readonly at?: page;
    /** The header's form: the pair alone, without the word that names it. */
    readonly compact?: boolean;
}): ReactNode {
    const { t } = useTranslation();
    /*
     * A screen rendered where no browser exists -- a test that renders the shell to
     * text -- has no page to place, so the switch states nothing there.
     */
    const where = at ?? (typeof window === 'undefined' ? undefined : window.location);
    if (where === undefined) return null;
    const here = servingDeployment(__ORE_SERVER_PORTS__, where);
    const peer = peerOf(__ORE_SERVER_PORTS__, where);
    if (here === undefined || peer === undefined) return null;
    const pair = (
        <div
            role="group"
            aria-label={t('signIn.server')}
            className="inline-flex overflow-hidden rounded border border-line"
        >
            <span
                aria-current="true"
                data-server={here}
                className="bg-accent/15 px-3 py-1 text-sm font-medium text-ink"
            >
                {t(serverKey(here))}
            </span>
            <a
                href={peer.url}
                data-server={peer.deployment}
                className="border-l border-line px-3 py-1 text-sm text-ink-muted hover:bg-panel-2 hover:text-ink"
            >
                {t(serverKey(peer.deployment))}
            </a>
        </div>
    );
    if (compact) return pair;
    return (
        <div className="mt-4 flex flex-wrap items-center gap-2 border-t border-line pt-4">
            <span className="text-xs text-ink-muted">{t('signIn.server')}</span>
            {pair}
        </div>
    );
}
