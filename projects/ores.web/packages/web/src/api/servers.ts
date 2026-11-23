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
 * Which server answers a screen while the port is in progress.
 *
 * The choice is made at two levels, and they are not the same switch.
 *
 * A whole deployment is one origin that serves the bundle and answers =/api= behind it:
 * the C++ server and the BFF are two such deployments, and the sign-in screen offers the
 * other one. Moving the app there is a navigation, and every request then asks the server
 * that served the page, which is why a build without =VITE_ORE_CPP_SCREENS= runs entirely
 * on whichever server the browser is on.
 *
 * Within one deployment, a single screen can still be sent across: an empty prefix asks
 * the server that served the page, and =/cpp= asks the C++ server through the dev server's
 * proxy. That is the finer switch, and it is the one =VITE_ORE_CPP_SCREENS= sets.
 *
 * The screen is named by the module that calls the API, not by its page title, so a
 * module shared by two screens is either moved for both or for neither.
 */

/** The prefixes a screen's requests can go through, in this deployment. */
export type serverPrefix = '' | '/cpp';

/** Where one screen's requests go. */
export function baseForScreen(screen: string, moved: readonly string[]): serverPrefix {
    return moved.includes(screen) ? '/cpp' : '';
}

/**
 * The screens this build has moved, read from =VITE_ORE_CPP_SCREENS=.
 *
 * A comma-separated list, so the flag is one variable whatever the number of screens
 * on the new server. Empty means none, which is the deployment the browser has today.
 */
export const screensOnCpp: readonly string[] = (
    (import.meta.env['VITE_ORE_CPP_SCREENS'] as string | undefined) ?? ''
)
    .split(',')
    .map((name) => name.trim())
    .filter((name) => name.length > 0);

/** The prefix for a screen, from the flag this build was made with. */
export function baseFor(screen: string): serverPrefix {
    return baseForScreen(screen, screensOnCpp);
}

/**
 * The two servers of the port, each a whole deployment the browser can be on.
 *
 * While =ores.http= takes over from the BFF, a person compares the two by moving the
 * app from one to the other: the screen in front of them is the same screen either
 * way, served from whichever origin they are on. So the choice names a deployment and
 * not a screen, and it moves the page rather than a prefix.
 */
export type deployment = 'cpp' | 'bff';

/** The deployments the browser may be on, in the order the sign-in screen names them. */
export const deployments: readonly deployment[] = ['cpp', 'bff'];

/** The ports the two deployments answer on, stamped in by =vite.config.ts=. */
export interface serverPorts {
    readonly cpp: string;
    readonly bff: string;
}

/** A page as the browser states it: enough to place it, and to move it. */
export interface page {
    readonly protocol: string;
    readonly hostname: string;
    readonly port: string;
    readonly pathname: string;
    readonly search: string;
    readonly hash: string;
}

/** The other of two deployments. */
export function otherOf(one: deployment): deployment {
    return one === 'cpp' ? 'bff' : 'cpp';
}

/** The origin @p one answers on, on the host the browser is on. */
export function originOf(one: deployment, ports: serverPorts, at: page): string {
    return `${at.protocol}//${at.hostname}:${ports[one]}`;
}

/**
 * The deployment the page is on, or nothing for a page on neither: a dev server answers
 * on its own port, and a page it serves has no peer to offer.
 */
export function servingDeployment(ports: serverPorts, at: page): deployment | undefined {
    return deployments.find((one) => ports[one] === at.port);
}

/**
 * The other deployment's copy of this page, or nothing when this page is on neither.
 *
 * A deployment the build names no port for is not served here, so there is nowhere to
 * go: a port converged onto one server states the other as empty, and the switch states
 * nothing rather than a dead link.
 */
export function peerOf(
    ports: serverPorts,
    at: page,
): { readonly deployment: deployment; readonly url: string } | undefined {
    const here = servingDeployment(ports, at);
    if (here === undefined) return undefined;
    const there = otherOf(here);
    if (ports[there].length === 0) return undefined;
    return {
        deployment: there,
        url: `${originOf(there, ports, at)}${at.pathname}${at.search}${at.hash}`,
    };
}
