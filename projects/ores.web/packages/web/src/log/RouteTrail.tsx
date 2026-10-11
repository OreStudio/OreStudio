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

import { useEffect, type ReactNode } from 'react';
import { useLocation } from 'react-router';
import { trail } from './clientLog.js';

const routes = trail('route');

/**
 * Writes each screen a person enters and leaves to the trail.
 *
 * Placed once inside the signed-in shell, so no screen logs its own arrival.
 * The path is logged and the query is not, because a query holds what a person
 * typed into a search.
 */
export function RouteTrail(): ReactNode {
    const { pathname } = useLocation();
    useEffect(() => {
        routes.info('screen entered', { route: pathname });
        return () => routes.info('screen left', { route: pathname });
    }, [pathname]);
    return null;
}
