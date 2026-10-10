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
import { useTranslation } from '../i18n/Provider.js';

/**
 * The doors from a person's own record to the screens that own the rest of it:
 * their password, sign-ins and sessions, and what their roles let them do.
 *
 * The profile's Access tab and a person's own page both draw it, so a person
 * finds the same two doors wherever they look at themselves.
 */
export function AccountDoors(): ReactNode {
    const { t } = useTranslation();
    return (
        <div className="space-y-2">
            <p className="text-sm text-ink-muted">
                {t('profile.access.protectWhy')}{' '}
                <Link to="/security" className="text-accent-bright hover:underline">
                    {t('profile.access.protect')}
                </Link>
                .
            </p>
            <p className="text-sm text-ink-muted">
                {t('profile.access.knowWhy')}{' '}
                <Link to="/access" className="text-accent-bright hover:underline">
                    {t('profile.access.know')}
                </Link>
                .
            </p>
        </div>
    );
}
