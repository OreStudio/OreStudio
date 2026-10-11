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
import type { LoginInfo } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { Detail } from '../ui/Primitives.js';
import { isZeroTimestamp, RelativeTime } from '../ui/Time.js';

/**
 * What the server says about an account's sign-ins: when it last signed in,
 * from where, and how many attempts failed.
 *
 * The member's Security screen and the administrator's Rescue access screen show
 * these three facts the same way. A time that was never written is "Never
 * signed in", not a date in 1970, and an account with no record has no facts.
 */
export function SignInFacts({
    state,
    children,
}: {
    readonly state: LoginInfo | null;
    /** Further facts for the screen that has them, drawn after these three. */
    readonly children?: ReactNode;
}): ReactNode {
    const { t } = useTranslation();
    if (state === null) {
        return <p className="text-sm text-ink-muted">{t('signInFacts.none')}</p>;
    }
    return (
        <dl className="grid gap-x-6 gap-y-3 sm:grid-cols-2 lg:grid-cols-3">
            <Detail
                label={t('signInFacts.lastSignIn')}
                value={
                    isZeroTimestamp(state.lastLogin) ? (
                        t('signInFacts.never')
                    ) : (
                        <RelativeTime at={state.lastLogin} />
                    )
                }
            />
            <Detail
                label={t('signInFacts.from')}
                value={state.lastAttemptIp === '' ? t('signInFacts.unknown') : state.lastAttemptIp}
                mono={state.lastAttemptIp !== ''}
            />
            <Detail label={t('signInFacts.failed')} value={String(state.failedLogins)} mono />
            {children}
        </dl>
    );
}
