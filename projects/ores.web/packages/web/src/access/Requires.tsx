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
import { usePermissions } from './holds.js';
import { NotAvailable } from './NotAvailable.js';
import type { Needs } from './permissions.js';

/**
 * Draws a screen or a form only for somebody who can use it.
 *
 * A screen declares what it needs, and the person either holds it or does not.
 * Nothing is drawn while the roles are being read, so a screen never asks for
 * data on a guess. A person who reaches a screen they may not use, by a saved
 * link, is told it is not available rather than shown a screen that refuses.
 */
export function Requires({
    needs,
    screen,
    children,
}: {
    readonly needs: Needs;
    /** The screen's name, for the sentence that says it cannot be opened. */
    readonly screen: string;
    readonly children: ReactNode;
}): ReactNode {
    const { t } = useTranslation();
    const { ready, can } = usePermissions();
    if (!ready) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (!can(needs)) {
        return <NotAvailable screen={screen} needs={needs} />;
    }
    return children;
}

/** One way a place can be drawn, and what it needs. */
export interface Variant {
    readonly needs: Needs;
    readonly screen: ReactNode;
}

/**
 * Draws the first variant the person can use.
 *
 * Two audiences for one place, such as the whole directory and the people of
 * your own parties, are two screens that each declare what they need. Neither
 * screen checks permissions to decide what it is.
 */
export function FirstAvailable({
    variants,
    screen,
}: {
    readonly variants: readonly Variant[];
    readonly screen: string;
}): ReactNode {
    const { t } = useTranslation();
    const { ready, can } = usePermissions();
    if (!ready) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    const chosen = variants.find((variant) => can(variant.needs));
    return chosen === undefined ? (
        <NotAvailable
            screen={screen}
            needs={{
                any: variants.flatMap((variant) => [
                    ...(variant.needs.all ?? []),
                    ...(variant.needs.any ?? []),
                ]),
            }}
        />
    ) : (
        chosen.screen
    );
}
