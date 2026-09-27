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
import { Notice } from '../ui/Primitives.js';
import { heroSplash } from '../assets/brand.js';

/**
 * The screen a browser shows while the deployment has no administrator.
 *
 * This is the only screen in bootstrap mode, which is why the gate sends every
 * route here rather than redirecting to a setup path: there is nothing else to
 * be at, and a redirect would leave a URL somebody could share that leads
 * nowhere.
 *
 * It is also the first thing an empty installation ever shows, so it carries the
 * banner and reads like an installation screen: the condition, the consequence,
 * and what the setup process begins with. It states the mode and does not
 * pretend to be the journey that creates the administrator.
 */
export function SetupPage({ message }: { readonly message: string }): ReactNode {
    const { t } = useTranslation();

    return (
        <div className="card overflow-hidden">
            {/* The banner the landing page uses, so the first screen of an
                installation looks like the product rather than like a notice. */}
            <img src={heroSplash} alt="" className="w-full border-b border-line" />
            <div className="p-6">
                <h1 className="text-lg font-semibold text-ink">{t('setup.title')}</h1>
                <p className="mt-3 text-sm text-ink-muted">{t('setup.bootstrapMode')}</p>
                {message !== '' && (
                    <div className="mt-4">
                        <Notice tone="info">{message}</Notice>
                    </div>
                )}
                <p className="mt-4 text-sm text-ink-muted">{t('setup.next')}</p>
            </div>
        </div>
    );
}
