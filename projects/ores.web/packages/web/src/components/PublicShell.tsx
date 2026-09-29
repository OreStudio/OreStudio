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
import { PROJECT_SITE, headerMark } from '../assets/brand.js';
import { VersionFooter } from './VersionFooter.js';

/**
 * The shell a visitor gets.
 *
 * Two shells rather than one, because the two situations have nothing in
 * common: a visitor has no session, no tenant and nothing to navigate, so the
 * navigation a signed-in person needs would be three empty lists. This one is
 * the mark, the name, and the screen.
 */
export function PublicShell({
    children,
    wide = false,
    serverVersion,
}: {
    readonly children: ReactNode;
    /**
     * Whether the screen is a journey rather than a form.
     *
     * A form reads badly when it is stretched across a desktop, so these
     * screens the width they were written for. A journey is a rail and a step
     * beside it, and a rail inside a form's column leaves the step about four
     * hundred pixels and makes every line of it wrap.
     */
    readonly wide?: boolean;
    /** The build the deployment answers with, or nothing before it answers. */
    readonly serverVersion?: string;
}): ReactNode {
    const { t } = useTranslation();
    const width = wide ? 'max-w-[1100px]' : 'max-w-[680px]';

    return (
        <div className="flex min-h-full flex-col bg-bg-primary">
            <header className="border-b border-line">
                <div className={`mx-auto flex w-full items-center gap-3 px-5 py-4 ${width}`}>
                    <img src={headerMark} alt="" className="h-7 w-auto" />
                    <a
                        href={PROJECT_SITE}
                        className="text-sm font-semibold tracking-tight text-ink hover:text-accent-bright"
                    >
                        {t('app.name')}
                    </a>
                </div>
            </header>
            <main className={`mx-auto w-full flex-1 px-5 py-12 ${width}`}>{children}</main>
            <VersionFooter serverVersion={serverVersion} />
        </div>
    );
}
