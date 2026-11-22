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
import { originOf, type page } from '../api/servers.js';
import { Crumbs } from '../refdata/shared.js';
import { useTheme } from '../theme/useTheme.js';
import { PageHeader } from '../ui/Primitives.js';

/** The path the server answers its own API reader on. */
export const API_REFERENCE = '/api-reference';

/** Where the reader itself lives, which is the server's page and not a screen here. */
export const API_DOCUMENT = '/swagger';

/**
 * The reader's address, of the C++ server and in the palette this screen is in.
 *
 * The C++ server composes the API, so its reader is the one that matters, and it is
 * named by that server's origin rather than by a path relative to this page: the BFF
 * serves the bundle today and is being retired, and asking it for a reader would ask a
 * server that has none. The palette travels in the query because the reader is a page of
 * its own, with a stylesheet of its own, and it cannot read this one's attribute.
 */
export function documentUrl(at: page | undefined, theme: string): string {
    const base =
        at === undefined
            ? API_DOCUMENT
            : `${originOf('cpp', __ORE_SERVER_PORTS__, at)}${API_DOCUMENT}`;
    return `${base}?theme=${encodeURIComponent(theme)}`;
}

/**
 * The API reference, shown inside the application.
 *
 * The reader is the server's own page, generated from the routes it serves, so it is
 * drawn in a frame rather than reimplemented: the document a person reads here is the
 * document a client generates from, and neither can be ahead of the other. A person who
 * wants the reader whole opens it in its own tab, where it has the width.
 */
export function ApiReferencePage(): ReactNode {
    const { t } = useTranslation();
    const { theme } = useTheme();
    const at = typeof window === 'undefined' ? undefined : window.location;
    const url = documentUrl(at, theme);
    return (
        <div className="space-y-4">
            <Crumbs
                parts={[{ label: t('shell.menu.home'), to: '/' }, { label: t('shell.menu.api') }]}
            />
            <div className="flex flex-wrap items-end justify-between gap-3">
                <PageHeader title={t('shell.menu.api')} description={t('shell.api.lead')} />
                <a
                    href={url}
                    target="_blank"
                    rel="noreferrer"
                    className="rounded border border-line px-3 py-2 text-sm text-ink-muted hover:bg-panel-2 hover:text-ink"
                >
                    {t('shell.api.open')}
                </a>
            </div>
            <iframe
                // A new address for a new palette, so the reader is mounted again in it.
                key={theme}
                src={url}
                title={t('shell.api.frameTitle')}
                className="h-[75vh] w-full rounded-md border border-line bg-surface-base"
            />
        </div>
    );
}
