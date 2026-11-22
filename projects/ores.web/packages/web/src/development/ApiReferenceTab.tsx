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

/** The path a server answers its own API reader on. */
const DOCUMENT = '/swagger';

/**
 * The C++ server's reader, wherever this page itself is served from.
 *
 * The reader is a page of the server that composes the API, so it is named by that
 * server's origin rather than by a path relative to this page: the BFF serves the
 * bundle today and is being retired, and a reader asked of it would be asked of a
 * server that does not have one.
 */
function documentUrl(at: page | undefined): string {
    if (at === undefined) return DOCUMENT;
    return `${originOf('cpp', __ORE_SERVER_PORTS__, at)}${DOCUMENT}`;
}

/**
 * The API the screens call, read from the server that answers it.
 *
 * The reader is generated from the routes the server registers, so it is drawn in a
 * frame rather than reimplemented here: the document a person reads is the document a
 * client generates from, and neither can be ahead of the other. A person who wants it
 * whole opens it in a tab of its own, where it has the width.
 */
export function ApiReferenceTab(): ReactNode {
    const { t } = useTranslation();
    const at = typeof window === 'undefined' ? undefined : window.location;
    const url = documentUrl(at);
    return (
        <div className="space-y-3">
            <div className="flex flex-wrap items-end justify-between gap-3">
                <p className="max-w-2xl text-sm text-ink-muted">{t('development.api.lead')}</p>
                <a
                    href={url}
                    target="_blank"
                    rel="noreferrer"
                    className="rounded border border-line px-3 py-2 text-sm text-ink-muted hover:bg-panel-2 hover:text-ink"
                >
                    {t('development.api.open')}
                </a>
            </div>
            <iframe
                src={url}
                title={t('development.api.frameTitle')}
                className="h-[70vh] w-full rounded-md border border-line bg-surface-base"
            />
        </div>
    );
}
