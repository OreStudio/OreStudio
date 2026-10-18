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
import type { InboxRequestPartView } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { PersonRef } from '../access/PersonRef.js';
import { Tag } from '../ui/Primitives.js';
import { formatDateTime } from '../ui/Time.js';
import { partLabel, partStateLabel, partStateTone } from './words.js';

/**
 * Who must answer a request, in the order they answer, and where each stands.
 *
 * A request that needs several functions to approve it waits on each. A part
 * whose turn has not come is shown as later, so the person who asked can see
 * why nothing has moved. A request that names no parts draws nothing.
 */
export function PartsPanel({
    parts,
}: {
    readonly parts: readonly InboxRequestPartView[];
}): ReactNode {
    const { t, language } = useTranslation();
    if (parts.length === 0) return null;
    return (
        <section className="space-y-2">
            <h2 className="text-sm font-semibold">{t('inbox.parts.title')}</h2>
            <ol className="divide-y divide-line-subtle rounded-md border border-line bg-surface-raised">
                {parts.map((part) => (
                    <li key={part.code} className="flex flex-wrap items-center gap-3 px-4 py-2">
                        <span className="min-w-28 text-sm font-medium">
                            {partLabel(t, part.code, part.name)}
                        </span>
                        <Tag tone={partStateTone(part.stateCode)}>
                            {partStateLabel(t, part.stateCode)}
                        </Tag>
                        {part.decidedBy !== '' && (
                            <span className="text-xs text-ink-muted">
                                <PersonRef who={part.decidedBy} />
                                {part.decidedAt !== null && (
                                    <span className="ml-2 text-ink-faint">
                                        {formatDateTime(part.decidedAt, language)}
                                    </span>
                                )}
                            </span>
                        )}
                        {part.comment !== '' && (
                            <span className="text-sm text-ink-muted">{part.comment}</span>
                        )}
                    </li>
                ))}
            </ol>
        </section>
    );
}
