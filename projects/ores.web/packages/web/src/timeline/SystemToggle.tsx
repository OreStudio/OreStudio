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

import { useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';

/** Whether an entry by this actor is drawn, given whether service entries are hidden. */
export function showsActor(
    hiding: boolean,
    isService: ((actor: string) => boolean | undefined) | undefined,
    actor: string,
): boolean {
    return !hiding || isService?.(actor) !== true;
}

/**
 * The state of the toggle that hides what service accounts did.
 *
 * `isService` answers for an actor, or undefined when the reader cannot tell a
 * service from a person. A reader who cannot tell is not offered the toggle,
 * because hiding by a guess would hide people.
 */
export function useSystemFilter(isService: ((actor: string) => boolean | undefined) | undefined): {
    readonly hiding: boolean;
    readonly setHiding: (hiding: boolean) => void;
    /** Whether an entry by this actor is drawn. */
    readonly shows: (actor: string) => boolean;
    /** Whether the toggle is worth offering for these actors. */
    readonly offeredFor: (actors: readonly string[]) => boolean;
} {
    const [hiding, setHiding] = useState(false);
    return {
        hiding,
        setHiding,
        shows: (actor) => showsActor(hiding, isService, actor),
        offeredFor: (actors) =>
            isService !== undefined && actors.some((a) => isService(a) === true),
    };
}

/** The button that hides or shows what service accounts did. */
export function SystemToggle({
    hiding,
    onChange,
}: {
    readonly hiding: boolean;
    readonly onChange: (hiding: boolean) => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <button
            type="button"
            aria-pressed={hiding}
            className={
                hiding
                    ? 'rounded-md border border-line bg-surface-overlay px-3 py-1 text-sm text-ink'
                    : 'rounded-md border border-line px-3 py-1 text-sm text-ink-muted hover:text-ink'
            }
            onClick={() => onChange(!hiding)}
        >
            {hiding ? t('timeline.showSystem') : t('timeline.hideSystem')}
        </button>
    );
}
