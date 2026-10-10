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

import { useEffect, type RefObject } from 'react';

/**
 * Closes whatever is open when the person clicks elsewhere or presses Escape.
 *
 * The account menu and the workflows menu are both a button with a panel that
 * is always in the page and hidden while closed, so both close the same way.
 */
export function useDismiss(
    root: RefObject<HTMLElement | null>,
    open: boolean,
    close: () => void,
): void {
    useEffect(() => {
        if (!open) return undefined;
        const outside = (event: MouseEvent) => {
            if (root.current !== null && !root.current.contains(event.target as Node)) {
                close();
            }
        };
        const escape = (event: KeyboardEvent) => {
            if (event.key === 'Escape') close();
        };
        document.addEventListener('mousedown', outside);
        document.addEventListener('keydown', escape);
        return () => {
            document.removeEventListener('mousedown', outside);
            document.removeEventListener('keydown', escape);
        };
    }, [open, root, close]);
}
