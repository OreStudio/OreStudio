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

import { useCallback, useEffect, useRef, useState } from 'react';

/** How long the word Copied stays after a copy. */
export const COPIED_MS = 2000;

/**
 * Writes a text to the clipboard and answers whether the browser took it.
 *
 * A clipboard write is refused outright when the page is not the focused one
 * and in a few other cases, and the refusal is a rejected promise rather than a
 * false return, so the answer is a boolean a screen can act on.
 */
export async function copyText(text: string): Promise<boolean> {
    try {
        await navigator.clipboard.writeText(text);
        return true;
    } catch {
        return false;
    }
}

/**
 * Copies a text to the clipboard and says how it went, for a screen that offers
 * a Copy button.
 *
 * `copied` holds while the button reads Copied and clears by itself, so the
 * screen states a copy that worked without a second control. `failed` holds
 * until the next attempt, so a copy the browser refused is not silent. The
 * timer is cleared when the screen goes, so a copy followed by a close leaves
 * nothing running.
 */
export function useCopyToClipboard(text: string): {
    readonly copied: boolean;
    readonly failed: boolean;
    readonly copy: () => void;
} {
    const [copied, setCopied] = useState(false);
    const [failed, setFailed] = useState(false);
    const timer = useRef<ReturnType<typeof setTimeout> | undefined>(undefined);
    useEffect(() => () => clearTimeout(timer.current), []);
    const copy = useCallback((): void => {
        void copyText(text).then((taken) => {
            setFailed(!taken);
            setCopied(taken);
            if (!taken) {
                return;
            }
            clearTimeout(timer.current);
            timer.current = setTimeout(() => setCopied(false), COPIED_MS);
        });
    }, [text]);
    return { copied, failed, copy };
}
