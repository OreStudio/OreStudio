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

import { useCallback, useId, useRef, useState } from 'react';
import { cx } from './Primitives.js';
import { useDismiss } from './useDismiss.js';
import type { ReactNode } from 'react';

/**
 * What holds a tooltip open.
 *
 * The pointer resting on the label, the label holding focus and a tap are kept
 * apart, because they end in different ways: the pointer leaving ends a hover
 * and must not end a hint the keyboard opened, and a tap has neither a pointer
 * to leave nor a focus to lose.
 */
export interface Reveal {
    readonly hovered: boolean;
    readonly focused: boolean;
    readonly tapped: boolean;
}

export const concealed: Reveal = { hovered: false, focused: false, tapped: false };

export type RevealAction = 'hover' | 'unhover' | 'focus' | 'blur' | 'tap' | 'dismiss';

/** The state a tooltip is in after one of the things that happen to its label. */
export function revealAfter(reveal: Reveal, action: RevealAction): Reveal {
    switch (action) {
        case 'hover':
            return { ...reveal, hovered: true };
        case 'unhover':
            return { ...reveal, hovered: false };
        case 'focus':
            return { ...reveal, focused: true };
        case 'blur':
            return { ...reveal, focused: false };
        case 'tap':
            return { ...reveal, tapped: true };
        case 'dismiss':
            return concealed;
    }
}

/** Whether any of the causes is holding the tooltip open. */
export function isRevealed(reveal: Reveal): boolean {
    return reveal.hovered || reveal.focused || reveal.tapped;
}

const TRIGGER =
    'cursor-help rounded-sm underline decoration-dotted underline-offset-2 ' +
    'focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent';

const REVEALED =
    'absolute left-0 top-full z-30 mt-1 block w-max max-w-[16rem] rounded-md border border-line ' +
    'bg-surface-overlay px-2 py-1 text-xs font-normal text-ink shadow-lg';

/**
 * A hint on a label, shown on hover, on keyboard focus and on a tap.
 *
 * A native title needs a pointer, so it is out of reach on a touch screen and
 * from the keyboard. This one makes the label focusable, opens on focus and on
 * a tap, and points `aria-describedby` at the words, so a screen reader reads
 * the hint along with the label. The words stay in the page while closed, out
 * of sight rather than out of the accessibility tree.
 */
export function Tooltip({
    text,
    children,
    triggerClassName,
}: {
    readonly text: string;
    readonly children: ReactNode;
    /**
     * Extra classes on the trigger, for a trigger that is already a marker: a
     * badge brings its own shape, so the dotted underline would only clutter it.
     * The hover, focus and tap behaviour stays.
     */
    readonly triggerClassName?: string;
}): ReactNode {
    const id = useId();
    const root = useRef<HTMLSpanElement>(null);
    const [reveal, setReveal] = useState<Reveal>(concealed);
    const dismiss = useCallback(() => setReveal(concealed), []);
    useDismiss(root, isRevealed(reveal), dismiss);
    const act = (action: RevealAction): void =>
        setReveal((current) => revealAfter(current, action));
    return (
        <span ref={root} className="relative inline-block">
            <span
                tabIndex={0}
                aria-describedby={id}
                className={cx(TRIGGER, triggerClassName)}
                onMouseEnter={() => act('hover')}
                onMouseLeave={() => act('unhover')}
                onFocus={() => act('focus')}
                onBlur={() => act('blur')}
                onPointerDown={() => act('tap')}
            >
                {children}
            </span>
            <span id={id} role="tooltip" className={isRevealed(reveal) ? REVEALED : 'sr-only'}>
                {text}
            </span>
        </span>
    );
}
