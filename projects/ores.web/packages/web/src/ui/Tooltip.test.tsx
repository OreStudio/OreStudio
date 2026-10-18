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

import { describe, expect, it } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import { Tooltip, concealed, isRevealed, revealAfter } from './Tooltip.js';
import type { Reveal, RevealAction } from './Tooltip.js';

const after = (...actions: readonly RevealAction[]): Reveal =>
    actions.reduce(revealAfter, concealed);

describe('the state of a tooltip', () => {
    it('opens on a hover and closes when the pointer leaves', () => {
        expect(isRevealed(after('hover'))).toBe(true);
        expect(isRevealed(after('hover', 'unhover'))).toBe(false);
    });

    it('opens on focus and closes on blur', () => {
        expect(isRevealed(after('focus'))).toBe(true);
        expect(isRevealed(after('focus', 'blur'))).toBe(false);
    });

    it('opens on a tap and stays open when the focus goes elsewhere', () => {
        expect(isRevealed(after('tap', 'blur'))).toBe(true);
    });

    it('stays open when the pointer leaves a label that still has focus', () => {
        expect(isRevealed(after('focus', 'hover', 'unhover'))).toBe(true);
        expect(isRevealed(after('focus', 'hover', 'unhover', 'blur'))).toBe(false);
    });

    it('closes every cause when it is dismissed', () => {
        expect(isRevealed(after('hover', 'focus', 'tap', 'dismiss'))).toBe(false);
    });
});

describe('the markup of a tooltip', () => {
    const html = renderToStaticMarkup(<Tooltip text="Where it writes">Target</Tooltip>);

    it('lets the label take focus and names the hint as its description', () => {
        expect(html).toContain('tabindex="0"');
        const described = /aria-describedby="([^"]+)"/.exec(html)?.[1];
        expect(described).toBeDefined();
        expect(html).toMatch(new RegExp(`id="${described}" role="tooltip"`));
    });

    it('keeps the words in the page while it is closed, out of sight', () => {
        expect(html).toContain('Where it writes');
        expect(html).toContain('sr-only');
    });

    it('marks the label so a person can see that a hint exists', () => {
        expect(html).toContain('cursor-help');
        expect(html).toContain('decoration-dotted');
    });
});
