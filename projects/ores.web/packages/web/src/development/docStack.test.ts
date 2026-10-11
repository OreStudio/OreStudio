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
import { closeDoc, openDoc, shownDoc } from './docStack.js';

describe('the docs followed inside a tab', () => {
    it('opens a linked doc on top of the one on show', () => {
        expect(openDoc(['story'], 'task')).toEqual(['story', 'task']);
        expect(openDoc(['story', 'task'], 'recipe')).toEqual(['story', 'task', 'recipe']);
    });

    it('does not stack the doc that is already on show', () => {
        expect(openDoc(['story', 'task'], 'task')).toEqual(['story', 'task']);
    });

    it('goes back one doc at a time and keeps the first', () => {
        expect(closeDoc(['story', 'task', 'recipe'])).toEqual(['story', 'task']);
        expect(closeDoc(['story', 'task'])).toEqual(['story']);
        expect(closeDoc(['story'])).toEqual(['story']);
        expect(closeDoc([])).toEqual([]);
    });

    it('shows the top of the stack, or the first doc when there is none', () => {
        expect(shownDoc(['story', 'task'], 'story')).toBe('task');
        expect(shownDoc([], 'story')).toBe('story');
    });

    it('can come back to a doc it has left, by following its link again', () => {
        const away = openDoc(['story'], 'task');
        expect(openDoc(away, 'story')).toEqual(['story', 'task', 'story']);
    });
});
