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

/**
 * How wide a screen is allowed to be.
 *
 * One number for every screen was wrong in both directions. A journey that
 * stands something up draws a banner, and a banner fills the column it is
 * given, so an unbounded column turns it into a wall on a wide display. A table
 * is the opposite: bounded at the width a form wants, it leaves a dead margin
 * on either side and reads as a box in the middle of nothing.
 *
 * So there are two, named for the shape they suit rather than for a number, and
 * a screen says which it is. Both are bounds and not widths: a small display
 * ignores them.
 */
export const SHELL_WIDTHS = {
    /** A rail, a form or a dialog: one column of prose and controls. */
    column: 'max-w-[1100px]',
    /** A list or a grid: content that should use the room it is given. */
    workspace: 'max-w-[1600px]',
} as const;

export type ShellWidth = keyof typeof SHELL_WIDTHS;
