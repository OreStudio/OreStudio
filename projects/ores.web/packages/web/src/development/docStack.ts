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
 * The docs a tester has followed inside the Story or the Task tab, as the ids
 * from the first doc to the one on show. A link opens a doc on top of the
 * stack, and Back takes it off. The tab never leaves the runner page, so what
 * the tester marked and wrote stays where it is.
 */
export type DocStack = readonly string[];

/** The stack with a doc opened on top. Opening the doc on show changes nothing. */
export function openDoc(stack: DocStack, id: string): DocStack {
    return stack[stack.length - 1] === id ? stack : [...stack, id];
}

/** The stack with the top doc closed. The first doc stays. */
export function closeDoc(stack: DocStack): DocStack {
    return stack.length > 1 ? stack.slice(0, -1) : stack;
}

/** The doc on show, or the first one when the stack is empty. */
export function shownDoc(stack: DocStack, first: string): string {
    return stack[stack.length - 1] ?? first;
}
