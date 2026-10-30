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
 */

import { z } from 'zod';

/**
 * The kinds a value set states, as the server serves them.
 *
 * This is the client side of the wire contract, so the list is stated once here
 * and every screen switches on it. A kind outside it is a set this build does
 * not describe, and the parse refuses it.
 */
export const SOURCE_KINDS = [
    'text',
    'integer',
    'number',
    'range',
    'enumerated',
    'catalogue',
    'regex',
    'boolean',
] as const;

export type SourceKind = (typeof SOURCE_KINDS)[number];

/** The kind a parameter carries when its setting names no value set. */
export const NO_SOURCE_KIND = '';

export type ValueSetKind = SourceKind | typeof NO_SOURCE_KIND;

/**
 * The fields of a value set, as every configuration screen reads them.
 *
 * One shape serves the pricing engine screen and the engine run settings
 * screen, because both draw a parameter's control from its set and neither
 * restates an allowed value.
 */
export const valueSetFields = {
    valueSet: z.string().default(''),
    sourceKind: z.enum([...SOURCE_KINDS, NO_SOURCE_KIND] as [ValueSetKind, ...ValueSetKind[]]).default(NO_SOURCE_KIND),
    catalogue: z.string().default(''),
    choices: z.array(z.object({ value: z.string(), label: z.string() })).default([]),
    minimum: z.number().nullable().default(null),
    maximum: z.number().nullable().default(null),
    pattern: z.string().default(''),
};
