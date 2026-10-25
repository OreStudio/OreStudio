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
import { invalidFields, type FieldSpec } from './records.js';

/**
 * The branching ratio is the one arrival parameter with a strict upper bound:
 * one or more has no stationary rate, so the screen refuses it before a save,
 * beside the rule the factory applies on the server.
 */
describe('a decimal with a strict upper bound', () => {
    const specs: readonly FieldSpec[] = [
        {
            field: 'branching_ratio',
            label: 'Branching Ratio',
            kind: { kind: 'decimal', below: 1 },
        },
    ];

    it('accepts a value below the bound', () => {
        expect(invalidFields(specs, { branching_ratio: '0.6' })).toEqual([]);
        expect(invalidFields(specs, { branching_ratio: '0' })).toEqual([]);
    });

    it('refuses the bound and anything above it', () => {
        expect(invalidFields(specs, { branching_ratio: '1' })).toEqual(['branching_ratio']);
        expect(invalidFields(specs, { branching_ratio: '1.5' })).toEqual(['branching_ratio']);
    });

    it('still refuses what is not a number', () => {
        expect(invalidFields(specs, { branching_ratio: 'strong' })).toEqual(['branching_ratio']);
    });
});
