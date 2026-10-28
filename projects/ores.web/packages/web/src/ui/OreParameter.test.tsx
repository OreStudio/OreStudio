/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { describe, expect, it } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import { TranslationProvider } from '../i18n/Provider.js';
import { textOf } from '../textOf.js';
import { ParameterDocumentationPanel } from './OreParameter.js';

/** The one documentation panel every screen of ORE leaves draws. */
function panel(choices?: readonly string[]): string {
    return textOf(
        renderToStaticMarkup(
            <TranslationProvider>
                <ParameterDocumentationPanel
                    mode="simple"
                    doc={{
                        documented: true,
                        name: 'InterpolationMethod',
                        description: 'The interpolation method to use.',
                        defaultText: 'LogLinear.',
                        format: 'Text.',
                        introducedIn: '1.8.0.7 or earlier',
                        removedIn: '',
                        xpath: '/CurveConfiguration/YieldCurves/YieldCurve/InterpolationMethod',
                        ...(choices === undefined ? {} : { choices }),
                    }}
                />
            </TranslationProvider>,
        ),
    );
}

describe('the documentation panel of one leaf', () => {
    it('states the values a closed list accepts, in the order the set gives them', () => {
        const words = panel(['LogLinear', 'Linear', 'Cubic']);
        expect(words).toContain('Valid values: LogLinear, Linear, Cubic');
    });

    it('states no values for a leaf whose set is not a closed list', () => {
        expect(panel()).not.toContain('Valid values');
    });
});
