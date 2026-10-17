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
 * The coded-choice control.
 *
 * The popup cannot be opened by a static render, so the drawing is proved on
 * the option list itself and the closed control is asserted where it renders.
 */

import { describe, expect, it, vi } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import type { BadgePresentation } from '@ores/wire-protocol/browser';
import { BadgeOptionList, BadgeSelect, type BadgeChoice } from './BadgeSelect.js';
import type { LabelCatalogue } from './shared.js';

function badge(code: string, label: string, backgroundColour: string): BadgePresentation {
    return {
        code,
        label,
        description: label,
        backgroundColour,
        textColour: '#ffffff',
        severity: 'info',
    };
}

/** The tenor kind domain: the row carries PERIOD, the domain paints a badge for it. */
const catalogue: LabelCatalogue = {
    byCode: new Map([
        ['tenor_kind_period', badge('tenor_kind_period', 'Period', '#3b82f6')],
        ['tenor_kind_special', badge('tenor_kind_special', 'Special', '#7c3aed')],
    ]),
    mapped: {
        tenor_kind: { PERIOD: 'tenor_kind_period', SPECIAL: 'tenor_kind_special' },
    },
};

const choices: readonly BadgeChoice[] = [
    { value: 'PERIOD', label: 'PERIOD' },
    { value: 'SPECIAL', label: 'SPECIAL' },
];

describe('the coded-choice control', () => {
    it('draws every option as the badge its domain paints, with the badge colours', () => {
        const html = renderToStaticMarkup(
            <BadgeOptionList
                choices={choices}
                domain="tenor_kind"
                catalogue={catalogue}
                value=""
                onChoose={vi.fn()}
            />,
        );
        expect(html).toContain('Period');
        expect(html).toContain('Special');
        expect(html).toContain('#3b82f6');
        expect(html).toContain('#7c3aed');
        expect(html).not.toContain('>PERIOD<');
    });

    it('marks the chosen option as selected', () => {
        const html = renderToStaticMarkup(
            <BadgeOptionList
                choices={choices}
                domain="tenor_kind"
                catalogue={catalogue}
                value="SPECIAL"
                onChoose={vi.fn()}
            />,
        );
        expect(html).toContain('aria-selected="true"');
    });

    it('draws a code the domain does not paint as the code, never as Unmapped', () => {
        const html = renderToStaticMarkup(
            <BadgeOptionList
                choices={[{ value: 'SOMETHING_NEW', label: 'SOMETHING_NEW' }]}
                domain="tenor_kind"
                catalogue={catalogue}
                value=""
                onChoose={vi.fn()}
            />,
        );
        expect(html).toContain('SOMETHING_NEW');
        expect(html).not.toContain('Unmapped');
    });

    it('draws the chosen value in the closed control, so it agrees with its list', () => {
        const html = renderToStaticMarkup(
            <BadgeSelect
                label="Kind"
                domain="tenor_kind"
                value="PERIOD"
                choices={choices}
                catalogue={catalogue}
                onChange={vi.fn()}
            />,
        );
        expect(html).toContain('Period');
        expect(html).toContain('role="combobox"');
        expect(html).toContain('aria-expanded="false"');
        // Closed, so the list is not in the document.
        expect(html).not.toContain('role="listbox"');
    });
});
