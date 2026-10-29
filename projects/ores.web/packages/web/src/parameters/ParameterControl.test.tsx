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

import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { describe, expect, it } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import { TranslationProvider } from '../i18n/Provider.js';
import { ParameterControl } from './ParameterControl.js';
import type { ControlModel } from './control.js';

const CALIBRATION: ControlModel = {
    kind: 'choice',
    choices: [
        { value: 'Bootstrap', label: 'Bootstrap calibration' },
        { value: 'BestFit', label: 'Best fit' },
    ],
};

/** The control as the page draws it, for a screen that states no exact request. */
function draw(
    model: ControlModel,
    value: string,
    extra: { unset?: string; label?: string; invalid?: boolean } = {},
): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <ParameterControl
                model={model}
                value={value}
                label={extra.label ?? 'Calibration'}
                {...(extra.unset === undefined ? {} : { unset: extra.unset })}
                {...(extra.invalid === undefined ? {} : { invalid: extra.invalid })}
                onChange={() => undefined}
            />
        </TranslationProvider>,
    );
}

/** A control drawn where the flag map may be read, as the record screens draw it. */
function drawWithFlags(model: ControlModel, value: string, unset?: string): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['image-map'], { currencies: { GBP: 'gbp.png' }, noFlag: 'none.png' });
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <ParameterControl
                    model={model}
                    value={value}
                    label="Currency"
                    {...(unset === undefined ? {} : { unset })}
                    onChange={() => undefined}
                />
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the control a catalogue draws', () => {
    const CURRENCIES: ControlModel = {
        kind: 'choice',
        catalogue: 'refdata.currency',
        choices: [
            { value: 'GBP', label: 'Pound Sterling' },
            { value: 'USD', label: 'US Dollar' },
        ],
    };

    it('draws the currency catalogue with the flagged picker the records use', () => {
        const html = drawWithFlags(CURRENCIES, 'GBP');

        expect(html).toContain('aria-haspopup="listbox"');
        expect(html).toContain('Pound Sterling (GBP)');
        expect(html).not.toContain('<select');
    });

    it('draws a closed list of the contract as a select, whatever the record screens do', () => {
        expect(draw(CALIBRATION, 'Bootstrap')).toContain('<select');
    });
});

describe('the control a choice draws', () => {
    it('states each choice by its label, with the value chosen', () => {
        const html = draw(CALIBRATION, 'Bootstrap');
        expect(html).toContain('<select');
        expect(html).toContain(
            '<option value="Bootstrap" selected="">Bootstrap calibration</option>',
        );
        expect(html).toContain('<option value="BestFit">Best fit</option>');
    });

    it('keeps a value the choices no longer offer selectable', () => {
        expect(draw(CALIBRATION, 'Legacy')).toContain(
            '<option value="Legacy" selected="">Legacy</option>',
        );
    });

    it('offers the not-set choice first when the screen states one, once', () => {
        const html = draw(CALIBRATION, '', { unset: 'Default: No. The engine default.' });
        expect(html).toContain(
            '<option value="" selected="">Default: No. The engine default.</option>',
        );
        expect(html.match(/<option value=""/g)?.length).toBe(1);
    });

    it('names the control after the label the screen gives it', () => {
        expect(draw(CALIBRATION, 'Bootstrap')).toContain('aria-label="Calibration"');
    });
});

describe('the control a boolean draws', () => {
    it('offers true and false, with the value chosen', () => {
        const html = draw({ kind: 'boolean' }, 'true');
        expect(html).toContain('<option value="true" selected="">true</option>');
        expect(html).toContain('<option value="false">false</option>');
    });

    it('offers the not-set choice when the screen states one', () => {
        expect(draw({ kind: 'boolean' }, '', { unset: 'Not set' })).toContain(
            '<option value="" selected="">Not set</option>',
        );
    });

    it('names the control after the label the screen gives it', () => {
        expect(draw({ kind: 'boolean' }, '', { label: 'Imply today’s fixings' })).toContain(
            'aria-label="Imply today’s fixings"',
        );
    });
});

describe('the control a number draws', () => {
    it('takes a whole number in its bounds', () => {
        const html = draw({ kind: 'number', minimum: 1, maximum: 500, whole: true }, '100');
        expect(html).toContain('type="number"');
        expect(html).toContain('min="1"');
        expect(html).toContain('max="500"');
        expect(html).toContain('step="1"');
        expect(html).toContain('inputMode="numeric"');
        expect(html).toContain('value="100"');
    });

    it('takes a fraction when the value need not be whole', () => {
        const html = draw({ kind: 'number', minimum: null, maximum: null, whole: false }, '0.5');
        expect(html).toContain('step="any"');
        expect(html).toContain('inputMode="decimal"');
        expect(html).not.toContain('min=');
        expect(html).not.toContain('max=');
    });

    it('states the not-set words in the field while no value is typed', () => {
        const html = draw({ kind: 'number', minimum: 1, maximum: null, whole: true }, '', {
            unset: 'Default: 1.',
        });
        expect(html).toContain('placeholder="Default: 1."');
    });

    it('names the control after the label the screen gives it', () => {
        expect(draw({ kind: 'number', minimum: 1, maximum: null, whole: true }, '4')).toContain(
            'aria-label="Calibration"',
        );
    });
});

describe('the control a text draws', () => {
    it('takes the text the pattern allows', () => {
        const html = draw({ kind: 'text', pattern: '^[A-Z]+$' }, 'TARGET');
        expect(html).toContain('<input');
        expect(html).toContain('pattern="^[A-Z]+$"');
        expect(html).toContain('value="TARGET"');
    });

    it('states no pattern when the value set states none', () => {
        expect(draw({ kind: 'text', pattern: '' }, 'TARGET')).not.toContain('pattern=');
    });

    it('names the control after the label the screen gives it', () => {
        expect(draw({ kind: 'text', pattern: '' }, 'TARGET')).toContain('aria-label="Calibration"');
    });
});

describe('a value the parameter does not take', () => {
    it('marks the control and points at the words that say what is wrong', () => {
        const html = draw({ kind: 'number', minimum: 1, maximum: null, whole: true }, '0', {
            label: 'Threads',
            invalid: true,
        });
        expect(html).toContain('aria-invalid="true"');
    });

    it('marks nothing when the screen states no problem', () => {
        const html = draw({ kind: 'boolean' }, 'true', { invalid: false });
        expect(html).toContain('aria-invalid="false"');
    });
});
