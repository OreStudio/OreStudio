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

import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { describe, expect, it } from 'vitest';
import type { RecordRow } from '../../api/client.js';
import { TranslationProvider } from '../../i18n/Provider.js';
import { RecordStoreProvider, syntheticStore } from '../../refdata/store.js';
import { curveLines, mixtureLines, processName, processTab, ProcessTag } from './processSummary.js';

const row = (fields: Record<string, unknown>): RecordRow => fields as unknown as RecordRow;
const t = (key: string): string => key;

describe('a feed’s process summary', () => {
    it('names a curve’s model and a spot feed’s engine, each in its own words', () => {
        expect(processName(t, { kind: 'ir_curve', engine: 'cir' })).toBe(
            'marketdata.irFeeds.engine.cir',
        );
        expect(processName(t, { kind: 'fx_spot', engine: 'gmm' })).toBe(
            'refdata.choices.process_type.gmm',
        );
    });

    it('opens the tab that edits each kind’s process', () => {
        expect(processTab('ir_curve')).toBe('process');
        expect(processTab('fx_spot')).toBe('behaviour');
    });

    it('lists a mixture’s components in order, each with its weight, mean and deviation', () => {
        const lines = mixtureLines([
            row({ component_index: 1, weight: 0.3, mean: 0, stdev: 0.004 }),
            row({ component_index: 0, weight: 0.7, mean: 0.0001, stdev: 0.001 }),
        ]);
        expect(lines).toEqual([
            { name: '#1', value: 'w 0.7 · μ 0.0001 · σ 0.001' },
            { name: '#2', value: 'w 0.3 · μ 0 · σ 0.004' },
        ]);
    });

    it('names a curve’s parameters by their definitions, in the definitions’ order', () => {
        const definitions = [
            row({ id: 'd1', display_name: 'Mean reversion', symbol: 'κ' }),
            row({ id: 'd2', display_name: 'Long-run rate', symbol: 'θ' }),
            row({ id: 'd3', display_name: 'Unused', symbol: '' }),
        ];
        const values = [
            row({ parameter_definition_id: 'd2', parameter_value: 0.03 }),
            row({ parameter_definition_id: 'd1', parameter_value: 0.3 }),
        ];
        expect(curveLines(values, definitions)).toEqual([
            { name: 'Mean reversion · κ', value: '0.3' },
            { name: 'Long-run rate · θ', value: '0.03' },
        ]);
    });

    it('draws only the process’s name until it is opened, so a list reads no parameters', () => {
        const queries = new QueryClient();
        const html = renderToStaticMarkup(
            <TranslationProvider>
                <QueryClientProvider client={queries}>
                    <RecordStoreProvider store={syntheticStore}>
                        <MemoryRouter>
                            <ProcessTag feed={{ id: 'feed-1', kind: 'fx_spot', engine: 'gmm' }} />
                        </MemoryRouter>
                    </RecordStoreProvider>
                </QueryClientProvider>
            </TranslationProvider>,
        );
        expect(html).toContain('aria-expanded="false"');
        expect(html).not.toContain('role="dialog"');
        expect(queries.getQueryCache().getAll()).toHaveLength(0);
    });
});
