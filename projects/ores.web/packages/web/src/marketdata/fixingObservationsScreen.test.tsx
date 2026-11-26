/**
 * The system view of the fixing observations: the rows the service states, the
 * empty series shown as a row of its own, and the line that says the screen
 * holds no authorisation.
 */

import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { renderToStaticMarkup } from 'react-dom/server';
import { MemoryRouter } from 'react-router';
import { describe, expect, it } from 'vitest';
import type { FixingObservation } from '../api/client.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { FixingObservationsPage } from './FixingObservationsPage.js';

const SOFR: FixingObservation = {
    series_id: 's1',
    oresmd_uri: 'oresmd://ir/USD?type=fixing&index=ibor&name=SOFR&tenor=1M',
    asset_class: 'ir',
    index_label: 'USD SOFR 1M',
    source: 'derived-a',
    provider_code: 'derived',
    latest_value: '0.039436',
    latest_date: '2026-11-26',
    days: 520,
};

const EMPTY: FixingObservation = {
    series_id: 's2',
    oresmd_uri: 'oresmd://ir/CHF?type=fixing&index=ibor&name=SARON&tenor=3M',
    asset_class: 'ir',
    index_label: 'CHF SARON 3M',
    source: '',
    provider_code: 'derived',
    latest_value: '',
    latest_date: null,
    days: 0,
};

const FILTERS = ['', '', ''];

function render(series: readonly FixingObservation[]): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    client.setQueryData(['fixing-observations', ...FILTERS], series);
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <MemoryRouter initialEntries={['/market-data/fixing-observations']}>
                    <FixingObservationsPage />
                </MemoryRouter>
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('the fixing observations screen', () => {
    it('draws what each series states, its latest day and its count', () => {
        const html = render([SOFR, EMPTY]);

        expect(html).toContain('Fixing observations');
        expect(html).toContain('USD SOFR 1M');
        expect(html).toContain('derived-a');
        expect(html).toContain('0.039436');
        expect(html).toContain('2026-11-26');
        expect(html).toContain('520');
    });

    it('shows a series that holds no day as a row rather than hiding it', () => {
        const html = render([SOFR, EMPTY]);

        expect(html).toContain('CHF SARON 3M');
        expect(html).toContain('No day');
        expect(html).toContain('1 of 2 series hold no day');
    });

    it('says it is the system view and offers no authorisation', () => {
        const html = render([SOFR]);

        expect(html).toContain('system view of the observations');
        expect(html).toContain('offers no authorisation');
        expect(html).not.toContain('Authorise');
        expect(html).not.toContain('Override');
    });
});
