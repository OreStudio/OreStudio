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
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { renderToStaticMarkup } from 'react-dom/server';
import { FlaggedCode, FlagOf } from './flags.js';

function render(node: ReactNode): string {
    const client = new QueryClient();
    client.setQueryData(['image-map'], {
        currencies: { EUR: 'eur-flag', USD: 'usd-flag' },
        countries: { GB: 'gb-flag' },
        calendars: { UK: 'gb-flag' },
        businessCentres: { GBLO: 'gb-flag' },
        noFlag: 'placeholder',
    });
    return renderToStaticMarkup(<QueryClientProvider client={client}>{node}</QueryClientProvider>);
}

describe('the shared flags', () => {
    it('draws a code with the flag the map gives its source', () => {
        const html = render(<FlaggedCode source="calendar" code="UK" />);
        expect(html).toContain('/api/images/gb-flag');
        expect(html).toContain('UK');
    });

    it('draws the placeholder for a code the map does not hold, and nothing for no code', () => {
        expect(render(<FlagOf source="currency" code="XTS" />)).toContain(
            '/api/images/placeholder',
        );
        expect(render(<FlagOf source="currency" code="" />)).toBe('');
    });

    it('draws both currencies of a pair', () => {
        const html = render(<FlagOf source="pair" code="EUR/USD" />);
        expect(html).toContain('/api/images/eur-flag');
        expect(html).toContain('/api/images/usd-flag');
    });

    it('draws an address the caller already holds', () => {
        const html = render(<FlaggedCode code="GBLO" src="/api/tenants/acme/images/x" />);
        expect(html).toContain('/api/tenants/acme/images/x');
    });
});
