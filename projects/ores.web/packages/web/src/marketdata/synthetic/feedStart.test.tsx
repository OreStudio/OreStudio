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
import { renderToStaticMarkup } from 'react-dom/server';
import { describe, expect, it } from 'vitest';
import type { FeedStart } from '../../api/client.js';
import { TranslationProvider } from '../../i18n/Provider.js';
import { FeedStartNotice } from './feedStart.js';

const START: FeedStart = {
    from_fixing: true,
    item: 'oresmd://fx/EUR?type=fixing&index=fx&source=ECB&ccy=USD',
    provider_code: 'ecb',
    vendor_id: 'EUR',
    value: '1.15',
    inverted: false,
    fixing_date: '2026-10-27',
    received_at: '2026-10-27 16:05:00.000000Z',
    level: 1.15,
    window_days: 400,
    used: 261,
    hourly_spread: 0.0004,
    note: '',
};

function render(start: FeedStart): string {
    const client = new QueryClient({ defaultOptions: { queries: { retry: false } } });
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <TranslationProvider>
                <FeedStartNotice start={start} />
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('where a feed run started', () => {
    it('names the provider, the item, the fixing date and the time it arrived', () => {
        const html = render(START);
        expect(html).toContain('Where this run started');
        expect(html).toContain('ECB');
        expect(html).toContain('2026-10-27');
        expect(html).toContain('261 fixings over 400 days measured its spread.');
        expect(html).toContain('oresmd://fx/EUR?type=fixing&amp;index=fx');
    });

    it('says the run kept its fixed level when no fixing set it', () => {
        const html = render({
            ...START,
            from_fixing: false,
            item: '',
            provider_code: '',
            fixing_date: null,
            received_at: null,
            used: 0,
            window_days: 0,
            note: 'The config names no fixing item.',
        });
        expect(html).toContain('kept its configured level');
        expect(html).toContain('The config names no fixing item.');
        expect(html).not.toContain('fixing of');
    });

    it('says the item quotes the pair the other way round when it was inverted', () => {
        const html = render({ ...START, inverted: true });
        expect(html).toContain('one over it');
    });
});
