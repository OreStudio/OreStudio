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
import { renderToStaticMarkup } from 'react-dom/server';
import { TranslationProvider } from '../i18n/Provider.js';
import { RemoveTenantDialog } from './RemoveTenantDialog.js';

/**
 * What the removal dialog says before anything happens.
 *
 * The words must match what the server does, so each one is checked: nobody
 * signs in, people already in lose access, the data is kept.
 */
function render(): string {
    return renderToStaticMarkup(
        <QueryClientProvider client={new QueryClient()}>
            <TranslationProvider>
                <RemoveTenantDialog
                    tenant={{ code: 'acme_corporation', name: 'Acme Corporation' }}
                    onClose={() => undefined}
                    onRemoved={() => undefined}
                />
            </TranslationProvider>
        </QueryClientProvider>,
    );
}

describe('RemoveTenantDialog', () => {
    it('says what removal does, in the server terms', () => {
        const html = render();

        expect(html).toContain('Remove Acme Corporation?');
        expect(html).toContain('Nobody can sign in to it any more.');
        expect(html).toContain('lose access');
        expect(html).toContain('Its data is kept, and it leaves the tenant list.');
    });

    it('asks for the tenant code, and keeps the removal disabled until it is typed', () => {
        const html = render();

        expect(html).toContain('Type acme_corporation to confirm');
        expect(html).toMatch(/<button[^>]*disabled=""[^>]*>Remove tenant<\/button>/);
    });
});
