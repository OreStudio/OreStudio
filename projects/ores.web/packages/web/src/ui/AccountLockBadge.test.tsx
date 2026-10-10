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
import { renderToStaticMarkup } from 'react-dom/server';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { AccountLockBadge } from './AccountLockBadge.js';

/** The catalogue as the server answers it, holding the two lock badges. */
const CATALOGUE = {
    labels: [
        {
            code: 'account_locked',
            label: 'Locked',
            description: 'Account locked',
            backgroundColour: '#ef4444',
            textColour: '#ffffff',
            severity: 'error',
        },
        {
            code: 'account_unlocked',
            label: 'Unlocked',
            description: 'Account accessible',
            backgroundColour: '#22c55e',
            textColour: '#ffffff',
            severity: 'success',
        },
    ],
    domains: {},
};

function render(locked: boolean, catalogue: typeof CATALOGUE | undefined): string {
    const client = new QueryClient();
    if (catalogue !== undefined) {
        client.setQueryData(['labels'], catalogue);
    }
    return renderToStaticMarkup(
        <QueryClientProvider client={client}>
            <AccountLockBadge locked={locked} />
        </QueryClientProvider>,
    );
}

describe('AccountLockBadge', () => {
    it('paints a locked account in the catalogue red', () => {
        const html = render(true, CATALOGUE);

        expect(html).toContain('Locked');
        expect(html).toContain('background-color:#ef4444');
    });

    it('paints an unlocked account in the catalogue green', () => {
        const html = render(false, CATALOGUE);

        expect(html).toContain('Unlocked');
        expect(html).toContain('background-color:#22c55e');
    });

    it('states the value in plain text until the catalogue arrives', () => {
        const html = render(false, undefined);

        expect(html).toContain('Unlocked');
        expect(html).not.toContain('background-color');
    });
});
