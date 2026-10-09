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

import { beforeEach, describe, expect, it } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import { dismissAll, report } from '../api/errors.js';
import { TranslationProvider } from '../i18n/Provider.js';
import { ErrorBanner } from './ErrorBanner.js';

function render(): string {
    return renderToStaticMarkup(
        <TranslationProvider>
            <ErrorBanner />
        </TranslationProvider>,
    );
}

/**
 * The banner's own contract: what it shows while nothing is wrong, and what it
 * shows for one report and for several. That the reports reach it is the store
 * and the query client's business, asserted where those live.
 */
describe('the error banner', () => {
    beforeEach(() => {
        dismissAll();
    });

    it('shows nothing while no request has failed', () => {
        expect(render()).toBe('');
    });

    it("states the server's own sentence in monospace", () => {
        report('The ledger is closed.');

        const html = render();

        expect(html).toContain('The ledger is closed.');
        expect(html).toContain('font-mono');
    });

    it('states every report and offers a control beside each', () => {
        report('the roster did not load');
        report('the save did not finish');

        const html = render();

        expect(html).toContain('the roster did not load');
        expect(html).toContain('the save did not finish');
        expect(html).toContain('>Dismiss<');
        expect(html).toContain('Dismiss all');
    });
});
