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
import type { QueryClient } from '@tanstack/react-query';
import { Route } from 'react-router';
import { accessOf, renderRecords } from '../refdata/testSupport.js';
import { textOf } from '../textOf.js';
import { ReportingPage } from './ReportingPage.js';

/**
 * The Reporting area page: its cards, and nothing else. A person who holds the
 * read of the Reports screen is offered its card; a person who does not is
 * offered no card, because the area holds no other screen.
 */

const REPORT_READ = 'reporting::report_definitions:read';

function page(permissionCodes: readonly string[]): string {
    return renderRecords(
        '/reporting',
        [],
        <Route path="/reporting" element={<ReportingPage />} />,
        (client: QueryClient) => client.setQueryData(['my-access'], accessOf(permissionCodes)),
    );
}

/** The words of the trail, so the test reads the path a person took. */
function trailOf(html: string): string {
    const nav = html.match(/<nav aria-label="[^"]*"[^>]*>(.*?)<\/nav>/s);
    if (nav === null) {
        return '(no breadcrumb)';
    }
    return textOf(nav[1] ?? '')
        .replace(/›/g, ' › ')
        .replace(/\s+/g, ' ')
        .trim();
}

describe('the Reporting area page', () => {
    it('draws the card of the reports a party runs', () => {
        const html = page([REPORT_READ]);
        const words = textOf(html);

        expect(words).toContain('Reports');
        expect(words).toContain(
            'The definitions this party runs, and the health of each latest run.',
        );
        expect(html).toContain('href="/reporting/reports"');
    });

    it('draws the trail Home, Reporting', () => {
        expect(trailOf(page([REPORT_READ]))).toBe('Home › Reporting');
    });

    it('draws no card for a person who does not hold the read', () => {
        const html = page([]);

        expect(textOf(html)).not.toContain('The definitions this party runs');
        expect(html).not.toContain('href="/reporting/reports"');
    });
});
