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
 * The tenor page's own parts.
 *
 * The page reaches the server for the catalogue, the pick lists and the
 * reasons, and the web package has no browser to put it in. The page is
 * asserted where it renders, and the confirm is asserted where it is built, in
 * `tenorsSteps.test.tsx`.
 */

import { describe, expect, it, vi } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import type { ReactNode } from 'react';
import { TranslationProvider } from '../i18n/Provider.js';
import { TenorJourney } from './TenorJourney.js';
import type { TenorsServer } from './tenorsServer.js';

function render(node: ReactNode): string {
    return renderToStaticMarkup(<TranslationProvider>{node}</TranslationProvider>);
}

function fakeServer(): TenorsServer {
    return {
        catalogue: vi.fn(async () => ({ tenors: [], conventions: [], schedules: [] })),
        pickLists: vi.fn(async () => {
            throw new Error('not reached');
        }),
        resolutions: vi.fn(async () => []),
        previewResolution: vi.fn(async () => {
            throw new Error('not reached');
        }),
        amendReasons: vi.fn(async () => []),
        writeTenor: vi.fn(async () => {
            throw new Error('not reached');
        }),
        writeConvention: vi.fn(async () => {
            throw new Error('not reached');
        }),
        writeMembership: vi.fn(async () => {
            throw new Error('not reached');
        }),
        removeMembership: vi.fn(async () => {
            throw new Error('not reached');
        }),
    };
}

describe('the tenor page', () => {
    it('opens on the choose step, with the rail the journey runs', () => {
        const html = render(<TenorJourney server={fakeServer()} onFinished={() => undefined} />);
        for (const step of ['Choose', 'Tenor', 'Convention', 'Test', 'Review']) {
            expect(html).toContain(step);
        }
        expect(html).toContain('New tenor');
        expect(html).toContain('New convention');
    });
});
