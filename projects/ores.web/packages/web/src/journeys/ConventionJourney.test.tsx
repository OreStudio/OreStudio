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
 * The convention page's own parts.
 *
 * The page reaches the server for the term lists and the reasons, and the web
 * package has no browser to put it in. The page is asserted where it renders,
 * and the confirm is asserted where it is built, in `conventionsSteps.test.tsx`.
 */

import { describe, expect, it, vi } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import type { ReactNode } from 'react';
import { TranslationProvider } from '../i18n/Provider.js';
import { ConventionJourney } from './ConventionJourney.js';
import type { ConventionsServer } from './conventionsServer.js';

function render(node: ReactNode): string {
    return renderToStaticMarkup(<TranslationProvider>{node}</TranslationProvider>);
}

function fakeServer(): ConventionsServer {
    return {
        families: vi.fn(async () => []),
        rowsOf: vi.fn(async () => ({ rows: [], total: 0 })),
        pickLists: vi.fn(async () => {
            throw new Error('not reached');
        }),
        amendReasons: vi.fn(async () => []),
        write: vi.fn(async () => {
            throw new Error('not reached');
        }),
    };
}

describe('the convention page', () => {
    it('opens on the instrument step, with the rail the journey runs', () => {
        const html = render(
            <ConventionJourney
                server={fakeServer()}

                onFinished={() => undefined}
            />,
        );
        for (const step of ['Instrument', 'Convention', 'Terms', 'Review']) {
            expect(html).toContain(step);
        }
        expect(html).toContain('Search families');
    });
});
