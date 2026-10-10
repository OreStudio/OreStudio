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
 * The book structure page's own parts.
 *
 * The page reaches the server for the tree, the pick lists and the reasons, and
 * the web package has no browser to put it in. The page is asserted where it
 * renders, and the confirm is asserted where it is built, in `booksSteps.test.tsx`.
 */

import { describe, expect, it, vi } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import type { ReactNode } from 'react';
import { TranslationProvider } from '../i18n/Provider.js';
import { BookStructureJourney } from './BookStructureJourney.js';
import type { BooksServer } from './booksServer.js';

function render(node: ReactNode): string {
    return renderToStaticMarkup(<TranslationProvider>{node}</TranslationProvider>);
}

/** A server that answers nothing, because no static render reaches it. */
function fakeServer(): BooksServer {
    const never = vi.fn(async () => {
        throw new Error('not reached');
    });
    return {
        tree: vi.fn(async () => ({ portfolios: [], books: [] })),
        pickLists: never,
        rightsAt: never,
        amendReasons: vi.fn(async () => []),
        writePortfolio: never,
        writeBook: never,
    };
}

describe('the book structure page', () => {
    it('opens on the book tree, with the rail the journey runs', () => {
        const html = render(
            <BookStructureJourney
                server={fakeServer()}
                partyId="party-1"
                onFinished={() => undefined}
            />,
        );
        for (const step of [
            'Book tree',
            'Portfolio',
            'Book',
            'Classification',
            'Rights',
            'Review',
        ]) {
            expect(html).toContain(step);
        }
    });
});
