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
 * The party details page's own parts.
 *
 * The page reaches the server for the pick lists and the reasons, and the web
 * package has no browser to put it in. The page is asserted where it renders,
 * and the confirm is asserted where it is built, in `partyDetailsSteps.test.tsx`.
 */

import { describe, expect, it, vi } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import type { ReactNode } from 'react';
import { TranslationProvider } from '../i18n/Provider.js';
import { PartyDetailsJourney } from './PartyDetailsJourney.js';
import { partyHeader } from './partyDetailsSteps.js';
import { blankDraft, reduceParty } from './partyDetailsState.js';
import type { PartyDetails } from './partyDetailsState.js';
import type { PartyDetailsServer } from './partyDetailsServer.js';
import type { Party } from '@ores/wire-protocol/generated/refdata/domain/party';
import { createTranslator } from '../i18n/translate.js';
import { enFlat } from '../i18n/locales/en.js';

const t = createTranslator('en', enFlat, enFlat).t;

function render(node: ReactNode): string {
    return renderToStaticMarkup(<TranslationProvider>{node}</TranslationProvider>);
}

/** A server that answers nothing, because no static render reaches it. */
function fakeServer(): PartyDetailsServer {
    const never = vi.fn(async () => {
        throw new Error('not reached');
    });
    return {
        listParties: vi.fn(async () => ({ rows: [], total: 0 })),
        setsOf: never,
        pickLists: never,
        compositeAsOf: never,
        amendReasons: vi.fn(async () => []),
        writeComposite: never,
        retireIdentifier: never,
        linkMembership: never,
        closeMembership: never,
        reclassifyUnit: never,
    };
}

describe('the party details page', () => {
    it('opens on the party list, with the rail the journey runs', () => {
        const html = render(
            <PartyDetailsJourney server={fakeServer()} onFinished={() => undefined} />,
        );
        for (const step of [
            'Parties',
            'Overview',
            'Identifiers',
            'Contacts',
            'Memberships',
            'Structure',
            'Review',
        ]) {
            expect(html).toContain(step);
        }
        expect(html).toContain('Search by name, short code or codename');
    });

    it('draws no header before a party is open, and the record once it is', () => {
        const draft = blankDraft();
        const state = { ...draft, changes: [] } as unknown as PartyDetails;
        expect(partyHeader(t, state)).toBeUndefined();
        const party = {
            version: 7,
            id: 'p',
            short_code: 'ACME',
            full_name: 'Acme Ltd',
            codename: 'c',
            transliterated_name: null,
            party_category: 'Operational',
            party_type: 'Corporate',
            parent_party_id: null,
            business_center_code: 'GBLO',
            status: 'Active',
            image_id: null,
            is_registration_default: false,
        } as unknown as Party;
        const open = reduceParty(draft, {
            kind: 'open',
            party,
            sets: {
                identifiers: [],
                contacts: [],
                countries: [],
                currencies: [],
                counterparties: [],
                units: [],
            },
        });
        const html = render(
            <>{partyHeader(t, { ...open, changes: [] } as unknown as PartyDetails)}</>,
        );
        expect(html).toContain('Acme Ltd');
        expect(html).toContain('ACME');
        expect(html).toContain('v7');
    });
});
