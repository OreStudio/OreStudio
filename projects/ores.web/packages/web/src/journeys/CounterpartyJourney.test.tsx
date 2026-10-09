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
 * The counterparty page's own parts.
 *
 * The screen reaches the server for the pick lists and the parent
 * counterparties before a person can fill the form, and the web package has no
 * browser to put it in. The page is therefore asserted where it renders, and
 * the rail it produces is asserted where it is built, in
 * `counterpartySteps.test.tsx`.
 */

import { describe, expect, it, vi } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import type { ReactNode } from 'react';
import { TranslationProvider } from '../i18n/Provider.js';
import { CounterpartyJourney, counterpartyHeader } from './CounterpartyJourney.js';
import {
    authoritativeIdentifier,
    blankDraft,
    hasIdentity,
    type NewCounterparty,
} from './counterpartyState.js';
import type { CounterpartyServer } from './counterpartyServer.js';
import { createTranslator } from '../i18n/translate.js';
import { enFlat } from '../i18n/locales/en.js';

const t = createTranslator('en', enFlat, enFlat).t;

function render(node: ReactNode): string {
    return renderToStaticMarkup(<TranslationProvider>{node}</TranslationProvider>);
}

const never = vi.fn(async () => undefined);

/** A server that answers nothing, because no static render reaches it. */
function fakeServer(): CounterpartyServer {
    return {
        listCounterparties: vi.fn(async () => ({ rows: [], total: 0 })),
        childrenOf: vi.fn(async () => []),
        visibilityOf: vi.fn(async () => []),
        pickLists: vi.fn(async () => ({
            partyTypes: [],
            partyStatuses: [],
            businessCentres: [],
            identifierSchemes: [],
            contactTypes: [],
            currencies: [],
        })),
        writeComposite: vi.fn(async () => ({
            success: false,
            code: '',
            message: '',
            fields: [],
            counterparty: undefined,
        })),
        writeBusinessCentres: never,
        writeVisibility: never,
    };
}

function asState(draft: ReturnType<typeof blankDraft>): NewCounterparty {
    return {
        ...draft,
        hasIdentity: hasIdentity(draft),
        authoritative: authoritativeIdentifier(draft.identifiers),
        startNew: never,
        open: never,
        setIdentity: never,
        setCentres: never,
        addIdentifier: never,
        removeIdentifier: never,
        markAuthoritative: never,
        addContact: never,
        removeContact: never,
        setContact: never,
        addAgreement: never,
        removeAgreement: never,
        setAgreement: never,
        addSet: never,
        removeSet: never,
        setSet: never,
        addSetIdentifier: never,
        removeSetIdentifier: never,
        setCsa: never,
        toggleCsa: never,
        addEligibleCurrency: never,
        removeEligibleCurrency: never,
        recordWritten: never,
        recordRefusal: never,
        clearRefusal: never,
    };
}

describe('the counterparty onboarding page', () => {
    it('opens on the counterparty list, with the rail the journey runs', () => {
        const html = render(
            <CounterpartyJourney
                server={fakeServer()}
                partyId="p-ores"
                onFinished={() => undefined}
            />,
        );

        expect(html).toContain('Counterparties');
        expect(html).toContain('The legal entities this tenant trades with.');
        expect(html).toContain('Onboard a counterparty');
        expect(html).toContain('Identity');
        expect(html).toContain('Legal agreements');
        expect(html).toContain('History');
    });
});

describe('the header the counterparty journey shows', () => {
    it('is nothing until the counterparty has a name', () => {
        expect(counterpartyHeader(t, asState(blankDraft()))).toBeUndefined();
    });

    it('states the name and the short code once it has them', () => {
        const named = asState({
            ...blankDraft(),
            fullName: 'Northwind Capital Ltd',
            shortCode: 'NWCAP',
        });

        const html = renderToStaticMarkup(
            <TranslationProvider>{counterpartyHeader(t, named)}</TranslationProvider>,
        );

        expect(html).toContain('Northwind Capital Ltd');
        expect(html).toContain('NWCAP');
    });
});
