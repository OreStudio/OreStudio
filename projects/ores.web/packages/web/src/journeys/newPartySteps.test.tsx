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

import { describe, expect, it, vi } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import { TranslationProvider } from '../i18n/Provider.js';
import { createTranslator } from '../i18n/translate.js';
import { enFlat } from '../i18n/locales/en.js';
import { newPartySteps, partyHeader } from './newPartySteps.js';
import { workInNewParty } from './NewPartyJourney.js';
import {
    EMPTY_PARTY,
    partyFullName,
    partyLei,
    reduceParty,
    type NewParty,
    type PartyAction,
    type PartyDraft,
} from './partyState.js';
import type { JourneyServer } from './server.js';
import type { LeiEntityChoice } from '@ores/wire-protocol/browser';

const t = createTranslator('en', enFlat, enFlat).t;

const barclays: LeiEntityChoice = {
    lei: '213800LBQA1Y9L22JB70',
    legalName: 'BARCLAYS PLC',
    country: 'GB',
    partyCount: 82,
};

function fakeServer(overrides: Partial<JourneyServer> = {}): JourneyServer {
    return {
        createAdministrator: vi.fn(async () => undefined),
        recheckBootstrap: vi.fn(async () => undefined),
        completeSystemOnboarding: vi.fn(async () => undefined),
        signIn: vi.fn(async () => ({ outcome: 'active', passwordResetRequired: false }) as const),
        chooseParty: vi.fn(async () => undefined),
        switchParty: vi.fn(async () => undefined),
        signOut: vi.fn(async () => undefined),
        passwordPolicy: vi.fn(async () => ({
            success: true,
            message: '',
            minLength: 12,
            requireUppercase: true,
            requireLowercase: true,
            requireDigit: true,
            requireSpecial: true,
            specialChars: '!@#$%^&*',
        })),
        seedProfiles: vi.fn(async () => []),
        tenantCodes: vi.fn(async () => []),
        leiEntities: vi.fn(async () => [barclays]),
        provision: vi.fn(async () => ({
            success: true,
            message: '',
            instanceId: '',
            tenantId: '',
            accountId: '',
        })),
        provisionParty: vi.fn(async () => ({
            success: true,
            message: '',
            instanceId: '9c1f0f5a-6bd2-4f2a-9a4a-6f1a3a2b4c5d',
            partyId: '0d0f9f24-6f3a-4c2b-9c2e-1a2b3c4d5e6f',
        })),
        progress: vi.fn(async () => ({
            success: true,
            message: '',
            status: 'completed',
            error: '',
            step_count: 1,
            current_step_index: 0,
            steps: [],
        })),
        retry: vi.fn(async () => ({
            success: true,
            message: '',
            instanceId: '',
            stepIndex: 0,
            stepName: '',
        })),
        changePassword: vi.fn(async () => undefined),
        ...overrides,
    };
}

function state(actions: PartyAction[] = []): PartyDraft {
    return actions.reduce(reduceParty, EMPTY_PARTY);
}

/** A draft as the page holds it, with the screens' actions stubbed out. */
function asNewParty(draft: PartyDraft): NewParty {
    return {
        ...draft,
        fullName: partyFullName(draft),
        lei: partyLei(draft),
        chooseEntity: vi.fn(),
        describeByName: vi.fn(),
        searchAgain: vi.fn(),
        typeName: vi.fn(),
        setShortCode: vi.fn(),
        recordRun: vi.fn(),
        recordRunComplete: vi.fn(),
        restart: vi.fn(),
    };
}

function steps(
    draft: PartyDraft,
    options: { readonly server?: JourneyServer } = {},
): ReturnType<typeof newPartySteps> {
    return newPartySteps({
        t,
        server: options.server ?? fakeServer(),
        state: asNewParty(draft),
        onWorkInIt: async () => undefined,
        onAnother: () => undefined,
        onDone: () => undefined,
    });
}

describe('the party journey a tenant administrator runs', () => {
    it('is the five party steps on one rail', () => {
        expect(steps(state()).map((step) => step.id)).toEqual([
            'entity',
            'describe',
            'review',
            'provisioning',
            'next',
        ]);
    });

    it('cannot describe a party until it has a name', () => {
        // Nothing chosen yet, and nothing typed for a party described by hand.
        expect(steps(state())[0]!.next?.enabled).toBe(false);
        expect(steps(state([{ kind: 'describe-by-hand' }]))[0]!.next?.enabled).toBe(false);
        // The spaces a person types instead of a name are not a name.
        expect(
            steps(state([{ kind: 'describe-by-hand' }, { kind: 'type-name', name: '   ' }]))[0]!
                .next?.enabled,
        ).toBe(false);
    });

    it('cannot move on without the short code the tenant will know it by', () => {
        const chosen = state([{ kind: 'choose-entity', entity: barclays }]);
        expect(steps(chosen)[1]!.next?.enabled).toBe(true);
        expect(steps({ ...chosen, shortCode: '' })[1]!.next?.enabled).toBe(false);
    });

    it('adds the party an entity names, with the LEI that names the entity', async () => {
        const server = fakeServer();
        const draft = state([
            { kind: 'choose-entity', entity: barclays },
            { kind: 'set-short-code', code: 'BRCLYS' },
        ]);
        const rendered = steps(draft, { server });

        await rendered[2]!.next!.run!();

        expect(server.provisionParty).toHaveBeenCalledWith({
            fullName: 'BARCLAYS PLC',
            shortCode: 'BRCLYS',
            lei: '213800LBQA1Y9L22JB70',
        });
        expect(rendered[3]!.id).toBe('provisioning');
    });

    it('adds a party that is not one of ours without an LEI', async () => {
        const server = fakeServer();
        const draft = state([
            { kind: 'describe-by-hand' },
            { kind: 'type-name', name: 'Northwind Capital' },
        ]);

        await steps(draft, { server })[2]!.next!.run!();

        expect(server.provisionParty).toHaveBeenCalledWith({
            fullName: 'Northwind Capital',
            shortCode: 'NORCAP',
            lei: '',
        });
    });

    it('records the party and the run the server started', async () => {
        const server = fakeServer();
        const recordRun = vi.fn();
        const draft = state([{ kind: 'choose-entity', entity: barclays }]);
        const rendered = newPartySteps({
            t,
            server,
            state: { ...asNewParty(draft), recordRun },
            onWorkInIt: async () => undefined,
            onAnother: () => undefined,
            onDone: () => undefined,
        });

        await rendered[2]!.next!.run!();

        expect(recordRun).toHaveBeenCalledWith(
            '0d0f9f24-6f3a-4c2b-9c2e-1a2b3c4d5e6f',
            '9c1f0f5a-6bd2-4f2a-9a4a-6f1a3a2b4c5d',
        );
    });

    it('reports the refusal the server stated rather than moving on', async () => {
        const server = fakeServer({
            provisionParty: vi.fn(async () => ({
                success: false,
                message: 'A party with this code already exists',
                instanceId: '',
                partyId: '',
            })),
        });

        await expect(
            steps(state([{ kind: 'choose-entity', entity: barclays }]), { server })[2]!.next!
                .run!(),
        ).rejects.toThrow('A party with this code already exists');
    });

    it('says so when the server made the party but named no run', async () => {
        const server = fakeServer({
            provisionParty: vi.fn(async () => ({
                success: true,
                message: '',
                instanceId: '',
                partyId: '0d0f9f24-6f3a-4c2b-9c2e-1a2b3c4d5e6f',
            })),
        });

        await expect(
            steps(state([{ kind: 'choose-entity', entity: barclays }]), { server })[2]!.next!
                .run!(),
        ).rejects.toThrow('named no run to follow');
    });

    it('walks the review of a party described by hand with no LEI in it', () => {
        const draft = state([
            { kind: 'describe-by-hand' },
            { kind: 'type-name', name: 'Northwind Capital' },
        ]);
        const html = renderToStaticMarkup(
            <TranslationProvider>{steps(draft)[2]!.body}</TranslationProvider>,
        );

        expect(html).toContain('Northwind Capital');
        expect(html).toContain('No LEI');
        expect(html).toContain('NORCAP');
        expect(html).toContain('standard data');
    });
});

describe('the header a party journey shows', () => {
    it('is nothing until the party has a name', () => {
        expect(partyHeader(t, asNewParty(state()))).toBeUndefined();
    });
});

describe('working as the party the journey made', () => {
    it('re-scopes the session to it', async () => {
        const server = fakeServer();

        await workInNewParty(server, '0d0f9f24-6f3a-4c2b-9c2e-1a2b3c4d5e6f');

        expect(server.switchParty).toHaveBeenCalledWith('0d0f9f24-6f3a-4c2b-9c2e-1a2b3c4d5e6f');
    });

    it('does nothing when the journey has no party yet', async () => {
        const server = fakeServer();

        await workInNewParty(server, undefined);

        expect(server.switchParty).not.toHaveBeenCalled();
    });
});
