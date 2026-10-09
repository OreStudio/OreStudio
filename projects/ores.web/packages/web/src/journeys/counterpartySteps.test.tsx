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
import {
    COUNTERPARTY_STEP_IDS,
    counterpartySteps,
    counterpartyStepIndex,
} from './counterpartySteps.js';
import {
    authoritativeIdentifier,
    blankDraft,
    hasIdentity,
    reduceCounterparty,
    type CounterpartyAction,
    type CounterpartyDraft,
    type NewCounterparty,
} from './counterpartyState.js';
import type {
    CounterpartyPickLists,
    CounterpartyServer,
    CounterpartyWriteOutcome,
} from './counterpartyServer.js';
import type { Counterparty } from '@ores/wire-protocol/generated/refdata/domain/counterparty';
import type { CounterpartyChildren } from './counterpartyState.js';

const t = createTranslator('en', enFlat, enFlat).t;

const PICK_LISTS: CounterpartyPickLists = {
    partyTypes: [],
    partyStatuses: [],
    businessCentres: [],
    identifierSchemes: [],
    contactTypes: [],
    currencies: [],
};

function counterpartyRow(): Counterparty {
    return {
        version: 1,
        tenant_id: 'tenant',
        id: '11111111-1111-1111-1111-111111111111',
        short_code: 'NWCAP',
        full_name: 'Northwind Capital Ltd',
        transliterated_name: null,
        party_type: 'Bank',
        parent_counterparty_id: null,
        status: 'Active',
        image_id: null,
        modified_by: 'a.tanaka',
        performed_by: 'a.tanaka',
        change_reason_code: 'system.new_record',
        change_commentary: '',
        recorded_at: '2026-10-06 09:14:00Z',
    };
}

function success(counterparty: Counterparty | undefined): CounterpartyWriteOutcome {
    return { success: true, code: '', message: '', fields: [], counterparty };
}

function refusal(message: string): CounterpartyWriteOutcome {
    return {
        success: false,
        code: 'conflict',
        message,
        fields: [{ field: 'short_code', code: 'taken', message }],
        counterparty: undefined,
    };
}

function fakeServer(overrides: Partial<CounterpartyServer> = {}): CounterpartyServer {
    return {
        listCounterparties: vi.fn(async () => ({ rows: [], total: 0 })),
        childrenOf: vi.fn(async () => [] as readonly CounterpartyChildren[]),
        visibilityOf: vi.fn(async () => []),
        pickLists: vi.fn(async () => PICK_LISTS),
        writeComposite: vi.fn(async () => success(counterpartyRow())),
        writeBusinessCentres: vi.fn(async () => success(undefined)),
        writeVisibility: vi.fn(async () => success(undefined)),
        ...overrides,
    };
}

function state(actions: readonly CounterpartyAction[] = []): CounterpartyDraft {
    return actions.reduce(reduceCounterparty, blankDraft());
}

/** A draft as the page holds it, with the screens' actions stubbed out. */
function asNewCounterparty(draft: CounterpartyDraft): NewCounterparty {
    const never = vi.fn();
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

function steps(
    draft: CounterpartyDraft,
    options: { readonly server?: CounterpartyServer; readonly state?: NewCounterparty } = {},
): ReturnType<typeof counterpartySteps> {
    return counterpartySteps({
        t,
        server: options.server ?? fakeServer(),
        state: options.state ?? asNewCounterparty(draft),
        partyId: 'p-ores',
        pickLists: PICK_LISTS,
        pickFailure: undefined,
        parents: [],
        onMove: () => undefined,
        onFinished: () => undefined,
        onAnother: () => undefined,
    });
}

function identityActions(): readonly CounterpartyAction[] {
    return [
        { kind: 'set-identity', field: 'shortCode', value: 'NWCAP' },
        { kind: 'set-identity', field: 'fullName', value: 'Northwind Capital Ltd' },
        { kind: 'add-identifier', scheme: 'LEI', value: '549300NWCAPITAL00001' },
    ];
}

describe('the counterparty journey a person runs', () => {
    it('is the eight steps on one rail, landing first', () => {
        expect(steps(state()).map((step) => step.id)).toEqual([...COUNTERPARTY_STEP_IDS]);
    });

    it('names each step where the refusal can jump back to it', () => {
        expect(counterpartyStepIndex('identity')).toBe(1);
        expect(counterpartyStepIndex('history')).toBe(7);
    });

    it('cannot move past the identity until it has a code and a name', () => {
        expect(steps(state())[1]!.next?.enabled).toBe(false);

        const named = state([
            { kind: 'set-identity', field: 'shortCode', value: 'NWCAP' },
            { kind: 'set-identity', field: 'fullName', value: 'Northwind Capital Ltd' },
        ]);

        expect(steps(named)[1]!.next?.enabled).toBe(true);
    });

    it('cannot move past the identifiers until one is authoritative and valued', () => {
        const without = state([
            { kind: 'set-identity', field: 'shortCode', value: 'NWCAP' },
            { kind: 'set-identity', field: 'fullName', value: 'Northwind Capital Ltd' },
        ]);
        expect(steps(without)[2]!.next?.enabled).toBe(false);

        const withIdentifier = state(identityActions());
        expect(steps(withIdentifier)[2]!.next?.enabled).toBe(true);
    });

    it('cannot move past the contacts with two rows of one type', () => {
        const draft = state([
            ...identityActions(),
            { kind: 'add-contact', contactType: 'Legal' },
            { kind: 'add-contact', contactType: 'Legal' },
        ]);

        expect(steps(draft)[3]!.next?.enabled).toBe(false);
    });

    it('writes the whole graph as one act, then the centres and the visibility', async () => {
        const server = fakeServer();
        const draft = state([...identityActions(), { kind: 'add-contact', contactType: 'Legal' }]);

        await steps(draft, { server })[5]!.next!.run!();

        expect(server.writeComposite).toHaveBeenCalledWith(
            expect.objectContaining({
                counterparty: expect.objectContaining({ short_code: 'NWCAP' }),
                identifiers: [expect.objectContaining({ id_value: '549300NWCAPITAL00001' })],
                contacts: [expect.objectContaining({ contact_type: 'Legal' })],
            }),
        );
        expect(server.writeBusinessCentres).toHaveBeenCalledWith(
            '11111111-1111-1111-1111-111111111111',
            [],
            expect.objectContaining({ reason_code: 'system.new_record' }),
        );
        expect(server.writeVisibility).toHaveBeenCalledWith(
            'p-ores',
            '11111111-1111-1111-1111-111111111111',
            expect.objectContaining({ reason_code: 'system.new_record' }),
        );
    });

    it('records the refusal and refuses to move on', async () => {
        const recordRefusal = vi.fn();
        const server = fakeServer({
            writeComposite: vi.fn(async () => refusal('That short code is taken.')),
        });
        const draft = state(identityActions());
        const rendered = steps(draft, {
            server,
            state: { ...asNewCounterparty(draft), recordRefusal },
        });

        await expect(rendered[5]!.next!.run!()).rejects.toThrow('That short code is taken.');

        expect(recordRefusal).toHaveBeenCalledWith(
            expect.objectContaining({
                step: 'review',
                subject: 'refdata.v1.ops.put_counterparty_composite',
                code: 'conflict',
            }),
        );
    });

    it('writes nothing further when the composite is refused', async () => {
        const server = fakeServer({
            writeComposite: vi.fn(async () => refusal('That short code is taken.')),
        });

        await expect(steps(state(identityActions()), { server })[5]!.next!.run!()).rejects.toThrow(
            'That short code is taken.',
        );

        expect(server.writeBusinessCentres).not.toHaveBeenCalled();
        expect(server.writeVisibility).not.toHaveBeenCalled();
    });

    it('walks the review with the identifiers and the act it sends', () => {
        const draft = state(identityActions());
        const html = renderToStaticMarkup(
            <TranslationProvider>{steps(draft)[5]!.body}</TranslationProvider>,
        );

        expect(html).toContain('549300NWCAPITAL00001');
        expect(html).toContain('NWCAP');
        expect(html).toContain('refdata.v1.ops.put_counterparty_composite');
        expect(steps(draft)[5]!.next?.label).toBe('Confirm and write');
        expect(steps(draft)[5]!.next?.enabled).toBe(true);
    });

    it('walks the refusal it recorded with the step that raised it', () => {
        const draft = state(identityActions());
        const refused = reduceCounterparty(draft, {
            kind: 'record-refusal',
            refusal: {
                step: 'review',
                subject: 'refdata.v1.ops.put_counterparty_composite',
                code: 'conflict',
                message: 'That short code is taken.',
                fields: [{ field: 'short_code', code: 'taken', message: 'taken' }],
            },
        });
        const html = renderToStaticMarkup(
            <TranslationProvider>{steps(refused)[5]!.body}</TranslationProvider>,
        );

        expect(html).toContain('The write was refused');
        expect(html).toContain('That short code is taken.');
        expect(html).toContain('Back to Review');
    });
});
