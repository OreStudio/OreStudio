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
import { createTranslator } from '../i18n/translate.js';
import { enFlat } from '../i18n/locales/en.js';
import { PARTY_STEP_IDS, partyStepIndex, partySteps } from './partyDetailsSteps.js';
import {
    blankDraft,
    changesOf,
    reduceParty,
    type PartyAction,
    type PartyDetails,
    type PartyDraft,
} from './partyDetailsState.js';
import type { PartyDetailsServer, PartySets, PartyWriteOutcome } from './partyDetailsServer.js';
import type { BusinessUnit } from '@ores/wire-protocol/generated/refdata/domain/business_unit';
import type { Party } from '@ores/wire-protocol/generated/refdata/domain/party';
import type { PartyIdentifier } from '@ores/wire-protocol/generated/refdata/domain/party_identifier';

const t = createTranslator('en', enFlat, enFlat).t;

const audit = {
    tenant_id: 't',
    modified_by: 'm',
    performed_by: 'p',
    change_reason_code: 'r',
    change_commentary: '',
    recorded_at: '2026-01-01T00:00:00Z',
};

const party: Party = {
    ...audit,
    version: 3,
    id: 'party-1',
    short_code: 'ACME',
    full_name: 'Acme Ltd',
    codename: 'swift-otter',
    transliterated_name: null,
    party_category: 'Operational',
    party_type: 'Corporate',
    parent_party_id: null,
    business_center_code: 'GBLO',
    status: 'Active',
    image_id: null,
    is_registration_default: false,
};

const identifier: PartyIdentifier = {
    ...audit,
    version: 2,
    id: 'id-1',
    party_id: 'party-1',
    id_scheme: 'LEI',
    id_value: 'LEI-OLD',
    description: '',
};

const unit: BusinessUnit = {
    ...audit,
    version: 5,
    id: 'desk',
    party_id: 'party-1',
    unit_name: 'Desk',
    parent_business_unit_id: null,
    unit_code: 'DESK',
    business_centre_code: 'GBLO',
    unit_type_id: null,
    status: 'Active',
};

const sets: PartySets = {
    identifiers: [identifier],
    contacts: [],
    countries: [{ ...audit, version: 1, party_id: 'party-1', country_alpha2_code: 'GB' }],
    currencies: [],
    counterparties: [],
    units: [unit],
};

function success(written?: Party): PartyWriteOutcome {
    return { success: true, code: '', message: '', fields: [], party: written };
}

function refused(message: string): PartyWriteOutcome {
    return {
        success: false,
        code: 'level_violation',
        message,
        fields: [{ field: 'unit_type_id', code: 'level', message }],
        party: undefined,
    };
}

/** A server that logs every write in the order it was made. */
function fakeServer(
    log: string[],
    overrides: Partial<PartyDetailsServer> = {},
): PartyDetailsServer {
    return {
        listParties: vi.fn(async () => ({ rows: [], total: 0 })),
        setsOf: vi.fn(async () => sets),
        pickLists: vi.fn(async () => {
            throw new Error('not read here');
        }),
        compositeAsOf: vi.fn(async () => ({ party, identifiers: [], contacts: [] })),
        amendReasons: vi.fn(async () => []),
        writeComposite: vi.fn(async () => {
            log.push('composite');
            return success({ ...party, version: 4 });
        }),
        retireIdentifier: vi.fn(async (_party, value) => {
            log.push(`retire ${value}`);
            return success();
        }),
        linkMembership: vi.fn(async (_party, junction, far) => {
            log.push(`link ${junction} ${far}`);
            return success();
        }),
        closeMembership: vi.fn(async (_party, junction, far) => {
            log.push(`close ${junction} ${far}`);
            return success();
        }),
        reclassifyUnit: vi.fn(async (row, type) => {
            log.push(`unit ${row.unit_code} ${String(type)}`);
            return success();
        }),
        ...overrides,
    };
}

function asDetails(draft: PartyDraft): PartyDetails {
    const never = vi.fn();
    return {
        ...draft,
        changes: changesOf(draft),
        open: never,
        setField: never,
        setDefault: never,
        addIdentifier: never,
        setIdentifier: never,
        setIdentifierDescription: never,
        retireIdentifier: never,
        addContact: never,
        setContact: never,
        markPrimary: never,
        toggleMembership: never,
        setUnitType: never,
        revertFields: never,
        setReason: never,
        setCommentary: never,
        recordWritten: vi.fn(),
        recordRefusal: vi.fn(),
        clearRefusal: vi.fn(),
    };
}

function opened(...actions: readonly PartyAction[]): PartyDraft {
    return actions.reduce(reduceParty, reduceParty(blankDraft(), { kind: 'open', party, sets }));
}

function stepsFor(state: PartyDetails, server: PartyDetailsServer) {
    return partySteps({
        t,
        server,
        state,
        pickLists: undefined,
        pickFailure: undefined,
        reasons: [],
        onMove: vi.fn(),
        onFinished: vi.fn(),
    });
}

function reviewOf(steps: ReturnType<typeof stepsFor>) {
    const review = steps[partyStepIndex('review')];
    if (review === undefined) {
        throw new Error('no review step');
    }
    return review;
}

describe('the party details steps', () => {
    it('lists the steps in the order the rail draws them', () => {
        const steps = stepsFor(asDetails(blankDraft()), fakeServer([]));
        expect(steps.map((step) => step.id)).toEqual([...PARTY_STEP_IDS]);
    });

    it('holds the confirm until something has changed', () => {
        const none = reviewOf(stepsFor(asDetails(opened()), fakeServer([])));
        expect(none.next?.enabled).toBe(false);
        const some = reviewOf(
            stepsFor(
                asDetails(opened({ kind: 'set-field', field: 'fullName', value: 'Acme plc' })),
                fakeServer([]),
            ),
        );
        expect(some.next?.enabled).toBe(true);
    });

    it('writes the composite first, then the retires, the links and the units', async () => {
        const log: string[] = [];
        const state = asDetails(
            opened(
                { kind: 'set-identifier', key: 'id-1', value: 'LEI-NEW' },
                { kind: 'toggle-membership', set: 'countries', code: 'GB' },
                { kind: 'toggle-membership', set: 'currencies', code: 'USD' },
                { kind: 'set-unit-type', unitId: 'desk', unitTypeId: 'type-1' },
            ),
        );
        await reviewOf(stepsFor(state, fakeServer(log))).next?.run?.();
        expect(log).toEqual([
            'composite',
            'retire LEI-OLD',
            'close countries GB',
            'link currencies USD',
            'unit DESK type-1',
        ]);
        expect(state.recordWritten).toHaveBeenCalledWith({ ...party, version: 4 });
    });

    it('stops at a refused composite and writes nothing else', async () => {
        const log: string[] = [];
        const state = asDetails(
            opened(
                { kind: 'set-field', field: 'fullName', value: 'Acme plc' },
                { kind: 'toggle-membership', set: 'countries', code: 'GB' },
            ),
        );
        const server = fakeServer(log, {
            writeComposite: vi.fn(async () => refused('Stale version.')),
        });
        await expect(reviewOf(stepsFor(state, server)).next?.run?.()).rejects.toThrow(
            /Stale version/,
        );
        expect(log).toEqual([]);
        expect(state.recordRefusal).toHaveBeenCalledWith(
            expect.objectContaining({
                step: 'overview',
                subject: 'refdata.v1.ops.put_party_composite',
            }),
        );
        expect(state.recordWritten).not.toHaveBeenCalled();
    });

    it('names the unit step when the server refuses a unit', async () => {
        const state = asDetails(
            opened({ kind: 'set-unit-type', unitId: 'desk', unitTypeId: 'type-1' }),
        );
        const server = fakeServer([], {
            reclassifyUnit: vi.fn(async () => refused('Level broken.')),
        });
        await expect(reviewOf(stepsFor(state, server)).next?.run?.()).rejects.toThrow();
        expect(state.recordRefusal).toHaveBeenCalledWith(
            expect.objectContaining({ step: 'structure', code: 'level_violation' }),
        );
        expect(state.recordWritten).not.toHaveBeenCalled();
    });
});
