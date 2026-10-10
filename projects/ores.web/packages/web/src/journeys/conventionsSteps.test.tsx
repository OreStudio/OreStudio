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
import { CONVENTION_STEP_IDS, conventionStepIndex, conventionSteps } from './conventionsSteps.js';
import {
    blankDraft,
    changesOf,
    reduceConvention,
    type ConventionAction,
    type ConventionDraft,
    type ConventionTerms,
} from './conventionsState.js';
import type {
    ConventionRow,
    ConventionWriteOutcome,
    ConventionsServer,
} from './conventionsServer.js';

const t = createTranslator('en', enFlat, enFlat).t;

const swap: ConventionRow = {
    version: 3,
    id: 'swap-1',
    party_id: 'party-1',
    fixed_calendar: null,
    fixed_frequency: 'Annual',
    fixed_convention: null,
    fixed_day_count_fraction: '30/360',
    index: 'EUR-EURIBOR-6M',
    float_frequency: null,
    sub_periods_coupon_type: null,
    oresmd_uri: null,
};

function outcome(success: boolean, message = ''): ConventionWriteOutcome {
    return {
        success,
        code: success ? '' : 'stale_version',
        message,
        fields: success ? [] : [{ field: 'index', code: 'stale', message }],
        row: success ? { ...swap, version: 4 } : undefined,
    };
}

function fakeServer(overrides: Partial<ConventionsServer> = {}): ConventionsServer {
    return {
        families: vi.fn(async () => []),
        rowsOf: vi.fn(async () => ({ rows: [], total: 0 })),
        pickLists: vi.fn(async () => {
            throw new Error('not read here');
        }),
        amendReasons: vi.fn(async () => []),
        write: vi.fn(async () => outcome(true)),
        ...overrides,
    };
}

function asTerms(draft: ConventionDraft): ConventionTerms {
    const never = vi.fn();
    return {
        ...draft,
        changes: changesOf(draft),
        chooseFamily: never,
        startConvention: never,
        openConvention: never,
        setTerm: never,
        setReason: never,
        setCommentary: never,
        recordWritten: vi.fn(),
        recordRefusal: vi.fn(),
        clearRefusal: vi.fn(),
    };
}

function opened(...actions: readonly ConventionAction[]): ConventionDraft {
    return [
        { kind: 'choose-family', family: 'swap' } as const,
        { kind: 'open-convention', row: swap } as const,
        ...actions,
    ].reduce(reduceConvention, blankDraft());
}

function stepsFor(state: ConventionTerms, server: ConventionsServer) {
    return conventionSteps({
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
    const review = steps[conventionStepIndex('review')];
    if (review === undefined) {
        throw new Error('no review step');
    }
    return review;
}

describe('the convention steps', () => {
    it('lists the steps in the order the rail draws them', () => {
        const steps = stepsFor(asTerms(blankDraft()), fakeServer());
        expect(steps.map((step) => step.id)).toEqual([...CONVENTION_STEP_IDS]);
    });

    it('holds the confirm until a term has changed', () => {
        expect(reviewOf(stepsFor(asTerms(opened()), fakeServer())).next?.enabled).toBe(false);
        const changed = opened({ kind: 'set-term', column: 'index', value: 'EUR-EURIBOR-3M' });
        expect(reviewOf(stepsFor(asTerms(changed), fakeServer())).next?.enabled).toBe(true);
    });

    it('holds the confirm while a required term is empty', () => {
        const emptied = opened({ kind: 'set-term', column: 'index', value: '' });
        expect(reviewOf(stepsFor(asTerms(emptied), fakeServer())).next?.enabled).toBe(false);
    });

    it('writes the family, the terms and the version read, then records the row', async () => {
        const write = vi.fn(async () => outcome(true));
        const state = asTerms(
            opened({ kind: 'set-term', column: 'index', value: 'EUR-EURIBOR-3M' }),
        );
        await reviewOf(stepsFor(state, fakeServer({ write }))).next?.run?.();
        expect(write).toHaveBeenCalledWith(
            'swap',
            expect.objectContaining({ id: 'swap-1', index: 'EUR-EURIBOR-3M' }),
            3,
            expect.objectContaining({ reason_code: 'common.non_material_update' }),
        );
        expect(state.recordWritten).toHaveBeenCalledWith({ ...swap, version: 4 });
    });

    it('records the refusal against the terms step and keeps the draft', async () => {
        const state = asTerms(
            opened({ kind: 'set-term', column: 'index', value: 'EUR-EURIBOR-3M' }),
        );
        const server = fakeServer({ write: vi.fn(async () => outcome(false, 'Stale version.')) });
        await expect(reviewOf(stepsFor(state, server)).next?.run?.()).rejects.toThrow(
            /Stale version/,
        );
        expect(state.recordRefusal).toHaveBeenCalledWith(
            expect.objectContaining({ step: 'terms', code: 'stale_version' }),
        );
        expect(state.recordWritten).not.toHaveBeenCalled();
    });
});
