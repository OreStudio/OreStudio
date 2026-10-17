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
import { TENOR_STEP_IDS, tenorStepIndex, tenorSteps } from './tenorsSteps.js';
import {
    blankDraft,
    changesOf,
    reduceTenor,
    type TenorAction,
    type TenorDetails,
    type TenorDraft,
} from './tenorsState.js';
import type {
    TenorCatalogue,
    TenorPickLists,
    TenorResolutionPreview,
    TenorWriteOutcome,
    TenorsServer,
} from './tenorsServer.js';
import type { LabelCatalogue } from '../refdata/shared.js';
import type { BadgePresentation } from '@ores/wire-protocol/browser';
import type { Tenor } from '@ores/wire-protocol/generated/refdata/domain/tenor';
import type { TenorConvention } from '@ores/wire-protocol/generated/refdata/domain/tenor_convention';

const t = createTranslator('en', enFlat, enFlat).t;

const STAMP = {
    tenant_id: 'tenant-1',
    modified_by: 'marco',
    performed_by: 'marco',
    change_reason_code: 'common.non_material_update',
    change_commentary: '',
    recorded_at: '2026-10-15T00:00:00Z',
} as const;

const overnight: Tenor = {
    ...STAMP,
    version: 3,
    code: 'O/N',
    display_name: 'Overnight',
    description: 'Overnight tenor',
    sort_order: 1,
    kind: 'period',
    unit: 'day',
    multiplier: 1,
};

const convention: TenorConvention = {
    ...STAMP,
    version: 7,
    code: 'TARGET_STANDARD',
    description: 'Target standard',
    measured_from: 'spot',
    resolution_algorithm: 'calendar_days',
    business_day_convention_type: 'ModifiedFollowing',
    oresmd_uri: null,
};

const catalogue: TenorCatalogue = { tenors: [overnight], conventions: [convention], schedules: [] };

function ok<Row>(row: Row | undefined): TenorWriteOutcome<Row> {
    return { success: true, code: '', message: '', fields: [], row };
}

function refused<Row>(): TenorWriteOutcome<Row> {
    return {
        success: false,
        code: 'stale_version',
        message: 'Stale version.',
        fields: [{ field: 'code', code: 'stale', message: 'Stale version.' }],
        row: undefined,
    };
}

function fakeServer(overrides: Partial<TenorsServer> = {}): TenorsServer {
    return {
        catalogue: vi.fn(async () => catalogue),
        pickLists: vi.fn(async () => {
            throw new Error('not read here');
        }),
        resolutions: vi.fn(async () => []),
        previewResolution: vi.fn(async () => {
            throw new Error('not read here');
        }),
        amendReasons: vi.fn(async () => []),
        writeTenor: vi.fn(async () => ok({ ...overnight, version: 4 })),
        writeConvention: vi.fn(async () => ok({ ...convention, version: 8 })),
        writeMembership: vi.fn(async () => ok(null)),
        removeMembership: vi.fn(async () => ok(null)),
        ...overrides,
    };
}

/** A draft presented as the state the steps read. */
function asDetails(draft: TenorDraft): TenorDetails {
    const never = vi.fn();
    return {
        ...draft,
        changes: changesOf(draft),
        openTenor: never,
        startTenor: never,
        setTenorField: never,
        openConvention: never,
        startConvention: never,
        setConventionField: never,
        toggleMember: never,
        setAnchor: never,
        setTestHorizon: never,
        setTestSpot: never,
        setTestCalendar: never,
        recordTestPreview: never,
        recordTestFailure: never,
        setReason: never,
        setCommentary: never,
        recordWritten: vi.fn(),
        recordRefusal: vi.fn(),
        clearRefusal: vi.fn(),
    };
}

function draftOf(...actions: readonly TenorAction[]): TenorDraft {
    return actions.reduce(reduceTenor, blankDraft());
}

function stepsFor(
    state: TenorDetails,
    server: TenorsServer,
    labels: LabelCatalogue = { byCode: new Map(), mapped: {} },
    pickLists?: TenorPickLists,
) {
    return tenorSteps({
        t,
        server,
        state,
        labels,
        catalogue,
        catalogueFailure: undefined,
        pickLists,
        pickFailure: undefined,
        reasons: [],
        onMove: vi.fn(),
        onFinished: vi.fn(),
    });
}

function reviewOf(steps: ReturnType<typeof stepsFor>) {
    const review = steps[tenorStepIndex('review')];
    if (review === undefined) {
        throw new Error('no review step');
    }
    return review;
}

function testStepOf(steps: ReturnType<typeof stepsFor>) {
    const step = steps[tenorStepIndex('test')];
    if (step === undefined) {
        throw new Error('no test step');
    }
    return step;
}

describe('the tenor steps', () => {
    it('draws the rail the spec names: the list, the tenor, its convention, the schedule, the test, the review, the outcome and the history', () => {
        const steps = stepsFor(asDetails(blankDraft()), fakeServer());
        expect(steps.map((step) => step.id)).toEqual([
            'choose-tenor',
            'tenor',
            'choose-convention',
            'convention',
            'schedule',
            'test',
            'review',
            'outcome',
            'history',
        ]);
        expect(steps.map((step) => step.id)).toEqual([...TENOR_STEP_IDS]);
    });

    it('draws a tenor kind and unit through the badge catalogue, not as bare codes', () => {
        const badge = (code: string, label: string): BadgePresentation => ({
            code,
            label,
            description: label,
            backgroundColour: '#123456',
            textColour: '#ffffff',
            severity: 'info',
        });
        // The row carries 'period'; the domain's mapping points at the badge
        // code, and the catalogue paints that code.
        const labels: LabelCatalogue = {
            byCode: new Map([
                ['tenor_kind_period', badge('tenor_kind_period', 'Period')],
                ['tenor_unit_day', badge('tenor_unit_day', 'Day')],
            ]),
            mapped: {
                tenor_kind: { period: 'tenor_kind_period' },
                tenor_unit: { day: 'tenor_unit_day' },
            },
        };
        const steps = stepsFor(asDetails(blankDraft()), fakeServer(), labels);
        const choose = steps[tenorStepIndex('choose-tenor')];
        const html = renderToStaticMarkup(<>{choose?.body}</>);
        expect(html).toContain('Period');
        expect(html).toContain('Day');
        expect(html).not.toContain('>period<');
        expect(html).not.toContain('>day<');
    });

    it('shows the code rather than an unmapped badge when the catalogue has not painted it', () => {
        // A tenor kind the catalogue does not carry must still read as its code:
        // the Unmapped badge belongs to a row that is expected to be labelled.
        const steps = stepsFor(asDetails(blankDraft()), fakeServer(), {
            byCode: new Map(),
            mapped: {},
        });
        const choose = steps[tenorStepIndex('choose-tenor')];
        const html = renderToStaticMarkup(<>{choose?.body}</>);
        expect(html).toContain('period');
        expect(html).not.toContain('Unmapped');
    });

    it('holds the confirm until something has changed', () => {
        const opened = asDetails(draftOf({ kind: 'open-tenor', tenor: overnight }));
        expect(reviewOf(stepsFor(opened, fakeServer())).next?.enabled).toBe(false);
        const changed = asDetails(
            draftOf(
                { kind: 'open-tenor', tenor: overnight },
                { kind: 'set-tenor-field', field: 'displayName', value: 'O/N spot' },
            ),
        );
        expect(reviewOf(stepsFor(changed, fakeServer())).next?.enabled).toBe(true);
    });

    it('holds the confirm while a required tenor field is empty', () => {
        const missing = asDetails(
            draftOf(
                { kind: 'start-tenor' },
                { kind: 'set-tenor-field', field: 'code', value: '1M' },
            ),
        );
        expect(reviewOf(stepsFor(missing, fakeServer())).next?.enabled).toBe(false);
    });

    it('writes the tenor, the version read and the reason, then records the write', async () => {
        const writeTenor = vi.fn(async () => ok({ ...overnight, version: 4 }));
        const state = asDetails(
            draftOf(
                { kind: 'open-tenor', tenor: overnight },
                { kind: 'set-tenor-field', field: 'displayName', value: 'O/N spot' },
            ),
        );
        await reviewOf(stepsFor(state, fakeServer({ writeTenor }))).next?.run?.();
        expect(writeTenor).toHaveBeenCalledWith(
            expect.objectContaining({ code: 'O/N', display_name: 'O/N spot' }),
            3,
            expect.objectContaining({ reason_code: 'common.non_material_update' }),
        );
        expect(state.recordWritten).toHaveBeenCalled();
        expect(state.recordRefusal).not.toHaveBeenCalled();
    });

    it('writes a tenor and its convention in one confirm, the tenor first', async () => {
        const order: string[] = [];
        const state = asDetails(
            draftOf(
                { kind: 'open-tenor', tenor: overnight },
                { kind: 'set-tenor-field', field: 'displayName', value: 'O/N spot' },
                { kind: 'open-convention', convention, resolutions: [], tenorCodes: ['O/N'] },
                { kind: 'set-convention-field', field: 'description', value: 'Target' },
            ),
        );
        const server = fakeServer({
            writeTenor: vi.fn(async () => {
                order.push('tenor');
                return ok({ ...overnight, version: 4 });
            }),
            writeConvention: vi.fn(async () => {
                order.push('convention');
                return ok({ ...convention, version: 8 });
            }),
        });
        await reviewOf(stepsFor(state, server)).next?.run?.();
        expect(order).toEqual(['tenor', 'convention']);
        expect(state.recordWritten).toHaveBeenCalled();
    });

    it('links an added tenor and unlinks a removed one against the convention code', async () => {
        const writeMembership = vi.fn(async () => ok(null));
        const removeMembership = vi.fn(async () => ok(null));
        const state = asDetails(
            draftOf(
                {
                    kind: 'open-convention',
                    convention,
                    resolutions: [],
                    tenorCodes: ['3M'],
                },
                { kind: 'toggle-member', tenorCode: '3M' },
            ),
        );
        await reviewOf(
            stepsFor(state, fakeServer({ writeMembership, removeMembership })),
        ).next?.run?.();
        expect(writeMembership).toHaveBeenCalledWith(
            'TARGET_STANDARD',
            '3M',
            null,
            null,
            expect.objectContaining({ reason_code: 'common.non_material_update' }),
        );
        expect(removeMembership).not.toHaveBeenCalled();
        expect(state.recordWritten).toHaveBeenCalled();
    });

    it('records a refusal against the step that failed and keeps the draft', async () => {
        const state = asDetails(
            draftOf(
                { kind: 'open-tenor', tenor: overnight },
                { kind: 'set-tenor-field', field: 'displayName', value: 'O/N spot' },
            ),
        );
        const server = fakeServer({
            writeTenor: vi.fn(async () => refused<Tenor>()),
        });
        await expect(reviewOf(stepsFor(state, server)).next?.run?.()).rejects.toThrow(
            /Stale version/,
        );
        expect(state.recordRefusal).toHaveBeenCalledWith(
            expect.objectContaining({ step: 'tenor', code: 'stale_version' }),
        );
        expect(state.recordWritten).not.toHaveBeenCalled();
    });

    it('names the calls already written when a later one is refused', async () => {
        const state = asDetails(
            draftOf(
                { kind: 'open-tenor', tenor: overnight },
                { kind: 'set-tenor-field', field: 'displayName', value: 'O/N spot' },
                { kind: 'open-convention', convention, resolutions: [], tenorCodes: ['O/N'] },
                { kind: 'set-convention-field', field: 'description', value: 'Target' },
            ),
        );
        const server = fakeServer({
            writeConvention: vi.fn(async () => refused<TenorConvention>()),
        });
        await expect(reviewOf(stepsFor(state, server)).next?.run?.()).rejects.toThrow();
        expect(state.recordRefusal).toHaveBeenCalledWith(
            expect.objectContaining({
                step: 'convention',
                written: ['refdata.v1.tenors.put'],
            }),
        );
    });

    it('shows the resolved dates when the server answers ok', () => {
        const preview: TenorResolutionPreview = {
            result: { outcome: 'ok', code: 'ok', message: '', fields: [] },
            anchorDate: '2026-01-31',
            dateBeforeRoll: '2026-02-28',
            adjustedDate: '2026-02-27',
            rollRule: 'ModifiedFollowing',
            calendar: 'TARGET',
        };
        const state = asDetails(
            draftOf(
                { kind: 'open-tenor', tenor: overnight },
                { kind: 'open-convention', convention, resolutions: [], tenorCodes: ['O/N'] },
                { kind: 'set-test-horizon', value: '2026-01-15' },
                { kind: 'set-test-calendar', value: 'TARGET' },
                { kind: 'record-test-preview', preview },
            ),
        );
        const html = renderToStaticMarkup(
            <TranslationProvider>
                {testStepOf(stepsFor(state, fakeServer())).body}
            </TranslationProvider>,
        );

        expect(html).toContain('2026-01-31');
        expect(html).toContain('2026-02-28');
        expect(html).toContain('2026-02-27');
        expect(html).toContain('ModifiedFollowing');
        expect(html).toContain('TARGET');
        expect(html).toContain('The rule resolves to this date.');
    });

    it('shows a refusal beside the dates it reached, without losing the draft', () => {
        const preview: TenorResolutionPreview = {
            result: {
                outcome: 'invalid',
                code: 'missing_spot_date',
                message: 'The rule did not resolve.',
                fields: [{ field: 'spot', code: 'missing_spot_date', message: 'No spot date.' }],
            },
            anchorDate: '2026-01-31',
            dateBeforeRoll: '',
            adjustedDate: '',
            rollRule: 'ModifiedFollowing',
            calendar: 'TARGET',
        };
        const state = asDetails(
            draftOf(
                { kind: 'open-tenor', tenor: overnight },
                { kind: 'set-tenor-field', field: 'displayName', value: 'O/N spot' },
                { kind: 'open-convention', convention, resolutions: [], tenorCodes: ['O/N'] },
                { kind: 'set-test-horizon', value: '2026-01-15' },
                { kind: 'set-test-calendar', value: 'TARGET' },
                { kind: 'record-test-preview', preview },
            ),
        );
        const html = renderToStaticMarkup(
            <TranslationProvider>
                {testStepOf(stepsFor(state, fakeServer())).body}
            </TranslationProvider>,
        );

        expect(html).toContain('The server declined the rule.');
        expect(html).toContain('missing_spot_date');
        expect(html).toContain('The rule did not resolve.');
        expect(html).toContain('spot');
        expect(html).toContain('2026-01-31');
        // The form and the answer are still in the draft.
        expect(html).toContain('value="2026-01-15"');
        expect(state.tenor.displayName).toBe('O/N spot');
        expect(state.testHorizon).toBe('2026-01-15');
        expect(state.testPreview).toBe(preview);
    });
});
