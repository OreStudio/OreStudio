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

import { describe, expect, it } from 'vitest';
import type { Tenor } from '@ores/wire-protocol/generated/refdata/domain/tenor';
import type { TenorConvention } from '@ores/wire-protocol/generated/refdata/domain/tenor_convention';
import type { TenorConventionResolution } from '@ores/wire-protocol/generated/refdata/domain/tenor_convention_resolution';
import {
    blankDraft,
    changeText,
    changesOf,
    reduceTenor,
    resolutionDraftOf,
    tenorProblems,
    writePlan,
    type TenorAction,
    type TenorDraft,
} from './tenorsState.js';
import type { TenorResolution } from './tenorsServer.js';

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

const resolution: TenorConventionResolution = {
    ...STAMP,
    version: 2,
    convention_code: 'TARGET_STANDARD',
    tenor_code: 'O/N',
    anchor_override: null,
    offset_unit: null,
    offset_multiplier: null,
    schedule_code: null,
    schedule_step_count: null,
};

function walk(...actions: readonly TenorAction[]): TenorDraft {
    return actions.reduce(reduceTenor, blankDraft());
}

describe('the tenor draft', () => {
    it('holds nothing and offers the correction reason until something is opened', () => {
        const draft = blankDraft();
        expect(draft.tenorActive).toBe(false);
        expect(draft.conventionActive).toBe(false);
        expect(draft.members).toEqual([]);
        expect(draft.reasonCode).toBe('common.non_material_update');
        expect(writePlan(draft)).toBeUndefined();
    });

    it('reads an opened tenor into the form it is edited as', () => {
        const draft = walk({ kind: 'open-tenor', tenor: overnight });
        expect(draft.tenorActive).toBe(true);
        expect(draft.tenor).toEqual({
            code: 'O/N',
            displayName: 'Overnight',
            description: 'Overnight tenor',
            sortOrder: '1',
            kind: 'period',
            unit: 'day',
            multiplier: '1',
        });
    });

    it('offers the new-record reason when a tenor is started rather than opened', () => {
        const draft = walk({ kind: 'start-tenor' });
        expect(draft.tenorActive).toBe(true);
        expect(draft.tenorOpened).toBeUndefined();
        expect(draft.reasonCode).toBe('system.new_record');
    });

    it('asks a new tenor for the fields the server requires', () => {
        expect(tenorProblems(walk({ kind: 'start-tenor' })).map((p) => p.what)).toEqual([
            'journey.tenors.problems.nameTenor',
            'journey.tenors.problems.chooseKindAndUnit',
        ]);
        expect(tenorProblems(walk({ kind: 'open-tenor', tenor: overnight }))).toEqual([]);
    });

    it('refuses a sort order or multiplier that is not a whole number', () => {
        const draft = walk(
            { kind: 'open-tenor', tenor: overnight },
            { kind: 'set-tenor-field', field: 'sortOrder', value: '2.5' },
            { kind: 'set-tenor-field', field: 'multiplier', value: 'x' },
        );
        expect(tenorProblems(draft).map((p) => p.what)).toEqual([
            'journey.tenors.problems.sortOrderWhole',
            'journey.tenors.problems.multiplierWhole',
        ]);
    });

    it('has no change until a field differs from what was read', () => {
        expect(changesOf(walk({ kind: 'open-tenor', tenor: overnight }))).toEqual([]);
    });

    it('lists one change per field that differs, with its before and after', () => {
        const changes = changesOf(
            walk(
                { kind: 'open-tenor', tenor: overnight },
                { kind: 'set-tenor-field', field: 'displayName', value: 'O/N spot' },
            ),
        );
        expect(changes.map(changeText)).toEqual([
            {
                whatKey: 'journey.tenors.review.what.tenor',
                whatParams: { part: 'journey.tenors.tenor.fields.displayName' },
                before: { value: 'Overnight' },
                after: { value: 'O/N spot' },
            },
        ]);
    });

    it('names a new tenor as a change of its own', () => {
        const draft = walk(
            { kind: 'start-tenor' },
            { kind: 'set-tenor-field', field: 'code', value: '1M' },
        );
        expect(changesOf(draft).map(changeText)).toContainEqual(
            expect.objectContaining({
                whatKey: 'journey.tenors.review.what.newTenor',
                after: { value: '1M' },
            }),
        );
    });
});

describe('the convention and its set', () => {
    it('reads the members a convention resolves, marking the ones already in the set', () => {
        const draft = walk({
            kind: 'open-convention',
            convention,
            resolutions: [resolution],
            tenorCodes: ['O/N', '1M'],
        });
        expect(draft.members).toEqual([
            {
                tenorCode: 'O/N',
                original: resolution,
                wanted: true,
                anchorOverride: '',
            },
            {
                tenorCode: '1M',
                original: undefined,
                wanted: false,
                anchorOverride: '',
            },
        ]);
    });

    it('records a tenor added to the set and one removed from it, in the set order', () => {
        const draft = walk(
            {
                kind: 'open-convention',
                convention,
                resolutions: [resolution],
                tenorCodes: ['O/N', '1M'],
            },
            { kind: 'toggle-member', tenorCode: '1M' },
            { kind: 'toggle-member', tenorCode: 'O/N' },
        );
        expect(changesOf(draft)).toEqual([
            {
                id: 'member:O/N',
                what: 'journey.tenors.review.what.member',
                before: { kind: 'state', state: 'inSet', tenorCode: 'O/N' },
                after: { kind: 'state', state: 'notInSet', tenorCode: 'O/N' },
                operation: 'refdata.v1.tenor_convention_resolutions.delete',
            },
            {
                id: 'member:1M',
                what: 'journey.tenors.review.what.member',
                before: { kind: 'state', state: 'notInSet', tenorCode: '1M' },
                after: { kind: 'state', state: 'inSet', tenorCode: '1M' },
                operation: 'refdata.v1.tenor_convention_resolutions.put',
            },
        ]);
    });

    it('records an anchor override for a tenor that stays in the set', () => {
        const kept = walk(
            {
                kind: 'open-convention',
                convention,
                resolutions: [resolution],
                tenorCodes: ['O/N'],
            },
            { kind: 'set-anchor', tenorCode: 'O/N', value: 'today' },
        );
        expect(changesOf(kept)).toEqual([
            {
                id: 'member:O/N',
                what: 'journey.tenors.review.what.member',
                before: { kind: 'state', state: 'inSet', tenorCode: 'O/N' },
                after: { kind: 'state', state: 'inSet', tenorCode: 'O/N' },
                operation: 'refdata.v1.tenor_convention_resolutions.put',
            },
        ]);
    });

    it('lists a removed tenor once, by its removal', () => {
        const draft = walk(
            {
                kind: 'open-convention',
                convention,
                resolutions: [resolution],
                tenorCodes: ['O/N'],
            },
            { kind: 'toggle-member', tenorCode: 'O/N' },
        );
        expect(changesOf(draft)).toEqual([
            {
                id: 'member:O/N',
                what: 'journey.tenors.review.what.member',
                before: { kind: 'state', state: 'inSet', tenorCode: 'O/N' },
                after: { kind: 'state', state: 'notInSet', tenorCode: 'O/N' },
                operation: 'refdata.v1.tenor_convention_resolutions.delete',
            },
        ]);
    });

    it('asks a new convention for a name, an anchor and an algorithm', () => {
        const draft = walk({ kind: 'start-convention', tenorCodes: ['O/N'] });
        expect(tenorProblems(draft).map((p) => p.what)).toEqual([
            'journey.tenors.problems.nameConvention',
            'journey.tenors.problems.chooseAnchorAlgorithmRoll',
        ]);
    });

    it('asks a convention for nothing more once its name, anchor and algorithm are set', () => {
        const named = walk(
            { kind: 'start-convention', tenorCodes: ['O/N'] },
            { kind: 'set-convention-field', field: 'code', value: 'C1' },
            { kind: 'set-convention-field', field: 'measuredFrom', value: 'spot' },
            { kind: 'set-convention-field', field: 'resolutionAlgorithm', value: 'calendar_days' },
        );
        expect(tenorProblems(named)).toEqual([]);
    });

    it('holds a blank convention with the default roll rule until a field is set', () => {
        const draft = walk({ kind: 'start-convention', tenorCodes: [] });
        expect(draft.convention.rollRule).toBe('ModifiedFollowing');
        expect(draft.conventionActive).toBe(true);
    });
});

describe('the write plan', () => {
    it('sends a changed tenor against the version read, keeping the fields it read', () => {
        const plan = writePlan(
            walk(
                { kind: 'open-tenor', tenor: overnight },
                { kind: 'set-tenor-field', field: 'displayName', value: 'O/N spot' },
            ),
        );
        expect(plan?.tenor?.version).toBe(3);
        expect(plan?.tenor?.write).toMatchObject({
            code: 'O/N',
            display_name: 'O/N spot',
            multiplier: 1,
        });
        expect(plan?.convention).toBeUndefined();
        expect(plan?.links).toEqual([]);
        expect(plan?.unlinks).toEqual([]);
        expect(plan?.intent.reason_code).toBe('common.non_material_update');
    });

    it('sends a new tenor as new, under the new-record reason', () => {
        const plan = writePlan(
            walk(
                { kind: 'start-tenor' },
                { kind: 'set-tenor-field', field: 'code', value: '1M' },
                { kind: 'set-tenor-field', field: 'displayName', value: 'One month' },
                { kind: 'set-tenor-field', field: 'kind', value: 'period' },
                { kind: 'set-tenor-field', field: 'unit', value: 'month' },
            ),
        );
        expect(plan?.tenor?.version).toBeNull();
        expect(plan?.tenor?.write).toMatchObject({ code: '1M', sort_order: 0 });
        expect(plan?.intent.reason_code).toBe('system.new_record');
    });

    it('links the tenors added and unlinks the ones removed, each at the version read', () => {
        const plan = writePlan(
            walk(
                {
                    kind: 'open-convention',
                    convention,
                    resolutions: [resolution],
                    tenorCodes: ['O/N', '1M'],
                },
                { kind: 'toggle-member', tenorCode: '1M' },
                { kind: 'toggle-member', tenorCode: 'O/N' },
            ),
        );
        expect(plan?.convention).toBeUndefined();
        expect(plan?.links).toEqual([{ tenorCode: '1M', anchorOverride: null, version: null }]);
        expect(plan?.unlinks).toEqual([{ tenorCode: 'O/N', version: 2 }]);
    });

    it('sends an anchor override as the text typed, and blank as null', () => {
        const plan = writePlan(
            walk(
                {
                    kind: 'open-convention',
                    convention,
                    resolutions: [resolution],
                    tenorCodes: ['O/N'],
                },
                { kind: 'set-anchor', tenorCode: 'O/N', value: ' today ' },
            ),
        );
        expect(plan?.links).toEqual([{ tenorCode: 'O/N', anchorOverride: ' today ', version: 2 }]);
    });

    it('carries the reason and commentary the person chose', () => {
        const plan = writePlan(
            walk(
                { kind: 'open-tenor', tenor: overnight },
                { kind: 'set-tenor-field', field: 'displayName', value: 'O/N spot' },
                { kind: 'set-reason', reasonCode: 'common.correction' },
                { kind: 'set-commentary', commentary: 'fix the name' },
            ),
        );
        expect(plan?.intent).toEqual({
            reason_code: 'common.correction',
            commentary: 'fix the name',
        });
    });
});

describe('the test step', () => {
    const preview: TenorResolution = {
        result: { outcome: 'ok', code: 'ok', message: '', fields: [] },
        anchorDate: '2026-01-31',
        dateBeforeRoll: '2026-02-28',
        adjustedDate: '2026-02-27',
        rollRule: 'ModifiedFollowing',
        calendar: 'TARGET',
    };

    it('assembles the draft rows and the dates asked for, whatever changed', () => {
        const draft = walk(
            { kind: 'open-tenor', tenor: overnight },
            { kind: 'open-convention', convention, resolutions: [resolution], tenorCodes: ['O/N'] },
            { kind: 'set-test-horizon', value: '2026-01-15' },
            { kind: 'set-test-calendar', value: 'TARGET' },
        );
        expect(resolutionDraftOf(draft)).toEqual({
            tenor: overnight,
            convention,
            resolution,
            calendar_code: 'TARGET',
            horizon: '2026-01-15',
            spot: null,
        });
    });

    it('takes the anchor override the form holds and a blank spot as absent', () => {
        const draft = walk(
            { kind: 'open-tenor', tenor: overnight },
            { kind: 'open-convention', convention, resolutions: [resolution], tenorCodes: ['O/N'] },
            { kind: 'set-anchor', tenorCode: 'O/N', value: 'today' },
            { kind: 'set-test-spot', value: '  ' },
        );
        const request = resolutionDraftOf(draft);
        expect(request.resolution.anchor_override).toBe('today');
        expect(request.spot).toBeNull();
    });

    it('keeps the answer and the reason the call failed apart', () => {
        const answered = walk({ kind: 'record-test-preview', preview });
        expect(answered.testPreview).toBe(preview);
        expect(answered.testFailure).toBeUndefined();

        const failed = walk(
            { kind: 'record-test-preview', preview },
            { kind: 'record-test-failure', failure: 'offline' },
        );
        expect(failed.testPreview).toBeUndefined();
        expect(failed.testFailure).toBe('offline');
    });
});
