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
import type { TenorResolution } from '../api/tenors.js';
import { answerOf, canResolve, fieldLabelKey, reachedDates } from './tenorResolution.js';

const answered: TenorResolution = {
    result: { outcome: 'ok', code: '', message: '', fields: [] },
    anchorDate: '2026-03-16',
    dateBeforeRoll: '2026-06-16',
    adjustedDate: '2026-06-16',
    rollRule: 'ModifiedFollowing',
    calendar: 'Argentina',
};

const refused: TenorResolution = {
    result: {
        outcome: 'invalid',
        code: 'tenor_not_in_convention',
        message: 'The convention does not resolve this tenor.',
        fields: [
            {
                field: 'tenor_code',
                code: 'tenor_not_in_convention',
                message: 'No rule resolves 3M.',
            },
        ],
    },
    anchorDate: '',
    dateBeforeRoll: '',
    adjustedDate: '',
    rollRule: 'ModifiedFollowing',
    calendar: 'Argentina',
};

describe('the answer of a resolve', () => {
    it('is idle before anything is asked', () => {
        expect(answerOf(undefined, null, false)).toEqual({ kind: 'idle' });
    });

    it('is asking while the request is in flight', () => {
        expect(answerOf(undefined, null, true)).toEqual({ kind: 'asking' });
    });

    it('is answered when the server accepts the question', () => {
        expect(answerOf(answered, null, false)).toEqual({ kind: 'answered', resolution: answered });
    });

    it('is refused when the server declines the rule', () => {
        expect(answerOf(refused, null, false)).toEqual({ kind: 'refused', resolution: refused });
    });

    it('is unreachable when the request itself throws', () => {
        expect(answerOf(undefined, new Error('the socket closed'), false)).toEqual({
            kind: 'unreachable',
            message: 'the socket closed',
        });
    });
});

describe('the dates a resolve reached', () => {
    it('reports the three dates when the resolution adjusted one', () => {
        expect(reachedDates(answered)).toEqual({
            anchor: '2026-03-16',
            beforeRoll: '2026-06-16',
            adjusted: '2026-06-16',
        });
    });

    it('reports no dates when the resolution adjusted none', () => {
        expect(reachedDates(refused)).toBeUndefined();
    });
});

describe('canResolve', () => {
    const full = {
        conventionCode: 'RATES_SPOT_FORWARD',
        tenorCode: '3M',
        calendarCode: 'Argentina',
        horizon: '2026-03-16',
        spot: '',
    };

    it('asks a rule that does not measure from spot without a spot date', () => {
        expect(canResolve(full, false)).toBe(true);
    });

    it('will not ask a rule that measures from spot until it has one', () => {
        expect(canResolve(full, true)).toBe(false);
        expect(canResolve({ ...full, spot: '2026-03-18' }, true)).toBe(true);
    });

    it('will not ask until all four answers are given', () => {
        expect(canResolve({ ...full, conventionCode: '' }, false)).toBe(false);
        expect(canResolve({ ...full, tenorCode: '' }, false)).toBe(false);
        expect(canResolve({ ...full, calendarCode: '' }, false)).toBe(false);
        expect(canResolve({ ...full, horizon: '   ' }, false)).toBe(false);
    });
});

describe('a refusal that got part of the way', () => {
    it('keeps the dates it reached and leaves the ones it did not', () => {
        expect(
            reachedDates({
                ...refused,
                anchorDate: '2026-03-18',
                dateBeforeRoll: '',
                adjustedDate: '',
            }),
        ).toEqual({ anchor: '2026-03-18', beforeRoll: '', adjusted: '' });
    });

    it('has nothing to show when it reached no date at all', () => {
        expect(reachedDates(refused)).toBeUndefined();
    });
});

describe('fieldLabelKey', () => {
    it('names the fields the page draws', () => {
        expect(fieldLabelKey('calendar_code')).toBe('refdata.tenors.calendar');
        expect(fieldLabelKey('spot')).toBe('refdata.tenors.spot');
        expect(fieldLabelKey('tenor_anchor')).toBe('refdata.tenors.anchor');
    });

    it('says nothing for a field the page does not draw, so no wire name reaches a person', () => {
        expect(fieldLabelKey('some_internal_column')).toBeUndefined();
    });
});
