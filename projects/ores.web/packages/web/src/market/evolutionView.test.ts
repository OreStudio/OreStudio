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
import type { CurveEvolution, CurveSnapshot } from '@ores/contracts';
import {
    endOfDay,
    formatAge,
    identityOf,
    matrixOf,
    neverQuoted,
    nodeLabel,
    nowRowsOf,
    rangeOf,
    startOfDay,
} from './evolutionView.js';

const FORM = {
    asset: 'fx',
    scope: 'NZD',
    instrumentType: 'fx_option',
    quoteType: 'rate_lnvol',
    fields: 'ccy=PLN',
};

describe('identityOf', () => {
    it('names the object the form states', () => {
        expect(identityOf(FORM)).toEqual({
            ok: true,
            identity: {
                asset: 'fx',
                scope: 'NZD',
                instrument_type: 'fx_option',
                quote_type: 'rate_lnvol',
                party_id: '',
                fields: [{ name: 'ccy', text: 'PLN' }],
            },
        });
    });

    it('trims the four words and skips blank field lines', () => {
        const result = identityOf({ ...FORM, asset: ' fx ', fields: '\n ccy = PLN \n\n' });
        expect(result.ok && result.identity.asset).toBe('fx');
        expect(result.ok && result.identity.fields).toEqual([{ name: 'ccy', text: 'PLN' }]);
    });

    it('refuses a form that leaves one of the four words empty', () => {
        expect(identityOf({ ...FORM, quoteType: '  ' })).toEqual({ ok: false, reason: 'missing' });
    });

    it('refuses a field line that is not name=value', () => {
        expect(identityOf({ ...FORM, fields: 'ccy' })).toEqual({ ok: false, reason: 'field' });
        expect(identityOf({ ...FORM, fields: '=PLN' })).toEqual({ ok: false, reason: 'field' });
        expect(identityOf({ ...FORM, fields: 'ccy=' })).toEqual({ ok: false, reason: 'field' });
    });

    it('keeps an equals sign inside the value', () => {
        const result = identityOf({ ...FORM, fields: 'curve_id=A=B' });
        expect(result.ok && result.identity.fields).toEqual([{ name: 'curve_id', text: 'A=B' }]);
    });
});

describe('the day edges', () => {
    it('writes a day as the first and the last second it holds', () => {
        expect(startOfDay('2026-10-01')).toBe('2026-10-01 00:00:00Z');
        expect(endOfDay('2026-10-01')).toBe('2026-10-01 23:59:59Z');
    });
});

describe('rangeOf', () => {
    it('opens on the first second of the first day and closes on the last of the last', () => {
        expect(rangeOf('2026-10-01', '2026-10-09')).toEqual({
            ok: true,
            range: { from: '2026-10-01 00:00:00Z', to: '2026-10-09 23:59:59Z' },
        });
    });

    it('accepts a single day', () => {
        expect(rangeOf('2026-10-01', '2026-10-01').ok).toBe(true);
    });

    it('refuses a cleared or malformed day', () => {
        expect(rangeOf('', '2026-10-09')).toEqual({ ok: false, reason: 'missing' });
        expect(rangeOf('2026-10-01', '')).toEqual({ ok: false, reason: 'missing' });
        expect(rangeOf('10/01/2026', '2026-10-09')).toEqual({ ok: false, reason: 'missing' });
    });

    it('refuses a range that ends before it starts', () => {
        expect(rangeOf('2026-10-09', '2026-10-01')).toEqual({ ok: false, reason: 'range' });
    });
});

describe('matrixOf', () => {
    const evolution: CurveEvolution = {
        coordinate_fields: ['expiry', 'strike_label'],
        nodes: [
            { coordinates: ['1Y', 'ATM'], status: 'persistent' },
            { coordinates: ['1Y', '25RR'], status: 'added' },
            { coordinates: ['2Y', 'ATM'], status: 'never_quoted' },
        ],
        instants: [
            { as_of: '2026-10-01 00:00:00Z', values: ['0.080', '', ''] },
            { as_of: '2026-10-02 00:00:00Z', values: ['0.081', '0.011', ''] },
        ],
    };

    it('lays a row per node and a cell per instant', () => {
        const matrix = matrixOf(evolution);
        expect(matrix.columns).toEqual(['2026-10-01 00:00:00Z', '2026-10-02 00:00:00Z']);
        expect(matrix.rows.map((row) => row.label)).toEqual(['1Y / ATM', '1Y / 25RR', '2Y / ATM']);
        expect(matrix.rows[1]?.cells).toEqual(['', '0.011']);
        expect(matrix.rows[1]?.status).toBe('added');
    });

    it('leaves a missing value as a hole and never as zero', () => {
        const short = matrixOf({
            ...evolution,
            instants: [{ as_of: '2026-10-01 00:00:00Z', values: ['0.080'] }],
        });
        expect(short.rows[2]?.cells).toEqual(['']);
    });

    it('counts the nodes that hold a value at no instant', () => {
        expect(neverQuoted(matrixOf(evolution))).toBe(1);
    });
});

describe('nodeLabel', () => {
    it('joins the coordinates in the order the shape declares them', () => {
        expect(nodeLabel(['5Y'])).toBe('5Y');
        expect(nodeLabel(['1Y', 'ATM'])).toBe('1Y / ATM');
    });
});

describe('nowRowsOf', () => {
    const snapshot: CurveSnapshot = {
        as_of: '2026-10-10 12:00:00Z',
        coordinate_fields: ['expiry', 'strike_label'],
        nodes: [
            { coordinates: ['1Y', 'ATM'], status: '' },
            { coordinates: ['1Y', '25RR'], status: '' },
            { coordinates: ['2Y', 'ATM'], status: '' },
        ],
        values: ['0.080', '0.011', ''],
        recorded_at: ['2026-10-10 09:00:00Z', '1970-01-01 00:00:00Z', '1970-01-01 00:00:00Z'],
        provenance: [],
        oldest_age_seconds: 10800,
        spread_seconds: 0,
        warning: false,
    };

    it('ages a value from its record time to the snapshot instant', () => {
        expect(nowRowsOf(snapshot)[0]?.ageSeconds).toBe(3 * 3600);
    });

    it('gives no age when the record times are shorter than the nodes', () => {
        const rows = nowRowsOf({ ...snapshot, recorded_at: [] });
        expect(rows[0]?.value).toBe('0.080');
        expect(rows[0]?.ageSeconds).toBeUndefined();
    });

    it('gives no age to a hole or to a value with no record time', () => {
        const rows = nowRowsOf(snapshot);
        expect(rows[1]?.value).toBe('0.011');
        expect(rows[1]?.ageSeconds).toBeUndefined();
        expect(rows[2]?.value).toBe('');
        expect(rows[2]?.ageSeconds).toBeUndefined();
    });

    it('reads the source kind only when provenance was asked', () => {
        expect(nowRowsOf(snapshot)[0]?.sourceKind).toBe('');
        const asked = nowRowsOf({
            ...snapshot,
            provenance: [
                {
                    source_kind: 'manual',
                    modified_by: 'trader',
                    change_reason_code: 'common.correction',
                    change_commentary: '',
                    recorded_at: '2026-10-10 09:00:00Z',
                    derivation_config_id: '',
                    derivation_config_version: 0,
                    source_as_of: '1970-01-01 00:00:00Z',
                },
            ],
        });
        expect(asked[0]?.sourceKind).toBe('manual');
        expect(asked[1]?.sourceKind).toBe('');
    });

    it('names who keyed a manual value and the recipe behind a derived one', () => {
        const provenance = (overrides: object) => ({
            source_kind: 'quoted',
            modified_by: '',
            change_reason_code: '',
            change_commentary: '',
            recorded_at: '1970-01-01 00:00:00Z',
            derivation_config_id: '',
            derivation_config_version: 0,
            source_as_of: '1970-01-01 00:00:00Z',
            ...overrides,
        });
        const rows = nowRowsOf({
            ...snapshot,
            provenance: [
                provenance({ source_kind: 'manual', modified_by: 'trader' }),
                provenance({
                    source_kind: 'derived',
                    derivation_config_id: '12345678-aaaa-bbbb-cccc-1234567890ab',
                    derivation_config_version: 3,
                }),
                provenance({}),
            ],
        });
        expect(rows.map((row) => row.detail)).toEqual(['trader', '12345678 v3', '']);
    });
});

describe('formatAge', () => {
    it('uses the largest whole unit that fits', () => {
        expect(formatAge(45)).toBe('45s');
        expect(formatAge(59.9)).toBe('59s');
        expect(formatAge(60)).toBe('1m');
        expect(formatAge(3600)).toBe('1h');
        expect(formatAge(2 * 86400 + 5)).toBe('2d');
    });
});
