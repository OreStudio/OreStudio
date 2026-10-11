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

import type {
    CurveEvolution,
    CurveIdentity,
    CurveSnapshot,
    ListedSeries,
    PointProvenance,
} from '@ores/contracts';
import { fromWireTimestamp, isWireTimestamp } from '@ores/wire-protocol/browser';

/** What the person types to name an object. */
export interface IdentityForm {
    readonly asset: string;
    readonly scope: string;
    readonly instrumentType: string;
    readonly quoteType: string;
    /** One `name=value` per line, for the fields the type declares beyond the four. */
    readonly fields: string;
    /** The party a picked series belongs to. Empty when the identity is typed. */
    readonly partyId: string;
}

/** The identity the form states, or the reason it does not state one. */
export type IdentityResult =
    | { readonly ok: true; readonly identity: CurveIdentity }
    | { readonly ok: false; readonly reason: 'missing' | 'field' };

/**
 * The typed identity the form names.
 *
 * The party is left empty, which asks across the parties the person may see. A
 * field line without an equals sign, or with an empty side, is refused, so a
 * typo does not become an identity that names nothing.
 */
export function identityOf(form: IdentityForm): IdentityResult {
    const asset = form.asset.trim();
    const scope = form.scope.trim();
    const instrumentType = form.instrumentType.trim();
    const quoteType = form.quoteType.trim();
    if (asset === '' || scope === '' || instrumentType === '' || quoteType === '') {
        return { ok: false, reason: 'missing' };
    }
    const fields: { name: string; text: string }[] = [];
    for (const line of form.fields.split('\n')) {
        const trimmed = line.trim();
        if (trimmed === '') {
            continue;
        }
        const at = trimmed.indexOf('=');
        const name = at < 0 ? '' : trimmed.slice(0, at).trim();
        const text = at < 0 ? '' : trimmed.slice(at + 1).trim();
        if (name === '' || text === '') {
            return { ok: false, reason: 'field' };
        }
        fields.push({ name, text });
    }
    return {
        ok: true,
        identity: {
            asset,
            scope,
            instrument_type: instrumentType,
            quote_type: quoteType,
            party_id: form.partyId,
            fields,
        },
    };
}

/** A day the person picked, as the first second it holds on the wire. */
export function startOfDay(day: string): string {
    return `${day} 00:00:00Z`;
}

/** A day the person picked, as the last second it holds on the wire. */
export function endOfDay(day: string): string {
    return `${day} 23:59:59Z`;
}

/** The range the two days state, or why they state none. */
export type RangeResult =
    | { readonly ok: true; readonly range: { readonly from: string; readonly to: string } }
    | { readonly ok: false; readonly reason: 'missing' | 'range' };

const DAY = /^\d{4}-\d{2}-\d{2}$/;

/**
 * The range from the first second of one day to the last second of another.
 *
 * An empty or malformed day is refused, so a cleared date input does not become
 * a request with a blank instant. Days are written year first, so the text order
 * is the time order.
 */
export function rangeOf(from: string, to: string): RangeResult {
    if (!DAY.test(from) || !DAY.test(to)) {
        return { ok: false, reason: 'missing' };
    }
    if (from > to) {
        return { ok: false, reason: 'range' };
    }
    return { ok: true, range: { from: startOfDay(from), to: endOfDay(to) } };
}

/** The label of a node: its coordinates in the order the shape declares them. */
export function nodeLabel(coordinates: readonly string[]): string {
    return coordinates.join(' / ');
}

/** One row of the matrix: a node, the change it shows, and a cell for every instant. */
export interface MatrixRow {
    readonly label: string;
    readonly status: string;
    /** An empty cell is a hole at that instant. */
    readonly cells: readonly string[];
}

export interface Matrix {
    readonly columns: readonly string[];
    readonly rows: readonly MatrixRow[];
}

/**
 * The evolution as rows of nodes and columns of instants.
 *
 * Every instant carries a value for every node, so no cell is guessed. A row
 * whose instant lacks a value gets an empty cell, which the screen draws as a
 * hole and not as a zero.
 */
export function matrixOf(evolution: CurveEvolution): Matrix {
    const rows = evolution.nodes.map((node, index) => ({
        label: nodeLabel(node.coordinates),
        status: node.status,
        cells: evolution.instants.map((instant) => instant.values[index] ?? ''),
    }));
    return { columns: evolution.instants.map((instant) => instant.as_of), rows };
}

/** How many nodes hold a value at no instant of the range. */
export function neverQuoted(matrix: Matrix): number {
    return matrix.rows.filter((row) => row.cells.every((cell) => cell === '')).length;
}

/** One node of the snapshot: its value, the age of that value, and where it came from. */
export interface NowRow {
    readonly label: string;
    readonly value: string;
    /** Seconds from the record time to the snapshot instant, or none for a hole. */
    readonly ageSeconds: number | undefined;
    readonly sourceKind: string;
    /** Who keyed a manual value, or the recipe that built a derived one; empty otherwise. */
    readonly detail: string;
}

/** The first characters of a recipe id, enough to tell two recipes apart on a row. */
const RECIPE_ID_SHOWN = 8;

function detailOf(provenance: PointProvenance | undefined): string {
    if (provenance === undefined) {
        return '';
    }
    if (provenance.source_kind === 'manual') {
        return provenance.modified_by;
    }
    if (provenance.source_kind === 'derived' && provenance.derivation_config_id !== '') {
        return `${provenance.derivation_config_id.slice(0, RECIPE_ID_SHOWN)} v${provenance.derivation_config_version}`;
    }
    return '';
}

function seconds(at: string): number | undefined {
    return isWireTimestamp(at) ? fromWireTimestamp(at).getTime() / 1000 : undefined;
}

/**
 * The snapshot as one row per node.
 *
 * A hole has an empty value and no age. A value whose record time is the epoch
 * has no age either, because the epoch is how an absent time is written.
 */
export function nowRowsOf(snapshot: CurveSnapshot): readonly NowRow[] {
    const at = seconds(snapshot.as_of);
    return snapshot.nodes.map((node, index) => {
        const value = snapshot.values[index] ?? '';
        const recorded = seconds(snapshot.recorded_at[index] ?? '');
        const absent = value === '' || recorded === undefined || recorded <= 0 || at === undefined;
        return {
            label: nodeLabel(node.coordinates),
            value,
            ageSeconds: absent ? undefined : Math.max(0, at - recorded),
            sourceKind: snapshot.provenance[index]?.source_kind ?? '',
            detail: detailOf(snapshot.provenance[index]),
        };
    });
}

/** An age in the largest whole unit that fits, such as `45s`, `3h` or `2d`. */
export function formatAge(ageSeconds: number): string {
    const whole = Math.floor(ageSeconds);
    if (whole < 60) {
        return `${whole}s`;
    }
    if (whole < 3600) {
        return `${Math.floor(whole / 60)}m`;
    }
    if (whole < 86400) {
        return `${Math.floor(whole / 3600)}h`;
    }
    return `${Math.floor(whole / 86400)}d`;
}

/** What a series is called in the picker: its four words, then the fields that tell it apart. */
export function seriesLabel(listed: ListedSeries): string {
    const { asset, scope, instrument_type, quote_type, fields } = listed.identity;
    const words = [asset, scope, instrument_type, quote_type].join(' / ');
    if (fields.length === 0) {
        return words;
    }
    return `${words} · ${fields.map((field) => `${field.name}=${field.text}`).join(', ')}`;
}

/** Whether the evolution can draw the series: only a series with a declared shape has a grid. */
export function canBeShown(listed: ListedSeries): boolean {
    return listed.axes.length > 0;
}

/** The form a picked series fills in, with its party kept so the read names this series. */
export function formFrom(listed: ListedSeries): IdentityForm {
    const { asset, scope, instrument_type, quote_type, fields, party_id } = listed.identity;
    return {
        asset,
        scope,
        instrumentType: instrument_type,
        quoteType: quote_type,
        fields: fields.map((field) => `${field.name}=${field.text}`).join('\n'),
        partyId: party_id,
    };
}
