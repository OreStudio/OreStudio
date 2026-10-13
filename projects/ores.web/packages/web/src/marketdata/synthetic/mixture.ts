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

import type { RecordRow } from '../../api/client.js';

/**
 * One Gaussian of a feed's mixture as the screen holds it while it is edited.
 * The numbers are kept as typed, so a half-typed value stays on the screen;
 * `version` is the version read, or null for a component not yet saved.
 */
export interface DraftComponent {
    readonly id: string;
    readonly version: number | null;
    readonly description: string;
    readonly mean: string;
    readonly stdev: string;
    readonly weight: string;
}

/** A component's numbers, once every one of them reads as a number. */
export interface Gaussian {
    readonly mean: number;
    readonly stdev: number;
    readonly weight: number;
}

/** Why a mixture cannot be saved or drawn as it stands. */
export type MixtureProblem = 'empty' | 'number' | 'negative' | 'weights';

function text(value: unknown): string {
    return value === null || value === undefined ? '' : String(value);
}

/** The saved mixture as a draft, in the order of its index. */
export function draftOf(rows: readonly RecordRow[]): readonly DraftComponent[] {
    return [...rows]
        .sort((a, b) => Number(a['component_index']) - Number(b['component_index']))
        .map((row) => ({
            id: text(row['id']),
            version: row.version,
            description: text(row['description']),
            mean: text(row['mean']),
            stdev: text(row['stdev']),
            weight: text(row['weight']),
        }));
}

/** A new component: a plain Gaussian with no drift, so a person starts from a shape they know. */
export function newComponent(id: string): DraftComponent {
    return { id, version: null, description: '', mean: '0', stdev: '0.01', weight: '1' };
}

function number(value: string): number | undefined {
    if (value.trim() === '') {
        return undefined;
    }
    const parsed = Number(value);
    return Number.isFinite(parsed) ? parsed : undefined;
}

/** The mixture's numbers, or undefined while any of them is not a number. */
export function gaussiansOf(draft: readonly DraftComponent[]): readonly Gaussian[] | undefined {
    const out: Gaussian[] = [];
    for (const component of draft) {
        const mean = number(component.mean);
        const stdev = number(component.stdev);
        const weight = number(component.weight);
        if (mean === undefined || stdev === undefined || weight === undefined) {
            return undefined;
        }
        out.push({ mean, stdev, weight });
    }
    return out;
}

/** The weights as typed, added up. The save does not change them; the draw normalises them. */
export function weightSum(gaussians: readonly Gaussian[]): number {
    return gaussians.reduce((sum, gaussian) => sum + gaussian.weight, 0);
}

/**
 * What stops the mixture from being saved: no component, a value that is not a
 * number, a negative deviation or weight (the table refuses both), or weights
 * that add up to nothing.
 */
export function problemsOf(draft: readonly DraftComponent[]): readonly MixtureProblem[] {
    if (draft.length === 0) {
        return ['empty'];
    }
    const gaussians = gaussiansOf(draft);
    if (gaussians === undefined) {
        return ['number'];
    }
    const problems: MixtureProblem[] = [];
    if (gaussians.some((gaussian) => gaussian.stdev < 0 || gaussian.weight < 0)) {
        problems.push('negative');
    }
    if (weightSum(gaussians) <= 0) {
        problems.push('weights');
    }
    return problems;
}

function normal(x: number, mean: number, stdev: number): number {
    const z = (x - mean) / stdev;
    return Math.exp(-0.5 * z * z) / (stdev * Math.sqrt(2 * Math.PI));
}

/**
 * The density the mixture implies, f(x) = Σ wᵢ·N(x; μᵢ, σᵢ) with the weights
 * normalised as the draw normalises them, and each weighted component, over
 * four deviations either side. A component with no deviation has no density
 * and is left out.
 */
export function densityOf(
    gaussians: readonly Gaussian[],
    points = 141,
): {
    readonly xs: readonly number[];
    readonly mixture: readonly number[];
    readonly parts: readonly (readonly number[])[];
} {
    const total = weightSum(gaussians);
    const drawn = gaussians.filter((gaussian) => gaussian.stdev > 0);
    if (drawn.length === 0 || total <= 0) {
        return { xs: [], mixture: [], parts: [] };
    }
    const low = Math.min(...drawn.map((gaussian) => gaussian.mean - 4 * gaussian.stdev));
    const high = Math.max(...drawn.map((gaussian) => gaussian.mean + 4 * gaussian.stdev));
    const xs = Array.from({ length: points }, (_, i) => low + ((high - low) * i) / (points - 1));
    const parts = drawn.map((gaussian) =>
        xs.map((x) => (gaussian.weight / total) * normal(x, gaussian.mean, gaussian.stdev)),
    );
    const mixture = xs.map((_, i) => parts.reduce((sum, part) => sum + (part[i] ?? 0), 0));
    return { xs, mixture, parts };
}

/**
 * The writes that make the saved mixture the draft: each component the draft
 * holds, indexed by its place, against the version read; and each saved
 * component the draft no longer holds, to be removed. A component that did not
 * change is not written.
 */
export function savePlan(
    feedId: string,
    saved: readonly DraftComponent[],
    draft: readonly DraftComponent[],
): {
    readonly changes: readonly {
        readonly write: Readonly<Record<string, unknown>>;
        readonly version: number | null;
    }[];
    readonly removals: readonly { readonly key: string; readonly version: number | null }[];
} {
    const gaussians = gaussiansOf(draft) ?? [];
    const before = new Map(saved.map((component, index) => [component.id, { component, index }]));
    const changes = draft.flatMap((component, index) => {
        const gaussian = gaussians[index];
        if (gaussian === undefined) {
            return [];
        }
        const was = before.get(component.id);
        const unchanged =
            was !== undefined &&
            was.index === index &&
            was.component.description === component.description &&
            Number(was.component.mean) === gaussian.mean &&
            Number(was.component.stdev) === gaussian.stdev &&
            Number(was.component.weight) === gaussian.weight;
        if (unchanged) {
            return [];
        }
        return [
            {
                write: {
                    id: component.id,
                    fx_spot_config_id: feedId,
                    component_index: index,
                    description: component.description,
                    ...gaussian,
                },
                version: component.version,
            },
        ];
    });
    const kept = new Set(draft.map((component) => component.id));
    const removals = saved
        .filter((component) => !kept.has(component.id))
        .map((component) => ({ key: component.id, version: component.version }));
    return { changes, removals };
}

/**
 * A component's description as a name and a note: the text before the first
 * colon names it, and what follows says how it was set, such as "calibrated to
 * ~8.5% annualised realised vol.". A description with no colon is all name.
 */
export function describedAs(description: string): { readonly name: string; readonly note: string } {
    const at = description.indexOf(':');
    return at < 0
        ? { name: description.trim(), note: '' }
        : { name: description.slice(0, at).trim(), note: description.slice(at + 1).trim() };
}

/**
 * Each component's share of the draws, as the draw normalises the weights:
 * its weight over the sum. Undefined while a weight is not a number, or when
 * the weights add up to nothing.
 */
export function sharesOf(draft: readonly DraftComponent[]): readonly (number | undefined)[] {
    const gaussians = gaussiansOf(draft);
    const total = gaussians === undefined ? 0 : weightSum(gaussians);
    return draft.map((_, index) => {
        const weight = gaussians?.[index]?.weight;
        return weight === undefined || total <= 0 ? undefined : weight / total;
    });
}

/** The fields of a component the mixture table shows and the save compares. */
export const COMPONENT_VALUES = ['description', 'mean', 'stdev', 'weight'] as const;

export type ComponentValue = (typeof COMPONENT_VALUES)[number];

/** What a row of the draft is against the saved mixture: new, or the fields it changed and whether it moved. */
export interface RowChange {
    readonly added: boolean;
    readonly moved: boolean;
    readonly fields: ReadonlySet<ComponentValue>;
}

/** How each row of the draft differs from what is saved, in the draft's order. */
export function changesOf(
    saved: readonly DraftComponent[],
    draft: readonly DraftComponent[],
): readonly RowChange[] {
    const before = new Map(saved.map((component, index) => [component.id, { component, index }]));
    return draft.map((component, index) => {
        const was = before.get(component.id);
        if (was === undefined) {
            return { added: true, moved: false, fields: new Set() };
        }
        const fields = new Set(
            COMPONENT_VALUES.filter((field) =>
                field === 'description'
                    ? was.component.description !== component.description
                    : Number(was.component[field]) !== Number(component[field]),
            ),
        );
        return { added: false, moved: was.index !== index, fields };
    });
}

/** The saved components the draft no longer holds, in their saved order. */
export function removedFrom(
    saved: readonly DraftComponent[],
    draft: readonly DraftComponent[],
): readonly DraftComponent[] {
    const kept = new Set(draft.map((component) => component.id));
    return saved.filter((component) => !kept.has(component.id));
}

/** The numbers a mixture holds, one entry per component. */
export interface MixtureNumbers {
    readonly means: readonly number[];
    readonly stdevs: readonly number[];
    readonly weights: readonly number[];
}

function plainNumber(value: number | undefined): string {
    return String(Number((value ?? 0).toPrecision(10)));
}

/**
 * The draft the simple view's mixture makes. The primary component keeps the
 * identity of the one that had the most weight, and the jump regime that of a
 * component four times as wide, so a save updates them in place; a
 * mean-reverting mixture keeps the first. A component the new mixture has no
 * room for is left out, and a saved one shows as deleted until the save.
 */
export function draftFromMixture(
    current: readonly DraftComponent[],
    mixture: MixtureNumbers,
    reverting: boolean,
    newId: () => string,
): readonly DraftComponent[] {
    const gaussians = gaussiansOf(current);
    let primary = 0;
    if (gaussians !== undefined) {
        gaussians.forEach((one, i) => {
            if (one.weight > (gaussians[primary]?.weight ?? 0)) {
                primary = i;
            }
        });
    }
    const primaryOf = reverting ? 0 : primary;
    const jump = current.findIndex(
        (_, i) =>
            i !== primaryOf &&
            gaussians !== undefined &&
            (gaussians[i]?.stdev ?? 0) >= 4 * (gaussians[primaryOf]?.stdev ?? 0),
    );
    const sources = [current[primaryOf], jump >= 0 ? current[jump] : undefined];
    const names = ['Primary process', 'Jump regime (approx.)'];
    return mixture.means.slice(0, 2).map((mean, i) => {
        const source = sources[i];
        return {
            id: source?.id ?? newId(),
            version: source?.version ?? null,
            description: source?.description || names[i] || '',
            mean: plainNumber(mean),
            stdev: plainNumber(mixture.stdevs[i]),
            weight: plainNumber(mixture.weights[i]),
        };
    });
}
