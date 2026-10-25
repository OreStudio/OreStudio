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
 */

import type { EngineSetting } from '../api/engineSettings.js';
import type { PricingVocabulary } from '../api/pricingEngines.js';

/**
 * The control a parameter's value needs, apart from the screen that draws it.
 *
 * Two screens hold the same kind of thing in two vocabularies: the pricing
 * engine screen reads a value set, which names a source kind, and the engine
 * settings screen reads a setting row, which names a kind. Both name one of
 * four controls, so the mapping is done here instead of in each screen, and a
 * control is added by adding it to the model rather than to both screens.
 */

/** One value a choice offers, as the person reads it. */
export interface ControlChoice {
    readonly value: string;
    readonly label: string;
}

export type ControlModel =
    | { kind: 'choice'; choices: readonly ControlChoice[] }
    | { kind: 'boolean' }
    | { kind: 'number'; minimum: number | null; maximum: number | null; whole: boolean }
    | { kind: 'text'; pattern: string };

/** The value-set fields of one pricing setting, as the vocabulary serves them. */
export type ValueSetFields = PricingVocabulary['valueSets'][string];

/**
 * The control a pricing value set calls for.
 *
 * The source kind is the vocabulary's own word for what the values are, so a
 * kind this build does not know is read as free text rather than guessed at. A
 * set that states choices draws them whatever kind it names, because a list a
 * person may not type is the one thing the choices decide.
 */
export function controlOfValueSet(fields: ValueSetFields | undefined): ControlModel {
    if (fields === undefined) {
        return { kind: 'text', pattern: '' };
    }
    /*
     * A flag is a flag even though its set states the two values as choices: the
     * control for it is the switch, and the choices are what it holds.
     */
    if (fields.sourceKind === 'boolean') {
        return { kind: 'boolean' };
    }
    if (
        fields.sourceKind === 'enumerated' ||
        fields.sourceKind === 'catalogue' ||
        fields.choices.length > 0
    ) {
        return { kind: 'choice', choices: fields.choices };
    }
    switch (fields.sourceKind) {
        case 'integer':
            return {
                kind: 'number',
                minimum: fields.minimum,
                maximum: fields.maximum,
                whole: true,
            };
        case 'number':
        case 'range':
            return {
                kind: 'number',
                minimum: fields.minimum,
                maximum: fields.maximum,
                whole: false,
            };
        default:
            return { kind: 'text', pattern: fields.pattern };
    }
}

/**
 * The control an engine setting's kind calls for.
 *
 * The row states the list a choice offers and the bounds a whole number takes,
 * so the options are both the value and the label: an engine setting's words
 * are the ORE codes it accepts.
 */
export function controlOfEngineSetting(
    kind: EngineSetting['kind'],
    options: readonly string[],
    min: number | null,
    max: number | null,
): ControlModel {
    switch (kind) {
        case 'flag':
            return { kind: 'boolean' };
        case 'whole':
            return { kind: 'number', minimum: min, maximum: max, whole: true };
        case 'choice':
            return {
                kind: 'choice',
                choices: options.map((option) => ({ value: option, label: option })),
            };
    }
}
