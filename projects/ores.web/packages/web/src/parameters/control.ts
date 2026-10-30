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

import type { PricingVocabulary } from '../api/pricingEngines.js';
import { NO_SOURCE_KIND, type ValueSetKind } from '../api/valueSets.js';

/**
 * The control a parameter's value needs, apart from the screen that draws it.
 *
 * Both configuration screens read a value set, which names a source kind, so
 * one mapping serves the pricing screen and the engine run settings screen. A
 * control is added by adding it to this model rather than to both screens.
 */

/** One value a choice offers, as the person reads it. */
export interface ControlChoice {
    readonly value: string;
    readonly label: string;
}

export type ControlModel =
    | {
          kind: 'choice';
          choices: readonly ControlChoice[];
          /**
           * The catalogue the list draws on, such as =refdata.currency=, or empty
           * when it is a closed list of the contract's own. A catalogue is drawn
           * with that record's own control, so a currency carries its flag.
           */
          catalogue?: string;
      }
    | { kind: 'boolean' }
    | { kind: 'number'; minimum: number | null; maximum: number | null; whole: boolean }
    | { kind: 'text'; pattern: string };

/** The value-set fields of one pricing setting, as the vocabulary serves them. */
export type ValueSetFields = PricingVocabulary['valueSets'][string];

/** The fields of a value set that decide its control, as any leaf carries them. */
export interface ValueSetShape {
    readonly sourceKind: ValueSetKind;
    readonly catalogue?: string;
    readonly choices: readonly ControlChoice[];
    readonly minimum: number | null;
    readonly maximum: number | null;
    readonly pattern: string;
}

/**
 * The control a pricing value set calls for.
 *
 * The source kind is the vocabulary's own word for what the values are, so a
 * kind this build does not know is read as free text rather than guessed at. A
 * set that states choices draws them whatever kind it names, because a list a
 * person may not type is the one thing the choices decide.
 */
export function controlOfValueSet(fields: ValueSetShape | undefined): ControlModel {
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
        // A list the contract states itself carries no catalogue, so the key is
        // absent rather than empty: the control is the plain select.
        return {
            kind: 'choice',
            choices: fields.choices,
            ...(fields.catalogue === undefined || fields.catalogue === ''
                ? {}
                : { catalogue: fields.catalogue }),
        };
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
        case 'regex':
        case 'text':
        case NO_SOURCE_KIND:
            return { kind: 'text', pattern: fields.pattern };
    }
}

/**
 * The default of a leaf in a few words, as *Default (X)* prints it: the first
 * sentence of its default text, which says the value and then what it means, as
 * in No. Today's fixing is loaded.
 */
export function shortDefault(defaultText: string): string {
    const first = defaultText.split(/\.(?:\s|$)/, 1)[0] ?? '';
    return first.trim();
}
