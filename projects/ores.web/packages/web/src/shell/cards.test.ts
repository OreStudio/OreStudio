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
import { enFlat } from '../i18n/locales/en.js';
import { frFlat } from '../i18n/locales/fr.js';
import { ptFlat } from '../i18n/locales/pt.js';
import { AREA_GROUPS, CARDS, areaOfCard, cardOf, type CardId } from './cards.js';

/**
 * A file card leads to one screen and names it word for word, in every
 * language. A card that said one thing while its screen said another is the
 * menu the product owner could not follow.
 */

const IDS = Object.keys(CARDS) as CardId[];

describe('the cards', () => {
    for (const [language, catalogue] of [
        ['English', enFlat],
        ['French', frFlat],
        ['Portuguese', ptFlat],
    ] as const) {
        it(`name their screens word for word in ${language}`, () => {
            const mismatches = IDS.flatMap((id) => {
                const card = cardOf(id);
                if (card.screenTitleKey === undefined) {
                    return [];
                }
                const onCard = catalogue[card.titleKey];
                const onScreen = catalogue[card.screenTitleKey];
                return onCard === onScreen
                    ? []
                    : [`${id}: "${onCard}" but the screen says "${onScreen}"`];
            });

            expect(mismatches).toEqual([]);
        });
    }

    it('have a title and a line about the screen in every language', () => {
        for (const id of IDS) {
            const card = cardOf(id);
            for (const catalogue of [enFlat, frFlat, ptFlat]) {
                expect(catalogue[card.titleKey], `${id} title`).toBeTruthy();
                expect(catalogue[card.bodyKey], `${id} body`).toBeTruthy();
            }
        }
    });

    it('belong to one area each, except the cards of the person themselves', () => {
        const homeless = IDS.filter((id) => areaOfCard(id) === undefined);

        expect(homeless).toEqual(['myAccess', 'security']);
    });

    it('keep the workflows together, apart from the screens', () => {
        for (const groups of Object.values(AREA_GROUPS)) {
            for (const group of groups) {
                const kinds = new Set(group.cards.map((id) => cardOf(id).kind ?? 'screen'));
                expect(kinds.size, group.titleKey).toBe(1);
                if (kinds.has('workflow')) {
                    expect(group.titleKey).toMatch(/workflows$/);
                }
            }
        }
    });

    it('name a workflow with a verb and its object, in English', () => {
        const names = IDS.filter((id) => cardOf(id).kind === 'workflow').map(
            (id) => enFlat[cardOf(id).titleKey] ?? '',
        );

        expect(names.length).toBeGreaterThan(0);
        for (const name of names) {
            expect(name, name).toMatch(/^(Onboard|Correct|Shape|Define) /);
        }
    });

    it('are drawn by no group twice', () => {
        const drawn = Object.values(AREA_GROUPS).flatMap((groups) =>
            groups.flatMap((group) => [...group.cards]),
        );

        expect(new Set(drawn).size).toBe(drawn.length);
    });
});
