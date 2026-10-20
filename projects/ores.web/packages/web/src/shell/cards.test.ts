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
import { AREA_CARDS, CARDS, areaOfCard, cardOf, workflowsOn, type CardId } from './cards.js';

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

    it('name the Reports screen word for word in every language', () => {
        const card = cardOf('reports');
        const titles = [
            [enFlat, 'Reports'],
            [frFlat, 'Rapports'],
            [ptFlat, 'Relatórios'],
        ] as const;

        expect(card.screenTitleKey).toBe('reporting.title');
        for (const [catalogue, expected] of titles) {
            expect(catalogue[card.titleKey]).toBe(expected);
            expect(catalogue[card.screenTitleKey ?? '']).toBe(expected);
        }
    });

    it('belong to one area each, except the cards of the person themselves', () => {
        const homeless = IDS.filter(
            (id) => areaOfCard(id) === undefined && cardOf(id).on === undefined,
        );

        expect(homeless).toEqual(['myAccess', 'security']);
    });

    it('list the workflows after the screens on an area page', () => {
        for (const [area, ids] of Object.entries(AREA_CARDS)) {
            const kinds = (ids as readonly CardId[]).map((id) => cardOf(id).kind ?? 'screen');
            const firstWorkflow = kinds.indexOf('workflow');
            if (firstWorkflow >= 0) {
                expect(
                    kinds.slice(firstWorkflow).every((kind) => kind === 'workflow'),
                    area,
                ).toBe(true);
            }
        }
    });

    it('list a workflow once: on the screen it acts on, or on the area page', () => {
        for (const id of IDS.filter((candidate) => cardOf(candidate).kind === 'workflow')) {
            const onArea = areaOfCard(id) !== undefined;
            const on = cardOf(id).on;
            expect(onArea !== (on !== undefined), id).toBe(true);
            if (on !== undefined) {
                expect(IDS, `${id} acts on ${on}`).toContain(on);
                expect(workflowsOn(on as CardId)).toContain(id);
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

    it('are drawn by no area twice', () => {
        const drawn = Object.values(AREA_CARDS).flatMap((ids) => [...ids]);

        expect(new Set(drawn).size).toBe(drawn.length);
    });
});
