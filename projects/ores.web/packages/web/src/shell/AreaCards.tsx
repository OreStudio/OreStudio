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

import type { ReactNode } from 'react';
import type { SessionMode } from '@ores/wire-protocol/browser';
import { useHeld } from '../access/holds.js';
import { useTranslation } from '../i18n/Provider.js';
import { Tiles, type Tile } from '../ui/Tiles.js';
import { MENU, offered } from './areas.js';
import { ACCOUNT_CARDS, AREA_CARDS, FOLDERS, cardOf, type CardArea, type CardId } from './cards.js';

/** The cards of one list that this person is offered, named and worded in their language. */
function useTiles(ids: readonly CardId[]): readonly Tile[] {
    const { t } = useTranslation();
    const holds = useHeld();
    return ids
        .map(cardOf)
        .filter((card) => offered(card, holds))
        .map((card) => ({
            title: t(card.titleKey),
            body: t(card.bodyKey),
            to: card.to,
            icon: card.icon,
            ...(card.kind === 'workflow' ? { workflow: t('cardKind.workflow') } : {}),
        }));
}

/**
 * The cards of one area, as the area page draws them: the ones the person may
 * open, packed close together, with no headings. A workflow card is told from a
 * screen card by its hue and its tag.
 */
export function AreaCards({ area }: { readonly area: CardArea }): ReactNode {
    const ids: readonly CardId[] = AREA_CARDS[area];
    return <Tiles tiles={useTiles(ids)} dense />;
}

/**
 * Home's Active modules: the folder card of each area the person is offered, in
 * the order of the menu, then the cards of their own account. A folder card
 * opens its area, whose page draws the file cards of the screens in it.
 */
export function ActiveCards(): ReactNode {
    const { t } = useTranslation();
    const holds = useHeld();
    const folders: Tile[] = MENU.flatMap((entry) => {
        const folder = FOLDERS.find((candidate) => candidate.to === entry.to);
        return folder !== undefined && offered(entry, holds)
            ? [
                  {
                      title: t(entry.nameKey),
                      body: t(folder.bodyKey),
                      to: folder.to,
                      icon: folder.icon,
                  },
              ]
            : [];
    });
    const account = useTiles(ACCOUNT_CARDS);
    return <Tiles tiles={[...folders, ...account]} />;
}
