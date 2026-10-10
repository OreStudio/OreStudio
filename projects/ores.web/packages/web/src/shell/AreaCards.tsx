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
import { useHolds } from '../access/holds.js';
import { useTranslation } from '../i18n/Provider.js';
import { Tiles, type Tile } from '../ui/Tiles.js';
import { offered } from './areas.js';
import {
    ACCOUNT_CARDS,
    AREA_GROUPS,
    cardOf,
    type CardArea,
    type CardGroup,
    type CardId,
} from './cards.js';

/** The cards of one list that this person is offered, named and worded in their language. */
function useTiles(ids: readonly CardId[], mode: SessionMode): readonly Tile[] {
    const { t } = useTranslation();
    const holds = useHolds();
    return ids
        .map(cardOf)
        .filter((card) => offered(card, holds, mode))
        .map((card) => ({
            title: t(card.titleKey),
            body: t(card.bodyKey),
            to: card.to,
            icon: card.icon,
        }));
}

function Group({
    group,
    mode,
    heading,
}: {
    readonly group: CardGroup;
    readonly mode: SessionMode;
    readonly heading: boolean;
}): ReactNode {
    const { t } = useTranslation();
    const tiles = useTiles(group.cards, mode);
    if (tiles.length === 0) {
        return null;
    }
    return (
        <section className="space-y-3">
            {heading && <h2 className="text-sm font-semibold text-ink">{t(group.titleKey)}</h2>}
            <Tiles tiles={tiles} />
        </section>
    );
}

/**
 * The cards of one area. An area with several groups heads each group, so the
 * page reads as an index of topics. A group with no card the person may open is
 * not drawn.
 */
export function AreaCards({
    area,
    mode,
}: {
    readonly area: CardArea;
    readonly mode: SessionMode;
}): ReactNode {
    const groups: readonly CardGroup[] = AREA_GROUPS[area];
    return (
        <div className="space-y-8">
            {groups.map((group) => (
                <Group key={group.titleKey} group={group} mode={mode} heading={groups.length > 1} />
            ))}
        </div>
    );
}

/**
 * Every card the person may open, area by area, then their own account. This is
 * Home's Active modules: the same cards the area pages draw, as shortcuts.
 */
export function ActiveCards({ mode }: { readonly mode: SessionMode }): ReactNode {
    const { t } = useTranslation();
    const areas: readonly (readonly [CardArea, string])[] = [
        ['tenants', 'shell.menu.tenants'],
        ['refdata', 'shell.menu.refdata'],
        ['organisation', 'shell.menu.organisation'],
        ['operations', 'shell.menu.operations'],
    ];
    return (
        <div className="space-y-10">
            {areas.map(([area, nameKey]) => (
                <AreaSection key={area} area={area} name={t(nameKey)} mode={mode} />
            ))}
            <AccountSection mode={mode} />
        </div>
    );
}

function AreaSection({
    area,
    name,
    mode,
}: {
    readonly area: CardArea;
    readonly name: string;
    readonly mode: SessionMode;
}): ReactNode {
    const holds = useHolds();
    const any = AREA_GROUPS[area].some((group) =>
        group.cards.some((id) => offered(cardOf(id), holds, mode)),
    );
    if (!any) {
        return null;
    }
    return (
        <section className="space-y-4">
            <h2 className="text-base font-semibold text-ink">{name}</h2>
            <AreaCards area={area} mode={mode} />
        </section>
    );
}

function AccountSection({ mode }: { readonly mode: SessionMode }): ReactNode {
    const { t } = useTranslation();
    const tiles = useTiles(ACCOUNT_CARDS, mode);
    return (
        <section className="space-y-4">
            <h2 className="text-base font-semibold text-ink">{t('shell.account')}</h2>
            <Tiles tiles={tiles} />
        </section>
    );
}
