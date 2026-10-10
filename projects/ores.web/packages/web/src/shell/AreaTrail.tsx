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
import { useTranslation } from '../i18n/Provider.js';
import { Crumbs } from '../refdata/shared.js';

/** The areas a screen can belong to, with the address of each area's page. */
const AREAS = {
    organisation: { nameKey: 'shell.menu.organisation', to: '/organisation' },
    operations: { nameKey: 'shell.menu.operations', to: '/operations' },
    requests: { nameKey: 'shell.menu.requests', to: '/requests' },
} as const;

export type AreaName = keyof typeof AREAS;

/** A step of the trail below its area: a screen with its address, or the page the reader is on. */
export interface TrailStep {
    readonly label: string;
    readonly to?: string;
}

/**
 * The parts of the trail of a screen that belongs to an area: Home, the area,
 * then each step down to the page the reader is on.
 *
 * Every screen has one area and states it in the same words, so a person
 * reaches the same screen by the same trail from wherever they arrived. The
 * area is a link unless the reader is on the area's own page. A list that draws
 * its own header takes these parts as its crumbs.
 */
export function useAreaParts(
    area: AreaName,
    screen?: string,
    steps: readonly TrailStep[] = [],
): readonly TrailStep[] {
    const { t } = useTranslation();
    const { nameKey, to } = AREAS[area];
    const below: readonly TrailStep[] =
        screen === undefined ? steps : [...steps, { label: screen }];
    return [
        { label: t('shell.menu.home'), to: '/' },
        below.length === 0 ? { label: t(nameKey) } : { label: t(nameKey), to },
        ...below,
    ];
}

/** The trail of a screen that belongs to an area, drawn above its header. */
export function AreaTrail({
    area,
    screen,
    steps,
}: {
    readonly area: AreaName;
    /** The page the reader is on, when it is one level below the area. */
    readonly screen?: string;
    /** The screens between the area and the page the reader is on. */
    readonly steps?: readonly TrailStep[];
}): ReactNode {
    return <Crumbs parts={useAreaParts(area, screen, steps)} />;
}
