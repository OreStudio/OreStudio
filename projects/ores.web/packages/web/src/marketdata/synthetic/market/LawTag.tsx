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

import type { ReactNode } from 'react';
import type { TraceFeed } from '../../../api/client.js';
import { useTranslation } from '../../../i18n/Provider.js';
import { SummaryLines, SummaryTag, type SummaryLine } from '../SummaryTag.js';
import { FeedLink, type FeedPages } from './FeedLink.js';
import { clean } from './numbers.js';

export const LAWS = ['periodic', 'poisson', 'hawkes'] as const;
export type Law = (typeof LAWS)[number];

type Translate = (key: string, values?: Readonly<Record<string, string | number>>) => string;

/** Whether a law's clustering settles: always for the memoryless laws, below one for Hawkes. */
export function lawStable(law: TraceFeed): boolean {
    return law.mode !== 'hawkes' || law.branchingRatio < 1;
}

/**
 * The numbers behind a law, one to a line: its rate an hour and its sensitivity, and for a
 * Hawkes law its branching, its decay and whether it settles.
 */
export function lawLines(law: TraceFeed, t: Translate): readonly SummaryLine[] {
    const line = (column: string, value: string): SummaryLine => ({
        name: t(`marketdata.market.laws.columns.${column}`),
        value,
    });
    const lines = [
        line('perHour', clean(law.rate * 3600, 4)),
        line('sensitivity', clean(law.sensitivity, 3)),
    ];
    if (law.mode === 'hawkes') {
        lines.push(
            line('branching', clean(law.branchingRatio, 3)),
            line('decay', clean(law.decay, 3)),
            line(
                'stable',
                t(
                    lawStable(law)
                        ? 'marketdata.market.laws.stable'
                        : 'marketdata.market.laws.runsAway',
                ),
            ),
        );
    }
    return lines;
}

/**
 * A feed's law as a tag: its name, which opens the feed's Arrivals tab where the law is set,
 * and its numbers when the pointer rests on it, as a process tag shows its parameters.
 */
export function LawTag({
    law,
    pages,
}: {
    readonly law: TraceFeed;
    readonly pages: FeedPages;
}): ReactNode {
    const { t } = useTranslation();
    const name = t(`marketdata.marketRegime.modes.${law.mode}`);
    const page = pages.get(law.sourceName);
    return (
        <SummaryTag
            name={name}
            label={t('marketdata.market.laws.summary', { feed: law.sourceName })}
            trigger={
                <FeedLink sourceName={law.sourceName} pages={pages} tab="arrivals">
                    {name}
                </FeedLink>
            }
            link={
                page === undefined
                    ? undefined
                    : { to: `${page}?tab=arrivals`, text: t('marketdata.market.laws.open') }
            }
        >
            <SummaryLines lines={lawLines(law, t)} />
        </SummaryTag>
    );
}
