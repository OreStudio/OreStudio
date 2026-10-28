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
import type { RecordRow } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { show, useRecords } from '../../refdata/records.js';
import { cx } from '../../ui/Primitives.js';
import { feedPageOf } from './feedTree.js';
import { engineName } from './IrProcessPanel.js';
import { PARAMETER_VALUES } from './irFeeds.js';
import { COMPONENTS } from './MixturePanel.js';
import { clean } from './market/numbers.js';
import { SummaryLines, SummaryTag, type SummaryLine } from './SummaryTag.js';

/** What a list knows of a feed: enough to name its process and to open it. */
export interface ProcessFeed {
    readonly id: string;
    readonly kind: string;
    readonly engine: string;
}

/** A process as a person names it: a curve's short-rate model, or a spot feed's engine. */
export function processName(
    t: (key: string) => string,
    feed: Pick<ProcessFeed, 'kind' | 'engine'>,
): string {
    return feed.kind === 'ir_curve'
        ? engineName(t, feed.engine)
        : t(`refdata.choices.process_type.${feed.engine}`);
}

/** What tells one process from another across feeds: the kind of feed and its engine. */
export function processKey(feed: Pick<ProcessFeed, 'kind' | 'engine'>): string {
    return `${feed.kind}:${feed.engine}`;
}

/** The tab of a feed's page that edits its process. */
export function processTab(kind: string): string {
    return kind === 'ir_curve' ? 'process' : 'behaviour';
}

function numberOf(row: RecordRow, field: string): number {
    return Number(row[field]);
}

/** A mixture's components, in order: each one's weight, mean and deviation a tick. */
export function mixtureLines(components: readonly RecordRow[]): readonly SummaryLine[] {
    return [...components]
        .sort(
            (left, right) => numberOf(left, 'component_index') - numberOf(right, 'component_index'),
        )
        .map((row, index) => ({
            name: `#${String(index + 1)}`,
            value: `w ${clean(numberOf(row, 'weight'), 3)} · μ ${clean(numberOf(row, 'mean'), 3)} · σ ${clean(numberOf(row, 'stdev'), 3)}`,
        }));
}

/** A curve process's parameters, each named as its definition names it, in the definitions' order. */
export function curveLines(
    values: readonly RecordRow[],
    definitions: readonly RecordRow[],
): readonly SummaryLine[] {
    const byDefinition = new Map(values.map((row) => [show(row['parameter_definition_id']), row]));
    return definitions
        .filter((definition) => byDefinition.has(show(definition['id'])))
        .map((definition) => {
            const symbol = show(definition['symbol']);
            const name = show(definition['display_name']);
            const value = byDefinition.get(show(definition['id']));
            return {
                name: symbol === '' ? name : `${name} · ${symbol}`,
                value: clean(Number(value?.['parameter_value']), 4),
            };
        });
}

/** The parameters of one feed's process, read when this is drawn. */
function ProcessParameters({ feed }: { readonly feed: ProcessFeed }): ReactNode {
    const { t } = useTranslation();
    const curve = feed.kind === 'ir_curve';
    const parts = useRecords(curve ? PARAMETER_VALUES : COMPONENTS, feed.id);
    const definitions = useRecords('parameter-definitions');
    if (parts.data === undefined || (curve && definitions.data === undefined)) {
        return <p className="text-xs text-ink-faint">{t('common.loading')}</p>;
    }
    const lines = curve ? curveLines(parts.data, definitions.data ?? []) : mixtureLines(parts.data);
    if (lines.length === 0) {
        return <p className="text-xs text-ink-faint">{t('marketdata.processSummary.none')}</p>;
    }
    return <SummaryLines lines={lines} />;
}

/**
 * A feed's process as a tag: its name always, and its parameters when the pointer rests on it
 * or it is clicked. A list of a hundred feeds draws a hundred names from the list itself and
 * reads one feed's parameters only when a person asks for them.
 */
export function ProcessTag({
    feed,
    className,
}: {
    readonly feed: ProcessFeed;
    readonly className?: string;
}): ReactNode {
    const { t } = useTranslation();
    if (feed.engine === '') {
        return <span className={cx('text-ink-faint', className)}>—</span>;
    }
    const name = processName(t, feed);
    return (
        <SummaryTag
            name={name}
            label={t('marketdata.processSummary.title', { process: name })}
            className={className}
            link={
                feed.id === ''
                    ? undefined
                    : {
                          to: `${feedPageOf(feed)}?tab=${processTab(feed.kind)}`,
                          text: t('marketdata.processSummary.open'),
                      }
            }
        >
            {feed.id === '' ? null : <ProcessParameters feed={feed} />}
        </SummaryTag>
    );
}
