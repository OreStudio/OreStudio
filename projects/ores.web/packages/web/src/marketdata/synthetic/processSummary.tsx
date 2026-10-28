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

import { useEffect, useRef, useState, type ReactNode } from 'react';
import { Link } from 'react-router';
import type { RecordRow } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { show, useRecords } from '../../refdata/records.js';
import { cx } from '../../ui/Primitives.js';
import { feedPageOf } from './feedTree.js';
import { engineName } from './IrProcessPanel.js';
import { PARAMETER_VALUES } from './irFeeds.js';
import { COMPONENTS } from './MixturePanel.js';
import { clean } from './market/numbers.js';

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

/** The tab of a feed's page that edits its process. */
export function processTab(kind: string): string {
    return kind === 'ir_curve' ? 'process' : 'behaviour';
}

/** One line of a summary: what the number is, and its value. */
export interface ProcessLine {
    readonly name: string;
    readonly value: string;
}

function numberOf(row: RecordRow, field: string): number {
    return Number(row[field]);
}

/** A mixture's components, in order: each one's weight, mean and deviation a tick. */
export function mixtureLines(components: readonly RecordRow[]): readonly ProcessLine[] {
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
): readonly ProcessLine[] {
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
    return (
        <dl className="grid gap-1 font-mono text-xs">
            {lines.map((line) => (
                <div key={line.name} className="flex justify-between gap-4">
                    <dt className="text-ink-faint">{line.name}</dt>
                    <dd className="text-right tabular-nums">{line.value}</dd>
                </div>
            ))}
        </dl>
    );
}

/**
 * A feed's process as a tag: its name always, and its parameters when opened. A list of a
 * hundred feeds draws a hundred names from the list itself and reads one feed's parameters
 * only when a person asks for them.
 */
export function ProcessTag({
    feed,
    className,
}: {
    readonly feed: ProcessFeed;
    readonly className?: string;
}): ReactNode {
    const { t } = useTranslation();
    const [open, setOpen] = useState(false);
    const holder = useRef<HTMLSpanElement>(null);
    // One summary at a time: a click anywhere else, opening another tag included, or Escape
    // closes this one.
    useEffect(() => {
        if (!open) {
            return;
        }
        const away = (event: MouseEvent): void => {
            if (!holder.current?.contains(event.target as Node)) {
                setOpen(false);
            }
        };
        const escape = (event: KeyboardEvent): void => {
            if (event.key === 'Escape') {
                setOpen(false);
            }
        };
        document.addEventListener('mousedown', away);
        document.addEventListener('keydown', escape);
        return () => {
            document.removeEventListener('mousedown', away);
            document.removeEventListener('keydown', escape);
        };
    }, [open]);
    const name = feed.engine === '' ? '—' : processName(t, feed);
    return (
        <span ref={holder} className={cx('relative inline-block', className)}>
            <button
                type="button"
                aria-expanded={open}
                onClick={(event) => {
                    event.stopPropagation();
                    setOpen((was) => !was);
                }}
                className="rounded-chip border border-line px-1.5 font-mono text-[11px] text-ink-muted hover:border-line-strong hover:text-ink"
            >
                {name}
            </button>
            {open ? (
                <span
                    role="dialog"
                    aria-label={t('marketdata.processSummary.title', { process: name })}
                    className="absolute top-full left-0 z-30 mt-1 grid w-72 gap-2 rounded-md border border-line-strong bg-surface-overlay p-3 text-left shadow-xl"
                >
                    <span className="flex items-baseline justify-between gap-2">
                        <span className="font-mono text-xs font-bold tracking-wide uppercase">
                            {name}
                        </span>
                        <button
                            type="button"
                            aria-label={t('marketdata.processSummary.close')}
                            onClick={() => setOpen(false)}
                            className="text-ink-faint hover:text-ink"
                        >
                            ×
                        </button>
                    </span>
                    {feed.id === '' ? null : <ProcessParameters feed={feed} />}
                    {feed.id === '' ? null : (
                        <Link
                            to={`${feedPageOf(feed)}?tab=${processTab(feed.kind)}`}
                            className="text-xs text-accent hover:underline"
                        >
                            {t('marketdata.processSummary.open')}
                        </Link>
                    )}
                </span>
            ) : null}
        </span>
    );
}
