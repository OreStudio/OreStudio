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
import { cx } from '../../../ui/Primitives.js';

/** The small glyphs the market's cards and tabs carry; the product's own icon set has none of these. */
export type GlyphName =
    'pace' | 'force' | 'log' | 'settings' | 'arrivals' | 'laws' | 'fit' | 'health';

const PATHS: Readonly<Record<GlyphName, ReactNode>> = {
    pace: <path d="M1 12l4-5 3 3 4-6 3 4" fill="none" stroke="currentColor" strokeWidth="1.6" />,
    force: <path d="M9 1L3 9h4l-1 6 6-8H8z" fill="currentColor" />,
    log: <path d="M2 3h12M2 8h12M2 13h8" fill="none" stroke="currentColor" strokeWidth="1.6" />,
    settings: (
        <g fill="none" stroke="currentColor" strokeWidth="1.6">
            <path d="M3 2v12M8 2v12M13 2v12" />
            <circle cx="3" cy="10" r="1.6" fill="currentColor" />
            <circle cx="8" cy="5" r="1.6" fill="currentColor" />
            <circle cx="13" cy="9" r="1.6" fill="currentColor" />
        </g>
    ),
    arrivals: (
        <g fill="currentColor">
            <rect x="1" y="3" width="1.4" height="2.5" />
            <rect x="5" y="3" width="1.4" height="2.5" />
            <rect x="7" y="3" width="1.4" height="2.5" />
            <rect x="13" y="3" width="1.4" height="2.5" />
            <rect x="2" y="10" width="1.4" height="2.5" />
            <rect x="9" y="10" width="1.4" height="2.5" />
            <rect x="10.6" y="10" width="1.4" height="2.5" />
            <rect x="12.2" y="10" width="1.4" height="2.5" />
        </g>
    ),
    laws: (
        <path d="M3 13c2-9 4-9 5-5s3 4 5-5" fill="none" stroke="currentColor" strokeWidth="1.6" />
    ),
    fit: (
        <g fill="currentColor" stroke="currentColor" strokeWidth="1.6">
            <path d="M2 14L14 2" fill="none" />
            <circle cx="4" cy="11" r="1" />
            <circle cx="9" cy="6" r="1" />
        </g>
    ),
    health: (
        <g fill="none" stroke="currentColor" strokeWidth="1.6">
            <circle cx="8" cy="8" r="6" />
            <path d="M8 4v4l3 2" />
        </g>
    ),
};

export function Glyph({
    name,
    className,
}: {
    readonly name: GlyphName;
    readonly className?: string;
}): ReactNode {
    return (
        <svg aria-hidden viewBox="0 0 16 16" className={cx('size-4 shrink-0', className)}>
            {PATHS[name]}
        </svg>
    );
}

/**
 * One card of the market screens: a glyph, an all-caps title, a line that says what the card
 * is for, and an action on the right. Every card on the market's tabs is this one, so they read
 * as one console.
 */
export function MarketCard({
    title,
    lead,
    glyph,
    tone = 'accent',
    action,
    label,
    children,
    className,
}: {
    readonly title: string;
    readonly lead?: string;
    readonly glyph?: GlyphName;
    readonly tone?: 'accent' | 'warn';
    readonly action?: ReactNode;
    /** The region's name for a reader, when it differs from the title. */
    readonly label?: string;
    readonly children: ReactNode;
    readonly className?: string;
}): ReactNode {
    return (
        <section
            aria-label={label ?? title}
            className={cx(
                'grid min-w-0 content-start gap-3.5 rounded-tile border border-line bg-surface-raised p-4',
                className,
            )}
        >
            <header className="flex items-start gap-3 border-b border-line-subtle pb-3">
                {glyph === undefined ? null : (
                    <span
                        className={cx(
                            'grid size-7 shrink-0 place-items-center rounded-md border',
                            tone === 'warn'
                                ? 'border-warn/35 bg-warn/10 text-warn'
                                : 'border-accent-line bg-accent-dim text-accent',
                        )}
                    >
                        <Glyph name={glyph} className="size-[15px]" />
                    </span>
                )}
                <div className="min-w-0">
                    <h2 className="font-mono text-xs font-bold tracking-wider uppercase">
                        {title}
                    </h2>
                    {lead === undefined ? null : (
                        <p className="mt-0.5 text-[13px] leading-snug text-ink-muted">{lead}</p>
                    )}
                </div>
                {action === undefined ? null : (
                    <div className="ml-auto flex shrink-0 items-center gap-2">{action}</div>
                )}
            </header>
            {children}
        </section>
    );
}

/** The all-caps mono label above a figure or a control. */
export function MicroLabel({ children }: { readonly children: ReactNode }): ReactNode {
    return (
        <span className="font-mono text-micro font-semibold tracking-label text-ink-faint uppercase">
            {children}
        </span>
    );
}

/**
 * A segmented control: one choice of a few, or a few switches, in one pill. The pressed
 * segment takes the accent's tint, as the prototype draws it.
 */
export function Segmented<T extends string>({
    label,
    options,
    value,
    onChange,
    titleOf,
}: {
    readonly label: string;
    readonly options: readonly T[];
    /** The one chosen, or the set switched on. */
    readonly value: T | ReadonlySet<T>;
    readonly onChange: (option: T) => void;
    readonly titleOf: (option: T) => string;
}): ReactNode {
    const many = typeof value !== 'string';
    return (
        <span
            role={many ? 'group' : 'radiogroup'}
            aria-label={label}
            className="inline-flex gap-0.5 rounded-md border border-line bg-bg-primary p-[3px]"
        >
            {options.map((option) => {
                const on = many ? value.has(option) : value === option;
                return (
                    <button
                        key={option}
                        type="button"
                        role={many ? undefined : 'radio'}
                        aria-checked={many ? undefined : on}
                        aria-pressed={many ? on : undefined}
                        onClick={() => onChange(option)}
                        className={cx(
                            'rounded px-3 py-1 font-mono text-xs font-semibold',
                            on
                                ? 'bg-accent-dim text-accent shadow-[inset_0_0_0_1px_var(--color-accent-line)]'
                                : 'text-ink-muted hover:text-ink',
                        )}
                    >
                        {titleOf(option)}
                    </button>
                );
            })}
        </span>
    );
}

/** Key and value rows on the sunk ground, for the facts a card states under its figure. */
export function FactRows({
    rows,
}: {
    readonly rows: readonly (readonly [string, ReactNode])[];
}): ReactNode {
    return (
        <dl className="grid gap-1.5 rounded-md border border-line-subtle bg-bg-primary px-3 py-2.5 font-mono text-xs">
            {rows.map(([key, value]) => (
                <div key={key} className="flex justify-between gap-3">
                    <dt className="tracking-wide text-ink-faint uppercase">{key}</dt>
                    <dd className="text-right">{value}</dd>
                </div>
            ))}
        </dl>
    );
}
