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

import { useEffect, useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { Field, Input, cx } from '../ui/Primitives.js';

/** A field whose value differs from the saved one, tinted as a changed cell of the table is. */
export const CHANGED_FIELD = 'border-warn bg-warn/10 text-warn';

export function differs(a: number, b: number | undefined): boolean {
    return b !== undefined && Math.abs(a - b) > 1e-9 * Math.max(1, Math.abs(a), Math.abs(b));
}

/**
 * One answer of the simple view. The text is the person's until they leave the
 * field or press Enter, so a half-typed number never reaches the service.
 */
export function Answer({
    label,
    hint,
    unit,
    value,
    positive,
    signed = false,
    bps = false,
    disabled,
    changed,
    onCommit,
}: {
    readonly label: string;
    readonly hint: string;
    readonly unit: string;
    readonly value: number;
    /** Whether zero is refused as well as a negative number. */
    readonly positive: boolean;
    /** Whether a negative number is allowed. */
    readonly signed?: boolean;
    /** Whether a small percent is also said in basis points, in brackets. */
    readonly bps?: boolean;
    readonly disabled: boolean;
    /** Whether the value differs from the saved one. */
    readonly changed: boolean;
    readonly onCommit: (value: number) => void;
}): ReactNode {
    const { t } = useTranslation();
    const [text, setText] = useState(String(value));
    useEffect(() => setText(String(value)), [value]);
    const parsed = Number(text);
    const wrong =
        text.trim() === '' ||
        !Number.isFinite(parsed) ||
        (!signed && parsed < 0) ||
        (positive && parsed === 0);
    const commit = (): void => {
        if (!wrong && parsed !== value) {
            onCommit(parsed);
        }
    };
    return (
        <Field
            label={label}
            hint={hint}
            {...(wrong
                ? { error: t(positive ? 'editingMode.positive' : 'editingMode.number') }
                : {})}
        >
            <div className="flex items-center gap-2">
                <Input
                    className={cx('max-w-40 font-mono', changed && CHANGED_FIELD)}
                    inputMode="decimal"
                    value={text}
                    disabled={disabled}
                    aria-invalid={wrong}
                    onChange={(event) => setText(event.target.value)}
                    onBlur={commit}
                    onKeyDown={(event) => {
                        if (event.key === 'Enter') {
                            commit();
                        }
                    }}
                />
                <span className="text-sm text-ink-muted">
                    {unit}
                    {bps && !wrong && parsed !== 0 && Math.abs(parsed) < 1
                        ? ` (${String(Number((parsed * 100).toPrecision(4)))} ${t('editingMode.bps')})`
                        : ''}
                </span>
            </div>
        </Field>
    );
}

/** A row of ready answers, the one that matches the value marked. */
export function Presets({
    items,
    current,
    disabled,
    onPick,
}: {
    readonly items: readonly {
        readonly key: string;
        readonly value: number;
        readonly label: string;
    }[];
    readonly current: number;
    readonly disabled: boolean;
    readonly onPick: (value: number) => void;
}): ReactNode {
    return (
        <div className="flex flex-wrap gap-2">
            {items.map((item) => (
                <button
                    key={item.key}
                    type="button"
                    disabled={disabled}
                    aria-pressed={Math.abs(item.value - current) < 1e-9}
                    className={cx(
                        'rounded-full border px-3 py-0.5 text-xs disabled:opacity-50',
                        Math.abs(item.value - current) < 1e-9
                            ? 'border-accent/60 bg-accent/10 text-ink'
                            : 'border-line text-ink-muted hover:bg-surface-hover',
                    )}
                    onClick={() => onPick(item.value)}
                >
                    {item.label}
                </button>
            ))}
        </div>
    );
}

/** What the answers make of the model, in a sentence, set apart so it is found at a glance. */
export function Summary({ children }: { readonly children: ReactNode }): ReactNode {
    return (
        <p className="rounded-md border border-accent/30 bg-accent/5 px-3 py-2 text-sm text-ink">
            {children}
        </p>
    );
}
