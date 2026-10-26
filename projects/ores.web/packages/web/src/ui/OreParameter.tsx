/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option) any later
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

import { useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { Detail } from './Primitives.js';
import type { EditingMode } from './EditingMode.js';

/**
 * The one way a screen draws an ORE parameter: its label and value, and the
 * documentation of it from the ore parameter contract, in Simple or Advanced.
 * The engine run settings and the product pricing screens both use this, so a
 * parameter reads the same wherever a person meets it.
 */

/** The documentation of one parameter. */
export interface ParameterDocumentation {
    readonly documented: boolean;
    readonly name: string;
    readonly description: string;
    readonly defaultText: string;
    readonly format: string;
    readonly introducedIn: string;
    readonly removedIn: string;
    readonly xpath: string;
}

/** The documentation panel: what the parameter means, and in Advanced its schema. */
export function ParameterDocumentationPanel({
    doc,
    mode,
}: {
    readonly doc: ParameterDocumentation;
    readonly mode: EditingMode;
}): ReactNode {
    const { t } = useTranslation();
    if (!doc.documented) {
        return (
            <div className="rounded-md bg-surface-overlay p-3 text-sm">
                {t('oreParameter.undocumented', { name: doc.name })}
            </div>
        );
    }
    return (
        <div className="space-y-2 rounded-md bg-surface-overlay p-3 text-sm">
            <p>{doc.description}</p>
            <p className="text-xs text-ink-faint">
                {t('oreParameter.defaultIs', { text: doc.defaultText })}
            </p>
            {mode === 'advanced' && (
                <dl className="grid gap-x-6 gap-y-2 border-t border-line-subtle pt-2 sm:grid-cols-2">
                    <Detail label={t('oreParameter.schema.xml')} value={doc.xpath} mono />
                    <Detail label={t('oreParameter.schema.format')} value={doc.format} />
                    <Detail label={t('oreParameter.schema.introduced')} value={doc.introducedIn} />
                    <Detail
                        label={t('oreParameter.schema.removed')}
                        value={doc.removedIn === '' ? '—' : doc.removedIn}
                    />
                </dl>
            )}
        </div>
    );
}

/**
 * The Documentation button under a parameter, and the panel it opens.
 *
 * A screen whose section opens every leaf at once passes `open` and `onToggle`
 * and owns the state; a screen that does not lets each leaf keep its own.
 */
export function ParameterDocuments({
    doc,
    mode,
    open: controlled,
    onToggle,
}: {
    readonly doc: ParameterDocumentation;
    readonly mode: EditingMode;
    readonly open?: boolean;
    readonly onToggle?: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const [own, setOwn] = useState(false);
    const open = controlled ?? own;
    return (
        <>
            <button
                type="button"
                aria-expanded={open}
                className="text-xs text-accent hover:text-accent-bright"
                onClick={() => (onToggle === undefined ? setOwn((before) => !before) : onToggle())}
            >
                {open ? t('oreParameter.hideDocumentation') : t('oreParameter.documentation')}
            </button>
            {open && <ParameterDocumentationPanel doc={doc} mode={mode} />}
        </>
    );
}

/** A filter a parameter list offers, drawn the same everywhere. */
export function FilterChip({
    pressed,
    onClick,
    children,
}: {
    readonly pressed: boolean;
    readonly onClick: () => void;
    readonly children: ReactNode;
}): ReactNode {
    return (
        <button
            type="button"
            aria-pressed={pressed}
            onClick={onClick}
            className={
                pressed
                    ? 'rounded-full border border-accent/60 bg-accent/15 px-3 py-1 text-xs text-ink'
                    : 'rounded-full border border-line px-3 py-1 text-xs text-ink-muted hover:bg-surface-hover'
            }
        >
            {children}
        </button>
    );
}

/**
 * The filters a list of leaves offers: only the values the document holds, and
 * only the leaves that change the numbers. The second is drawn only where the
 * caller can answer it, and the count names the leaves with the kind's noun.
 */
export function ParameterFilters({
    set,
    numbers,
    onSet,
    onNumbers,
    shown,
    total,
    noun,
}: {
    readonly set: boolean;
    readonly numbers?: boolean;
    readonly onSet: () => void;
    readonly onNumbers?: () => void;
    readonly shown: number;
    readonly total: number;
    readonly noun: string;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <div className="flex flex-wrap items-center gap-2">
            <FilterChip pressed={set} onClick={onSet}>
                {t('oreParameter.filters.set')}
            </FilterChip>
            {onNumbers !== undefined && (
                <FilterChip pressed={numbers === true} onClick={onNumbers}>
                    {t('oreParameter.filters.numbers')}
                </FilterChip>
            )}
            <span className="text-xs text-ink-faint">
                {t('oreParameter.filters.shown', {
                    shown: String(shown),
                    total: String(total),
                    noun,
                })}
            </span>
        </div>
    );
}
