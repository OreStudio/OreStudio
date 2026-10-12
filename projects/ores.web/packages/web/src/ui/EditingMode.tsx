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
import { useSyncExternalStore, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { cx } from './Primitives.js';

/**
 * How a person edits the numbers of a model: Simple asks plain questions,
 * Advanced shows the model's own parameters. One choice holds for every screen
 * that offers both, so a person who picks Advanced once gets it everywhere.
 */
export type EditingMode = 'simple' | 'advanced';

const KEY = 'ores.editingMode';
const listeners = new Set<() => void>();
let remembered: EditingMode | undefined;

function stored(): EditingMode {
    if (remembered !== undefined) {
        return remembered;
    }
    try {
        return window.localStorage.getItem(KEY) === 'advanced' ? 'advanced' : 'simple';
    } catch {
        return 'simple';
    }
}

function subscribe(listener: () => void): () => void {
    listeners.add(listener);
    return () => listeners.delete(listener);
}

/** The editing mode and the way to change it. Simple until the person picks Advanced. */
export function useEditingMode(): readonly [EditingMode, (mode: EditingMode) => void] {
    const mode = useSyncExternalStore(subscribe, stored, () => 'simple' as const);
    const choose = (next: EditingMode): void => {
        remembered = next;
        try {
            window.localStorage.setItem(KEY, next);
        } catch {
            // The choice holds for this visit without storage.
        }
        listeners.forEach((listener) => listener());
    };
    return [mode, choose];
}

/** The Simple and Advanced switch, drawn the same on every screen that offers it. */
export function EditingModeToggle({
    mode,
    onChange,
}: {
    readonly mode: EditingMode;
    readonly onChange: (mode: EditingMode) => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <div
            role="radiogroup"
            aria-label={t('editingMode.label')}
            className="inline-flex overflow-hidden rounded-md border border-line text-sm"
        >
            {(['simple', 'advanced'] as const).map((candidate) => (
                <button
                    key={candidate}
                    type="button"
                    role="radio"
                    aria-checked={mode === candidate}
                    title={t(`editingMode.${candidate}Help`)}
                    className={cx(
                        'px-3 py-1',
                        mode === candidate
                            ? 'bg-accent/15 text-ink'
                            : 'text-ink-muted hover:bg-surface-hover',
                    )}
                    onClick={() => onChange(candidate)}
                >
                    {t(`editingMode.${candidate}`)}
                </button>
            ))}
        </div>
    );
}
