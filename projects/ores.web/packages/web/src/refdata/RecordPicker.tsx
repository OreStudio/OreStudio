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

import { useEffect, useId, useRef, useState, type KeyboardEvent, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { Icon } from '../ui/Icon.js';
import { Flag } from '../images/flags.js';

/** One choice of a picker: the value it writes, the words it shows, and its flag if it has one. */
export interface Choice {
    readonly value: string;
    readonly label: string;
    readonly image?: string | null;
}

/**
 * A picker over the records of another kind: the chosen record with its flag,
 * and a list to search, each choice with its flag. It is the flagged picker
 * the record screen standard asks for wherever a field names another record.
 */
export function RecordPicker({
    value,
    choices,
    onChange,
    disabled = false,
    optional = false,
    placeholder,
    label,
    onBlur,
}: {
    readonly value: string;
    readonly choices: readonly Choice[];
    readonly onChange: (value: string) => void;
    readonly disabled?: boolean;
    readonly optional?: boolean;
    readonly placeholder: string;
    readonly label: string;
    readonly onBlur?: (() => void) | undefined;
}): ReactNode {
    const { t } = useTranslation();
    const listId = useId();
    const root = useRef<HTMLDivElement>(null);
    const [open, setOpen] = useState(false);
    const [search, setSearch] = useState('');
    const [active, setActive] = useState(0);
    const chosen = choices.find((choice) => choice.value === value);
    const wanted = search.trim().toLowerCase();
    const shown = [
        ...(optional ? [{ value: '', label: '—' }] : []),
        ...choices.filter(
            (choice) =>
                wanted === '' ||
                choice.label.toLowerCase().includes(wanted) ||
                choice.value.toLowerCase().includes(wanted),
        ),
    ];

    useEffect(() => {
        if (!open) {
            return undefined;
        }
        const close = (event: MouseEvent): void => {
            if (root.current !== null && !root.current.contains(event.target as Node)) {
                setOpen(false);
                onBlur?.();
            }
        };
        document.addEventListener('mousedown', close);
        return () => document.removeEventListener('mousedown', close);
    }, [open, onBlur]);

    const choose = (choice: Choice | undefined): void => {
        if (choice !== undefined) {
            onChange(choice.value);
        }
        setOpen(false);
        setSearch('');
        onBlur?.();
    };
    const onKey = (event: KeyboardEvent): void => {
        if (event.key === 'ArrowDown') {
            event.preventDefault();
            setActive(Math.min(active + 1, shown.length - 1));
        } else if (event.key === 'ArrowUp') {
            event.preventDefault();
            setActive(Math.max(active - 1, 0));
        } else if (event.key === 'Enter') {
            event.preventDefault();
            choose(shown[active]);
        } else if (event.key === 'Escape') {
            setOpen(false);
        }
    };

    return (
        <div ref={root} className="relative">
            <button
                type="button"
                disabled={disabled}
                aria-label={label}
                aria-haspopup="listbox"
                aria-expanded={open}
                aria-controls={listId}
                className="flex w-full items-center gap-2 rounded-md border border-line bg-surface-base px-3 py-2 text-left text-sm text-ink transition-colors duration-100 hover:border-line-strong focus:border-accent focus:outline-none focus:ring-3 focus:ring-accent/20 disabled:opacity-50"
                onClick={() => {
                    setActive(0);
                    setOpen(!open);
                }}
            >
                {chosen === undefined ? (
                    <span className="flex-1 truncate text-ink-faint">
                        {value === '' ? placeholder : value}
                    </span>
                ) : (
                    <>
                        <Flag src={chosen.image ?? null} />
                        <span className="flex-1 truncate">{chosen.label}</span>
                    </>
                )}
                <span aria-hidden className="text-ink-faint">
                    ▾
                </span>
            </button>
            {open && (
                <div className="absolute z-20 mt-1 w-full rounded-md border border-line bg-surface-raised shadow-lg">
                    <label className="relative block border-b border-line p-2">
                        <span className="pointer-events-none absolute inset-y-0 left-4 flex items-center text-ink-faint">
                            <Icon name="search" size={16} />
                        </span>
                        <input
                            autoFocus
                            value={search}
                            placeholder={t('refdata.records.search')}
                            aria-label={t('refdata.records.search')}
                            aria-controls={listId}
                            className="w-full rounded-md border border-line bg-surface-base py-1.5 pr-3 pl-8 text-sm text-ink focus:border-accent focus:outline-none"
                            onChange={(event) => {
                                setSearch(event.target.value);
                                setActive(0);
                            }}
                            onKeyDown={onKey}
                        />
                    </label>
                    <ul
                        id={listId}
                        role="listbox"
                        aria-label={label}
                        className="max-h-64 overflow-auto py-1"
                    >
                        {shown.length === 0 && (
                            <li className="px-3 py-1.5 text-sm text-ink-muted">
                                {t('refdata.records.noMatch')}
                            </li>
                        )}
                        {shown.map((choice, index) => (
                            <li
                                key={choice.value}
                                role="option"
                                aria-selected={choice.value === value}
                                className={`flex cursor-pointer items-center gap-2 px-3 py-1.5 text-sm ${index === active ? 'bg-surface-hover' : ''}`}
                                onMouseEnter={() => setActive(index)}
                                onMouseDown={(event) => {
                                    event.preventDefault();
                                    choose(choice);
                                }}
                            >
                                <Flag src={'image' in choice ? (choice.image ?? null) : null} />
                                <span className="flex-1 truncate">{choice.label}</span>
                            </li>
                        ))}
                    </ul>
                </div>
            )}
        </div>
    );
}
