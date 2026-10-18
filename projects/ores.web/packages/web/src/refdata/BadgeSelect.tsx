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

/**
 * The one control for choosing a value that is a catalogue code.
 *
 * A native select cannot draw its options as badges: the browser paints them,
 * and an option carries no markup to paint. So a list of coded values needs a
 * listbox of its own, and this is it — one control, shared by every screen that
 * offers a choice of codes, so the badges a person picks from are the same ones
 * the rest of the application shows.
 *
 * The option list is a component of its own (`BadgeOptionList`) because the
 * popup cannot be opened by a static render, and the drawing is what has to be
 * proved. Its tests render the list directly; the control's tests render the
 * closed control, which is all a static render can reach.
 */

import { useEffect, useRef, useState, type ReactNode } from 'react';
import { Label } from '../ui/Label.js';
import { badgeFor, type LabelCatalogue } from './shared.js';

/** One choice: the code it stores, and the words to fall back to. */
export interface BadgeChoice {
    readonly value: string;
    readonly label: string;
}

/** A code as its domain's badge, or as the code when the domain paints none. */
export function BadgeChoiceMark({
    code,
    domain,
    catalogue,
    fallback,
}: {
    readonly code: string;
    readonly domain: string;
    readonly catalogue: LabelCatalogue;
    /** What to draw when the code is empty, or the catalogue paints nothing. */
    readonly fallback?: string | undefined;
}): ReactNode {
    if (code === '') {
        return <span className="text-ink-faint">{fallback ?? '—'}</span>;
    }
    const badge = badgeFor(catalogue, domain, code);
    if (badge !== undefined) {
        return <Label text={badge.label} badge={badge} />;
    }
    return <>{code}</>;
}

/**
 * The choices, drawn as the badges their domain paints.
 *
 * Exported so a caller may draw the list where the control cannot go, and so
 * the drawing is testable without opening a popup.
 */
export function BadgeOptionList({
    choices,
    domain,
    catalogue,
    value,
    empty,
    onChoose,
}: {
    readonly choices: readonly BadgeChoice[];
    readonly domain: string;
    readonly catalogue: LabelCatalogue;
    readonly value: string;
    readonly empty?: string;
    readonly onChoose: (value: string) => void;
}): ReactNode {
    return (
        <ul role="listbox" className="card max-h-64 overflow-y-auto py-1 shadow-lg">
            {empty !== undefined && (
                <li
                    role="option"
                    aria-selected={value === ''}
                    className="cursor-pointer px-3 py-1.5 text-sm hover:bg-surface-hover"
                    onClick={() => onChoose('')}
                >
                    <span className="text-ink-faint">{empty}</span>
                </li>
            )}
            {choices.map((choice) => (
                <li
                    key={choice.value}
                    role="option"
                    aria-selected={choice.value === value}
                    className={`flex cursor-pointer items-center gap-2 px-3 py-1.5 text-sm hover:bg-surface-hover ${
                        choice.value === value ? 'bg-surface-hover' : ''
                    }`}
                    onClick={() => onChoose(choice.value)}
                >
                    <BadgeChoiceMark
                        code={choice.value}
                        domain={domain}
                        catalogue={catalogue}
                        fallback={choice.label}
                    />
                </li>
            ))}
        </ul>
    );
}

/**
 * A choice of coded values, each drawn as its domain's badge.
 *
 * The closed control draws the chosen value as a badge too, so the control and
 * its list agree about what a code looks like.
 */
export function BadgeSelect({
    label,
    domain,
    value,
    choices,
    catalogue,
    empty,
    disabled = false,
    onChange,
}: {
    readonly label: string;
    /** The code domain the choices belong to, such as `tenor_kind`. */
    readonly domain: string;
    readonly value: string;
    readonly choices: readonly BadgeChoice[];
    readonly catalogue: LabelCatalogue;
    readonly empty?: string;
    readonly disabled?: boolean;
    readonly onChange: (value: string) => void;
}): ReactNode {
    const [open, setOpen] = useState(false);
    const [active, setActive] = useState(-1);
    const box = useRef<HTMLDivElement | null>(null);

    /*
     * A popup that outlives the pointer is a popup nobody can close, so a press
     * anywhere else closes it. Registered only while open, so a closed control
     * adds no listener to the document.
     */
    useEffect(() => {
        if (!open) {
            return undefined;
        }
        const away = (event: MouseEvent): void => {
            if (box.current !== null && !box.current.contains(event.target as Node)) {
                setOpen(false);
            }
        };
        document.addEventListener('mousedown', away);
        return () => document.removeEventListener('mousedown', away);
    }, [open]);

    const choose = (chosen: string): void => {
        onChange(chosen);
        setOpen(false);
        setActive(-1);
    };

    const key = (event: { key: string; preventDefault: () => void }): void => {
        if (event.key === 'Escape') {
            setOpen(false);
            return;
        }
        if (event.key === 'ArrowDown' || event.key === 'ArrowUp') {
            event.preventDefault();
            if (!open) {
                setOpen(true);
                setActive(0);
                return;
            }
            const step = event.key === 'ArrowDown' ? 1 : -1;
            const next = active + step;
            setActive(next < 0 ? 0 : next >= choices.length ? choices.length - 1 : next);
            return;
        }
        if ((event.key === 'Enter' || event.key === ' ') && open && active >= 0) {
            const choice = choices[active];
            if (choice !== undefined) {
                choose(choice.value);
            }
        }
    };

    return (
        <div className="space-y-1" ref={box}>
            <span className="block text-xs text-ink-muted">{label}</span>
            <div className="relative">
                <button
                    type="button"
                    role="combobox"
                    aria-expanded={open}
                    aria-haspopup="listbox"
                    disabled={disabled}
                    className="flex w-full items-center justify-between gap-2 rounded-md border border-line bg-surface-base px-3 py-2 text-left text-sm text-ink hover:border-line-strong focus:border-accent focus:outline-none focus:ring-3 focus:ring-accent/20 disabled:opacity-50"
                    onClick={() => setOpen(!open)}
                    onKeyDown={key}
                >
                    <BadgeChoiceMark
                        code={value}
                        domain={domain}
                        catalogue={catalogue}
                        fallback={empty}
                    />
                    <span aria-hidden="true" className="text-ink-faint">
                        ▾
                    </span>
                </button>
                {open && (
                    <div className="absolute z-20 mt-1 w-full">
                        <BadgeOptionList
                            choices={choices}
                            domain={domain}
                            catalogue={catalogue}
                            value={value}
                            {...(empty === undefined ? {} : { empty })}
                            onChoose={choose}
                        />
                    </div>
                )}
            </div>
        </div>
    );
}
