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
import { useTranslation } from '../../i18n/Provider.js';
import { cx } from '../../ui/Primitives.js';

/** One line of a summary: what the number is, and its value. */
export interface SummaryLine {
    readonly name: string;
    readonly value: string;
}

/** How long the pointer rests before a summary opens, and leaves before it closes. */
const OPEN_AFTER_MS = 200;
const CLOSE_AFTER_MS = 150;

/** A summary's lines, one name and value to a row. */
export function SummaryLines({ lines }: { readonly lines: readonly SummaryLine[] }): ReactNode {
    return (
        <dl className="grid gap-1 font-mono text-xs">
            {lines.map((line, index) => (
                <div key={`${String(index)}-${line.name}`} className="flex justify-between gap-4">
                    <dt className="text-ink-faint">{line.name}</dt>
                    <dd className="text-right tabular-nums">{line.value}</dd>
                </div>
            ))}
        </dl>
    );
}

/**
 * A name that shows its summary when the pointer rests on it, so a list reads as names and
 * the numbers stay one hover away. A click keeps the summary open until a click elsewhere,
 * Escape, or its close button. Given a trigger of its own, such as a link, the tag keeps
 * that trigger's click and opens on hover and on focus only. The body is drawn only while
 * the summary is open, so what it reads is read only when asked for.
 */
export function SummaryTag({
    name,
    label,
    trigger,
    link,
    className,
    children,
}: {
    readonly name: string;
    readonly label: string;
    readonly trigger?: ReactNode;
    readonly link?: { readonly to: string; readonly text: string } | undefined;
    readonly className?: string | undefined;
    readonly children?: ReactNode;
}): ReactNode {
    const { t } = useTranslation();
    const [hovered, setHovered] = useState(false);
    const [pinned, setPinned] = useState(false);
    const open = hovered || pinned;
    const holder = useRef<HTMLSpanElement>(null);
    const button = useRef<HTMLButtonElement>(null);
    const panel = useRef<HTMLSpanElement>(null);
    const timer = useRef<ReturnType<typeof setTimeout> | undefined>(undefined);
    const later = (after: number, then: () => void): void => {
        clearTimeout(timer.current);
        timer.current = setTimeout(then, after);
    };
    const shut = (): void => {
        clearTimeout(timer.current);
        setPinned(false);
        setHovered(false);
    };
    useEffect(() => () => clearTimeout(timer.current), []);
    useEffect(() => {
        if (!open) {
            return;
        }
        const away = (event: MouseEvent): void => {
            if (!holder.current?.contains(event.target as Node)) {
                shut();
            }
        };
        const escape = (event: KeyboardEvent): void => {
            if (event.key === 'Escape') {
                shut();
                button.current?.focus();
            }
        };
        document.addEventListener('mousedown', away);
        document.addEventListener('keydown', escape);
        return () => {
            document.removeEventListener('mousedown', away);
            document.removeEventListener('keydown', escape);
        };
    }, [open]);
    // A click opens the summary to be used, so focus moves into it and its link is next.
    useEffect(() => {
        if (pinned) {
            panel.current?.focus();
        }
    }, [pinned]);
    return (
        <span
            ref={holder}
            className={cx('relative inline-block max-w-full', className)}
            onMouseEnter={() => later(OPEN_AFTER_MS, () => setHovered(true))}
            onMouseLeave={() => later(CLOSE_AFTER_MS, () => setHovered(false))}
            onFocus={trigger === undefined ? undefined : () => setHovered(true)}
            onBlur={(event) => {
                if (!pinned && !holder.current?.contains(event.relatedTarget as Node | null)) {
                    setHovered(false);
                }
            }}
        >
            {trigger ?? (
                <button
                    ref={button}
                    type="button"
                    aria-expanded={open}
                    aria-haspopup="dialog"
                    onClick={(event) => {
                        event.stopPropagation();
                        if (pinned) {
                            shut();
                        } else {
                            clearTimeout(timer.current);
                            setPinned(true);
                        }
                    }}
                    className="block max-w-full truncate rounded-chip border border-line px-1.5 font-mono text-[11px] text-ink-muted hover:border-line-strong hover:text-ink"
                >
                    {name}
                </button>
            )}
            {open ? (
                // The padding, not a margin, spaces the card from its tag, so the pointer
                // crosses into the card without leaving the tag.
                <span className="absolute top-full left-0 z-30 pt-1">
                    <span
                        ref={panel}
                        role="dialog"
                        tabIndex={-1}
                        aria-label={label}
                        className="grid w-72 gap-2 rounded-md border border-line-strong bg-surface-overlay p-3 text-left font-sans whitespace-normal shadow-xl outline-none"
                    >
                        <span className="flex items-baseline justify-between gap-2">
                            <span className="font-mono text-xs font-bold tracking-wide uppercase">
                                {name}
                            </span>
                            <button
                                type="button"
                                aria-label={t('marketdata.processSummary.close')}
                                onClick={() => {
                                    shut();
                                    button.current?.focus();
                                }}
                                className="text-ink-faint hover:text-ink"
                            >
                                ×
                            </button>
                        </span>
                        {children}
                        {link === undefined ? null : (
                            <Link
                                to={link.to}
                                onClick={shut}
                                className="text-xs text-accent hover:underline"
                            >
                                {link.text}
                            </Link>
                        )}
                    </span>
                </span>
            ) : null}
        </span>
    );
}
