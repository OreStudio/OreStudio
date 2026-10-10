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
import { useTranslation } from '../i18n/Provider.js';
import { LinkButton } from '../ui/Primitives.js';
import type { StatusTone } from './dashboardStatus.js';

/**
 * The parts every home dashboard is made of: a panel with a mark beside its
 * title, the one status at the top, and the mark itself. The system
 * administrator's home and the tenant administrator's home draw them, so the
 * two read as one family.
 */

/** How a panel states its status: the tone paints the mark, the words are the panel's. */
export interface PanelStatus {
    readonly tone: StatusTone;
    readonly text: string;
}

const MARK: Readonly<Record<StatusTone, { readonly glyph: string; readonly classes: string }>> = {
    ok: { glyph: '✓', classes: 'border-up/50 bg-up/10 text-up' },
    attention: { glyph: '!', classes: 'border-warn/50 bg-warn/10 text-warn' },
    quiet: { glyph: '–', classes: 'border-line bg-surface-base text-ink-faint' },
    pending: { glyph: '…', classes: 'border-line bg-surface-base text-ink-faint' },
};

function StatusMark({
    tone,
    label,
}: {
    readonly tone: StatusTone;
    /** What a screen reader says, since the mark itself is a glyph. */
    readonly label: string;
}): ReactNode {
    const mark = MARK[tone];
    return (
        <span
            role="img"
            aria-label={label}
            className={`grid h-5 w-5 shrink-0 place-items-center rounded-full border text-xs font-bold ${mark.classes}`}
        >
            {mark.glyph}
        </span>
    );
}

/** The one verdict for the whole installation, in the words every panel uses. */
export function StatusChip({ status }: { readonly status: PanelStatus }): ReactNode {
    const classes =
        status.tone === 'ok'
            ? 'border-up/50 bg-up/10 text-up'
            : 'border-warn/50 bg-warn/10 text-warn';
    return (
        <span
            className={`inline-flex items-center gap-2 rounded-full border px-3 py-1.5 text-xs font-medium ${classes}`}
        >
            <span aria-hidden="true">{MARK[status.tone].glyph}</span>
            {status.text}
        </span>
    );
}

/**
 * One dashboard panel: a title with a mark that says how it is doing, the
 * figures behind it, and a footer that says how old they are.
 *
 * A panel that is fine says so with its mark alone, because four sentences
 * saying the same thing take the room the figures need. A panel that needs the
 * person, or has not been read, says why in a line under its title.
 */
export function Panel({
    title,
    to,
    status,
    footer,
    children,
}: {
    readonly title: string;
    readonly to: string;
    readonly status: PanelStatus;
    readonly footer?: ReactNode;
    readonly children: ReactNode;
}): ReactNode {
    const { t } = useTranslation();

    return (
        <section className="card flex flex-col justify-between gap-4 p-5">
            <div className="space-y-4">
                <header className="flex flex-wrap items-center justify-between gap-2">
                    <div className="flex items-center gap-2">
                        <h2 className="text-sm font-semibold text-ink">{title}</h2>
                        <StatusMark tone={status.tone} label={status.text} />
                    </div>
                    <LinkButton to={to} size="sm">
                        {t('home.system.panels.open')}
                    </LinkButton>
                </header>
                {status.tone !== 'ok' && (
                    <p
                        className={`text-sm ${status.tone === 'attention' ? 'text-warn' : 'text-ink-muted'}`}
                    >
                        {status.text}
                    </p>
                )}
                {children}
            </div>
            {footer !== undefined && (
                <footer className="flex flex-wrap items-center justify-between gap-2 border-t border-line-subtle pt-3 text-xs text-ink-muted">
                    {footer}
                </footer>
            )}
        </section>
    );
}
