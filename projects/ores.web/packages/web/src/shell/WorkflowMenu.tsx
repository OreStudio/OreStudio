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

import { useCallback, useRef, useState, type ReactNode } from 'react';
import { Link } from 'react-router';
import { useHolds } from '../access/holds.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button } from '../ui/Primitives.js';
import { useDismiss } from '../ui/useDismiss.js';
import { offered } from './areas.js';
import { cardOf, workflowsOn, type CardId } from './cards.js';

/**
 * The workflows a person may start from a screen, in a menu in its header.
 *
 * A workflow is listed on the screen of the thing it acts on, so a person who
 * is looking at the parties finds how to onboard one there. The menu lists only
 * the workflows the person holds the read or write for, and is not drawn when
 * there are none.
 */
export function WorkflowMenu({ on }: { readonly on: CardId }): ReactNode {
    const { t } = useTranslation();
    const holds = useHolds();
    const [open, setOpen] = useState(false);
    const root = useRef<HTMLDivElement>(null);
    const close = useCallback(() => setOpen(false), []);
    useDismiss(root, open, close);

    const workflows = workflowsOn(on)
        .map(cardOf)
        .filter((card) => offered(card, holds, 'application'));
    if (workflows.length === 0) {
        return null;
    }
    return (
        <div ref={root} className="relative">
            <Button aria-haspopup="menu" aria-expanded={open} onClick={() => setOpen(!open)}>
                {t('cardKind.workflows')} ▾
            </Button>
            <div
                role="menu"
                hidden={!open}
                className="absolute right-0 z-30 mt-2 w-72 rounded-md border border-line bg-surface-overlay p-1 shadow-xl"
            >
                {workflows.map((card) => (
                    <Link
                        key={card.to}
                        to={card.to}
                        role="menuitem"
                        onClick={close}
                        className="block rounded px-2 py-1.5 hover:bg-surface-hover"
                    >
                        <span className="block text-sm text-ink">{t(card.titleKey)}</span>
                        <span className="block text-xs text-ink-muted">{t(card.bodyKey)}</span>
                    </Link>
                ))}
            </div>
        </div>
    );
}
