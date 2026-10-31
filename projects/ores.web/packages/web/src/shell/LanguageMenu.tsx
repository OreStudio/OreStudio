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
import { useTranslation } from '../i18n/Provider.js';
import { LANGUAGE_INFO, type Language } from '../i18n/languages.js';
import { Icon } from '../ui/Icon.js';
import { useDismiss } from '../ui/useDismiss.js';

/**
 * The language menu in the header.
 *
 * A globe, which is the icon the set draws for a language, opening the
 * languages the interface speaks: each named in itself, because that is how a
 * person recognises the one they read, and the one in force marked. The choice
 * is the translation context's, so the menu only states it.
 *
 * It lives beside the brand rather than inside the account menu: which language
 * a screen is read in is a setting about the screen, not one of the person's own
 * pages, and a person who cannot read the page they are on will not look for it
 * behind their own picture.
 */
export function LanguageMenu(): ReactNode {
    const { t, language, setLanguage, languages } = useTranslation();
    const [open, setOpen] = useState(false);
    const root = useRef<HTMLDivElement>(null);
    const close = useCallback(() => {
        setOpen(false);
    }, []);
    useDismiss(root, open, close);

    const choose = (next: Language): void => {
        setLanguage(next);
        close();
    };

    return (
        <div ref={root} className="relative">
            <button
                type="button"
                aria-haspopup="menu"
                aria-expanded={open}
                aria-label={t('nav.language')}
                title={t('nav.language')}
                onClick={() => {
                    setOpen(!open);
                }}
                className="flex size-9 items-center justify-center rounded-full text-ink-muted hover:bg-surface-hover hover:text-ink focus-visible:outline focus-visible:outline-2 focus-visible:outline-accent"
            >
                <Icon name="language" />
            </button>
            <div
                role="menu"
                aria-label={t('nav.language')}
                hidden={!open}
                className="absolute right-0 z-30 mt-2 w-48 rounded-md border border-line bg-surface-overlay p-1 shadow-xl"
            >
                {languages.map((code) => (
                    <button
                        key={code}
                        type="button"
                        role="menuitemradio"
                        aria-checked={code === language}
                        lang={code}
                        onClick={() => {
                            choose(code);
                        }}
                        className={
                            code === language
                                ? 'flex w-full items-center justify-between gap-2 rounded px-2 py-1.5 text-left text-sm text-ink bg-surface-hover'
                                : 'flex w-full items-center justify-between gap-2 rounded px-2 py-1.5 text-left text-sm text-ink hover:bg-surface-hover'
                        }
                    >
                        <span>{LANGUAGE_INFO[code].name}</span>
                        {code === language && <Icon name="current" size={16} />}
                    </button>
                ))}
            </div>
        </div>
    );
}
