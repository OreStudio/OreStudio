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

import { useState, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { Button } from '../ui/Primitives.js';
import { OresmdEditorDialog } from './OresmdEditor.js';
import { OresmdViewDialog } from './OresmdView.js';

/**
 * An oresmd URL drawn as text with a button that opens it in a dialog.
 *
 * It is how a screen shows a URL it holds: the person reads the URL, and the
 * dialog shows what it says field by field. With `onApply` the dialog is the
 * editor and can hand back an edited URL. Without it the dialog is the read-only
 * view, which is right for a screen that keeps no URL of its own to change, such
 * as a feed, whose URL follows from its currencies and its curve.
 */
export function OresmdUrl({
    uri,
    onApply,
}: {
    readonly uri: string;
    readonly onApply?: (uri: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const [open, setOpen] = useState(false);
    if (uri === '') return '';
    return (
        <span className="flex flex-wrap items-center gap-2">
            <span className="break-all font-mono text-xs">{uri}</span>
            <Button type="button" size="sm" variant="ghost" onClick={() => setOpen(true)}>
                {t('oresmd.open')}
            </Button>
            {open &&
                (onApply === undefined ? (
                    <OresmdViewDialog url={uri} onClose={() => setOpen(false)} />
                ) : (
                    <OresmdEditorDialog
                        url={uri}
                        onApply={(edited) => {
                            setOpen(false);
                            onApply(edited);
                        }}
                        onClose={() => setOpen(false)}
                    />
                ))}
        </span>
    );
}
