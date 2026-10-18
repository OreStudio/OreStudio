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

import { useQuery } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { Link } from 'react-router';
import { useTranslation } from '../i18n/Provider.js';
import { Button } from '../ui/Primitives.js';
import { useActingPartyId } from '../session/SessionProvider.js';
import { seriesTargetOf } from './identity.js';
import { OresmdEditorDialog } from './OresmdEditor.js';
import { oresmdQueries } from './queries.js';
import { OresmdViewDialog } from './OresmdView.js';

const URL_CLASSES = 'break-all font-mono text-xs';

/**
 * An oresmd URL as a reference, drawn the way the app draws every other.
 *
 * A click on the URL goes to the object it names, so it is a real link: it opens
 * in a new tab and copies as a link. A series or a quote names a series the party
 * has. One that declares an axis opens on the Evolution page. One with no axis,
 * such as a spot rate, opens on the series page, which draws its values over time.
 * Otherwise the URL is plain text: a fixing has no page, and a series the party
 * does not have is not there. The server's parse and the party's series list say
 * which, so the link holds none of the grammar.
 *
 * Beside the URL a button opens the dialog. Where the screen stores the URL, with
 * `onApply`, it is Edit and opens the editor, and the edited URL goes back through
 * `onApply`. Where the screen keeps no URL to change, such as a feed whose URL
 * follows from its currencies and its curve, it is Inspect and opens the URL read
 * as its parts.
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
    const partyId = useActingPartyId();
    const parsed = useQuery({ ...oresmdQueries.parse(uri), enabled: uri !== '' });
    const schema = useQuery({ ...oresmdQueries.schema(), enabled: uri !== '' });
    const list = useQuery({
        ...oresmdQueries.seriesList(partyId ?? ''),
        enabled: uri !== '' && partyId !== undefined,
    });
    if (uri === '') return '';
    // The target is undecided until the codec and the party's series list answer,
    // and a URL that names no series has none. Both are drawn as plain text.
    const to =
        parsed.data === undefined || schema.data === undefined || list.data === undefined
            ? undefined
            : seriesTargetOf(uri, schema.data, parsed.data, list.data);
    if (onApply === undefined) return <OresmdUrlResolved uri={uri} to={to} />;
    return (
        <span className="flex flex-wrap items-center gap-2">
            <UrlText uri={uri} to={to} />
            <Button type="button" size="sm" variant="ghost" onClick={() => setOpen(true)}>
                {t('oresmd.edit')}
            </Button>
            {open && (
                <OresmdEditorDialog
                    url={uri}
                    onApply={(edited) => {
                        setOpen(false);
                        onApply(edited);
                    }}
                    onClose={() => setOpen(false)}
                />
            )}
        </span>
    );
}

/**
 * An oresmd URL whose target is already decided, with the Inspect button beside it.
 *
 * A list of records draws one of these a row. It decides the targets of its rows
 * together, in one read, so a grid does not ask the codec about a URI a row, and
 * this reference only draws what it was given.
 */
export function OresmdUrlResolved({
    uri,
    to,
}: {
    readonly uri: string;
    readonly to: string | undefined;
}): ReactNode {
    const { t } = useTranslation();
    const [open, setOpen] = useState(false);
    // A row that keeps no URL has nothing to read, so it draws no button either.
    if (uri === '') return '';
    return (
        <span className="flex flex-wrap items-center gap-2">
            <UrlText uri={uri} to={to} />
            <Button type="button" size="sm" variant="ghost" onClick={() => setOpen(true)}>
                {t('oresmd.inspect')}
            </Button>
            {open && <OresmdViewDialog url={uri} onClose={() => setOpen(false)} />}
        </span>
    );
}

/** The URL itself: a link where a page opens the series it names, and plain text otherwise. */
function UrlText({
    uri,
    to,
}: {
    readonly uri: string;
    readonly to: string | undefined;
}): ReactNode {
    return to === undefined ? (
        <span className={URL_CLASSES}>{uri}</span>
    ) : (
        <Link
            to={to}
            className={`${URL_CLASSES} underline decoration-line-strong underline-offset-2 hover:decoration-accent`}
        >
            {uri}
        </Link>
    );
}
