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
import { evolutionRoute, seriesOfUrl, seriesRoute, seriesVerdict } from './identity.js';
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
    const named =
        parsed.data === undefined || schema.data === undefined
            ? undefined
            : seriesOfUrl(schema.data, parsed.data);
    // A link is offered only where a page can open the series. Anything else is
    // text, because a link that ends in an apology is worse than no link.
    const verdict =
        named?.ok === true && list.data !== undefined
            ? seriesVerdict(list.data, named.form).kind
            : undefined;
    const target =
        verdict === 'shown'
            ? evolutionRoute(uri)
            : verdict === 'no-shape'
              ? seriesRoute(uri)
              : undefined;
    return (
        <span className="flex flex-wrap items-center gap-2">
            {target !== undefined ? (
                <Link
                    to={target}
                    className={`${URL_CLASSES} underline decoration-line-strong underline-offset-2 hover:decoration-accent`}
                >
                    {uri}
                </Link>
            ) : (
                <span className={URL_CLASSES}>{uri}</span>
            )}
            <Button type="button" size="sm" variant="ghost" onClick={() => setOpen(true)}>
                {t(onApply === undefined ? 'oresmd.inspect' : 'oresmd.edit')}
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
