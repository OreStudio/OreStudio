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
import type { ReactNode } from 'react';
import { useNavigate } from 'react-router';
import { ApiFailure } from '../api/transport.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Notice } from '../ui/Primitives.js';
import { OrgText } from './OrgText.js';
import { docKey, qaQueries } from './queries.js';

/** The route that shows a doc, by its id. The id is encoded, as the read of the doc encodes it. */
export function docRoute(id: string): string {
    return `/development/docs/${encodeURIComponent(id)}`;
}

export { docKey };

/**
 * A story, a task or a scenario, drawn from its org doc.
 *
 * An id link inside the doc opens the doc it points to. A link to an id the
 * runner cannot show says so: the server answers not found for an id no doc has
 * and for a doc of a kind it does not serve, and the tester cannot tell the two
 * apart and need not.
 */
export function DocPanel({
    id,
    onDocLink,
    onBack,
}: {
    readonly id: string;
    /** Where an id link goes. By default the app opens the doc on its own page. */
    readonly onDocLink?: (id: string) => void;
    /** Closes this doc and shows the one it was reached from. Absent for the first doc. */
    readonly onBack?: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const doc = useQuery(qaQueries.doc(id));
    const follow = onDocLink ?? ((target: string) => void navigate(docRoute(target)));
    const back =
        onBack === undefined ? null : (
            <Button type="button" size="sm" variant="ghost" onClick={onBack}>
                {t('development.docs.back')}
            </Button>
        );

    if (doc.isError) {
        const unresolved = doc.error instanceof ApiFailure && doc.error.code === 'not-found';
        return (
            <div className="space-y-3">
                {back}
                <Notice tone={unresolved ? 'warn' : 'error'}>
                    {unresolved ? t('development.docs.unresolved', { id }) : doc.error.message}
                </Notice>
            </div>
        );
    }
    if (doc.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    return (
        <section className="card space-y-3 p-4">
            {back}
            <div>
                <h2 className="text-base font-semibold text-ink">{doc.data.title}</h2>
                <p className="font-mono text-xs text-ink-faint">{doc.data.path}</p>
            </div>
            <OrgText source={doc.data.text} docHref={docRoute} onDocLink={follow} />
        </section>
    );
}
