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

import type { ReactNode } from 'react';
import { Link } from 'react-router';
import type { InboxRequestView } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { publishedCurveComparisonPath } from '../shell/paths.js';
import { Detail, Notice, Tag } from '../ui/Primitives.js';
import { formatDateTime } from '../ui/Time.js';

/**
 * What only a curve publish request shows: the subject the request authorises,
 * and the way to the comparison it is decided on.
 *
 * A publish request is about a curve, a party and an as-of, and none of them is
 * in the request's own words: the market data component holds the subject,
 * keyed by the request's id, and the queue carries it here. The subject block
 * names the kind beside the curve, so a decider reads which kind of request it
 * is, and *Open the comparison* opens the published curve's record on the tab
 * that answers what the candidate changes.
 *
 * A request whose subject this reader may not read says so, rather than drawing
 * an empty block that reads like a request about nothing.
 */
export function CurvePublishExtras({ request }: { readonly request: InboxRequestView }): ReactNode {
    const { t, language } = useTranslation();
    const subject = request.publishedSubject ?? null;
    if (subject === null) {
        // A refused read and a read that found nothing are different facts: the
        // first says the person lacks the read, the second says the component
        // holds no set for this request. The BFF knows which one happened.
        return (
            <Notice tone="info">
                {request.publishedSubjectState === 'refused'
                    ? t('inbox.curvePublish.noSubjectRefused')
                    : t('inbox.curvePublish.noSubjectMissing')}
            </Notice>
        );
    }
    return (
        <section className="space-y-3 rounded-md border border-line bg-surface-raised p-4">
            <div className="flex flex-wrap items-center gap-2">
                <h2 className="text-sm font-semibold">{t('inbox.curvePublish.subject')}</h2>
                <Tag tone="accent" small>
                    <span className="font-mono">{request.kindCode}</span>
                </Tag>
            </div>
            <dl className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4">
                <Detail label={t('inbox.curvePublish.curve')} value={subject.curve} mono />
                <Detail label={t('inbox.curvePublish.party')} value={subject.party} />
                <Detail
                    label={t('inbox.curvePublish.asOf')}
                    value={formatDateTime(subject.as_of, language)}
                    mono
                />
                <Detail
                    label={t('inbox.curvePublish.points')}
                    value={String(subject.point_count)}
                    mono
                />
            </dl>
            <Link
                className="inline-block text-sm text-accent hover:text-accent-bright"
                to={publishedCurveComparisonPath(subject.curve, subject.party, subject.as_of)}
            >
                {t('inbox.curvePublish.openComparison')}
            </Link>
        </section>
    );
}
