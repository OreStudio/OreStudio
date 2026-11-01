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
import { cutPath } from '../shell/paths.js';
import { Detail, Notice, Tag } from '../ui/Primitives.js';
import { formatDateTime } from '../ui/Time.js';

/**
 * What only a cut's authorisation request shows: the body the request froze, and
 * the way to the record that states it.
 *
 * A request is a header. The subject is the reporting component's own detail row,
 * keyed by the request's id, and the cut it names holds the label, the kind, the
 * instant and the scope. The counts are the ones the take took when it froze the
 * body, so the decider reads the size of what they sign and not the store's size
 * today.
 *
 * The body itself is read on the cut's own record, so the block points at it
 * rather than copying it: two copies of one act drift.
 */
export function CutAuthorisationExtras({
    request,
}: {
    readonly request: InboxRequestView;
}): ReactNode {
    const { t, language } = useTranslation();
    const subject = request.cutSubject ?? null;
    if (subject === null) {
        // A refused read and a read that found nothing are different facts, and
        // the BFF knows which one happened.
        return (
            <Notice tone="info">
                {request.cutSubjectState === 'refused'
                    ? t('inbox.cutAuthorisation.noSubjectRefused')
                    : t('inbox.cutAuthorisation.noSubjectMissing')}
            </Notice>
        );
    }
    return (
        <section className="space-y-3 rounded-md border border-line bg-surface-raised p-4">
            <div className="flex flex-wrap items-center gap-2">
                <h2 className="text-sm font-semibold">{t('inbox.cutAuthorisation.subject')}</h2>
                <Tag tone="accent" small>
                    <span className="font-mono">{request.kindCode}</span>
                </Tag>
                <span className="font-mono text-sm">{subject.label}</span>
            </div>
            <dl className="grid gap-3 sm:grid-cols-2 lg:grid-cols-4">
                <Detail label={t('inbox.cutAuthorisation.kind')} value={subject.kind_name} />
                <Detail
                    label={t('inbox.cutAuthorisation.timepoint')}
                    value={formatDateTime(subject.timepoint, language)}
                    mono
                />
                <Detail label={t('inbox.cutAuthorisation.scope')} value={subject.scope_code} mono />
                <Detail
                    label={t('inbox.cutAuthorisation.centre')}
                    value={subject.business_centre_code}
                    mono
                />
                <Detail
                    label={t('inbox.cutAuthorisation.deals')}
                    value={String(subject.deal_count)}
                    mono
                />
                <Detail
                    label={t('inbox.cutAuthorisation.sets')}
                    value={String(subject.market_data_set_count)}
                    mono
                />
                <Detail
                    label={t('inbox.cutAuthorisation.gaps')}
                    value={String(subject.gap_count)}
                    mono
                />
                <Detail
                    label={t('inbox.cutAuthorisation.state')}
                    value={t(`cuts.states.${subject.state}`)}
                />
            </dl>
            <Link
                className="inline-block text-sm text-accent hover:text-accent-bright"
                to={cutPath(subject.cut_id)}
            >
                {t('inbox.cutAuthorisation.openCut')}
            </Link>
        </section>
    );
}
