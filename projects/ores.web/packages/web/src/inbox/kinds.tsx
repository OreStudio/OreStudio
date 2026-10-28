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
import type { InboxRequestView, RequestChange } from '@ores/wire-protocol/browser';
import type { InterpolationValues } from '../i18n/translate.js';
import { CurvePublishExtras } from './CurvePublishExtras.js';
import { RoleRequestExtras } from './RoleRequestExtras.js';
import { kindLabel } from './words.js';
import { Tag } from '../ui/Primitives.js';

/**
 * How the shared request screens draw one kind of request.
 *
 * The queue, the list of a person's own requests, the request page, the story
 * and the review panel are the same screens for every kind. A kind adds a
 * descriptor here and nothing else on the web: its name, the line that stands for
 * it in a list, the words for its fields, and where its subject lives. A kind
 * with something of its own to show, such as the bundle a role would grant, adds
 * a section.
 *
 * A kind with no descriptor is still drawn, with its code for a name and its
 * fields as the server named them, because a request nobody can name is still a
 * request somebody has to answer.
 */

export type Translate = (key: string, values?: InterpolationValues) => string;

/** What a descriptor reads of a request: its kind, the roles it names and its subject. */
export type Summarised = Pick<InboxRequestView, 'kindCode' | 'roles'> & {
    readonly publishedSubject?: InboxRequestView['publishedSubject'];
};

export interface KindDescriptor {
    readonly code: string;
    /** The kind's name. */
    readonly label: (t: Translate) => string;
    /** The line that stands for a request in a list and in a title. */
    readonly summary: (t: Translate, request: Summarised) => string;
    /**
     * What the queue draws in its Asked for column, for a kind whose subject is
     * more than a phrase: a badge, a code and the facts of the subject. A kind
     * without one draws its summary.
     */
    readonly subjectCell?: (t: Translate, request: InboxRequestView) => ReactNode;
    /** Whether the Asked for column gives this kind's subject the wider room. */
    readonly wideSubject?: boolean;
    /** The words on the button that approves a request of this kind. */
    readonly approveLabel: (t: Translate, request: Pick<InboxRequestView, 'roles'>) => string;
    /** The page a line's subject lives on, or null when the subject has none. */
    readonly subjectRoute: (change: RequestChange) => string | null;
    /** A field's name in the person's language, or the name itself. */
    readonly fieldLabel: (t: Translate, name: string) => string;
    /** Whether the page draws the lines as a delta. A kind that draws its own does not. */
    readonly drawsDelta: boolean;
    /** What this kind shows that no other does, drawn below the reason. */
    readonly Extras?: (props: { readonly request: InboxRequestView }) => ReactNode;
}

/** A field's name through the kind's own words, or the name when it has none. */
function fieldLabelUnder(prefix: string): KindDescriptor['fieldLabel'] {
    return (t, name) => {
        const key = `inbox.field.${prefix}.${name}`;
        const label = t(key);
        return label === key ? name : label;
    };
}

/** The roles a request names, or the kind's name when the reader may not read them. */
function roleNames(request: Pick<InboxRequestView, 'roles'>): string {
    return request.roles
        .map((role) => role.name)
        .filter((name) => name !== '')
        .join(', ');
}

const ROLE_GRANT: KindDescriptor = {
    code: 'iam.role_grant',
    label: (t) => kindLabel(t, 'iam.role_grant'),
    summary: (t, request) => roleNames(request) || kindLabel(t, 'iam.role_grant'),
    approveLabel: (t, request) =>
        t('inbox.request.give', {
            role: roleNames(request) || kindLabel(t, 'iam.role_grant'),
        }),
    subjectRoute: () => null,
    fieldLabel: fieldLabelUnder('iam.role_grant'),
    drawsDelta: false,
    Extras: RoleRequestExtras,
};

const BOOK_CHANGE: KindDescriptor = {
    code: 'refdata.book_change',
    label: (t) => kindLabel(t, 'refdata.book_change'),
    summary: (t) => kindLabel(t, 'refdata.book_change'),
    approveLabel: (t) => t('inbox.request.approve'),
    subjectRoute: () => null,
    fieldLabel: fieldLabelUnder('refdata.book_change'),
    drawsDelta: true,
};

/**
 * The subject of a curve publish, as the queue draws it: the kind beside the
 * curve, and the party and the as-of under it.
 *
 * A publish request's subject is three facts and not a phrase, so the row takes
 * two lines for this kind and one for every other. The subject is the market data
 * component's read and not the request's own words; a reader who may not read it
 * sees the kind alone, which is still the truth.
 */
function curvePublishSubject(t: Translate, request: InboxRequestView): ReactNode {
    const subject = request.publishedSubject ?? null;
    return (
        <span className="block">
            <span className="flex flex-wrap items-center gap-2">
                <Tag tone="accent" small>
                    <span className="font-mono">{request.kindCode}</span>
                </Tag>
                {subject !== null && <span className="font-mono">{subject.curve}</span>}
            </span>
            {subject !== null && (
                <span className="mt-0.5 block text-xs text-ink-muted">
                    {t('inbox.curvePublish.row', {
                        party: subject.party,
                        asOf: subject.as_of,
                        points: String(subject.point_count),
                    })}
                </span>
            )}
        </span>
    );
}

const CURVE_PUBLISH: KindDescriptor = {
    code: 'marketdata.curve_publish',
    label: (t) => kindLabel(t, 'marketdata.curve_publish'),
    summary: (t, request) => {
        const subject = request.publishedSubject ?? null;
        if (subject === undefined || subject === null || subject.curve === '') {
            return kindLabel(t, 'marketdata.curve_publish');
        }
        return t('inbox.curvePublish.summary', {
            curve: subject.curve,
            party: subject.party,
            asOf: subject.as_of,
        });
    },
    approveLabel: (t) => t('inbox.curvePublish.approve'),
    subjectCell: curvePublishSubject,
    wideSubject: true,
    subjectRoute: () => null,
    fieldLabel: fieldLabelUnder('marketdata.curve_publish'),
    drawsDelta: false,
    Extras: CurvePublishExtras,
};

/**
 * A change to a number-changing group of engine run settings.
 *
 * The lines are the settings of the group beside the values in force, which the
 * reporting service reads, so the kind draws the shared delta and adds nothing.
 */
const ENGINE_SETTINGS_CHANGE: KindDescriptor = {
    code: 'reporting.engine_settings_change',
    label: (t) => kindLabel(t, 'reporting.engine_settings_change'),
    summary: (t) => kindLabel(t, 'reporting.engine_settings_change'),
    approveLabel: (t) => t('inbox.request.approve'),
    subjectRoute: () => null,
    fieldLabel: fieldLabelUnder('reporting.engine_settings_change'),
    drawsDelta: true,
};

const KINDS: Readonly<Record<string, KindDescriptor>> = {
    [ROLE_GRANT.code]: ROLE_GRANT,
    [BOOK_CHANGE.code]: BOOK_CHANGE,
    [CURVE_PUBLISH.code]: CURVE_PUBLISH,
    [ENGINE_SETTINGS_CHANGE.code]: ENGINE_SETTINGS_CHANGE,
};

/** The descriptor of a kind, or a plain one for a kind this build does not know. */
export function descriptorFor(code: string): KindDescriptor {
    return (
        KINDS[code] ?? {
            code,
            label: (t) => kindLabel(t, code),
            summary: (t) => kindLabel(t, code),
            approveLabel: (t) => t('inbox.request.approve'),
            subjectRoute: () => null,
            fieldLabel: (_t, name) => name,
            drawsDelta: true,
        }
    );
}

/** The line that stands for a request, whichever kind it is. */
export function summaryOf(t: Translate, request: Summarised): string {
    return descriptorFor(request.kindCode).summary(t, request);
}

/** The kinds this build draws specially, for a test that every one is described. */
export const DESCRIBED_KINDS: readonly string[] = Object.keys(KINDS);
