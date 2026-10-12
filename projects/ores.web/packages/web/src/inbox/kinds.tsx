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
import { RoleRequestExtras } from './RoleRequestExtras.js';
import { kindLabel } from './words.js';

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

export interface KindDescriptor {
    readonly code: string;
    /** The kind's name. */
    readonly label: (t: Translate) => string;
    /** The line that stands for a request in a list and in a title. */
    readonly summary: (t: Translate, request: Pick<InboxRequestView, 'roles'>) => string;
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

const KINDS: Readonly<Record<string, KindDescriptor>> = {
    [ROLE_GRANT.code]: ROLE_GRANT,
    [BOOK_CHANGE.code]: BOOK_CHANGE,
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
export function summaryOf(
    t: Translate,
    request: Pick<InboxRequestView, 'kindCode' | 'roles'>,
): string {
    return descriptorFor(request.kindCode).summary(t, request);
}

/** The kinds this build draws specially, for a test that every one is described. */
export const DESCRIBED_KINDS: readonly string[] = Object.keys(KINDS);
