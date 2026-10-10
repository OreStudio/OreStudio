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
import type { ReportingTreeNode, ReportingTreeParty } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { personPath } from '../access/PeoplePage.js';
import { roleLabel } from '../access/words.js';
import { Link } from 'react-router';
import { Avatar, imageUrl } from '../ui/Images.js';
import { Tag } from '../ui/Primitives.js';
import { partyLabel } from './organisation.js';

/** The role that makes a person their tenant's administrator. */
export const TENANT_ADMIN = 'TenantAdmin';

/** The name a person is shown by. */
export function nameOf(node: ReportingTreeNode): string {
    return node.fullName === '' ? node.username : node.fullName;
}

/**
 * A person's picture, drawn from the picture id the tree carries.
 *
 * The tree answers the id with the person, so a reader who may see the tree
 * needs no read of the account to draw them. Without a picture the person is
 * drawn as their initials.
 */
export function NodeAvatar({
    node,
    size = 'sm',
}: {
    readonly node: ReportingTreeNode;
    readonly size?: 'sm' | 'md' | 'lg';
}): ReactNode {
    return (
        <Avatar
            name={nameOf(node)}
            size={size}
            src={node.imageId === null ? null : imageUrl(node.imageId)}
        />
    );
}

/**
 * A person's name, opening their page only when the reader may read accounts.
 *
 * The page is an account read. A reader who holds the organisation read and not
 * that one sees the name as text rather than a link that would be refused.
 */
export function NameLink({
    node,
    openable,
    className,
    children,
}: {
    readonly node: ReportingTreeNode;
    readonly openable: boolean;
    readonly className?: string;
    readonly children?: ReactNode;
}): ReactNode {
    const content = children ?? nameOf(node);
    return openable ? (
        <Link to={personPath(node.username)} className={className}>
            {content}
        </Link>
    ) : (
        <span className={className}>{content}</span>
    );
}

/**
 * What marks a person out: the reader themselves, their tenant's administrator,
 * a manager the reader may not see, and the parties they work in.
 */
export function Badges({
    node,
    isMe,
    isAdmin,
    parties,
}: {
    readonly node: ReportingTreeNode;
    readonly isMe: boolean;
    readonly isAdmin: boolean;
    readonly parties: ReadonlyMap<string, ReportingTreeParty>;
}): ReactNode {
    const { t } = useTranslation();
    const mine = node.partyIds
        .map((id) => parties.get(id))
        .filter((party): party is ReportingTreeParty => party !== undefined);
    // One party is the whole answer, so naming it on everybody says nothing.
    const showParties = parties.size > 1 && mine.length > 0;
    if (!isMe && !isAdmin && !node.reportsOutsideScope && !showParties) return null;
    return (
        <span className="mt-1 flex flex-wrap gap-1">
            {isMe && <Tag tone="accent">{t('membership.reporting.you')}</Tag>}
            {isAdmin && <Tag tone="warn">{roleLabel(t, TENANT_ADMIN)}</Tag>}
            {node.reportsOutsideScope && (
                <Tag tone="muted">{t('membership.reporting.outside')}</Tag>
            )}
            {showParties &&
                mine.map((party) => (
                    <Tag key={party.partyId} tone="neutral" small>
                        {party.shortCode !== '' ? party.shortCode : partyLabel(party)}
                    </Tag>
                ))}
        </span>
    );
}
