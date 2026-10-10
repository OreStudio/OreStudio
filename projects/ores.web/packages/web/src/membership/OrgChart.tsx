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
import type { ReportingTreeNode } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { roleLabel } from '../access/words.js';
import { AccountPicture } from '../ui/Images.js';
import { Tag } from '../ui/Primitives.js';

/** The role that makes a person their tenant's administrator. */
const TENANT_ADMIN = 'TenantAdmin';

/**
 * The org chart: each person as a card above the people who report to them.
 *
 * It draws the same tree as the list, from the same read, and a card selects a
 * person the same way a row does. The chart is as wide as its widest level, so
 * it scrolls sideways rather than squeezing the cards.
 */
export function OrgChart({
    roots,
    children,
    selectedId,
    meId,
    admins,
    onSelect,
}: {
    readonly roots: readonly ReportingTreeNode[];
    readonly children: ReadonlyMap<string, ReportingTreeNode[]>;
    readonly selectedId: string;
    readonly meId: string;
    readonly admins: ReadonlySet<string>;
    readonly onSelect: (accountId: string) => void;
}): ReactNode {
    return (
        <div className="orgchart overflow-x-auto pb-2">
            <ul className="min-w-max">
                {roots.map((node) => (
                    <Branch
                        key={node.accountId}
                        node={node}
                        children={children}
                        selectedId={selectedId}
                        meId={meId}
                        admins={admins}
                        onSelect={onSelect}
                    />
                ))}
            </ul>
        </div>
    );
}

function Branch({
    node,
    children,
    selectedId,
    meId,
    admins,
    onSelect,
}: {
    readonly node: ReportingTreeNode;
    readonly children: ReadonlyMap<string, ReportingTreeNode[]>;
    readonly selectedId: string;
    readonly meId: string;
    readonly admins: ReadonlySet<string>;
    readonly onSelect: (accountId: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const kids = children.get(node.accountId) ?? [];
    const isMe = node.accountId === meId;
    const selected = node.accountId === selectedId;
    return (
        <li>
            <button
                type="button"
                onClick={() => onSelect(node.accountId)}
                aria-pressed={selected}
                className={`card inline-flex w-44 flex-col items-center gap-1 p-3 text-center hover:border-accent-line ${
                    selected ? 'border-accent' : ''
                } ${isMe ? 'ring-1 ring-accent' : ''}`}
            >
                <AccountPicture username={node.username} name={node.fullName} size="lg" />
                <span className={`text-sm ${isMe ? 'font-semibold' : 'font-medium'}`}>
                    {node.fullName}
                </span>
                <span className="text-xs text-ink-muted">{node.jobTitle}</span>
                {(isMe || admins.has(node.accountId)) && (
                    <span className="flex flex-wrap justify-center gap-1">
                        {isMe && <Tag tone="accent">{t('membership.reporting.you')}</Tag>}
                        {admins.has(node.accountId) && (
                            <Tag tone="warn">{roleLabel(t, TENANT_ADMIN)}</Tag>
                        )}
                    </span>
                )}
            </button>
            {kids.length > 0 && (
                <ul>
                    {kids.map((child) => (
                        <Branch
                            key={child.accountId}
                            node={child}
                            children={children}
                            selectedId={selectedId}
                            meId={meId}
                            admins={admins}
                            onSelect={onSelect}
                        />
                    ))}
                </ul>
            )}
        </li>
    );
}
