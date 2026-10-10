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

import { useRef, useState, type ReactNode } from 'react';
import { Link } from 'react-router';
import type { ReportingTreeNode } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { personPath } from '../access/PeoplePage.js';
import { roleLabel } from '../access/words.js';
import { AccountPicture } from '../ui/Images.js';
import { Button, Tag } from '../ui/Primitives.js';

/** How far the chart zooms out and in, and by how much a click moves it. */
const MIN_ZOOM = 0.3;
const MAX_ZOOM = 2;
const ZOOM_STEP = 0.1;

/** The role that makes a person their tenant's administrator. */
const TENANT_ADMIN = 'TenantAdmin';

/**
 * The org chart: each person as a card above the people who report to them.
 *
 * It draws the same tree as the list, from the same read, and a card opens the
 * person it shows. The chart is as wide as its widest level, so it scrolls
 * sideways rather than squeezing the cards.
 */
export function OrgChart({
    roots,
    children,
    meId,
    admins,
}: {
    readonly roots: readonly ReportingTreeNode[];
    readonly children: ReadonlyMap<string, ReportingTreeNode[]>;
    readonly meId: string;
    readonly admins: ReadonlySet<string>;
}): ReactNode {
    const { t } = useTranslation();
    const [zoom, setZoom] = useState(1);
    const frame = useRef<HTMLDivElement>(null);
    const step = (change: number): void =>
        setZoom((current) =>
            Math.min(MAX_ZOOM, Math.max(MIN_ZOOM, Math.round((current + change) * 100) / 100)),
        );
    /*
     * The chart is as wide as its widest level, whatever the window. Fitting
     * measures the chart at the zoom it has, takes it back to full size, and
     * chooses the zoom at which it just fills the frame.
     */
    const fit = (): void => {
        const element = frame.current;
        if (element === null) return;
        const natural = element.scrollWidth / zoom;
        if (natural <= 0) return;
        setZoom(
            Math.min(
                MAX_ZOOM,
                Math.max(MIN_ZOOM, Math.floor((element.clientWidth / natural) * 100) / 100),
            ),
        );
    };
    return (
        <div>
            <div className="mb-3 flex items-center justify-end gap-2">
                <Button
                    size="sm"
                    aria-label={t('membership.reporting.zoomOut')}
                    title={t('membership.reporting.zoomOut')}
                    disabled={zoom <= MIN_ZOOM}
                    onClick={() => step(-ZOOM_STEP)}
                >
                    −
                </Button>
                <span className="w-12 text-center text-xs text-ink-muted tabular-nums">
                    {Math.round(zoom * 100)}%
                </span>
                <Button
                    size="sm"
                    aria-label={t('membership.reporting.zoomIn')}
                    title={t('membership.reporting.zoomIn')}
                    disabled={zoom >= MAX_ZOOM}
                    onClick={() => step(ZOOM_STEP)}
                >
                    +
                </Button>
                <Button size="sm" variant="ghost" onClick={fit}>
                    {t('membership.reporting.zoomFit')}
                </Button>
                <Button size="sm" variant="ghost" disabled={zoom === 1} onClick={() => setZoom(1)}>
                    {t('membership.reporting.zoomReset')}
                </Button>
            </div>
            <div ref={frame} className="orgchart overflow-auto pb-2">
                <div style={{ zoom }} className="w-max min-w-full">
                    <ul className="min-w-max">
                        {roots.map((node) => (
                            <Branch
                                key={node.accountId}
                                node={node}
                                children={children}
                                meId={meId}
                                admins={admins}
                            />
                        ))}
                    </ul>
                </div>
            </div>
        </div>
    );
}

function Branch({
    node,
    children,
    meId,
    admins,
}: {
    readonly node: ReportingTreeNode;
    readonly children: ReadonlyMap<string, ReportingTreeNode[]>;
    readonly meId: string;
    readonly admins: ReadonlySet<string>;
}): ReactNode {
    const { t } = useTranslation();
    const kids = children.get(node.accountId) ?? [];
    const isMe = node.accountId === meId;
    return (
        <li>
            <Link
                to={personPath(node.username)}
                className={`card inline-flex w-44 flex-col items-center gap-1 p-3 text-center hover:border-accent-line focus-visible:outline-accent ${
                    isMe ? 'ring-1 ring-accent' : ''
                }`}
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
            </Link>
            {kids.length > 0 && (
                <ul>
                    {kids.map((child) => (
                        <Branch
                            key={child.accountId}
                            node={child}
                            children={children}
                            meId={meId}
                            admins={admins}
                        />
                    ))}
                </ul>
            )}
        </li>
    );
}
