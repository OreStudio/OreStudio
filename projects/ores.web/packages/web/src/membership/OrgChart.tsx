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
import type { ReportingTreeNode, ReportingTreeParty } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { Button } from '../ui/Primitives.js';
import { Badges, NameLink, NodeAvatar, PartyLine, nameOf } from './NodeParts.js';
import { partyLabel, type PartyBranch, type PartyForest } from './organisation.js';

/** How far the chart zooms out and in, and by how much a click moves it. */
const MIN_ZOOM = 0.3;
const MAX_ZOOM = 2;
const ZOOM_STEP = 0.1;

/** What the chart needs to mark a person out and to open them. */
interface Marks {
    readonly meId: string;
    readonly admins: ReadonlySet<string>;
    readonly parties: ReadonlyMap<string, ReportingTreeParty>;
    /** Whether a card opens the person's page, which is an account read. */
    readonly openable: boolean;
}

/**
 * The org chart: each person as a card above the people who report to them, or
 * each party as a box above the parties below it, with the reporting chart of
 * its people inside.
 *
 * It draws the same read as the list, and a person's card opens their page when
 * the reader may read accounts. The chart is as wide as its widest level, so it
 * scrolls sideways rather than squeezing the cards, and it zooms.
 */
export function OrgChart({
    reporting,
    party,
    marks,
}: {
    readonly reporting?: {
        readonly roots: readonly ReportingTreeNode[];
        readonly children: ReadonlyMap<string, ReportingTreeNode[]>;
    };
    readonly party?: PartyForest;
    readonly marks: Marks;
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
                        {reporting?.roots.map((node) => (
                            <Branch
                                key={node.accountId}
                                node={node}
                                children={reporting.children}
                                marks={marks}
                            />
                        ))}
                        {party?.branches.map((branch) => (
                            <PartyBox key={branch.party.partyId} branch={branch} marks={marks} />
                        ))}
                    </ul>
                    {party !== undefined && party.unaffiliated.length > 0 && (
                        <PeopleRow
                            title={t('membership.reporting.noParty')}
                            people={party.unaffiliated}
                            marks={marks}
                        />
                    )}
                </div>
            </div>
        </div>
    );
}

function Card({ node, marks }: { readonly node: ReportingTreeNode; readonly marks: Marks }) {
    const isMe = node.accountId === marks.meId;
    return (
        <NameLink
            node={node}
            openable={marks.openable}
            className={`card inline-flex w-44 flex-col items-center gap-1 p-3 text-center hover:border-accent-line focus-visible:outline-accent ${
                isMe ? 'ring-1 ring-accent' : ''
            }`}
        >
            <NodeAvatar node={node} size="lg" />
            <span className={`text-sm ${isMe ? 'font-semibold' : 'font-medium'}`}>
                {nameOf(node)}
            </span>
            <span className="text-xs text-ink-muted">{node.jobTitle}</span>
            <PartyLine node={node} parties={marks.parties} />
            <span className="flex justify-center">
                <Badges node={node} isMe={isMe} isAdmin={marks.admins.has(node.accountId)} />
            </span>
        </NameLink>
    );
}

function Branch({
    node,
    children,
    marks,
}: {
    readonly node: ReportingTreeNode;
    readonly children: ReadonlyMap<string, ReportingTreeNode[]>;
    readonly marks: Marks;
}): ReactNode {
    const kids = children.get(node.accountId) ?? [];
    return (
        <li>
            <Card node={node} marks={marks} />
            {kids.length > 0 && (
                <ul>
                    {kids.map((child) => (
                        <Branch
                            key={child.accountId}
                            node={child}
                            children={children}
                            marks={marks}
                        />
                    ))}
                </ul>
            )}
        </li>
    );
}

/** A party as a box: its name, the people who work in it, and the parties below. */
function PartyBox({
    branch,
    marks,
}: {
    readonly branch: PartyBranch;
    readonly marks: Marks;
}): ReactNode {
    return (
        <li>
            <div className="card inline-block min-w-48 p-3 text-center">
                <p className="text-sm font-semibold">{partyLabel(branch.party)}</p>
                {branch.party.shortCode !== '' && branch.party.name !== '' && (
                    <p className="font-mono text-[11px] text-ink-faint">{branch.party.shortCode}</p>
                )}
                <ul className="mt-2">
                    {branch.shape.roots.map((node) => (
                        <Branch
                            key={node.accountId}
                            node={node}
                            children={branch.shape.children}
                            marks={marks}
                        />
                    ))}
                </ul>
            </div>
            {branch.below.length > 0 && (
                <ul>
                    {branch.below.map((below) => (
                        <PartyBox key={below.party.partyId} branch={below} marks={marks} />
                    ))}
                </ul>
            )}
        </li>
    );
}

/** The people who work in no party, in a row of their own. */
function PeopleRow({
    title,
    people,
    marks,
}: {
    readonly title: string;
    readonly people: readonly ReportingTreeNode[];
    readonly marks: Marks;
}): ReactNode {
    return (
        <div className="mt-6 text-center">
            <p className="mb-2 text-sm font-semibold text-ink-muted">{title}</p>
            <div className="flex flex-wrap justify-center gap-2">
                {people.map((node) => (
                    <Card key={node.accountId} node={node} marks={marks} />
                ))}
            </div>
        </div>
    );
}
