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

import { useId, useState, type KeyboardEvent, type ReactNode } from 'react';
import type { ReportingTreeNode } from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { Input } from '../ui/Primitives.js';
import { NodeAvatar } from './NodeParts.js';

/** How many matches the list shows, so a large tenant does not draw every person. */
const SHOWN = 50;

/** The name a person is listed by: their full name, or their username when they have none. */
function nameOf(node: ReportingTreeNode): string {
    return node.fullName === '' ? node.username : node.fullName;
}

/**
 * The people a text matches, by name, in name order.
 *
 * The text matches the name, the username and the job title, anywhere in them
 * and in any case. An empty text matches everyone, and a large tenant is cut
 * to the first few so the list stays a list.
 */
export function matchPeople(
    nodes: readonly ReportingTreeNode[],
    text: string,
): readonly ReportingTreeNode[] {
    const wanted = text.trim().toLowerCase();
    return [...nodes]
        .sort((a, b) => nameOf(a).localeCompare(nameOf(b)))
        .filter(
            (node) =>
                wanted === '' ||
                nameOf(node).toLowerCase().includes(wanted) ||
                node.username.toLowerCase().includes(wanted) ||
                node.jobTitle.toLowerCase().includes(wanted),
        )
        .slice(0, SHOWN);
}

/**
 * A field you type a person's name into, with the people who match listed
 * under it.
 *
 * It shows the chosen person's name while you are not typing, and an empty
 * field with every person listed while you are, so it reads as a search box
 * and not as a pick list. The text matches the name, the username and the job
 * title. The arrow keys move through the matches and Enter chooses one.
 */
export function PersonSearch({
    nodes,
    value,
    onChange,
}: {
    readonly nodes: readonly ReportingTreeNode[];
    readonly value: string;
    readonly onChange: (accountId: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const listId = useId();
    const [typing, setTyping] = useState(false);
    const [text, setText] = useState('');
    const [active, setActive] = useState(0);

    const chosen = nodes.find((node) => node.accountId === value);
    const matches = matchPeople(nodes, text);

    const choose = (node: ReportingTreeNode | undefined): void => {
        if (node !== undefined) onChange(node.accountId);
        setTyping(false);
        setText('');
    };
    const onKey = (event: KeyboardEvent): void => {
        if (event.key === 'ArrowDown') {
            event.preventDefault();
            setActive(Math.min(active + 1, matches.length - 1));
        } else if (event.key === 'ArrowUp') {
            event.preventDefault();
            setActive(Math.max(active - 1, 0));
        } else if (event.key === 'Enter') {
            event.preventDefault();
            choose(matches[active]);
        } else if (event.key === 'Escape') {
            setTyping(false);
            setText('');
        }
    };

    return (
        <div className="relative max-w-sm">
            <Input
                type="text"
                role="combobox"
                aria-expanded={typing}
                aria-controls={listId}
                aria-autocomplete="list"
                aria-label={t('membership.reporting.findPerson')}
                placeholder={
                    chosen === undefined ? t('membership.reporting.findPerson') : nameOf(chosen)
                }
                value={typing ? text : chosen === undefined ? '' : nameOf(chosen)}
                onFocus={(event) => {
                    setTyping(true);
                    setText('');
                    setActive(0);
                    event.currentTarget.select();
                }}
                onChange={(event) => {
                    setTyping(true);
                    setText(event.target.value);
                    setActive(0);
                }}
                onBlur={() => {
                    setTyping(false);
                    setText('');
                }}
                onKeyDown={onKey}
            />
            {typing && (
                <ul
                    id={listId}
                    role="listbox"
                    className="absolute z-20 mt-1 max-h-72 w-full overflow-auto rounded-md border border-line bg-surface-overlay py-1 shadow-lg"
                >
                    {matches.length === 0 && (
                        <li className="px-3 py-2 text-sm text-ink-muted">
                            {t('access.nothingMatches')}
                        </li>
                    )}
                    {matches.map((node, index) => (
                        <li
                            key={node.accountId}
                            role="option"
                            aria-selected={node.accountId === value}
                            // A click would blur the field first and close the list before it lands.
                            onMouseDown={(event) => {
                                event.preventDefault();
                                choose(node);
                            }}
                            onMouseEnter={() => setActive(index)}
                            className={`flex cursor-pointer items-center gap-2 px-3 py-1.5 text-sm ${
                                index === active ? 'bg-surface-hover' : ''
                            }`}
                        >
                            <NodeAvatar node={node} size="sm" />
                            <span className="font-medium">{nameOf(node)}</span>
                            <span className="truncate text-xs text-ink-muted">{node.jobTitle}</span>
                        </li>
                    ))}
                </ul>
            )}
        </div>
    );
}
