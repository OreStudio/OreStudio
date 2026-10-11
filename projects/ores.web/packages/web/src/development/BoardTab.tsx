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
import { useState, type ReactNode } from 'react';
import { Link, useSearchParams } from 'react-router';
import type { AgileState, AgileStory, AgileTask, Board } from '@ores/contracts';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Input, Notice, Select, Tag, cx } from '../ui/Primitives.js';
import { columnOf, columnsOf, percentOfTasks, shownDay, visibleStories } from './board.js';
import { docRoute } from './DocPanel.js';
import { agileQueries } from './queries.js';

const TONES: Readonly<Record<AgileState, 'neutral' | 'accent' | 'warn' | 'muted' | 'up' | 'down'>> =
    {
        DISCOVERED: 'muted',
        BACKLOG: 'neutral',
        STARTED: 'accent',
        BLOCKED: 'warn',
        DONE: 'up',
        ABANDONED: 'muted',
        UNKNOWN: 'down',
    };

const PULL_REQUEST = 'https://github.com/OreStudio/OreStudio/pull/';

function StateTag({ state }: { readonly state: AgileState }): ReactNode {
    const { t } = useTranslation();
    return <Tag tone={TONES[state]}>{t(`development.board.states.${state}`)}</Tag>;
}

/**
 * The sprint board: the sprint line, the tiles, the epic and text filters, a
 * column for each state with a card for each story, and the detail of the
 * story that was opened. Variant A of the accepted prototype.
 *
 * The tab names this screen, so it carries no heading of its own. The sprint
 * is held in the address, so a link reopens the same sprint.
 */
export function BoardTab(): ReactNode {
    const { t } = useTranslation();
    const [search, setSearch] = useSearchParams();
    const sprint = search.get('sprint') ?? undefined;
    const sprints = useQuery(agileQueries.sprints());
    const board = useQuery(agileQueries.board(sprint));
    const [text, setText] = useState('');
    const [epic, setEpic] = useState<string | null>(null);
    const [openId, setOpenId] = useState<string | null>(null);

    const pickSprint = (name: string): void => {
        setOpenId(null);
        setText('');
        setEpic(null);
        setSearch(
            (previous) => {
                const next = new URLSearchParams(previous);
                next.set('sprint', name);
                return next;
            },
            { replace: true },
        );
    };
    const names = sprints.data?.map((ref) => ref.name);

    if (board.isError) {
        return (
            <div className="space-y-4">
                <SprintPicker names={names ?? []} value={sprint} onPick={pickSprint} />
                <Notice tone="error">{board.error.message}</Notice>
            </div>
        );
    }
    if (board.isPending) return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;

    const data = board.data;
    const stories = visibleStories(data.stories, { text, epic });
    const open = data.stories.find((story) => story.id === openId);
    const filtered = text !== '' || epic !== null;

    return (
        <div className="space-y-4">
            <SprintLine
                board={data}
                names={names ?? [data.sprint.name]}
                onSprint={pickSprint}
                onRefresh={() => void board.refetch()}
            />
            <Tiles board={data} />
            <div className="flex flex-wrap items-center gap-2">
                <Input
                    type="search"
                    className="max-w-xs"
                    value={text}
                    placeholder={t('development.board.search')}
                    aria-label={t('development.board.searchLabel')}
                    onChange={(event) => setText(event.target.value)}
                />
                {data.filters.epics.map((name) => (
                    <button
                        key={name}
                        type="button"
                        aria-pressed={epic === name}
                        className={cx(
                            'rounded-full border px-2 py-0.5 text-[11px]',
                            epic === name
                                ? 'border-accent bg-accent/10 text-accent-bright'
                                : 'border-line text-ink-muted hover:text-ink',
                        )}
                        onClick={() => setEpic(epic === name ? null : name)}
                    >
                        {name}
                    </button>
                ))}
                {filtered && (
                    <Button
                        type="button"
                        size="sm"
                        variant="ghost"
                        onClick={() => {
                            setText('');
                            setEpic(null);
                        }}
                    >
                        {t('development.board.clear')}
                    </Button>
                )}
            </div>
            <div className={cx('grid gap-4', open !== undefined && 'lg:grid-cols-[1fr_22rem]')}>
                <div className="grid gap-3 md:grid-cols-3 xl:grid-cols-5">
                    {columnsOf(data).map((column) => {
                        const cards = stories.filter((story) => columnOf(story.state) === column);
                        return (
                            <section
                                key={column}
                                className="space-y-2"
                                aria-label={t(`development.board.states.${column}`)}
                            >
                                <header className="flex items-center gap-2">
                                    <StateTag state={column} />
                                    <span className="text-xs text-ink-faint">{cards.length}</span>
                                </header>
                                {cards.length === 0 ? (
                                    <p className="text-xs text-ink-faint">
                                        {t('development.board.nothing')}
                                    </p>
                                ) : (
                                    cards.map((story) => (
                                        <StoryCard
                                            key={story.id}
                                            story={story}
                                            selected={story.id === openId}
                                            onOpen={() => setOpenId(story.id)}
                                        />
                                    ))
                                )}
                            </section>
                        );
                    })}
                </div>
                {open !== undefined && <StoryDetail story={open} onClose={() => setOpenId(null)} />}
            </div>
        </div>
    );
}

function SprintPicker({
    names,
    value,
    onPick,
}: {
    readonly names: readonly string[];
    readonly value: string | undefined;
    readonly onPick: (name: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <Select
            className="max-w-[10rem]"
            value={value ?? ''}
            aria-label={t('development.board.sprintLabel')}
            onChange={(event) => onPick(event.target.value)}
        >
            {value === undefined && <option value="" />}
            {names.map((name) => (
                <option key={name} value={name}>
                    {name}
                </option>
            ))}
            {value !== undefined && !names.includes(value) && (
                <option value={value}>{value}</option>
            )}
        </Select>
    );
}

function SprintLine({
    board,
    names,
    onSprint,
    onRefresh,
}: {
    readonly board: Board;
    readonly names: readonly string[];
    readonly onSprint: (name: string) => void;
    readonly onRefresh: () => void;
}): ReactNode {
    const { t, plural } = useTranslation();
    const { sprint } = board;
    const day = shownDay(sprint.dayOfSprint, sprint.totalDays);
    return (
        <div className="flex flex-wrap items-center gap-3">
            <SprintPicker names={names} value={sprint.name} onPick={onSprint} />
            <h2 className="text-lg font-semibold text-ink">{sprint.title}</h2>
            {day !== null && sprint.totalDays !== null && (
                <span className="text-sm text-ink-muted">
                    {t('development.board.day', { day, total: sprint.totalDays })}
                </span>
            )}
            <span className="text-sm text-ink-muted">
                {plural('development.board.storyCount', board.counts.stories)}
            </span>
            <span className="ml-auto">
                <Button type="button" size="sm" variant="ghost" onClick={onRefresh}>
                    {t('development.board.refresh')}
                </Button>
            </span>
        </div>
    );
}

function Tiles({ board }: { readonly board: Board }): ReactNode {
    const { t } = useTranslation();
    const { counts } = board;
    const tiles: readonly (readonly [string, string])[] = [
        [t('development.board.tiles.stories'), String(counts.stories)],
        [t('development.board.tiles.inFlight'), String(counts.storiesStarted)],
        [t('development.board.tiles.blocked'), String(counts.storiesBlocked)],
        [t('development.board.tiles.tasksDone'), `${counts.tasksDone}/${counts.tasks}`],
    ];
    return (
        <div className="grid grid-cols-2 gap-3 md:grid-cols-4">
            {tiles.map(([label, value]) => (
                <section key={label} className="card grid gap-0.5 px-4 py-3">
                    <span className="text-[11px] uppercase tracking-wider text-ink-faint">
                        {label}
                    </span>
                    <span className="text-2xl font-bold text-ink">{value}</span>
                </section>
            ))}
        </div>
    );
}

function StoryCard({
    story,
    selected,
    onOpen,
}: {
    readonly story: AgileStory;
    readonly selected: boolean;
    readonly onOpen: () => void;
}): ReactNode {
    const { t, plural } = useTranslation();
    return (
        <button
            type="button"
            aria-pressed={selected}
            className={cx('card block w-full space-y-2 p-3 text-left', selected && 'border-accent')}
            onClick={onOpen}
        >
            <span className="block text-sm font-medium text-ink">{story.title}</span>
            <span className="flex flex-wrap gap-1">
                {story.environment !== '' && <Tag small>{story.environment}</Tag>}
                {story.epic !== '' && (
                    <Tag small tone="muted">
                        {story.epic}
                    </Tag>
                )}
            </span>
            <span className="block h-1 rounded bg-line">
                <span
                    className="block h-1 rounded bg-up"
                    style={{ width: `${percentOfTasks(story)}%` }}
                />
            </span>
            <span className="block text-xs text-ink-muted">
                {plural('development.board.taskProgress', story.progress.total, {
                    total: story.progress.total,
                    done: story.progress.done,
                })}
            </span>
        </button>
    );
}

export function StoryDetail({
    story,
    onClose,
}: {
    readonly story: AgileStory;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const none = t('development.board.none');
    return (
        <aside className="card space-y-3 p-4" aria-label={t('development.board.detail')}>
            <div className="flex items-start gap-2">
                <h3 className="text-base font-semibold text-ink">{story.title}</h3>
                <span className="ml-auto">
                    <Button type="button" size="sm" variant="ghost" onClick={onClose}>
                        {t('development.board.close')}
                    </Button>
                </span>
            </div>
            <dl className="grid grid-cols-[7rem_1fr] gap-x-3 gap-y-1 text-sm">
                <dt className="text-ink-faint">{t('development.board.fields.id')}</dt>
                <dd className="break-all font-mono text-xs text-ink">{story.id}</dd>
                <dt className="text-ink-faint">{t('development.board.fields.state')}</dt>
                <dd>
                    <StateTag state={story.state} />
                </dd>
                <dt className="text-ink-faint">{t('development.board.fields.environment')}</dt>
                <dd className="text-ink">{story.environment || none}</dd>
                <dt className="text-ink-faint">{t('development.board.fields.epic')}</dt>
                <dd className="text-ink">{story.epic || none}</dd>
                <dt className="text-ink-faint">{t('development.board.fields.created')}</dt>
                <dd className="text-ink">{story.created || none}</dd>
                <dt className="text-ink-faint">{t('development.board.fields.branches')}</dt>
                <dd className="break-all font-mono text-xs text-ink">
                    {story.branches.length === 0 ? none : story.branches.join(', ')}
                </dd>
                <dt className="text-ink-faint">{t('development.board.fields.pullRequests')}</dt>
                <dd className="text-ink">
                    {story.prs.length === 0
                        ? none
                        : story.prs.map((number) => (
                              <a
                                  key={number}
                                  className="mr-2 text-accent-bright hover:underline"
                                  href={`${PULL_REQUEST}${number}`}
                                  target="_blank"
                                  rel="noreferrer"
                              >
                                  #{number}
                              </a>
                          ))}
                </dd>
            </dl>
            {story.description !== '' && (
                <p className="text-sm text-ink-muted">{story.description}</p>
            )}
            <p className="text-sm">
                <Link className="text-accent-bright hover:underline" to={docRoute(story.id)}>
                    {t('development.board.openDoc')}
                </Link>
            </p>
            <h4 className="text-sm font-semibold text-ink">{t('development.board.tasks')}</h4>
            {story.tasks.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('development.board.noTasks')}</p>
            ) : (
                <ul className="space-y-2">
                    {story.tasks.map((task) => (
                        <TaskRow key={task.id || task.slug} task={task} />
                    ))}
                </ul>
            )}
        </aside>
    );
}

function TaskRow({ task }: { readonly task: AgileTask }): ReactNode {
    const { t } = useTranslation();
    const [expanded, setExpanded] = useState(false);
    const none = t('development.board.none');
    const pr = Number(/^#?(\d+)$/.exec(task.pr.trim())?.[1]);
    return (
        <li className="rounded-md border border-line-subtle p-2">
            <button
                type="button"
                aria-expanded={expanded}
                className="flex w-full items-center gap-2 text-left text-sm text-ink"
                onClick={() => setExpanded(!expanded)}
            >
                <StateTag state={task.state} />
                <span>{task.title}</span>
            </button>
            <div className="mt-1 flex flex-wrap gap-2 text-xs text-ink-muted">
                {task.environment !== '' && <Tag small>{task.environment}</Tag>}
                {Number.isInteger(pr) && pr > 0 && (
                    <a
                        className="text-accent-bright hover:underline"
                        href={`${PULL_REQUEST}${pr}`}
                        target="_blank"
                        rel="noreferrer"
                    >
                        #{pr}
                    </a>
                )}
            </div>
            {expanded && (
                <dl className="mt-2 grid grid-cols-[7rem_1fr] gap-x-3 gap-y-1 text-xs">
                    <dt className="text-ink-faint">{t('development.board.fields.branch')}</dt>
                    <dd className="break-all font-mono text-ink">{task.branch || none}</dd>
                    <dt className="text-ink-faint">{t('development.board.fields.blockedOn')}</dt>
                    <dd className="text-ink">{task.blockedOn || none}</dd>
                    <dt className="text-ink-faint">{t('development.board.fields.created')}</dt>
                    <dd className="text-ink">{task.created || none}</dd>
                    <dt className="text-ink-faint">{t('development.board.fields.updated')}</dt>
                    <dd className="text-ink">{task.updated || none}</dd>
                    <dt className="text-ink-faint">{t('development.board.fields.doc')}</dt>
                    <dd>
                        <Link className="text-accent-bright hover:underline" to={docRoute(task.id)}>
                            {t('development.board.openDoc')}
                        </Link>
                    </dd>
                </dl>
            )}
        </li>
    );
}
