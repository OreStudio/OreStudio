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

/**
 * The app version page, from doc/knowledge/journeys/compute/journey_keep_what_the_grid_runs.org.
 *
 * A job pins an app version, and the job page links here. A person who follows the
 * link asks what this release is and what changed in it. The page shows the build,
 * the release note stored with it, the packages the wrapper downloads and the other
 * versions of its app.
 *
 * A release is one engine build and one wrapper build, packaged for several
 * platforms, so the note belongs to the version and not to a package. Every
 * signed-in role reads the page. The super administrator alone sees the control
 * that edits the note, and the edit states the version it read.
 */

import { useState, type ReactNode } from 'react';
import { Link, useParams } from 'react-router';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { RELEASE_NOTE_MAX, type AppVersionPageView } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { ApiFailure } from '../api/transport.js';
import { useTranslation } from '../i18n/Provider.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { gridAppVersionPath } from '../shell/paths.js';
import { Button, Detail, LinkButton, Notice, PageHeader, Tag, Textarea } from '../ui/Primitives.js';
import { formatDateTime } from '../ui/Time.js';
import { ReadFailure } from './GridJobs.js';

/** Where the page's answer is cached, so a test can seed it. */
export const APP_VERSION_QUERY_KEY = 'operations-grid-app-version';

/** One block of a release note: a heading, an item of a list, or a paragraph. */
export type NoteBlock =
    | { readonly kind: 'heading'; readonly text: string }
    | { readonly kind: 'item'; readonly text: string }
    | { readonly kind: 'paragraph'; readonly text: string };

/**
 * The blocks of a stored release note.
 *
 * The note is plain text. A line that starts with "## " is a heading and a line
 * that starts with "- " is an item. Any other line of text is a paragraph, and a
 * blank line separates nothing and is dropped.
 */
export function parseNote(note: string): readonly NoteBlock[] {
    return note
        .split('\n')
        .map((line) => line.trimEnd())
        .filter((line) => line.trim() !== '')
        .map((line): NoteBlock => {
            if (line.startsWith('## ')) {
                return { kind: 'heading', text: line.slice(3).trim() };
            }
            if (line.startsWith('- ')) {
                return { kind: 'item', text: line.slice(2).trim() };
            }
            return { kind: 'paragraph', text: line.trim() };
        });
}

function Panel({
    title,
    note,
    actions,
    children,
}: {
    readonly title: string;
    readonly note?: string;
    readonly actions?: ReactNode;
    readonly children: ReactNode;
}): ReactNode {
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-3">
                <h2 className="text-lg font-medium">{title}</h2>
                <span className="flex items-center gap-3 text-xs text-ink-faint">
                    {note}
                    {actions}
                </span>
            </header>
            {children}
        </section>
    );
}

/** The note as blocks, headings and lists drawn as such. */
function NoteBody({ note }: { readonly note: string }): ReactNode {
    const blocks = parseNote(note);
    const nodes: ReactNode[] = [];
    let items: string[] = [];
    const flush = (key: number): void => {
        if (items.length > 0) {
            nodes.push(
                <ul key={`list-${key}`} className="ml-5 list-disc space-y-1 text-sm">
                    {items.map((item, index) => (
                        <li key={index}>{item}</li>
                    ))}
                </ul>,
            );
            items = [];
        }
    };
    blocks.forEach((block, index) => {
        if (block.kind === 'item') {
            items.push(block.text);
            return;
        }
        flush(index);
        nodes.push(
            block.kind === 'heading' ? (
                <h3 key={index} className="text-sm font-medium">
                    {block.text}
                </h3>
            ) : (
                <p key={index} className="text-sm">
                    {block.text}
                </p>
            ),
        );
    });
    flush(blocks.length);
    return <div className="space-y-3">{nodes}</div>;
}

/** The editor of the note: a text box, the limit, and the save that states the version read. */
function NoteEditor({
    page,
    onDone,
}: {
    readonly page: AppVersionPageView;
    readonly onDone: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const [text, setText] = useState(page.version.release_note);
    const save = useMutation({
        mutationFn: () => api.saveReleaseNote(page.version.id, text, page.version.version),
        onSuccess: (fresh) => {
            queries.setQueryData([APP_VERSION_QUERY_KEY, page.version.id], fresh);
            onDone();
        },
    });
    const conflict = save.error instanceof ApiFailure && save.error.status === 409;
    // After a conflict the draft is not the latest note, so the editor reads the page
    // again and takes the note as it stands, which also gives the next save its version.
    const reload = async (): Promise<void> => {
        const fresh = await queries.fetchQuery({
            queryKey: [APP_VERSION_QUERY_KEY, page.version.id],
            queryFn: () => api.gridAppVersion(page.version.id),
            staleTime: 0,
        });
        setText(fresh.version.release_note);
        save.reset();
    };
    return (
        <div className="space-y-3">
            <Textarea
                aria-label={t('operations.grid.appVersion.note.label')}
                rows={14}
                maxLength={RELEASE_NOTE_MAX}
                value={text}
                onChange={(event) => setText(event.target.value)}
            />
            <p className="text-xs text-ink-faint">
                {t('operations.grid.appVersion.note.help', { limit: RELEASE_NOTE_MAX })}
            </p>
            {save.isError && (
                <Notice tone="error">
                    {conflict ? t('operations.grid.appVersion.note.conflict') : save.error.message}
                    {conflict && (
                        <>
                            {' '}
                            <Button size="sm" onClick={() => void reload()}>
                                {t('operations.grid.appVersion.note.reload')}
                            </Button>
                        </>
                    )}
                </Notice>
            )}
            <div className="flex gap-2">
                <Button
                    variant="primary"
                    pending={save.isPending}
                    disabled={text === page.version.release_note}
                    onClick={() => save.mutate()}
                >
                    {t('operations.grid.appVersion.note.save')}
                </Button>
                <Button variant="ghost" onClick={onDone}>
                    {t('operations.grid.appVersion.note.cancel')}
                </Button>
            </div>
        </div>
    );
}

function ReleaseNote({ page }: { readonly page: AppVersionPageView }): ReactNode {
    const { t } = useTranslation();
    const [editing, setEditing] = useState(false);
    const note = page.version.release_note;
    return (
        <Panel
            title={t('operations.grid.appVersion.note.title')}
            actions={
                page.may_write && !editing ? (
                    <Button size="sm" onClick={() => setEditing(true)}>
                        {t('operations.grid.appVersion.note.edit')}
                    </Button>
                ) : undefined
            }
        >
            {editing ? (
                <NoteEditor page={page} onDone={() => setEditing(false)} />
            ) : note.trim() === '' ? (
                <p className="text-sm text-ink-muted">
                    {t('operations.grid.appVersion.note.none')}
                </p>
            ) : (
                <NoteBody note={note} />
            )}
        </Panel>
    );
}

function Build({ page }: { readonly page: AppVersionPageView }): ReactNode {
    const { t, language } = useTranslation();
    const { version } = page;
    const none = t('operations.grid.jobs.none');
    return (
        <Panel
            title={t('operations.grid.appVersion.build.title')}
            note={t('operations.grid.jobs.details.readOnly')}
        >
            <div>
                <p className="text-xs text-ink-faint">{t('operations.grid.jobs.fields.id')}</p>
                <p className="break-all font-mono text-sm">{version.id}</p>
            </div>
            <dl className="grid gap-3 sm:grid-cols-3">
                <Detail
                    label={t('operations.grid.appVersion.build.app')}
                    value={version.app_name ?? none}
                />
                <Detail
                    label={t('operations.grid.appVersion.build.engine')}
                    value={version.engine_version}
                />
                <Detail
                    label={t('operations.grid.appVersion.build.wrapper')}
                    value={version.wrapper_version}
                />
                <Detail
                    label={t('operations.grid.appVersion.build.minRam')}
                    value={t('operations.grid.appVersion.build.mib', { value: version.min_ram_mb })}
                />
                <Detail
                    label={t('operations.grid.jobs.fields.modifiedBy')}
                    value={version.modified_by}
                />
                <Detail
                    label={t('operations.grid.jobs.fields.recordedAt')}
                    value={formatDateTime(version.recorded_at, language)}
                />
            </dl>
        </Panel>
    );
}

function Packages({ page }: { readonly page: AppVersionPageView }): ReactNode {
    const { t, plural } = useTranslation();
    return (
        <Panel
            title={t('operations.grid.appVersion.packages.title')}
            note={plural('operations.grid.appVersion.packages.count', page.packages.length)}
        >
            {page.packages.length === 0 ? (
                <Notice tone="warn">{t('operations.grid.appVersion.packages.none')}</Notice>
            ) : (
                <div className="overflow-x-auto">
                    <table className="w-full text-sm">
                        <thead className="text-left text-xs text-ink-faint">
                            <tr>
                                {[
                                    t('operations.grid.appVersion.packages.platform'),
                                    t('operations.grid.appVersion.packages.state'),
                                    t('operations.grid.appVersion.packages.uri'),
                                    t('operations.grid.appVersion.packages.sha256'),
                                ].map((heading) => (
                                    <th key={heading} className="py-1 pr-3 font-normal">
                                        {heading}
                                    </th>
                                ))}
                            </tr>
                        </thead>
                        <tbody>
                            {page.packages.map((row) => (
                                <tr
                                    key={row.platform_id}
                                    className="border-t border-line-subtle align-top"
                                >
                                    <td className="py-1.5 pr-3">{row.code}</td>
                                    <td className="py-1.5 pr-3">
                                        <Tag tone={row.is_active ? 'up' : 'muted'}>
                                            {t(
                                                row.is_active
                                                    ? 'operations.grid.appVersion.packages.active'
                                                    : 'operations.grid.appVersion.packages.inactive',
                                            )}
                                        </Tag>
                                    </td>
                                    <td className="break-all py-1.5 pr-3 font-mono text-xs">
                                        {row.package_uri}
                                    </td>
                                    <td className="break-all py-1.5 pr-3 font-mono text-xs">
                                        {row.sha256}
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            )}
        </Panel>
    );
}

function OtherVersions({ page }: { readonly page: AppVersionPageView }): ReactNode {
    const { t, language, plural } = useTranslation();
    return (
        <Panel
            title={t('operations.grid.appVersion.versions.title', {
                app: page.version.app_name ?? page.version.app_id.slice(0, 8),
            })}
            note={plural('operations.grid.appVersion.versions.count', page.versions.length)}
        >
            <table className="w-full text-sm">
                <thead className="text-left text-xs text-ink-faint">
                    <tr>
                        <th className="py-1 pr-3 font-normal">
                            {t('operations.grid.appVersion.build.engine')}
                        </th>
                        <th className="py-1 pr-3 font-normal">
                            {t('operations.grid.appVersion.build.wrapper')}
                        </th>
                        <th className="py-1 pr-3 font-normal">
                            {t('operations.grid.jobs.fields.recordedAt')}
                        </th>
                    </tr>
                </thead>
                <tbody>
                    {page.versions.map((row) => (
                        <tr
                            key={row.id}
                            className={`border-t border-line-subtle ${row.id === page.version.id ? 'bg-bg-hover' : ''}`}
                        >
                            <td className="py-1.5 pr-3" title={row.id}>
                                {row.id === page.version.id ? (
                                    `${row.engine_version} ${t('operations.grid.appVersion.versions.here')}`
                                ) : (
                                    <Link
                                        to={gridAppVersionPath(row.id)}
                                        className="underline decoration-line-strong underline-offset-2 hover:decoration-accent"
                                    >
                                        {row.engine_version}
                                    </Link>
                                )}
                            </td>
                            <td className="py-1.5 pr-3">{row.wrapper_version}</td>
                            <td className="py-1.5 pr-3">
                                {formatDateTime(row.recorded_at, language)}
                            </td>
                        </tr>
                    ))}
                </tbody>
            </table>
        </Panel>
    );
}

/** The app version page. */
export function GridAppVersionPage(): ReactNode {
    const { t } = useTranslation();
    const { id = '' } = useParams();
    const page = useQuery({
        queryKey: [APP_VERSION_QUERY_KEY, id],
        queryFn: () => api.gridAppVersion(id),
        meta: { quiet: true },
    });
    const label =
        page.data === undefined
            ? id.slice(0, 8)
            : `${page.data.version.app_name ?? ''} ${page.data.version.engine_version}`.trim();
    return (
        <div className="space-y-6">
            <div>
                <AreaTrail
                    area="operations"
                    steps={[{ label: t('operations.screens.grid'), to: '/operations/grid' }]}
                    screen={label}
                />
                <PageHeader
                    title={t('operations.grid.appVersion.title')}
                    description={t('operations.grid.appVersion.description')}
                    actions={
                        <LinkButton to="/operations/grid?tab=jobs" variant="secondary">
                            {t('operations.grid.job.back')}
                        </LinkButton>
                    }
                />
            </div>
            {page.isPending && (
                <p className="text-sm text-ink-muted">{t('operations.grid.appVersion.loading')}</p>
            )}
            {page.isError && <ReadFailure error={page.error} onRetry={() => void page.refetch()} />}
            {page.isSuccess && (
                <>
                    <Build page={page.data} />
                    <ReleaseNote page={page.data} />
                    <Packages page={page.data} />
                    <OtherVersions page={page.data} />
                </>
            )}
        </div>
    );
}
