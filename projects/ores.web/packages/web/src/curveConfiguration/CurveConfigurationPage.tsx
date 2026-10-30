/** -*- mode: typescript-ts-mode; tab-width: 4; indent-tabs-mode: nil -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51 Franklin
 * Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 *
 */

import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useEffect, useState, type ReactNode } from 'react';
import { Link, useParams } from 'react-router';
import type { ConfigurationDetail } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useHeld } from '../access/holds.js';
import {
    curveConfiguration,
    type CurveChange,
    type CurveConfiguration,
    type CurveEntry,
    type CurveParameter,
    type CurveQuote,
    type CurveSection,
} from '../api/curveConfiguration.js';
import { ApiFailure } from '../api/transport.js';
import { useTranslation } from '../i18n/Provider.js';
import { ParameterControl } from '../parameters/ParameterControl.js';
import { ParameterRow } from '../parameters/ParameterRow.js';
import { controlOfValueSet, shortDefault } from '../parameters/control.js';
import { configurationKey } from '../reporting/ConfigurationPage.js';
import { TabPane } from '../refdata/parties.js';
import { RecordTimeline } from '../refdata/RecordTimeline.js';
import { Crumbs, ReasonFields, useReason } from '../refdata/shared.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { configurationPath, reportPath } from '../shell/paths.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { EditingModeToggle, useEditingMode, type EditingMode } from '../ui/EditingMode.js';
import { ParameterDocuments, ParameterFilters } from '../ui/OreParameter.js';
import { Button, Detail, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { useTabs } from '../ui/Tabs.js';

/**
 * One curve configuration: its details, its curves, the reports that bind it
 * and its history.
 *
 * The curves are drawn as the product pricing screen draws its products,
 * because the document has the same shape: many entries of a few kinds, where
 * the kind is a group and the entry is the row a person looks for. One tab holds
 * them, a searchable rail groups them by the section that declares them, and the
 * chosen curve's parameters are read beside it. A section that holds nothing is
 * named once at the foot of the rail, rather than given a tab that opens empty.
 *
 * Every word comes from the server: the sections and their entry elements from
 * the curve section data, each parameter's label, meaning, default and value set
 * from the ORE parameter contract, and each value from the entry's own row.
 *
 * A change writes the leaves a person touched and nothing else: the route reads
 * the stored document, changes those leaves and writes back only the families
 * they reach, so the rows this page does not draw keep their versions. It states
 * the version the page read, so a save from a stale page is a conflict the person
 * reads again, never an overwrite. The blocks an entry holds -- its segments,
 * quotes and bootstrap -- are named and not yet drawn, so their leaves are not
 * offered for change.
 */

/** The entity type the history of a curve configuration row is recorded under. */
export const CURVE_CONFIGURATION_ENTITY = 'ores.refdata.curve_configuration';

/** Where the curve configuration the address names is cached. */
export function curveConfigurationKey(configurationId: string): readonly unknown[] {
    return ['curve-configuration', configurationId];
}

/** The documentation switch of one leaf of one curve, which the workbench opens. */
/**
 * The key of one leaf on the page: the row that holds it and its ORE name. A
 * collection row's leaves repeat across rows, so the row is part of the key.
 */
function documentationKey(holder: string, leaf: CurveParameter): string {
    return `${holder}|${leaf.ore}`;
}

/** The eight characters a person reads to tell one identifier from another. */
function shortId(id: string): string {
    return id.slice(0, 8);
}

/** The sections that hold a curve, in the order the document declares them. */
function filled(sections: readonly CurveSection[]): readonly CurveSection[] {
    return sections.filter((section) => section.entries.length > 0);
}

/** Every curve of the document, in the order the document declares them. */
function curvesOf(sections: readonly CurveSection[]): readonly CurveEntry[] {
    return sections.flatMap((section) => section.entries);
}

export function CurveConfigurationPage(): ReactNode {
    const { id = '' } = useParams();
    const { t } = useTranslation();
    const area = useAreaRoot('reporting');
    const [mode, setMode] = useEditingMode();
    const held = useHeld();
    const mayWrite = held('refdata::curve_configurations:write');
    const queries = useQueryClient();
    /*
     * A save names the version the page read. The server refuses a version that
     * moved on, so the page reads again after a save and states a conflict
     * rather than writing over somebody else's change.
     */
    const amend = useMutation({
        mutationFn: async (amendment: {
            readonly changes: readonly CurveChange[];
            readonly reasonCode: string;
            readonly commentary: string;
        }) => {
            await curveConfiguration.amend(id, { ...amendment, version: curve?.version ?? 0 });
            await queries.invalidateQueries({ queryKey: curveConfigurationKey(id) });
        },
    });
    const reading = useQuery({
        queryKey: curveConfigurationKey(id),
        queryFn: () => curveConfiguration.read(id),
    });
    /*
     * The report configuration names the curve configuration and lists the
     * reports that bind it. A read that fails leaves the curves in view and says
     * so, because the document is what the person came for.
     */
    const serving = useQuery({
        queryKey: configurationKey(id),
        queryFn: () => api.configuration(id),
    });
    // Every hook runs before the first return, because the page draws its
    // loading state, its refusal and its content from one component.
    const tabs = ['details', 'curves', 'usedBy', 'history'];
    const panel = useTabs({
        label: t('curveConfiguration.tabs.label'),
        tabs,
        titleOf: (tab) => t(`curveConfiguration.tabs.${tab}`),
    });
    // A tab the person opened stays drawn and hidden while another is open, so
    // leaving the curves never loses the curve they were reading.
    const [seen, setSeen] = useState<ReadonlySet<string>>(() => new Set([panel.tab]));
    useEffect(() => {
        setSeen((before) => (before.has(panel.tab) ? before : new Set(before).add(panel.tab)));
    }, [panel.tab]);
    const trail = [...area, { label: t('curveConfiguration.title') }];
    if (reading.isPending) {
        return (
            <ScreenFrame
                trail={<Crumbs parts={trail} />}
                title={shortId(id)}
                description={t('curveConfiguration.lead')}
            >
                <p className="text-sm text-ink-muted" role="status">
                    {t('common.loading')}
                </p>
            </ScreenFrame>
        );
    }
    if (reading.isError) {
        const missing = reading.error instanceof ApiFailure && reading.error.status === 404;
        return (
            <ScreenFrame
                trail={<Crumbs parts={[...trail, { label: shortId(id) }]} />}
                title={shortId(id)}
                description={t('curveConfiguration.lead')}
            >
                {missing ? (
                    <Notice tone="info">{t('curveConfiguration.missing')}</Notice>
                ) : (
                    <Notice tone="error">
                        {t('curveConfiguration.readFailed', { message: reading.error.message })}
                    </Notice>
                )}
            </ScreenFrame>
        );
    }
    const curve = reading.data;
    const name = serving.data?.configuration.name ?? curve.name;
    const pane = (paneName: string, children: ReactNode): ReactNode => (
        <TabPane name={paneName} tab={panel.tab} seen={seen}>
            {children}
        </TabPane>
    );
    return (
        <div className="space-y-4">
            <Crumbs parts={[...trail, { label: name }]} />
            <PageHeader
                title={name}
                description={t('curveConfiguration.recordLead', {
                    version: String(curve.version),
                    documented: String(curve.documented),
                    total: String(curve.total),
                })}
                subdued
                actions={<EditingModeToggle mode={mode} onChange={setMode} words="leaves" />}
            />
            {panel.bar}
            {pane('details', <DetailsTab curve={curve} name={name} serving={serving.data} />)}
            {pane(
                'curves',
                <CurvesTab
                    curve={curve}
                    mode={mode}
                    mayWrite={mayWrite}
                    onAmend={(changes, reasonCode, commentary) =>
                        amend.mutateAsync({ changes, reasonCode, commentary })
                    }
                />,
            )}
            {pane('usedBy', <UsedByTab configurationId={id} />)}
            {pane(
                'history',
                <section className="card space-y-4 p-4">
                    <h2 className="text-sm font-medium">{t('curveConfiguration.tabs.history')}</h2>
                    <p className="text-sm text-ink-muted">{t('curveConfiguration.history.lead')}</p>
                    <RecordTimeline entityType={CURVE_CONFIGURATION_ENTITY} entityId={curve.id} />
                </section>,
            )}
        </div>
    );
}

/**
 * The document's curves: a rail grouped by the section that declares each, and
 * the chosen curve's parameters beside it.
 *
 * The rail is the document's own shape. A section is a group with its count, a
 * curve is a row, and the sections that hold nothing are named together at the
 * foot, so a person looking for a curve reads the curves and not the chapters.
 */
function CurvesTab({
    curve,
    mode,
    mayWrite,
    onAmend,
}: {
    readonly curve: CurveConfiguration;
    readonly mode: EditingMode;
    readonly mayWrite: boolean;
    readonly onAmend: (
        changes: readonly CurveChange[],
        reason: string,
        commentary: string,
    ) => Promise<void>;
}): ReactNode {
    const { t } = useTranslation();
    const groups = filled(curve.sections);
    const empty = curve.sections.filter((section) => section.entries.length === 0);
    const everyEntry = curvesOf(curve.sections);
    const [needle, setNeedle] = useState('');
    const [chosen, setChosen] = useState<string>(() => everyEntry[0]?.id ?? '');
    const [open, setOpen] = useState<ReadonlySet<string>>(
        () => new Set(groups.slice(0, 1).map((section) => section.code)),
    );
    const search = needle.trim().toLowerCase();
    const matches = (section: CurveSection, entry: CurveEntry): boolean =>
        search === '' ||
        entry.curveId.toLowerCase().includes(search) ||
        entry.description.toLowerCase().includes(search) ||
        section.code.toLowerCase().includes(search);
    const shown = groups
        .map((section) => ({
            section,
            entries: section.entries.filter((entry) => matches(section, entry)),
        }))
        .filter((group) => group.entries.length > 0);
    const inView = shown.reduce((count, group) => count + group.entries.length, 0);
    const selected = everyEntry.find((entry) => entry.id === chosen);
    const opened = (code: string): boolean => search !== '' || open.has(code);
    const allOpen = groups.length > 0 && open.size >= groups.length;
    const toggleGroup = (code: string, isOpen: boolean): void => {
        setOpen((before) => {
            const next = new Set(before);
            if (isOpen) {
                next.add(code);
            } else {
                next.delete(code);
            }
            return next;
        });
    };
    return (
        <div className="grid gap-4 lg:grid-cols-[16rem_minmax(0,1fr)]">
            <aside className="card space-y-3 p-3">
                <div className="space-y-1">
                    <h2 className="text-sm font-medium">{t('curveConfiguration.curves.title')}</h2>
                    <p className="text-xs text-ink-faint">
                        {t('curveConfiguration.curves.count', {
                            count: String(everyEntry.length),
                            filled: String(groups.length),
                            sections: String(curve.sections.length),
                        })}
                    </p>
                </div>
                {everyEntry.length > 0 && (
                    <input
                        value={needle}
                        onChange={(event) => setNeedle(event.target.value)}
                        placeholder={t('curveConfiguration.curves.filter')}
                        aria-label={t('curveConfiguration.curves.filter')}
                        className="w-full rounded border border-line bg-surface-base px-2 py-1 text-sm text-ink placeholder:text-ink-faint"
                    />
                )}
                {groups.length > 0 && (
                    <button
                        type="button"
                        className="px-1 text-xs text-accent hover:text-accent-bright"
                        onClick={() =>
                            setOpen(
                                allOpen
                                    ? new Set()
                                    : new Set(groups.map((section) => section.code)),
                            )
                        }
                    >
                        {allOpen
                            ? t('curveConfiguration.curves.collapseAll')
                            : t('curveConfiguration.curves.expandAll')}
                    </button>
                )}
                {everyEntry.length === 0 ? (
                    <p className="text-sm text-ink-muted">{t('curveConfiguration.curves.none')}</p>
                ) : shown.length === 0 ? (
                    <p className="text-sm text-ink-muted">
                        {t('curveConfiguration.curves.noMatch')}
                    </p>
                ) : (
                    shown.map((group) => (
                        <details
                            key={group.section.code}
                            open={opened(group.section.code)}
                            onToggle={(event) =>
                                toggleGroup(group.section.code, event.currentTarget.open)
                            }
                        >
                            <summary className="flex cursor-pointer items-baseline justify-between gap-2 px-1 pt-2 text-[10px] tracking-wide text-ink-faint uppercase">
                                <span>{group.section.code}</span>
                                <span>{group.entries.length}</span>
                            </summary>
                            <div className="space-y-1 pt-1">
                                {group.entries.map((entry) => (
                                    <button
                                        key={entry.id}
                                        type="button"
                                        title={
                                            entry.description === ''
                                                ? entry.curveId
                                                : entry.description
                                        }
                                        onClick={() => setChosen(entry.id)}
                                        className={
                                            entry.id === selected?.id
                                                ? 'block w-full rounded border-l-2 border-accent bg-panel-2 px-2 py-1 text-left text-sm break-words text-ink [overflow-wrap:anywhere]'
                                                : 'block w-full rounded border-l-2 border-transparent px-2 py-1 text-left text-sm break-words text-ink-muted hover:text-ink [overflow-wrap:anywhere]'
                                        }
                                    >
                                        {entry.curveId === '' ? shortId(entry.id) : entry.curveId}
                                    </button>
                                ))}
                            </div>
                        </details>
                    ))
                )}
                {empty.length > 0 && (
                    <details className="border-t border-line-subtle pt-2">
                        <summary className="cursor-pointer px-1 text-xs text-ink-faint">
                            {t('curveConfiguration.curves.emptySections', {
                                count: String(empty.length),
                            })}
                        </summary>
                        <ul className="space-y-1 pt-1">
                            {empty.map((section) => (
                                <li
                                    key={section.code}
                                    className="flex items-baseline justify-between gap-2 px-1 text-xs text-ink-faint"
                                >
                                    <span>{section.code}</span>
                                    <span>{t('curveConfiguration.curves.empty')}</span>
                                </li>
                            ))}
                        </ul>
                    </details>
                )}
                <p className="border-t border-line-subtle pt-2 text-xs text-ink-faint">
                    {t('curveConfiguration.curves.inView', {
                        shown: String(inView),
                        total: String(everyEntry.length),
                    })}
                </p>
            </aside>
            <div className="space-y-4">
                {selected === undefined ? (
                    <section className="card p-4">
                        <p className="text-sm text-ink-muted">
                            {t('curveConfiguration.curves.choose')}
                        </p>
                    </section>
                ) : (
                    <CurveWorkbench
                        key={selected.id}
                        entry={selected}
                        section={curve.sections.find((section) =>
                            section.entries.some((entry) => entry.id === selected.id),
                        )}
                        mode={mode}
                        mayWrite={mayWrite}
                        onAmend={onAmend}
                    />
                )}
            </div>
        </div>
    );
}

/** One curve: what it is, its parameters and the elements it holds. */
function CurveWorkbench({
    entry,
    section,
    mode,
    mayWrite,
    onAmend,
}: {
    readonly entry: CurveEntry;
    readonly section: CurveSection | undefined;
    readonly mode: EditingMode;
    readonly mayWrite: boolean;
    readonly onAmend: (
        changes: readonly CurveChange[],
        reason: string,
        commentary: string,
    ) => Promise<void>;
}): ReactNode {
    const { t, plural } = useTranslation();
    const [setOnly, setSetOnly] = useState(false);
    const [opened, setOpened] = useState<ReadonlySet<string>>(new Set());
    /* The text of each leaf while the entry is being changed, keyed by entity and column. */
    const [editing, setEditing] = useState<Readonly<Record<string, string>> | null>(null);
    const [commentary, setCommentary] = useState('');
    const [refusal, setRefusal] = useState<string | null>(null);
    /* The server's answer when the version moved on, which a save cannot settle. */
    const [conflict, setConflict] = useState(false);
    /*
     * A curve's identifier and description are its own; the rest are the
     * parameters the section's entry carries, as a product's model and engine
     * settings are the product's.
     */
    const identity = entry.parameters.filter((leaf) => leaf.entity === 'curve_definition');
    /* A block the page draws rows of is drawn once, as those rows. */
    const blocks = entry.blocks.filter(
        (block) => !entry.collections.some((collection) => collection.element === block.element),
    );
    const parameters = entry.parameters.filter((leaf) => leaf.entity !== 'curve_definition');
    const visible = parameters.filter((leaf) => !setOnly || leaf.value !== null);
    const gaps = [
        ...parameters,
        ...entry.collections.flatMap((collection) => collection.rows.flatMap((row) => row.leaves)),
    ].filter((leaf) => !leaf.documented).length;
    const keyOf = (leaf: CurveParameter): string => `${leaf.entity}.${leaf.column}`;
    const changed =
        editing === null
            ? []
            : entry.parameters.filter(
                  (leaf) => (editing[keyOf(leaf)] ?? '') !== (leaf.value ?? ''),
              );
    const reason = useReason('amend', changed.length > 0);
    const missing = reason.needsCommentary && commentary.trim() === '';
    const ready = changed.length > 0 && !missing && reason.code !== '' && !conflict;
    const save = useMutation({
        mutationFn: () =>
            onAmend(
                changed.map((leaf) => ({
                    entity: leaf.entity,
                    entryId: entry.id,
                    column: leaf.column,
                    value:
                        (editing?.[keyOf(leaf)] ?? '') === ''
                            ? null
                            : (editing?.[keyOf(leaf)] ?? ''),
                })),
                reason.code,
                commentary.trim(),
            ),
        onSuccess: () => {
            setEditing(null);
            setCommentary('');
            setRefusal(null);
            setConflict(false);
        },
        onError: (error) => {
            setConflict(error instanceof ApiFailure && error.status === 409);
            setRefusal(error instanceof Error ? error.message : String(error));
        },
    });
    const cancel = (): void => {
        setEditing(null);
        setCommentary('');
        setRefusal(null);
        setConflict(false);
    };
    const edit = (): void =>
        setEditing(
            Object.fromEntries(entry.parameters.map((leaf) => [keyOf(leaf), leaf.value ?? ''])),
        );
    /*
     * Every leaf the entry draws, including the ones its collection rows hold, so
     * the button that opens the documentation opens all of it.
     */
    const drawable = [
        ...parameters.map((leaf) => documentationKey(entry.id, leaf)),
        ...entry.collections.flatMap((collection) =>
            collection.rows.flatMap((row) =>
                row.leaves.map((leaf) => documentationKey(row.id, leaf)),
            ),
        ),
    ];
    const allOpen = drawable.length > 0 && drawable.every((key) => opened.has(key));
    const toggle = (key: string): void =>
        setOpened((before) => {
            const next = new Set(before);
            if (!next.delete(key)) {
                next.add(key);
            }
            return next;
        });
    const leave = (leaf: CurveParameter): ReactNode => {
        const key = documentationKey(entry.id, leaf);
        const leafKey = keyOf(leaf);
        const text = editing === null ? null : (editing[leafKey] ?? '');
        return (
            <LeafRow
                key={key}
                leaf={leaf}
                mode={mode}
                open={opened.has(key)}
                text={text}
                changed={text !== null && text !== (leaf.value ?? '')}
                refused={refusal}
                onText={(value) =>
                    setEditing((before) =>
                        before === null ? before : { ...before, [leafKey]: value },
                    )
                }
                onToggle={() => toggle(key)}
            />
        );
    };
    /*
     * A collection row is read, not changed in place: the standard draws its rows
     * as cards and changes them in a dialog, which this page does not offer yet.
     * Its leaves take the same row and the same documentation as an entry's.
     */
    const leafOfRow =
        (rowId: string) =>
        (leaf: CurveParameter): ReactNode => {
            const key = documentationKey(rowId, leaf);
            return (
                <LeafRow
                    key={key}
                    leaf={leaf}
                    mode={mode}
                    open={opened.has(key)}
                    text={null}
                    changed={false}
                    refused={null}
                    onText={() => undefined}
                    onToggle={() => toggle(key)}
                />
            );
        };
    /* The edit and its save sit under the two cards, so the count and the reason
       are read where the leaves they apply to are. */
    const foot =
        mayWrite && editing !== null ? (
            <section className="card space-y-4 p-4">
                <header className="flex flex-wrap items-baseline justify-between gap-2">
                    <h3 className="text-sm font-medium">{t('curveConfiguration.edit.title')}</h3>
                    <span className="text-xs text-ink-faint">
                        {t('curveConfiguration.edit.changed', { count: String(changed.length) })}
                    </span>
                </header>
                {conflict ? (
                    <Notice tone="warn">{t('curveConfiguration.edit.conflict')}</Notice>
                ) : null}
                {refusal !== null && !conflict ? <Notice tone="error">{refusal}</Notice> : null}
                <ReasonFields
                    reason={reason}
                    commentary={commentary}
                    onCommentary={setCommentary}
                    missing={missing}
                />
                <div className="flex flex-wrap items-center justify-end gap-2">
                    <Button size="sm" variant="ghost" onClick={cancel}>
                        {t('curveConfiguration.edit.cancel')}
                    </Button>
                    <Button
                        size="sm"
                        icon="save"
                        disabled={!ready || save.isPending}
                        onClick={() => save.mutate()}
                    >
                        {t('curveConfiguration.edit.save')}
                    </Button>
                </div>
            </section>
        ) : null;
    return (
        <div className="space-y-4">
            <section className="card space-y-2 p-4">
                <header className="flex flex-wrap items-baseline justify-between gap-2">
                    <div className="min-w-0">
                        <h2 className="text-lg font-semibold">
                            {entry.curveId === '' ? shortId(entry.id) : entry.curveId}
                        </h2>
                        <p className="text-xs text-ink-faint">
                            {[
                                section?.code ?? '',
                                section?.entryElement ?? '',
                                t('curveConfiguration.curves.position', {
                                    position: String(entry.position),
                                }),
                            ]
                                .filter((part) => part !== '')
                                .join(' · ')}
                        </p>
                    </div>
                    <div className="flex flex-wrap items-center gap-2">
                        <Tag small>
                            {t('curveConfiguration.curves.parameters', {
                                count: String(parameters.length),
                            })}
                        </Tag>
                        {gaps > 0 && (
                            <Tag small tone="warn">
                                {t('curveConfiguration.curves.gaps', { count: String(gaps) })}
                            </Tag>
                        )}
                    </div>
                </header>
                <p className="text-sm text-ink-muted">
                    {entry.description === ''
                        ? t('curveConfiguration.curves.noDescription', {
                              entry: section?.entryElement ?? '',
                          })
                        : entry.description}
                </p>
            </section>
            {identity.length > 0 && (
                <section className="card space-y-2 p-4">
                    <h3 className="text-sm font-medium">
                        {t('curveConfiguration.curves.identity')}
                    </h3>
                    <ul className="divide-y divide-line-subtle">{identity.map(leave)}</ul>
                </section>
            )}
            <section className="card space-y-2 p-4">
                <header className="flex flex-wrap items-center justify-between gap-2">
                    <h3 className="text-sm font-medium">
                        {t('curveConfiguration.curves.parametersTitle')}
                    </h3>
                    <div className="flex flex-wrap items-center gap-2">
                        <ParameterFilters
                            set={setOnly}
                            onSet={() => setSetOnly((before) => !before)}
                            shown={visible.length}
                            total={parameters.length}
                            noun={t('oreParameter.nouns.parameters')}
                        />
                        <button
                            type="button"
                            className="text-xs text-accent hover:text-accent-bright"
                            onClick={() => setOpened(allOpen ? new Set() : new Set(drawable))}
                        >
                            {allOpen
                                ? t('oreParameter.hideAllDocumentation')
                                : t('oreParameter.showAllDocumentation')}
                        </button>
                        {mayWrite && editing === null && (
                            <Button size="sm" icon="edit" onClick={edit}>
                                {t('curveConfiguration.edit.start')}
                            </Button>
                        )}
                    </div>
                </header>
                {visible.length === 0 ? (
                    <p className="text-sm text-ink-muted">
                        {t('oreParameter.filters.none', {
                            noun: t('oreParameter.nouns.parameters'),
                        })}
                    </p>
                ) : (
                    <ul className="divide-y divide-line-subtle">{visible.map(leave)}</ul>
                )}
            </section>
            {entry.collections.map((collection) => (
                <section key={collection.element} className="card space-y-2 p-4">
                    <header className="flex flex-wrap items-baseline justify-between gap-2">
                        <h3 className="text-sm font-medium">
                            {t('curveConfiguration.curves.collection', {
                                element: collection.element,
                            })}
                        </h3>
                        <Tag small>
                            {plural(
                                'curveConfiguration.curves.collectionRows',
                                collection.rows.length,
                            )}
                        </Tag>
                    </header>
                    <ul className="space-y-3">
                        {collection.rows.map((row) => (
                            <li key={row.id} className="rounded-md border border-line-subtle p-3">
                                <p className="text-xs font-medium">
                                    {row.kind === ''
                                        ? t('curveConfiguration.curves.collectionRow', {
                                              id: shortId(row.id),
                                          })
                                        : row.kind}
                                </p>
                                <ul className="divide-y divide-line-subtle">
                                    {row.leaves.map(leafOfRow(row.id))}
                                </ul>
                                {row.quotes.length > 0 && (
                                    <div className="mt-2 space-y-2 border-t border-line-subtle pt-2">
                                        <p className="text-xs text-ink-faint">
                                            {plural(
                                                'curveConfiguration.curves.quotes',
                                                row.quotes.length,
                                            )}
                                        </p>
                                        <ul className="space-y-2">
                                            {row.quotes.map((quote) => (
                                                <li key={quote.id} className="space-y-1">
                                                    <ParameterRow
                                                        label={t(
                                                            'curveConfiguration.curves.quote',
                                                        )}
                                                    >
                                                        <span className="font-mono text-xs">
                                                            {quote.text !== ''
                                                                ? quote.text
                                                                : compositeOf(quote, t)}
                                                        </span>
                                                    </ParameterRow>
                                                    {quote.list === '' ? null : (
                                                        <ParameterRow
                                                            label={t(
                                                                'curveConfiguration.curves.quoteList',
                                                            )}
                                                        >
                                                            <span className="text-xs">
                                                                {quote.list}
                                                            </span>
                                                        </ParameterRow>
                                                    )}
                                                    <ParameterRow
                                                        label={t(
                                                            'curveConfiguration.curves.quoteOptional',
                                                        )}
                                                    >
                                                        <span className="text-xs">
                                                            {t(
                                                                quote.optional
                                                                    ? 'oreParameter.yes'
                                                                    : 'oreParameter.no',
                                                            )}
                                                        </span>
                                                    </ParameterRow>
                                                </li>
                                            ))}
                                        </ul>
                                    </div>
                                )}
                            </li>
                        ))}
                    </ul>
                </section>
            ))}
            {blocks.length > 0 && (
                <section className="card space-y-1 p-4">
                    <h3 className="text-sm font-medium">
                        {t('curveConfiguration.curves.blocks')}
                    </h3>
                    <dl className="divide-y divide-line-subtle">
                        {blocks.map((block) => (
                            <div
                                key={block.element}
                                className="flex flex-wrap items-baseline justify-between gap-2 py-2"
                            >
                                <dt className="text-sm">{block.element}</dt>
                                <dd className="text-xs text-ink-faint">
                                    {block.present === null
                                        ? t('curveConfiguration.curves.blockNotRecorded')
                                        : block.present
                                          ? t('curveConfiguration.curves.blockStated')
                                          : t('curveConfiguration.curves.blockAbsent')}
                                </dd>
                            </div>
                        ))}
                    </dl>
                </section>
            )}
            {foot}
        </div>
    );
}

/**
 * A composite quote's pair, as one string. A quote that states its own text uses
 * that instead, and a quote that states neither is left empty rather than drawn
 * as a slash between two blanks.
 */
function compositeOf(
    quote: CurveQuote,
    t: (key: string, values?: Record<string, string | number>) => string,
): string {
    if (quote.rateQuote === '' && quote.spreadQuote === '') return '';
    return t('curveConfiguration.curves.quotePair', {
        rate: quote.rateQuote,
        spread: quote.spreadQuote,
    });
}

/** One leaf: its label, the value in force, and the documentation under it. */
function LeafRow({
    leaf,
    mode,
    open,
    text,
    changed,
    refused,
    onText,
    onToggle,
}: {
    readonly leaf: CurveParameter;
    readonly mode: EditingMode;
    readonly open: boolean;
    /** The text of the control while the entry is being changed, else null. */
    readonly text: string | null;
    readonly changed: boolean;
    /** The server's words when it refused the save, which the row shows beside its value. */
    readonly refused: string | null;
    readonly onText: (value: string) => void;
    readonly onToggle: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const label = leaf.label === '' ? leaf.ore : leaf.label;
    const words = leaf.choices.find((choice) => choice.value === leaf.value)?.label;
    const flag = leaf.sourceKind === 'boolean';
    const standard = shortDefault(leaf.defaultText);
    const invalid = changed && refused !== null;
    return (
        <li className={changed ? 'space-y-2 bg-accent/5 py-3' : 'space-y-2 py-3'}>
            {/* The help is the same sentence the documentation panel gives, because
                the guide supplies one line for both, so the row states none of it:
                the documentation is read where it is collapsed. */}
            <ParameterRow label={label} required={leaf.required}>
                {text === null ? (
                    <div className="flex flex-wrap items-center gap-2">
                        {leaf.value === null ? (
                            <span className="text-sm text-ink-faint">
                                {leaf.documented
                                    ? t('oreParameter.defaultIs', { text: leaf.defaultText })
                                    : '—'}
                            </span>
                        ) : (
                            <span className={flag ? 'text-sm' : 'font-mono text-xs'}>
                                {flag
                                    ? t(
                                          leaf.value === 'true'
                                              ? 'oreParameter.yes'
                                              : 'oreParameter.no',
                                      )
                                    : words !== undefined && words !== leaf.value
                                      ? `${words} (${leaf.value})`
                                      : leaf.value}
                            </span>
                        )}
                    </div>
                ) : (
                    <ParameterControl
                        model={controlOfValueSet(leaf)}
                        label={label}
                        value={text}
                        unset={t('oreParameter.defaultOption', {
                            value: standard === '' ? '—' : standard,
                        })}
                        invalid={invalid}
                        onChange={onText}
                    />
                )}
            </ParameterRow>
            <ParameterDocuments
                doc={{
                    documented: leaf.documented,
                    name: leaf.ore,
                    description: leaf.description,
                    defaultText: leaf.defaultText,
                    format: leaf.format,
                    introducedIn: leaf.introducedIn,
                    removedIn: leaf.removedIn,
                    xpath: leaf.xpath,
                    choices: leaf.choices.map((choice) => choice.value),
                }}
                mode={mode}
                open={open}
                onToggle={onToggle}
            />
        </li>
    );
}

/** What the configuration is, who holds it, and how much of it the contract documents. */
function DetailsTab({
    curve,
    name,
    serving,
}: {
    readonly curve: CurveConfiguration;
    readonly name: string;
    readonly serving: ConfigurationDetail | undefined;
}): ReactNode {
    const { t } = useTranslation();
    const party =
        serving === undefined || serving.configuration.partyName === ''
            ? curve.partyId
            : serving.configuration.partyName;
    return (
        <section className="card space-y-4 p-4">
            <h2 className="text-sm font-medium">{t('curveConfiguration.tabs.details')}</h2>
            <p className="text-sm text-ink-muted">{t('curveConfiguration.details.lead')}</p>
            <p className="text-sm">{t('curveConfiguration.details.decides')}</p>
            <dl className="grid gap-x-6 gap-y-3 sm:grid-cols-4">
                <Detail label={t('curveConfiguration.fields.name')} value={name} />
                <Detail
                    label={t('curveConfiguration.fields.description')}
                    value={curve.description === '' ? '—' : curve.description}
                />
                <Detail
                    label={t('curveConfiguration.fields.version')}
                    value={String(curve.version)}
                />
                <Detail
                    label={t('curveConfiguration.fields.party')}
                    value={party === '' ? '—' : party}
                />
                <Detail
                    label={t('curveConfiguration.fields.sections')}
                    value={t('curveConfiguration.fields.sectionsValue', {
                        filled: String(filled(curve.sections).length),
                        total: String(curve.sections.length),
                    })}
                />
                <Detail
                    label={t('curveConfiguration.fields.entries')}
                    value={String(curvesOf(curve.sections).length)}
                />
                <Detail
                    label={t('curveConfiguration.fields.documented')}
                    value={t('curveConfiguration.fields.documentedValue', {
                        documented: String(curve.documented),
                        total: String(curve.total),
                    })}
                />
                <Detail
                    label={t('curveConfiguration.fields.lastChanged')}
                    value={curve.recordedAt === '' ? '—' : curve.recordedAt}
                />
                <Detail
                    label={t('curveConfiguration.fields.boundBy')}
                    value={
                        serving === undefined
                            ? '—'
                            : t('curveConfiguration.fields.boundByValue', {
                                  count: String(serving.reports.length),
                              })
                    }
                />
            </dl>
            <div className="space-y-1 border-t border-line-subtle pt-3">
                <h3 className="text-xs text-ink-faint">
                    {t('curveConfiguration.details.related')}
                </h3>
                <p className="text-sm">
                    <Link
                        to={configurationPath(curve.configurationId)}
                        className="text-accent hover:text-accent-bright"
                    >
                        {serving === undefined
                            ? shortId(curve.configurationId)
                            : serving.configuration.name}
                    </Link>
                </p>
                <p className="text-xs text-ink-faint">
                    {t('curveConfiguration.details.relatedLead')}
                </p>
            </div>
        </section>
    );
}

/** The reports that bind the configuration, read from the report configuration it fills. */
function UsedByTab({ configurationId }: { readonly configurationId: string }): ReactNode {
    const { t } = useTranslation();
    const serving = useQuery({
        queryKey: configurationKey(configurationId),
        queryFn: () => api.configuration(configurationId),
    });
    return (
        <section className="card space-y-4 p-4">
            <h2 className="text-sm font-medium">{t('curveConfiguration.tabs.usedBy')}</h2>
            <p className="text-sm text-ink-muted">{t('curveConfiguration.usedBy.lead')}</p>
            {serving.isPending ? (
                <p className="text-sm text-ink-muted" role="status">
                    {t('common.loading')}
                </p>
            ) : serving.isError ? (
                serving.error instanceof ApiFailure && serving.error.status === 403 ? (
                    <Notice tone="info">{t('curveConfiguration.usedBy.refused')}</Notice>
                ) : (
                    <Notice tone="error">
                        {t('curveConfiguration.usedBy.unread', { message: serving.error.message })}
                    </Notice>
                )
            ) : serving.data.reports.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('curveConfiguration.usedBy.none')}</p>
            ) : (
                <ul className="space-y-1 text-sm">
                    {serving.data.reports.map((report) => (
                        <li key={report.id} className="flex items-center gap-2">
                            <Link
                                to={reportPath(report.id)}
                                className="text-accent hover:text-accent-bright"
                            >
                                {report.name}
                            </Link>
                            {report.partyName !== '' && <Tag>{report.partyName}</Tag>}
                        </li>
                    ))}
                </ul>
            )}
        </section>
    );
}
