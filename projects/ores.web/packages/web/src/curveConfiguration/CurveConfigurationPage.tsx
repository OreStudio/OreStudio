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

import { useQuery } from '@tanstack/react-query';
import { Fragment, useEffect, useState, type ReactNode } from 'react';
import { Link, useParams } from 'react-router';
import { api } from '../api/client.js';
import {
    curveConfiguration,
    type CurveConfiguration,
    type CurveEntry,
    type CurveParameter,
    type CurveSection,
} from '../api/curveConfiguration.js';
import { ApiFailure } from '../api/transport.js';
import { useTranslation } from '../i18n/Provider.js';
import { ParameterRow } from '../parameters/ParameterRow.js';
import { shortDefault } from '../parameters/control.js';
import { RecordTimeline } from '../refdata/RecordTimeline.js';
import { Crumbs } from '../refdata/shared.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { reportPath } from '../shell/paths.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { EditingModeToggle, useEditingMode, type EditingMode } from '../ui/EditingMode.js';
import { Detail, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { ParameterDocuments, ParameterFilters } from '../ui/OreParameter.js';
import { TabPane } from '../refdata/parties.js';
import { useTabs } from '../ui/Tabs.js';

/**
 * One curve configuration, read-only: its details, one tab per section of the
 * ORE document and the entries of that section, the reports that bind it, and
 * its history.
 *
 * Every word comes from the server. The sections and their entry elements come
 * from the curve section data, each leaf's label, meaning, default and value set
 * come from the ORE parameter contract, and each value comes from the entry's
 * own row. The screen holds no table that names a curve family and no ORE text,
 * so a family the data adds reaches the screen with no edit here.
 *
 * The page reads and does not write. A curve configuration is one document of
 * thirty-four families, and a write that drew only the leaves this page knows
 * would drop the rows it does not: the write arrives with the guard that refuses
 * a value outside its leaf's set, so no client can store one.
 */

/** The entity type the history of a curve configuration row is recorded under. */
export const CURVE_CONFIGURATION_ENTITY = 'ores.refdata.curve_configuration';

/** Where the curve configuration the address names is cached. */
export function curveConfigurationKey(configurationId: string): readonly unknown[] {
    return ['curve-configuration', configurationId];
}

/** Where the report configuration a curve configuration fills is cached, as the configuration page reads it. */
export function configurationKey(configurationId: string): readonly unknown[] {
    return ['configuration', configurationId];
}

/** The eight characters a person reads to tell one identifier from another. */
function shortId(id: string): string {
    return id.slice(0, 8);
}

export function CurveConfigurationPage(): ReactNode {
    const { id = '' } = useParams();
    const { t } = useTranslation();
    const area = useAreaRoot('reporting');
    const [mode, setMode] = useEditingMode();
    const reading = useQuery({
        queryKey: curveConfigurationKey(id),
        queryFn: () => curveConfiguration.read(id),
    });
    // The report configuration names the curve configuration and lists the
    // reports that bind it. A read that fails leaves the leaves in view and says
    // so, because the curve configuration is what the person came for.
    const serving = useQuery({
        queryKey: configurationKey(id),
        queryFn: () => api.configuration(id),
    });
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
    const tabs = [
        'details',
        ...curve.sections.map((section) => `section:${section.code}`),
        'usedBy',
        'history',
    ] as const;
    const panel = useTabs({
        label: t('curveConfiguration.tabs.label'),
        tabs,
        titleOf: (tab) =>
            tab.startsWith('section:')
                ? tab.slice('section:'.length)
                : t(`curveConfiguration.tabs.${tab}`),
    });
    // A tab the person opened stays drawn and hidden while another is open, so
    // leaving a section never loses the documentation they opened in it.
    const [seen, setSeen] = useState<ReadonlySet<string>>(() => new Set([panel.tab]));
    useEffect(() => {
        setSeen((before) => (before.has(panel.tab) ? before : new Set(before).add(panel.tab)));
    }, [panel.tab]);
    const pane = (name: string, children: ReactNode): ReactNode => (
        <TabPane name={name} tab={panel.tab} seen={seen}>
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
            {pane('details', <DetailsTab curve={curve} name={name} />)}
            {curve.sections.map((section) => (
                <Fragment key={section.code}>
                    {pane(`section:${section.code}`, <SectionTab section={section} mode={mode} />)}
                </Fragment>
            ))}
            {pane('usedBy', <UsedByTab configurationId={id} />)}
            {pane(
                'history',
                <section className="card space-y-4 p-4">
                    <h2 className="text-sm font-medium">{t('curveConfiguration.tabs.history')}</h2>
                    <p className="text-sm text-ink-muted">{t('curveConfiguration.history.lead')}</p>
                    <RecordTimeline entityType={CURVE_CONFIGURATION_ENTITY} entityId={curve.id} />
                </section>,
            )}
            {serving.isError && !(serving.error instanceof ApiFailure) && (
                <Notice tone="warn">
                    {t('curveConfiguration.usedBy.unread', { message: serving.error.message })}
                </Notice>
            )}
        </div>
    );
}

/** What the configuration is, who holds it, and how much of it the contract documents. */
function DetailsTab({
    curve,
    name,
}: {
    readonly curve: CurveConfiguration;
    readonly name: string;
}): ReactNode {
    const { t } = useTranslation();
    const entries = curve.sections.reduce((count, section) => count + section.entries.length, 0);
    return (
        <section className="card space-y-4 p-4">
            <h2 className="text-sm font-medium">{t('curveConfiguration.tabs.details')}</h2>
            <p className="text-sm text-ink-muted">{t('curveConfiguration.details.lead')}</p>
            <dl className="grid gap-x-6 gap-y-3 sm:grid-cols-4">
                <Detail label={t('curveConfiguration.fields.name')} value={name} />
                <Detail
                    label={t('curveConfiguration.fields.version')}
                    value={String(curve.version)}
                />
                <Detail
                    label={t('curveConfiguration.fields.party')}
                    value={curve.partyId === '' ? '—' : curve.partyId}
                />
                <Detail
                    label={t('curveConfiguration.fields.sections')}
                    value={String(curve.sections.length)}
                />
                <Detail label={t('curveConfiguration.fields.entries')} value={String(entries)} />
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
                <div className="sm:col-span-4">
                    <Detail
                        label={t('curveConfiguration.fields.curveConfiguration')}
                        value={curve.id === '' ? '—' : curve.id}
                        mono
                    />
                </div>
            </dl>
        </section>
    );
}

/** One section: its entries, each drawn as a collection row of leaves. */
function SectionTab({
    section,
    mode,
}: {
    readonly section: CurveSection;
    readonly mode: EditingMode;
}): ReactNode {
    const { t } = useTranslation();
    const [setOnly, setSetOnly] = useState(false);
    const [opened, setOpened] = useState<ReadonlySet<string>>(new Set());
    const leaves = section.entries.flatMap((entry) => entry.parameters);
    const shown = leaves.filter((leaf) => leaf.value !== null);
    return (
        <section className="space-y-4">
            <header className="space-y-1">
                <h2 className="text-sm font-medium">
                    {t('curveConfiguration.section.title', {
                        section: section.code,
                        entry: section.entryElement,
                    })}
                </h2>
                {section.description !== '' && (
                    <p className="text-sm text-ink-muted">{section.description}</p>
                )}
            </header>
            {!section.mapped && (
                <Notice tone="warn">{t('curveConfiguration.section.unmapped')}</Notice>
            )}
            <ParameterFilters
                set={setOnly}
                onSet={() => setSetOnly((before) => !before)}
                shown={shown.length}
                total={leaves.length}
                noun={t('oreParameter.nouns.parameters')}
            />
            {section.entries.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('curveConfiguration.section.empty')}</p>
            ) : (
                section.entries.map((entry) => (
                    <EntryCard
                        key={entry.id}
                        entry={entry}
                        section={section}
                        mode={mode}
                        setOnly={setOnly}
                        opened={opened}
                        onToggle={(key) =>
                            setOpened((before) => {
                                const next = new Set(before);
                                if (!next.delete(key)) {
                                    next.add(key);
                                }
                                return next;
                            })
                        }
                        onAll={(open) =>
                            setOpened(
                                open
                                    ? new Set(
                                          entry.parameters.map((leaf) => `${entry.id}|${leaf.ore}`),
                                      )
                                    : new Set(),
                            )
                        }
                    />
                ))
            )}
        </section>
    );
}

/** One entry: what it is, its parameters, and the child elements it holds. */
function EntryCard({
    entry,
    section,
    mode,
    setOnly,
    opened,
    onToggle,
    onAll,
}: {
    readonly entry: CurveEntry;
    readonly section: CurveSection;
    readonly mode: EditingMode;
    readonly setOnly: boolean;
    readonly opened: ReadonlySet<string>;
    readonly onToggle: (key: string) => void;
    readonly onAll: (open: boolean) => void;
}): ReactNode {
    const { t } = useTranslation();
    const visible = entry.parameters.filter((leaf) => !setOnly || leaf.value !== null);
    const gaps = entry.parameters.filter((leaf) => !leaf.documented).length;
    const allOpen =
        visible.length > 0 && visible.every((leaf) => opened.has(`${entry.id}|${leaf.ore}`));
    return (
        <article className="card space-y-3 p-4">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <div className="min-w-0 space-y-1">
                    <h3 className="text-sm font-medium">
                        {entry.curveId === '' ? shortId(entry.id) : entry.curveId}
                    </h3>
                    <p className="text-sm text-ink-muted">
                        {entry.description === ''
                            ? t('curveConfiguration.entry.noDescription', {
                                  entry: section.entryElement,
                              })
                            : entry.description}
                    </p>
                </div>
                <div className="flex flex-wrap items-center gap-2">
                    <Tag small>
                        {t('curveConfiguration.entry.parameters', {
                            count: String(entry.parameters.length),
                        })}
                    </Tag>
                    {gaps > 0 && (
                        <Tag small tone="warn">
                            {t('curveConfiguration.entry.gaps', { count: String(gaps) })}
                        </Tag>
                    )}
                    {visible.length > 0 && (
                        <button
                            type="button"
                            className="text-xs text-accent hover:text-accent-bright"
                            onClick={() => onAll(!allOpen)}
                        >
                            {allOpen
                                ? t('oreParameter.hideAllDocumentation')
                                : t('oreParameter.showAllDocumentation')}
                        </button>
                    )}
                </div>
            </header>
            {visible.length === 0 ? (
                <p className="text-sm text-ink-muted">
                    {t('oreParameter.filters.none', { noun: t('oreParameter.nouns.parameters') })}
                </p>
            ) : (
                <ul className="divide-y divide-line-subtle">
                    {visible.map((leaf) => {
                        const key = `${entry.id}|${leaf.ore}`;
                        return (
                            <LeafRow
                                key={key}
                                leaf={leaf}
                                mode={mode}
                                open={opened.has(key)}
                                onToggle={() => onToggle(key)}
                            />
                        );
                    })}
                </ul>
            )}
            {entry.blocks.length > 0 && (
                <footer className="space-y-1 border-t border-line-subtle pt-3">
                    <p className="text-xs text-ink-faint">{t('curveConfiguration.entry.blocks')}</p>
                    <ul className="flex flex-wrap gap-2">
                        {entry.blocks.map((block) => (
                            <li key={block.element} className="flex items-center gap-1">
                                <Tag small>{block.element}</Tag>
                                {block.present === null ? (
                                    <span className="text-xs text-ink-faint">
                                        {t('curveConfiguration.entry.blockNotRecorded')}
                                    </span>
                                ) : (
                                    <span className="text-xs text-ink-faint">
                                        {block.present
                                            ? t('curveConfiguration.entry.blockStated')
                                            : t('curveConfiguration.entry.blockAbsent')}
                                    </span>
                                )}
                            </li>
                        ))}
                    </ul>
                </footer>
            )}
        </article>
    );
}

/** One leaf: its label, the value in force, and the documentation under it. */
function LeafRow({
    leaf,
    mode,
    open,
    onToggle,
}: {
    readonly leaf: CurveParameter;
    readonly mode: EditingMode;
    readonly open: boolean;
    readonly onToggle: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const label = leaf.label === '' ? leaf.ore : leaf.label;
    const fallback = shortDefault(leaf.defaultText);
    return (
        <li className="space-y-2 py-3">
            <ParameterRow
                label={label}
                required={leaf.required}
                {...(leaf.help === '' ? {} : { hint: leaf.help })}
            >
                <div className="flex flex-wrap items-center gap-2">
                    {leaf.value === null ? (
                        <span className="text-sm text-ink-faint">
                            {fallback === ''
                                ? t('curveConfiguration.leaf.notSet')
                                : t('oreParameter.defaultIs', { text: fallback })}
                        </span>
                    ) : leaf.choices.length > 0 &&
                      leaf.choices.some((choice) => choice.value === leaf.value) ? (
                        <span className="text-sm">
                            {leaf.choices.find((choice) => choice.value === leaf.value)?.label}
                        </span>
                    ) : (
                        <span className="font-mono text-xs">{leaf.value}</span>
                    )}
                    {!leaf.documented && (
                        <Tag small tone="warn">
                            {t('curveConfiguration.leaf.gap')}
                        </Tag>
                    )}
                </div>
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
                }}
                mode={mode}
                open={open}
                onToggle={onToggle}
            />
        </li>
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
