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
 */

/**
 * One configuration of engine settings, from
 * doc/prototypes/reporting/see-the-engine-settings-a-party-holds.org.
 *
 * The page has a tab for the details, one for each group of settings, one for the
 * reports that bind the configuration and one for its history. A group is read
 * and changed in place, with the documentation of each setting under it, so a
 * person who changes a setting reads what it means first. Save writes the group
 * alone against the version of its row, with a reason, and a group that changed
 * since is a conflict and not a silent overwrite.
 *
 * The documentation is not in this file. It comes from the ore parameter of each
 * setting, so the wording is edited in the data.
 */

import { useEffect, useState, type ReactNode } from 'react';
import { Link, useNavigate, useParams } from 'react-router';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
    engineSettings,
    type EngineDocument,
    type EngineSetting,
    type EngineHistoryEntry,
    type GroupId,
    type SettingValue,
} from '../api/engineSettings.js';
import { ApiFailure } from '../api/transport.js';
import { useHolds } from '../access/holds.js';
import { PERMISSION } from '../access/permissions.js';
import { type WatchedEntity } from '../events/EntityEvents.js';
import { useEntityChanges } from '../events/useEntityChanges.js';
import { useLoadState } from '../events/useLoadState.js';
import { useTranslation } from '../i18n/Provider.js';
import { TabPane } from '../refdata/parties.js';
import {
    DeleteDialog,
    LeavingFooter,
    RecordHeader,
    markStale,
    useCloseGuard,
} from '../refdata/records.js';
import { Crumbs, ReasonFields, useReason } from '../refdata/shared.js';
import { MY_PARTIES_QUERY_KEY } from '../refdata/partyScope.js';
import { api } from '../api/client.js';
import { actorPathFor } from '../access/PeoplePage.js';
import { useActorPictures, usePeopleDirectory } from '../access/PersonRef.js';
import { Timeline } from '../timeline/Timeline.js';
import { orderTimeline, wireTimestamp, type TimelineEvent } from '@ores/wire-protocol/browser';
import { useActingPartyId, useSwitchParty } from '../session/SessionProvider.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { PATHS, reportPath } from '../shell/paths.js';
import { EditingModeToggle, useEditingMode, type EditingMode } from '../ui/EditingMode.js';
import { FilterChip, ParameterDocumentationPanel } from '../ui/OreParameter.js';
import { ParameterControl } from '../parameters/ParameterControl.js';
import { ParameterRow } from '../parameters/ParameterRow.js';
import { controlOfEngineSetting } from '../parameters/control.js';
import { RefreshButton } from '../ui/RefreshButton.js';
import { useTabs } from '../ui/Tabs.js';
import { Button, Detail, Dialog, Field, Input, Notice, Tag, Textarea } from '../ui/Primitives.js';
import { RelativeTime } from '../ui/Time.js';
import {
    NO_FILTERS,
    changedColumns,
    controlText,
    defaultsOf,
    differenceOf,
    draftFromValues,
    draftOf,
    labelOf,
    problemOf,
    revertValues,
    setCount,
    shortDefault,
    stepsOf,
    valueOfText,
    visibleSettings,
    type Change,
    type Draft,
    type Problem,
    type EngineGroup,
    type Filters,
    type Step,
} from './model.js';

/** Where one configuration is cached. */
export function engineSettingsKey(id: string): readonly unknown[] {
    return ['engine-settings', id];
}

/** Where the documentation is cached. It does not change while a person works. */
export const DOCUMENTATION_KEY = ['engine-settings-documentation'] as const;

/**
 * What the page shows, for the change news to be heard about it. The configuration
 * is named by its key, so the page does not turn Refresh gold for another
 * configuration. A group is keyed by something else and makes the page stale for any
 * configuration's group.
 */
export function engineWatches(name: string | undefined): readonly WatchedEntity[] {
    return [
        {
            component: 'reporting',
            entity: 'engine_settings',
            ...(name === undefined ? {} : { record: { name } }),
        },
        { component: 'reporting', entity: 'engine_settings_fixings' },
        { component: 'reporting', entity: 'engine_settings_building' },
        { component: 'reporting', entity: 'engine_settings_execution' },
    ];
}

/** Where the history of one configuration is cached. */
export function historyKey(id: string): readonly unknown[] {
    return ['engine-settings', id, 'history'];
}

const TABS = ['details', 'fixings', 'building', 'execution', 'reports', 'history'] as const;

/** The eight characters a person reads to tell one identifier from another. */
function shortId(id: string): string {
    return id.slice(0, 8);
}

type Translate = ReturnType<typeof useTranslation>['t'];

/** A value as a person reads it: Yes or No for a flag, the number, the word, or nothing for the default. */
function valueLabel(value: SettingValue, t: Translate): string | null {
    if (value === null) {
        return null;
    }
    if (typeof value === 'boolean') {
        return value ? t('engineSettings.yes') : t('engineSettings.no');
    }
    return String(value);
}

export function EngineSettingsRecordPage(): ReactNode {
    const { id = '' } = useParams();
    const { t } = useTranslation();
    const holds = useHolds();
    const queries = useQueryClient();
    const navigate = useNavigate();
    const area = useAreaRoot('reporting');
    const mayWrite = holds(PERMISSION.engineSettingsWrite);
    const mayDelete = holds(PERMISSION.engineSettingsDelete);
    const [dialog, setDialog] = useState<'rename' | 'delete' | null>(null);
    const open = useQuery({
        queryKey: engineSettingsKey(id),
        queryFn: () => engineSettings.read(id),
        meta: { quiet: true },
    });
    const acting = useActingPartyId();
    const switchParty = useSwitchParty();
    const owner = open.data?.partyId;
    const parties = useQuery({
        queryKey: MY_PARTIES_QUERY_KEY,
        queryFn: api.myParties,
        enabled: switchParty !== undefined,
        meta: { quiet: true },
    });
    // The switch works only to a party the account works in. Until the parties are
    // read, or when the owner is not one of them, the notice states the fact and
    // offers no button that would fail.
    const mayReach =
        switchParty !== undefined &&
        owner !== undefined &&
        parties.data?.parties.some((party) => party.partyId === owner) === true;
    const switchToOwner = useMutation({
        mutationFn: async () => {
            await switchParty?.(owner ?? '');
        },
        onSuccess: () => queries.invalidateQueries(),
    });
    const documentation = useQuery({
        queryKey: DOCUMENTATION_KEY,
        queryFn: engineSettings.documentation,
        staleTime: 5 * 60_000,
        meta: { quiet: true },
    });
    const load = useLoadState([['engine-settings', id]]);
    const news = useEntityChanges(engineWatches(open.data?.name), load);
    const tabs = useTabs({
        label: t('engineSettings.tabs.label'),
        tabs: TABS,
        titleOf: (tab) =>
            t(
                tab === 'details' || tab === 'reports' || tab === 'history'
                    ? `engineSettings.tabs.${tab}`
                    : `engineSettings.groups.${tab}.title`,
            ),
    });
    // A tab the person has opened stays drawn and is hidden while another is open,
    // so leaving a tab never unmounts its reads and what was typed in it is kept.
    const [seen, setSeen] = useState<ReadonlySet<string>>(() => new Set([tabs.tab]));
    useEffect(() => {
        setSeen((before) => (before.has(tabs.tab) ? before : new Set(before).add(tabs.tab)));
    }, [tabs.tab]);
    const trail = [...area, { label: t('engineSettings.title'), to: PATHS.engineSettings }];

    if (open.isPending) {
        return (
            <ScreenFrame
                trail={<Crumbs parts={trail} />}
                title={shortId(id)}
                description={t('engineSettings.lead')}
            >
                <p className="text-sm text-ink-muted" role="status">
                    {t('common.loading')}
                </p>
            </ScreenFrame>
        );
    }

    if (open.isError) {
        const missing = open.error instanceof ApiFailure && open.error.status === 404;
        return (
            <ScreenFrame
                trail={<Crumbs parts={[...trail, { label: shortId(id) }]} />}
                title={shortId(id)}
                description={t('engineSettings.lead')}
            >
                {missing ? (
                    <div className="space-y-3">
                        <Notice tone="info">{t('refdata.records.notFound')}</Notice>
                        <Link
                            to={PATHS.engineSettings}
                            className="text-sm text-accent hover:text-accent-bright"
                        >
                            {t('refdata.records.backToList')}
                        </Link>
                    </div>
                ) : (
                    <Notice tone="error">{open.error.message}</Notice>
                )}
            </ScreenFrame>
        );
    }

    const document = open.data;
    // A configuration is read and written under the party that owns it, so a person
    // who sees it from a parent party reads it and changes it only after switching.
    const foreign = acting !== undefined && acting !== document.partyId;
    const mayChange = mayWrite && !foreign;
    const mayRemove = mayDelete && !foreign;
    const ownerName = document.partyName === '' ? document.partyId : document.partyName;
    const reload = (): void => {
        void queries.invalidateQueries({ queryKey: engineSettingsKey(id) });
    };
    const pane = (name: string, children: ReactNode): ReactNode => (
        <TabPane name={name} tab={tabs.tab} seen={seen}>
            {children}
        </TabPane>
    );

    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[...trail, { label: document.name }]}
                title={document.name}
                recordKey={shortId(document.id)}
                version={document.version}
                subdued
                access={mayChange}
                own={<RefreshButton onClick={reload} load={load} news={news} />}
                onEdit={mayChange ? () => setDialog('rename') : undefined}
                onDelete={mayRemove ? () => setDialog('delete') : undefined}
            />
            {foreign && (
                <Notice tone="info">
                    <span>{t('engineSettings.otherParty.notice', { party: ownerName })}</span>{' '}
                    {mayReach && (
                        <Button
                            variant="ghost"
                            pending={switchToOwner.isPending}
                            onClick={() => switchToOwner.mutate()}
                        >
                            {t('engineSettings.otherParty.switch', { party: ownerName })}
                        </Button>
                    )}
                    {switchToOwner.isError && (
                        <span role="alert">
                            {' '}
                            {t('engineSettings.otherParty.failed', {
                                message: switchToOwner.error.message,
                            })}
                        </span>
                    )}
                </Notice>
            )}
            {tabs.bar}
            {pane('details', <DetailsTab document={document} />)}
            {GROUP_IDS.map((groupId) => {
                const group = documentation.data?.groups.find(
                    (candidate) => candidate.id === groupId,
                );
                return pane(
                    groupId,
                    group !== undefined ? (
                        <GroupTab
                            key={group.id}
                            document={document}
                            group={group}
                            mayWrite={mayChange}
                        />
                    ) : documentation.isError ? (
                        <Notice tone="error">
                            {t('engineSettings.documentationUnread', {
                                message: documentation.error.message,
                            })}
                        </Notice>
                    ) : (
                        <p className="text-sm text-ink-muted" role="status">
                            {t('common.loading')}
                        </p>
                    ),
                );
            })}
            {pane('reports', <ReportsTab document={document} />)}
            {pane(
                'history',
                foreign ? (
                    <Notice tone="info">
                        {t('engineSettings.otherParty.history', { party: ownerName })}
                    </Notice>
                ) : (
                    <HistoryTab
                        document={document}
                        groups={documentation.data?.groups ?? []}
                        mayWrite={mayChange}
                    />
                ),
            )}
            {dialog === 'rename' && (
                <RenameDialog document={document} onClose={() => setDialog(null)} />
            )}
            {dialog === 'delete' && (
                <DeleteDialog
                    title={t('engineSettings.delete.title', { name: document.name })}
                    warning={
                        document.reports !== null && document.reports.length > 0
                            ? t('engineSettings.delete.bound', {
                                  count: String(document.reports.length),
                              })
                            : t('engineSettings.delete.warning')
                    }
                    remove={async (intent) =>
                        engineSettings.remove(document.id, {
                            reason_code: intent.reasonCode,
                            commentary: intent.commentary,
                        })
                    }
                    onClose={() => setDialog(null)}
                    onRemoved={() => {
                        void queries.invalidateQueries({
                            queryKey: ['records', 'engine-settings'],
                        });
                        void navigate(PATHS.engineSettings);
                    }}
                />
            )}
        </div>
    );
}

const GROUP_IDS = ['fixings', 'building', 'execution'] as const;

/** The name, who holds the configuration, how much of it is set and who reaches it. */
function DetailsTab({ document }: { readonly document: EngineDocument }): ReactNode {
    const { t } = useTranslation();
    const set = (['fixings', 'building', 'execution'] as const).filter(
        (id) => document.groups[id] !== null,
    );
    return (
        <section className="card space-y-4 p-4">
            <h2 className="text-sm font-medium">{t('engineSettings.tabs.details')}</h2>
            <dl className="grid gap-x-6 gap-y-3 sm:grid-cols-4">
                <Detail label={t('engineSettings.fields.name')} value={document.name} />
                <Detail
                    label={t('engineSettings.fields.party')}
                    value={document.partyName === '' ? document.partyId : document.partyName}
                />
                <Detail
                    label={t('engineSettings.fields.groupsSet')}
                    value={t('engineSettings.cell.groups', {
                        set: String(set.length),
                        count: '3',
                    })}
                />
                <Detail
                    label={t('engineSettings.fields.boundBy')}
                    value={
                        document.reports === null
                            ? t('engineSettings.reports.unread')
                            : t('engineSettings.reports.count', {
                                  count: String(document.reports.length),
                              })
                    }
                />
                <div className="sm:col-span-4">
                    <Detail
                        label={t('engineSettings.fields.description')}
                        value={
                            document.description === '' ? (
                                <span className="text-ink-faint">
                                    {t('engineSettings.cell.noDescription')}
                                </span>
                            ) : (
                                document.description
                            )
                        }
                    />
                </div>
                <Detail label={t('engineSettings.fields.modifiedBy')} value={document.modifiedBy} />
                <Detail
                    label={t('engineSettings.fields.changed')}
                    value={<RelativeTime at={document.recordedAt} />}
                />
            </dl>
        </section>
    );
}

/** One group: its settings with their values and documentation, read in place or changed in place. */
function GroupTab({
    document,
    group,
    mayWrite,
}: {
    readonly document: EngineDocument;
    readonly group: EngineGroup;
    readonly mayWrite: boolean;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const [mode, setMode] = useEditingMode();
    const part = document.groups[group.id as GroupId];
    const saved = draftOf(group, part);
    const [filters, setFilters] = useState<Filters>(NO_FILTERS);
    const [editing, setEditing] = useState<Readonly<Record<string, string>> | null>(null);
    const [opened, setOpened] = useState<ReadonlySet<string>>(new Set());
    const [commentary, setCommentary] = useState('');
    const [conflict, setConflict] = useState(false);
    const draft: Draft | null =
        editing === null
            ? null
            : Object.fromEntries(
                  group.settings.map((setting) => {
                      const value = valueOfText(setting, editing[setting.column] ?? '');
                      return [setting.column, value === 'invalid' ? null : value];
                  }),
              );
    const found =
        editing === null
            ? []
            : group.settings.flatMap((setting) => {
                  const problem = problemOf(
                      setting,
                      editing[setting.column] ?? '',
                      labelOf(setting),
                  );
                  return problem === null ? [] : [{ column: setting.column, problem }];
              });
    const problems = found.map((entry) => entry.problem);
    const problemOfColumn = new Map(found.map((entry) => [entry.column, entry.problem]));
    const changes = draft === null ? [] : changedColumns(group, saved, draft);
    const reason = useReason('amend', changes.length > 0);
    const missing = reason.needsCommentary && commentary.trim() === '';
    const reach = document.reports === null ? null : document.reports.length;
    const visible = visibleSettings(group, saved, filters);
    // A problem shows beside its field. A field a filter hides cannot show it, so
    // the group states that one, or Save would be disabled with no reason.
    const shownColumns = new Set(visible.map((setting) => setting.column));
    const hiddenProblems = found
        .filter((entry) => !shownColumns.has(entry.column))
        .map((entry) => entry.problem);
    const allOpen = visible.length > 0 && visible.every((setting) => opened.has(setting.column));
    const leave = (): void => {
        setEditing(null);
        setCommentary('');
        setConflict(false);
    };
    const save = useMutation({
        mutationFn: () => {
            if (draft === null) {
                throw new Error('There is nothing to save.');
            }
            return engineSettings.saveGroup(
                document.id,
                group.id as GroupId,
                part?.version ?? null,
                draft,
                { reason_code: reason.code, commentary: commentary.trim() },
            );
        },
        onSuccess: async () => {
            markStale(queries, [engineSettingsKey(document.id), ['records', 'engine-settings']]);
            leave();
        },
        onError: (error) => setConflict(error instanceof ApiFailure && error.status === 409),
    });
    const ready =
        changes.length > 0 && problems.length === 0 && !missing && reason.code !== '' && !conflict;
    const toggle = (column: string): void =>
        setOpened((current) => {
            const next = new Set(current);
            if (!next.delete(column)) {
                next.add(column);
            }
            return next;
        });

    return (
        <section className="card space-y-4 p-4">
            <header className="flex flex-wrap items-start justify-between gap-3">
                <div className="min-w-0">
                    <h2 className="text-sm font-medium">
                        {t(`engineSettings.groups.${group.id}.title`)}
                    </h2>
                    <p className="text-xs text-ink-faint">
                        {t(`engineSettings.groups.${group.id}.what`)}
                    </p>
                </div>
                <div className="flex flex-wrap items-center gap-2">
                    <Tag tone={group.changesNumbers ? 'warn' : 'muted'}>
                        {group.changesNumbers
                            ? t('engineSettings.changesNumbers')
                            : t('engineSettings.operational')}
                    </Tag>
                    <Tag tone={part === null ? 'muted' : 'neutral'}>
                        {part === null
                            ? t('engineSettings.notSet')
                            : t('engineSettings.partVersion', { version: String(part.version) })}
                    </Tag>
                    <EditingModeToggle mode={mode} onChange={setMode} />
                </div>
            </header>
            <div className="flex flex-wrap items-center gap-2">
                <FilterChip
                    pressed={filters.set}
                    onClick={() => setFilters({ ...filters, set: !filters.set })}
                >
                    {t('engineSettings.filters.set')}
                </FilterChip>
                <FilterChip
                    pressed={filters.numbers}
                    onClick={() => setFilters({ ...filters, numbers: !filters.numbers })}
                >
                    {t('engineSettings.filters.numbers')}
                </FilterChip>
                <span className="text-xs text-ink-faint">
                    {t('engineSettings.filters.shown', {
                        shown: String(visible.length),
                        count: String(group.settings.length),
                    })}
                </span>
                <span className="grow" />
                <Button
                    size="sm"
                    variant="ghost"
                    onClick={() =>
                        setOpened(
                            allOpen ? new Set() : new Set(visible.map((setting) => setting.column)),
                        )
                    }
                >
                    {allOpen
                        ? t('engineSettings.hideDocumentation')
                        : t('engineSettings.showDocumentation')}
                </Button>
                {mayWrite && editing === null && (
                    <Button
                        size="sm"
                        icon="edit"
                        onClick={() =>
                            setEditing(
                                Object.fromEntries(
                                    group.settings.map((setting) => [
                                        setting.column,
                                        controlText(saved[setting.column] ?? null),
                                    ]),
                                ),
                            )
                        }
                    >
                        {t('engineSettings.edit')}
                    </Button>
                )}
            </div>
            {visible.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('engineSettings.filters.none')}</p>
            ) : (
                <ul className="divide-y divide-line-subtle">
                    {visible.map((setting) => (
                        <SettingRow
                            key={setting.column}
                            setting={setting}
                            saved={saved[setting.column] ?? null}
                            text={editing === null ? null : (editing[setting.column] ?? '')}
                            changed={changes.includes(setting.column)}
                            problem={problemOfColumn.get(setting.column) ?? null}
                            open={opened.has(setting.column)}
                            mode={mode}
                            onToggle={() => toggle(setting.column)}
                            onText={(text) =>
                                editing !== null &&
                                setEditing({ ...editing, [setting.column]: text })
                            }
                            onFocus={() =>
                                setOpened((current) => new Set(current).add(setting.column))
                            }
                        />
                    ))}
                </ul>
            )}
            {editing === null ? (
                <p className="text-xs text-ink-faint">
                    {part === null
                        ? t('engineSettings.noRow', {
                              group: t(`engineSettings.groups.${group.id}.title`),
                          })
                        : t('engineSettings.rowBy', { who: part.modifiedBy })}
                </p>
            ) : (
                <div className="space-y-3 border-t border-line pt-4">
                    <Notice tone={group.changesNumbers ? 'warn' : 'info'}>
                        {group.changesNumbers && (
                            <span className="mr-1">{t('engineSettings.changesTheNumbers')}</span>
                        )}
                        {reach === null
                            ? t('engineSettings.reach.unknown')
                            : reach === 0
                              ? t('engineSettings.reach.none')
                              : t('engineSettings.reach.some', { count: String(reach) })}
                    </Notice>
                    {conflict && (
                        <Notice tone="error">
                            <span className="mr-2">{t('engineSettings.conflict')}</span>
                            <Button
                                size="sm"
                                onClick={() => {
                                    leave();
                                    void queries.invalidateQueries({
                                        queryKey: engineSettingsKey(document.id),
                                    });
                                }}
                            >
                                {t('engineSettings.reload')}
                            </Button>
                        </Notice>
                    )}
                    {hiddenProblems.map((problem) => (
                        <Notice key={problem.key + JSON.stringify(problem.params)} tone="error">
                            {t(problem.key, problem.params)}
                        </Notice>
                    ))}
                    {save.isError && !conflict && (
                        <Notice tone="error">{save.error.message}</Notice>
                    )}
                    <ReasonFields
                        reason={reason}
                        commentary={commentary}
                        onCommentary={setCommentary}
                        missing={missing}
                    />
                    <div className="flex flex-wrap items-center gap-2">
                        <Button
                            variant="primary"
                            icon="save"
                            disabled={!ready}
                            pending={save.isPending}
                            onClick={() => save.mutate()}
                        >
                            {t('engineSettings.save')}
                        </Button>
                        <Button variant="ghost" icon="cancel" onClick={leave}>
                            {t('common.cancel')}
                        </Button>
                        <span className="text-xs text-ink-faint" aria-live="polite">
                            {changes.length === 0
                                ? t('engineSettings.noChanges')
                                : t(
                                      changes.length === 1
                                          ? 'engineSettings.oneChange'
                                          : 'engineSettings.changes',
                                      { count: String(changes.length) },
                                  )}
                        </span>
                    </div>
                </div>
            )}
        </section>
    );
}

/** One setting: its label, its value or its control, and its documentation. */
export function SettingRow({
    setting,
    saved,
    text,
    changed,
    problem,
    open,
    mode,
    onToggle,
    onText,
    onFocus,
}: {
    readonly setting: EngineSetting;
    readonly saved: SettingValue;
    /** The text of the control while the group is being changed, else null. */
    readonly text: string | null;
    readonly changed: boolean;
    /** What is wrong with the text typed for this setting, drawn beside the control. */
    readonly problem: Problem | null;
    readonly open: boolean;
    readonly mode: EditingMode;
    readonly onToggle: () => void;
    readonly onText: (text: string) => void;
    readonly onFocus: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const label = labelOf(setting);
    const standard = shortDefault(setting.defaultText);
    const unset = t('engineSettings.defaultOption', { value: standard });
    const shown = valueLabel(saved, t);
    const marks = (
        <>
            {setting.changesNumbers && (
                <Tag tone="warn" small>
                    {t('engineSettings.changesNumbers')}
                </Tag>
            )}
            {changed && (
                <Tag tone="accent" small>
                    {t('engineSettings.changed')}
                </Tag>
            )}
        </>
    );
    return (
        <li className={changed ? 'space-y-2 bg-accent/5 py-3' : 'space-y-2 py-3'}>
            {text === null ? (
                <div className="flex flex-wrap items-baseline justify-between gap-3">
                    <div className="min-w-0">
                        <span className="text-sm font-medium">{label}</span> {marks}
                    </div>
                    <div className="text-sm">
                        {shown === null ? (
                            <span className="text-ink-faint">
                                {t('engineSettings.default', { value: standard })}
                            </span>
                        ) : (
                            <span className="font-mono text-xs">{shown}</span>
                        )}
                    </div>
                </div>
            ) : (
                <>
                    {(setting.changesNumbers || changed) && (
                        <div className="flex flex-wrap items-baseline gap-2">{marks}</div>
                    )}
                    <ParameterRow
                        label={label}
                        {...(setting.help === '' ? {} : { hint: setting.help })}
                    >
                        <Control
                            setting={setting}
                            label={label}
                            text={text}
                            unset={unset}
                            describedBy={problem === null ? undefined : `problem-${setting.column}`}
                            onText={onText}
                            onFocus={onFocus}
                        />
                    </ParameterRow>
                    {problem !== null && (
                        <p
                            id={`problem-${setting.column}`}
                            role="alert"
                            className="mt-1 text-xs text-down"
                        >
                            {t(problem.key, problem.params)}
                        </p>
                    )}
                </>
            )}
            <button
                type="button"
                aria-expanded={open}
                className="text-xs text-accent hover:text-accent-bright"
                onClick={onToggle}
            >
                {open ? t('engineSettings.hideDocumentation') : t('engineSettings.documentation')}
            </button>
            {open && (
                <ParameterDocumentationPanel
                    doc={{
                        documented: setting.documented,
                        name: setting.ore,
                        description: setting.description,
                        defaultText: setting.defaultText,
                        format: setting.format,
                        introducedIn: setting.introducedIn,
                        removedIn: setting.removedIn,
                        xpath: setting.xpath,
                    }}
                    mode={mode}
                />
            )}
        </li>
    );
}

function Control({
    setting,
    label,
    text,
    unset,
    describedBy,
    onText,
    onFocus,
}: {
    readonly setting: EngineSetting;
    readonly label: string;
    readonly text: string;
    readonly unset: string;
    /** The id of the problem text, when the text typed is not a value the setting takes. */
    readonly describedBy: string | undefined;
    readonly onText: (text: string) => void;
    readonly onFocus: () => void;
}): ReactNode {
    const model = controlOfEngineSetting(setting.kind, setting.options, setting.min, setting.max);
    // The shared control states a flag as true and false; the group holds the
    // words its own reading of a flag is written in.
    const flag = model.kind === 'boolean';
    return (
        <ParameterControl
            model={model}
            label={label}
            value={flag ? flagValue(text) : text}
            unset={unset}
            invalid={describedBy !== undefined}
            {...(describedBy === undefined ? {} : { describedBy })}
            onFocus={onFocus}
            onChange={(value) => onText(flag ? flagText(value) : value)}
        />
    );
}

/** The value of a flag as the shared control states it, or the empty text of the default. */
function flagValue(text: string): string {
    if (text === 'yes') return 'true';
    if (text === 'no') return 'false';
    return '';
}

/** The text of a flag from the value the shared control states, or the empty text of the default. */
function flagText(value: string): string {
    if (value === 'true') return 'yes';
    if (value === 'false') return 'no';
    return '';
}

/** The reports that bind the configuration, and what a change reaches. */
function ReportsTab({ document }: { readonly document: EngineDocument }): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card space-y-3 p-4">
            <h2 className="text-sm font-medium">{t('engineSettings.tabs.reports')}</h2>
            <p className="text-sm text-ink-muted">{t('engineSettings.reports.lead')}</p>
            {document.reports === null ? (
                <Notice tone="warn">{t('engineSettings.reports.unread')}</Notice>
            ) : document.reports.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('engineSettings.reports.none')}</p>
            ) : (
                <ul className="divide-y divide-line-subtle">
                    {document.reports.map((report) => (
                        <li key={report.id} className="py-2 text-sm">
                            <Link to={reportPath(report.id)} className="text-accent">
                                {report.name}
                            </Link>
                        </li>
                    ))}
                </ul>
            )}
        </section>
    );
}

/** The change a step made to its group, as a list of settings with the value before and after. */
function ChangeList({
    group,
    changes,
}: {
    readonly group: EngineGroup;
    readonly changes: readonly Change[];
}): ReactNode {
    const { t } = useTranslation();
    const named = (column: string): EngineSetting | undefined =>
        group.settings.find((setting) => setting.column === column);
    return (
        <ul className="space-y-0.5 text-xs">
            {changes.map((change) => {
                const setting = named(change.column);
                const standard = shortDefault(setting?.defaultText ?? '');
                const word = (value: SettingValue): string =>
                    valueLabel(value, t) ?? t('engineSettings.default', { value: standard });
                return (
                    <li key={change.column}>
                        <span className="font-medium">
                            {setting === undefined ? change.column : labelOf(setting)}
                        </span>
                        : {word(change.before)} → {word(change.after)}
                    </li>
                );
            })}
        </ul>
    );
}

const HISTORY_ENTITY = 'ores.reporting.engine_settings';

/** The entity type a history entry carries, so the timeline pairs the versions of one record. */
function entityOf(kind: EngineHistoryEntry['kind']): string {
    return kind === 'central' ? HISTORY_ENTITY : `${HISTORY_ENTITY}_${kind}`;
}

/**
 * The history as the entries the shared timeline draws. A group's entry carries the
 * label and the value of each setting, and the configuration's carries its name and
 * description, so the timeline shows each change as the difference it made. A
 * version that only moved the configuration's version number, because a group
 * changed, carries the same fields as the one before it and the timeline leaves it out.
 */
function eventsOf(
    document: EngineDocument,
    steps: readonly Step[],
    groups: readonly EngineGroup[],
    t: Translate,
): TimelineEvent[] {
    return steps.map(({ entry }) => {
        const group = groups.find((candidate) => candidate.id === entry.kind);
        const values = entry.values;
        let fields: { name: string; value: string }[] = [];
        if (values !== null && entry.kind === 'central') {
            fields = [
                { name: t('engineSettings.fields.name'), value: String(values['name'] ?? '') },
                {
                    name: t('engineSettings.fields.description'),
                    value: String(values['description'] ?? ''),
                },
            ];
        } else if (values !== null && group !== undefined) {
            fields = group.settings.map((setting) => ({
                name: labelOf(setting),
                value:
                    valueLabel(values[setting.column] ?? null, t) ??
                    t('engineSettings.default', { value: shortDefault(setting.defaultText) }),
            }));
        }
        return {
            entityType: entityOf(entry.kind),
            entityId: entry.kind === 'central' ? document.id : `${document.id}:${entry.kind}`,
            kind: entry.version === 1 ? 'raised' : 'changed',
            at: wireTimestamp(entry.recordedAt),
            actor: entry.modifiedBy,
            version: entry.version,
            reasonCode: entry.changeReason,
            commentary: entry.changeCommentary,
            fields,
        };
    });
}

/**
 * Every version of the configuration and of its groups, drawn by the shared
 * timeline as the entity screen specification draws a history. A group's version
 * older than the current one offers Revert to a person who may write.
 */
function HistoryTab({
    document,
    groups,
    mayWrite,
}: {
    readonly document: EngineDocument;
    readonly groups: readonly EngineGroup[];
    readonly mayWrite: boolean;
}): ReactNode {
    const { t } = useTranslation();
    const holds = useHolds();
    const pictureOf = useActorPictures();
    const { isService } = usePeopleDirectory();
    const history = useQuery({
        queryKey: historyKey(document.id),
        queryFn: () => engineSettings.history(document.id),
        meta: { quiet: true },
    });
    const [reverting, setReverting] = useState<Step | null>(null);
    if (history.isPending) {
        return (
            <p className="text-sm text-ink-muted" role="status">
                {t('common.loading')}
            </p>
        );
    }
    if (history.isError) {
        return <Notice tone="error">{history.error.message}</Notice>;
    }
    const steps = stepsOf(history.data);
    const stepOf = (event: TimelineEvent): Step | undefined =>
        steps.find(
            (step) =>
                entityOf(step.entry.kind) === event.entityType &&
                step.entry.version === event.version,
        );
    const groupOf = (step: Step): EngineGroup | undefined =>
        groups.find((candidate) => candidate.id === step.entry.kind);
    return (
        <div className="space-y-3">
            <Timeline
                timeline={{
                    subject: 'engine_settings',
                    id: document.id,
                    events: orderTimeline(eventsOf(document, steps, groups, t)),
                    gaps: [],
                }}
                hideUnchanged
                recordName={(event) =>
                    event.entityType === HISTORY_ENTITY
                        ? t('engineSettings.history.central')
                        : t(
                              `engineSettings.groups.${event.entityType.slice(HISTORY_ENTITY.length + 1)}.title`,
                          )
                }
                actorPath={actorPathFor(holds('iam::accounts:read'))}
                actorPicture={pictureOf}
                isService={isService}
                renderActions={(event) => {
                    const step = stepOf(event);
                    const group = step === undefined ? undefined : groupOf(step);
                    if (!mayWrite || step === undefined || group === undefined) {
                        return undefined;
                    }
                    return revertValues(group, step) === null ? undefined : (
                        <div className="mt-2 flex justify-end">
                            <Button size="sm" icon="revert" onClick={() => setReverting(step)}>
                                {t('engineSettings.history.revert')}
                            </Button>
                        </div>
                    );
                }}
            />
            {reverting !== null && (
                <RevertDialog
                    document={document}
                    group={groupOf(reverting)}
                    step={reverting}
                    onClose={() => setReverting(null)}
                />
            )}
        </div>
    );
}

/** Writes the values a group held at an earlier version as a new version, with a reason. */
function RevertDialog({
    document,
    group,
    step,
    onClose,
}: {
    readonly document: EngineDocument;
    readonly group: EngineGroup | undefined;
    readonly step: Step;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const values = group === undefined ? null : revertValues(group, step);
    const part = group === undefined ? null : document.groups[group.id as GroupId];
    const changes =
        group === undefined || values === null
            ? []
            : differenceOf(group, draftOf(group, part), values);
    const reason = useReason('amend', changes.length > 0);
    const [commentary, setCommentary] = useState('');
    const missing = reason.needsCommentary && commentary.trim() === '';
    const revert = useMutation({
        mutationFn: () => {
            if (group === undefined || values === null) {
                throw new Error('There is nothing to revert.');
            }
            return engineSettings.saveGroup(
                document.id,
                group.id as GroupId,
                part?.version ?? null,
                values,
                { reason_code: reason.code, commentary: commentary.trim() },
            );
        },
        onSuccess: async () => {
            markStale(queries, [engineSettingsKey(document.id), ['records', 'engine-settings']]);
            onClose();
        },
    });
    return (
        <Dialog
            title={t('engineSettings.revert.title')}
            name="engine-settings-revert"
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" onClick={onClose}>
                        {t('common.cancel')}
                    </Button>
                    <Button
                        variant="primary"
                        icon="revert"
                        disabled={changes.length === 0 || missing || reason.code === ''}
                        pending={revert.isPending}
                        onClick={() => revert.mutate()}
                    >
                        {t('engineSettings.history.revert')}
                    </Button>
                </>
            }
        >
            <div className="space-y-4">
                <p className="text-sm text-ink-muted">{t('engineSettings.revert.lead')}</p>
                {group !== undefined && changes.length > 0 ? (
                    <ChangeList group={group} changes={changes} />
                ) : (
                    <Notice tone="info">{t('engineSettings.revert.nothing')}</Notice>
                )}
                <ReasonFields
                    reason={reason}
                    commentary={commentary}
                    onCommentary={setCommentary}
                    missing={missing}
                />
                {revert.isError && <Notice tone="error">{revert.error.message}</Notice>}
            </div>
        </Dialog>
    );
}

/** Changes the name and the description of a configuration. */
function RenameDialog({
    document,
    onClose,
}: {
    readonly document: EngineDocument;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const [name, setName] = useState(document.name);
    const [description, setDescription] = useState(document.description);
    const changed = name.trim() !== document.name || description.trim() !== document.description;
    const reason = useReason('amend', changed);
    const [commentary, setCommentary] = useState('');
    const missing = reason.needsCommentary && commentary.trim() === '';
    const guard = useCloseGuard(changed, onClose);
    const save = useMutation({
        mutationFn: () =>
            engineSettings.rename(document.id, document.version, {
                name: name.trim(),
                description: description.trim(),
                intent: { reason_code: reason.code, commentary: commentary.trim() },
            }),
        onSuccess: async () => {
            markStale(queries, [engineSettingsKey(document.id), ['records', 'engine-settings']]);
            onClose();
        },
    });
    return (
        <Dialog
            title={t('engineSettings.rename.title')}
            name="engine-settings-rename"
            onClose={guard.close}
            footer={
                guard.leaving ? (
                    <LeavingFooter onStay={guard.stay} onLeave={onClose} />
                ) : (
                    <>
                        <Button variant="ghost" onClick={guard.close}>
                            {t('common.cancel')}
                        </Button>
                        <Button
                            variant="primary"
                            icon="save"
                            disabled={
                                !changed || name.trim() === '' || missing || reason.code === ''
                            }
                            pending={save.isPending}
                            onClick={() => save.mutate()}
                        >
                            {t('engineSettings.save')}
                        </Button>
                    </>
                )
            }
        >
            <div className="space-y-4">
                <Field label={t('engineSettings.fields.name')}>
                    <Input
                        value={name}
                        maxLength={200}
                        onChange={(event) => setName(event.target.value)}
                    />
                </Field>
                <Field label={t('engineSettings.fields.description')}>
                    <Textarea
                        value={description}
                        rows={3}
                        maxLength={2000}
                        onChange={(event) => setDescription(event.target.value)}
                    />
                </Field>
                <ReasonFields
                    reason={reason}
                    commentary={commentary}
                    onCommentary={setCommentary}
                    missing={missing}
                />
                {save.isError && <Notice tone="error">{save.error.message}</Notice>}
            </div>
        </Dialog>
    );
}
