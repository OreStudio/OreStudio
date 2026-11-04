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
 * One report definition, as the record screen standard draws a record page: the
 * trail, the display field as the title, the key and version line, then the
 * actions, the tabs in the address, the details with the last-changed line, and
 * the history of the record.
 *
 * A change is made in the record dialog the standard names, opened by Edit on
 * the card it changes, and never in a form on the page. The BFF rebuilds the
 * write from the read it makes at write time, so a field the dialog does not
 * offer keeps its stored value, and the version the dialog was filled from
 * travels with the write so a record somebody else changed is a conflict rather
 * than a silent overwrite.
 *
 * The page's own gaps are stated where they matter and not faked: the server
 * offers no delete for a definition, no create, and no server search of the
 * runs it lists, so the page offers none of those actions.
 */

import { useEffect, useState, type ReactNode } from 'react';
import { Link, useNavigate, useParams } from 'react-router';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import type {
    ReportDetail,
    ReportNameChoice,
    ReportRunRowView,
    ReportScheduleOutcome,
} from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { ApiFailure } from '../api/transport.js';
import { useHolds } from '../access/holds.js';
import { PERMISSION } from '../access/permissions.js';
import { useTranslation } from '../i18n/Provider.js';
import { RecordDetails, RecordDialog, RecordHeader, type FieldSpec } from '../refdata/records.js';
import { RecordStoreProvider } from '../refdata/store.js';
import { useEntityChanges } from '../events/useEntityChanges.js';
import { useLoadState } from '../events/useLoadState.js';
import { Crumbs } from '../refdata/shared.js';
import { localSource, RecordTable } from '../refdata/RecordList.js';
import { ReadFailure } from '../operations/GridJobs.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { useTabs } from '../ui/Tabs.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { PATHS, reportPath, reportRunPath } from '../shell/paths.js';
import { AccessMark } from '../ui/AccessMark.js';
import { Button, Dialog } from '../ui/Primitives.js';
import { RefreshButton } from '../ui/RefreshButton.js';
import { Detail, Notice, Tag } from '../ui/Primitives.js';
import { RelativeTime } from '../ui/Time.js';
import { CronField, CronValue } from '../ui/CronField.js';
import { cronValid } from '../ui/cron.js';
import { useActingPartyId } from '../session/SessionProvider.js';
import { REPORTS_EVENTS, REPORTS_RESOURCE, reportingStore } from './store.js';
import { ReportConfigurations } from './ConfigurationPage.js';
import {
    Absent,
    HealthTag,
    ON_DEMAND_SCHEDULE,
    formatDuration,
    isOnDemand,
    runDuration,
} from './shared.js';

/** Where one definition's record is cached. */
export function reportKey(id: string): readonly unknown[] {
    return ['records', REPORTS_RESOURCE, 'key', id];
}

/** The eight characters a person reads to tell one identifier from another. */
function shortId(id: string): string {
    return id.slice(0, 8);
}

/**
 * What scheduling or unscheduling a report did, as the page states it.
 *
 * The operation answers a count, the ids it refused and its own words, and the
 * page turns that into one of these. Nothing is assumed: a count of zero is the
 * operation saying it changed nothing, which is not a success to dress up as
 * one.
 */
export type ScheduleNotice =
    | { readonly kind: 'scheduled' }
    | { readonly kind: 'already-scheduled' }
    | { readonly kind: 'unscheduled' }
    | { readonly kind: 'not-scheduled' }
    | { readonly kind: 'refused'; readonly message: string };

/**
 * What the schedule operation's answer means for the definition asked about.
 *
 * An id the operation refused is the refusal, whatever the count says. A refusal
 * carries the operation's words, because the reason a schedule was refused is the
 * server's to state: it is the party the definition belongs to, a cron the
 * scheduler cannot read, or a grant it would not make.
 */
export function noticeOfSchedule(outcome: ReportScheduleOutcome, id: string): ScheduleNotice {
    if (outcome.failedIds.includes(id)) {
        return {
            kind: 'refused',
            message:
                outcome.message === ''
                    ? 'The server refused to schedule this report and stated no reason.'
                    : outcome.message,
        };
    }
    return outcome.changed > 0 ? { kind: 'scheduled' } : { kind: 'already-scheduled' };
}

/** What the unschedule operation's answer means for the definition asked about. */
export function noticeOfUnschedule(outcome: ReportScheduleOutcome, id: string): ScheduleNotice {
    if (outcome.failedIds.includes(id)) {
        return {
            kind: 'refused',
            message:
                outcome.message === ''
                    ? 'The server refused to unschedule this report and stated no reason.'
                    : outcome.message,
        };
    }
    return outcome.changed > 0 ? { kind: 'unscheduled' } : { kind: 'not-scheduled' };
}

/**
 * The outcome of a schedule change in words.
 *
 * A refusal is an error and says why, because it is the one outcome a person has
 * to act on. Everything else is the state the report reached, and a change that
 * changed nothing says so rather than reading as a success.
 */
export function ScheduleNoticeView({ notice }: { readonly notice: ScheduleNotice }): ReactNode {
    const { t } = useTranslation();
    switch (notice.kind) {
        case 'scheduled':
            return <Notice tone="success">{t('reporting.schedule.scheduled')}</Notice>;
        case 'already-scheduled':
            return <Notice tone="info">{t('reporting.schedule.alreadyScheduled')}</Notice>;
        case 'unscheduled':
            return <Notice tone="success">{t('reporting.schedule.unscheduled')}</Notice>;
        case 'not-scheduled':
            return <Notice tone="info">{t('reporting.schedule.notScheduled')}</Notice>;
        case 'refused':
            return (
                <Notice tone="error">
                    {t('reporting.schedule.refused', { message: notice.message })}
                </Notice>
            );
    }
}

/** Whether an address segment names a record by id rather than by name. */
function isId(segment: string): boolean {
    return /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(segment);
}

/**
 * The definition's own fields, in the order the record states them.
 *
 * The field table drives the details and the dialog both, so the two cannot
 * disagree about what a definition holds. A field that is fixed once the record
 * exists is marked, so the dialog refuses to edit it.
 */
const DEFINITION_FIELDS: readonly FieldSpec[] = [
    {
        field: 'name',
        history: 'Name',
        labelKey: 'reporting.fields.name',
        kind: { kind: 'text', max: 200 },
        fixed: true,
        means: { key: 'reporting.means.name' },
    },
    {
        field: 'description',
        history: 'Description',
        labelKey: 'reporting.fields.description',
        kind: { kind: 'text', max: 2000 },
        blank: true,
    },
    {
        field: 'report_type',
        history: 'Report Type',
        labelKey: 'reporting.fields.report_type',
        kind: { kind: 'choice', options: ['risk'] },
        means: { key: 'reporting.means.reportType', values: ['risk'] },
    },
    {
        field: 'schedule_expression',
        history: 'Schedule Expression',
        labelKey: 'reporting.fields.schedule_expression',
        kind: { kind: 'cron' },
        help: 'reporting.detail.scheduleExpressionHint',
    },
    {
        field: 'concurrency_policy',
        history: 'Concurrency Policy',
        labelKey: 'reporting.fields.concurrency_policy',
        kind: { kind: 'choice', options: ['skip', 'queue', 'fail'] },
        means: { key: 'reporting.means.concurrencyPolicy', values: ['skip', 'queue', 'fail'] },
    },
    {
        field: 'prepared_input_key',
        history: 'Prepared Input Key',
        labelKey: 'reporting.fields.prepared_input_key',
        kind: { kind: 'text', max: 500 },
        blank: true,
        means: { key: 'reporting.means.preparedInputKey' },
    },
    {
        field: 'pre_processing',
        history: 'Pre-Processing',
        labelKey: 'reporting.fields.pre_processing',
        kind: { kind: 'choice', options: ['execute', 'substitute'] },
        means: { key: 'reporting.means.preProcessing', values: ['execute', 'substitute'] },
    },
    {
        field: 'post_processing',
        history: 'Post-Processing',
        labelKey: 'reporting.fields.post_processing',
        kind: { kind: 'choice', options: ['execute', 'ignore'] },
        means: { key: 'reporting.means.postProcessing', values: ['execute', 'ignore'] },
    },
    {
        field: 'skipped_families',
        history: 'Skipped Families',
        labelKey: 'reporting.fields.skipped_families',
        kind: { kind: 'text', max: 2000 },
        blank: true,
        means: { key: 'reporting.means.skippedFamilies' },
    },
    {
        field: 'is_official',
        history: 'Is Official',
        labelKey: 'reporting.fields.is_official',
        kind: { kind: 'bool' },
        help: 'reporting.detail.isOfficialHint',
    },
    {
        field: 'regulatory_book_type',
        history: 'Regulatory Book Type',
        labelKey: 'reporting.fields.regulatory_book_type',
        kind: { kind: 'choice', options: ['Trading', 'Banking'] },
        optional: true,
        means: { key: 'reporting.means.regulatoryBookType', values: ['Trading', 'Banking'] },
    },
];

/**
 * The record page, inside the store the shared record parts read.
 *
 * The store is the reporting one, so the dialog, the revert dialog and the
 * details read and write the reporting routes and not reference data's.
 */
export function ReportPage(): ReactNode {
    return (
        <RecordStoreProvider store={reportingStore}>
            <ReportBody />
        </RecordStoreProvider>
    );
}

function ReportBody(): ReactNode {
    const { id = '' } = useParams();
    const { t } = useTranslation();
    const holds = useHolds();
    const mayEdit = holds(PERMISSION.reportDefinitionsWrite);
    const mayRun = holds(PERMISSION.reportInstancesWrite);
    /*
     * Scheduling a report is changing a definition, so it is the write; taking a
     * schedule away is removing something the definition holds, so it is the
     * delete the unschedule operation itself checks.
     */
    const maySchedule = holds(PERMISSION.reportDefinitionsWrite);
    const mayUnschedule = holds(PERMISSION.reportDefinitionsDelete);
    const actingParty = useActingPartyId();
    const queries = useQueryClient();
    const navigate = useNavigate();
    const [editing, setEditing] = useState(false);
    /** What the last schedule change did, as the page states it under the title. */
    const [notice, setNotice] = useState<ScheduleNotice | null>(null);
    /** The schedule the dialog is filling, while a person chooses one. */
    const [choosingSchedule, setChoosingSchedule] = useState(false);
    const [chosenExpression, setChosenExpression] = useState('');
    /**
     * A run started from this page.
     *
     * The trigger answers the run it created and the dispatch is the service's,
     * so the page moves to that run rather than waiting for it: the run page
     * states the state it reaches, and the runs tab lists it beside the others.
     */
    const startRun = useMutation({
        mutationFn: async (definitionId: string) => await api.reportRunNow(definitionId),
        onSuccess: (runId) => {
            void queries.invalidateQueries({ queryKey: reportKey('runs') });
            if (runId !== '') {
                void navigate(reportRunPath(runId));
            }
        },
    });
    /**
     * The schedule change asked for, and the outcome it answered.
     *
     * The answer is a result and not an exception even when the operation
     * refuses, because the definition, its count and its words are the service's
     * to state; the notice under the title is where a person reads them. A call
     * that fails outright is a refusal too, and carries the transport's words.
     */
    const schedule = useMutation({
        mutationFn: async (asked: {
            readonly id: string;
            readonly expression: string | undefined;
            readonly unschedule: boolean;
        }) =>
            asked.unschedule
                ? await api.unscheduleReport(asked.id)
                : await api.scheduleReport(asked.id, asked.expression),
        onSuccess: (outcome, asked) => {
            setNotice(
                asked.unschedule
                    ? noticeOfUnschedule(outcome, asked.id)
                    : noticeOfSchedule(outcome, asked.id),
            );
            setChoosingSchedule(false);
            void queries.invalidateQueries({ queryKey: reportKey(asked.id) });
        },
        onError: (error: Error) => {
            setNotice({ kind: 'refused', message: error.message });
        },
    });
    const area = useAreaRoot('reporting');
    /*
     * The address is whatever a link or a bookmark carried: an id, or the name
     * an older address saved. The server resolves both, so this page never
     * answers a name with a refusal. A name that one definition carries comes
     * back as that record, and its id is seeded too, so the address can settle
     * on the canonical one without reading the record a second time.
     *
     * The page states its own failures, so the reads here are quiet and one
     * failure is one message rather than a notice and a banner both.
     */
    const open = useQuery({
        queryKey: reportKey(id),
        queryFn: async () => {
            const opened = await api.report(id);
            if (opened.kind === 'record' && opened.detail.definition.id !== id) {
                queries.setQueryData(reportKey(opened.detail.definition.id), opened);
            }
            return opened;
        },
        meta: { quiet: true },
    });
    const choices = useQuery({
        queryKey: ['report-choices'],
        queryFn: api.reportChoices,
        staleTime: 5 * 60_000,
        meta: { quiet: true },
    });
    const load = useLoadState([['records', REPORTS_RESOURCE]]);
    const news = useEntityChanges([REPORTS_EVENTS], load);
    const opened = open.data;
    const record = opened?.kind === 'record' ? opened.detail : undefined;
    const canonical = record?.definition.id;

    /*
     * A name that one definition carries replaces the address with that
     * definition's own, so a person who followed a stale link ends on the
     * address the list links to. The record is already drawn while this runs.
     */
    useEffect(() => {
        if (canonical !== undefined && canonical !== id) {
            void navigate(reportPath(canonical), { replace: true });
        }
    }, [canonical, id, navigate]);

    const trail = [...area, { label: t('reporting.title'), to: PATHS.reports }];
    const title = isId(id) ? shortId(id) : id;

    const { tab, bar } = useTabs({
        label: title,
        tabs: REPORT_TABS,
        titleOf: (candidate) => t(`reporting.tabs.${candidate}`),
    });
    if (open.isPending) {
        return (
            <ScreenFrame
                trail={<Crumbs parts={trail} />}
                title={id}
                description={t('reporting.lead')}
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
                trail={<Crumbs parts={[...trail, { label: title }]} />}
                title={title}
                description={t('reporting.lead')}
            >
                {missing ? (
                    <div className="space-y-3">
                        <Notice tone="info">{t('refdata.records.notFound')}</Notice>
                        <Link
                            to={PATHS.reports}
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

    if (open.data.kind === 'choice') {
        return <NameChooser name={id} choices={open.data.choices} />;
    }

    const { definition, report, unread } = open.data.detail;
    /*
     * Refresh reads again what the page shows and nothing else. It does not
     * reload the page, because a save's own change event turns this button gold
     * like any other change.
     */
    const reload = (): void => {
        void queries.invalidateQueries({ queryKey: reportKey(definition.id) });
    };
    const editFields = fieldsWith(
        choices.data ?? { reportTypes: [], concurrencyPolicies: [], regulatoryBookTypes: [] },
    );
    /*
     * Whether the session acts for the party that owns the definition.
     *
     * A schedule is made for the definition's party: its runs act there, under a
     * grant the person consents to in that party. So the actions are offered only
     * where the page can tell the two parties apart and they are the same. Outside
     * a signed-in session there is no party to compare, and the page does not
     * invent a difference: the server refuses a schedule of another party in its
     * own words.
     */
    const actsForTheParty = actingParty === undefined || actingParty === definition.party_id;
    /*
     * The dialog opens on the schedule the definition holds, unless it holds the
     * one that never fires: then there is nothing to offer, and the person states
     * the schedule the report will run on.
     */
    const askSchedule = (): void => {
        setChosenExpression(
            definition.schedule_expression === ON_DEMAND_SCHEDULE
                ? ''
                : definition.schedule_expression,
        );
        setChoosingSchedule(true);
    };
    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[...trail, { label: definition.name }]}
                title={definition.name}
                recordKey={shortId(definition.id)}
                version={definition.version}
                subdued
                access={null}
                own={
                    <>
                        {mayRun && (
                            <Button
                                pending={startRun.isPending}
                                pendingLabel={t('reporting.run.starting')}
                                onClick={() => startRun.mutate(definition.id)}
                            >
                                {t('reporting.run.start')}
                            </Button>
                        )}
                        {definition.scheduled
                            ? mayUnschedule && (
                                  <Button
                                      icon="remove"
                                      pending={schedule.isPending}
                                      pendingLabel={t('reporting.schedule.working')}
                                      disabled={!actsForTheParty}
                                      onClick={() =>
                                          schedule.mutate({
                                              id: definition.id,
                                              expression: undefined,
                                              unschedule: true,
                                          })
                                      }
                                  >
                                      {t('reporting.schedule.unschedule')}
                                  </Button>
                              )
                            : maySchedule && (
                                  <Button
                                      icon="calendar"
                                      pending={schedule.isPending}
                                      pendingLabel={t('reporting.schedule.working')}
                                      disabled={!actsForTheParty}
                                      onClick={askSchedule}
                                  >
                                      {t('reporting.schedule.schedule')}
                                  </Button>
                              )}
                        <RefreshButton onClick={reload} load={load} news={news} />
                    </>
                }
            />
            {!actsForTheParty && (maySchedule || mayUnschedule) && (
                <Notice tone="info">
                    {t('reporting.schedule.otherParty', {
                        party: definition.party_name,
                    })}
                </Notice>
            )}
            {notice !== null && <ScheduleNoticeView notice={notice} />}
            {bar}
            {tab === 'configurations' && <ReportConfigurations definitionId={definition.id} />}
            {tab === 'runs' && <ReportRuns definitionId={definition.id} />}
            {tab === 'details' && (
                <div className="space-y-4">
                    <RecordDetails
                        specs={DEFINITION_FIELDS}
                        row={definition}
                        heading={{
                            title: t('reporting.tabs.details'),
                            onEdit: mayEdit ? () => setEditing(true) : undefined,
                        }}
                        extra={[
                            [
                                t('reporting.detail.party'),
                                <PartyDetail detail={open.data.detail} />,
                                t('reporting.detail.partyHint'),
                            ],
                            [
                                t('reporting.detail.schedule'),
                                <ScheduleDetail
                                    definition={definition}
                                    nextFireAt={report.nextFireAt}
                                />,
                                t('reporting.detail.scheduleHint'),
                            ],
                            [
                                t('reporting.detail.health'),
                                <HealthTag
                                    health={report.health}
                                    stateName={report.stateName}
                                    hasRun={report.latestRun !== null}
                                />,
                                t('reporting.detail.healthHint'),
                            ],
                        ]}
                    />
                </div>
            )}
            {choosingSchedule && (
                <Dialog
                    title={t('reporting.schedule.chooseTitle', { name: definition.name })}
                    name="report schedule"
                    onClose={() => setChoosingSchedule(false)}
                    footer={
                        <div className="flex justify-end gap-2">
                            <Button
                                type="button"
                                variant="ghost"
                                icon="cancel"
                                onClick={() => setChoosingSchedule(false)}
                            >
                                {t('refdata.records.cancel')}
                            </Button>
                            <Button
                                type="button"
                                variant="primary"
                                icon="calendar"
                                pending={schedule.isPending}
                                pendingLabel={t('reporting.schedule.working')}
                                disabled={!cronValid(chosenExpression.trim())}
                                onClick={() =>
                                    schedule.mutate({
                                        id: definition.id,
                                        expression: chosenExpression.trim(),
                                        unschedule: false,
                                    })
                                }
                            >
                                {t('reporting.schedule.schedule')}
                            </Button>
                        </div>
                    }
                >
                    <div className="space-y-3">
                        <p className="text-sm text-ink-muted">
                            {t('reporting.schedule.chooseLead')}
                        </p>
                        <CronField
                            label={t('reporting.schedule.when')}
                            value={chosenExpression}
                            onChange={setChosenExpression}
                            hint={t('reporting.schedule.whenHint')}
                        />
                    </div>
                </Dialog>
            )}
            {editing && (
                <RecordDialog
                    title={t('refdata.records.editTitle', { code: definition.name })}
                    resource={REPORTS_RESOURCE}
                    specs={editFields}
                    row={definition}
                    keep={['id']}
                    onClose={() => setEditing(false)}
                />
            )}
        </div>
    );
}

/** The tabs of a report: the definition, its runs, and the results of its latest run. */
const REPORT_TABS = ['details', 'configurations', 'runs'] as const;

/**
 * The runs of one report, newest first. A row opens the run, whose page holds
 * its details, results, problems and files.
 */
function ReportRuns({ definitionId }: { readonly definitionId: string }): ReactNode {
    const { t } = useTranslation();
    const field = 'reporting.runs';
    const runs = useQuery({
        queryKey: ['report-runs', definitionId],
        queryFn: () => api.reportRuns(definitionId),
        meta: { quiet: true },
    });
    if (runs.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (runs.isError) {
        return <ReadFailure error={runs.error} onRetry={() => void runs.refetch()} />;
    }
    const time = (at: string | null): ReactNode => (at === null ? '—' : <RelativeTime at={at} />);
    return (
        <RecordTable<ReportRunRowView>
            source={localSource<ReportRunRowView>({
                key: `report-runs-${definitionId}`,
                rows: runs.data,
                revision: String(runs.dataUpdatedAt),
                searchOf: (row) => `${row.name} ${row.state_name}`,
            })}
            plural={t(`${field}.plural`)}
            keyOf={(row) => row.id}
            pathOf={(row) => reportRunPath(row.id)}
            columns={[
                {
                    id: 'name',
                    header: t(`${field}.name`),
                    cell: (row) => row.name || row.id.slice(0, 8),
                },
                {
                    id: 'state',
                    header: t(`${field}.state`),
                    cell: (row) => (
                        <HealthTag health={row.health} stateName={row.state_name} hasRun />
                    ),
                },
                {
                    id: 'started',
                    header: t(`${field}.started`),
                    cell: (row) => time(row.started_at),
                },
                {
                    id: 'completed',
                    header: t(`${field}.completed`),
                    cell: (row) => time(row.completed_at),
                },
                {
                    id: 'duration',
                    header: t(`${field}.duration`),
                    cell: (row) =>
                        row.duration_ms === null ? '—' : formatDuration(row.duration_ms, t),
                },
                {
                    id: 'pricedAt',
                    header: t(`${field}.pricedAt`),
                    mono: true,
                    cell: (row) => row.priced_at?.slice(0, 10) ?? '—',
                },
            ]}
        />
    );
}

/** The definition's fields, with the options of the coded fields as the server holds them. */
function fieldsWith(choices: {
    readonly reportTypes: readonly string[];
    readonly concurrencyPolicies: readonly string[];
    readonly regulatoryBookTypes: readonly string[];
}): readonly FieldSpec[] {
    const optionsOf: Readonly<Record<string, readonly string[]>> = {
        report_type: choices.reportTypes,
        concurrency_policy: choices.concurrencyPolicies,
        regulatory_book_type: choices.regulatoryBookTypes,
    };
    return DEFINITION_FIELDS.map((spec) => {
        const options = optionsOf[spec.field];
        return options !== undefined && options.length > 0 && spec.kind.kind === 'choice'
            ? { ...spec, kind: { ...spec.kind, options } }
            : spec;
    });
}

/** The party the definition belongs to, by name, and the page of the party it names. */
function PartyDetail({ detail }: { readonly detail: ReportDetail }): ReactNode {
    const { t } = useTranslation();
    if (detail.definition.party_name === '') {
        return <span className="text-ink-faint">{t('reporting.cell.partyNotStated')}</span>;
    }
    return (
        <Link
            to={`/parties/${encodeURIComponent(detail.definition.party_id)}`}
            className="text-accent hover:text-accent-bright"
        >
            {detail.definition.party_name}
        </Link>
    );
}

/**
 * When a definition runs, as the page states it beside the expression it holds.
 *
 * The field table already states the expression, so this says the part the
 * expression cannot: a report with no scheduler job is on demand and fires on
 * nothing, whatever it holds. A report that is scheduled states the fire the
 * scheduler gives it, and says so plainly when the scheduler states none.
 */
function ScheduleDetail({
    definition,
    nextFireAt,
}: {
    readonly definition: ReportDetail['definition'];
    readonly nextFireAt: string | null;
}): ReactNode {
    const { t } = useTranslation();
    if (isOnDemand(definition.scheduled, definition.schedule_expression)) {
        return <span>{t('reporting.schedule.onDemand')}</span>;
    }
    return nextFireAt === null ? (
        <span className="text-ink-faint">{t('reporting.detail.noNextFire')}</span>
    ) : (
        <RelativeTime at={nextFireAt} />
    );
}

/**
 * The definitions a shared name offers, as a page a person chooses from.
 *
 * A name is unique only within a party, so an address that names one can stand
 * for several records. The page states the name it was asked for, then one row
 * per definition: the party that owns it, its schedule and what its latest run
 * did. Each row links to its own id, which is what settles the address. It owns
 * the trail because it is a page in its own right and not a record page.
 */
function NameChooser({
    name,
    choices,
}: {
    readonly name: string;
    readonly choices: readonly ReportNameChoice[];
}): ReactNode {
    const { t } = useTranslation();
    const area = useAreaRoot('reporting');
    const trail = [...area, { label: t('reporting.title'), to: PATHS.reports }, { label: name }];
    return (
        <ScreenFrame
            trail={<Crumbs parts={trail} />}
            title={name}
            description={t('reporting.choose.lead', { count: String(choices.length) })}
        >
            <div className="space-y-4">
                <section className="overflow-hidden rounded-md border border-line">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                <th className="px-4 py-2 font-medium">
                                    {t('reporting.columns.report')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('reporting.columns.party')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('reporting.columns.schedule')}
                                </th>
                                <th className="px-4 py-2 font-medium">
                                    {t('reporting.columns.health')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {choices.map((choice) => (
                                <tr
                                    key={choice.id}
                                    className="border-b border-line-subtle last:border-b-0 hover:bg-surface-hover"
                                >
                                    <td className="px-4 py-2">
                                        <Link
                                            to={reportPath(choice.id)}
                                            className="text-accent hover:text-accent-bright"
                                        >
                                            {choice.name}
                                        </Link>
                                    </td>
                                    <td className="px-4 py-2">
                                        {choice.partyName === '' ? (
                                            <span className="text-ink-faint">
                                                {t('reporting.cell.partyNotStated')}
                                            </span>
                                        ) : (
                                            <Tag>{choice.partyName}</Tag>
                                        )}
                                    </td>
                                    <td className="px-4 py-2 text-xs">
                                        <CronValue expression={choice.scheduleExpression} />
                                    </td>
                                    <td className="px-4 py-2">
                                        <HealthTag
                                            health={choice.health}
                                            stateName={choice.stateName}
                                            hasRun={choice.hasRun}
                                        />
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </section>
                <Link to={PATHS.reports} className="text-sm text-accent hover:text-accent-bright">
                    {t('refdata.records.backToList')}
                </Link>
            </div>
        </ScreenFrame>
    );
}
