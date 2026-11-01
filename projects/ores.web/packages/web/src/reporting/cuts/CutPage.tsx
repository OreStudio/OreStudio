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
 * One cut: the body it fixed, the acts that made it and the gaps it could not
 * close.
 *
 * The body is four inputs — the deal population, the fixed market data
 * snapshot, the versions of the reference data and the settings, and the engine
 * and the code — and the design requires every one of them, because a result
 * that could not be reproduced is a number with a claim it cannot meet. The cut
 * records a reference to each, so the page draws each reference and says what it
 * is.
 *
 * The state and the label are derived and never stored, so the page prints what
 * the service derived and says the basis beside it. The authorisation is the
 * inbox's: a cut whose kind requires one raises an approval request, and a cut
 * whose kind requires none states the basis and is usable when taken.
 */

import { useQuery } from '@tanstack/react-query';
import { Link, useParams } from 'react-router';
import type { ReactNode } from 'react';
import type { CutBody, CutKindRow, CutRecord, CutRun } from '@ores/wire-protocol/browser';
import { api } from '../../api/client.js';
import { PERMISSION } from '../../access/permissions.js';
import { usePermissions } from '../../access/holds.js';
import { useTranslation } from '../../i18n/Provider.js';
import { RecordTimeline } from '../../refdata/RecordTimeline.js';
import { RecordHeader, useRecordTabs } from '../../refdata/records.js';
import { useAreaRoot } from '../../shell/AreaTrail.js';
import { PATHS, cutTypePath, reportRunPath, requestPath, takeCutPath } from '../../shell/paths.js';
import { Notice, Tag } from '../../ui/Primitives.js';
import { RelativeTime } from '../../ui/Time.js';
import { HealthTag } from '../shared.js';
import {
    Absent,
    CutAuthorisationLabel,
    CutStateTag,
    IndicativeTag,
    Reference,
    StateBasis,
    factsOfRecord,
} from './shared.js';

/** The entity type the cut's versions are read under. */
export const CUT_ENTITY_TYPE = 'ores.reporting.cut';

export function CutPage(): ReactNode {
    const { t } = useTranslation();
    const { id } = useParams();
    const cut = useQuery({
        queryKey: ['cuts', 'cut', id ?? ''],
        queryFn: () => api.cut(id ?? ''),
    });

    if (cut.isPending) {
        return (
            <p className="text-sm text-ink-muted" role="status">
                {t('common.loading')}
            </p>
        );
    }
    if (cut.isError) {
        return <Notice tone="error">{cut.error.message}</Notice>;
    }
    return <CutBodyPage record={cut.data} />;
}

function CutBodyPage({ record }: { readonly record: CutRecord }): ReactNode {
    const { t } = useTranslation();
    const root = useAreaRoot('reporting');
    const { tab, bar } = useRecordTabs({
        label: record.label === '' ? record.cut.id : record.label,
        tabs: ['body', 'authorisation', 'runs', 'history'],
    });
    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[
                    ...root,
                    { label: t('cuts.title'), to: PATHS.cuts },
                    { label: record.label === '' ? record.cut.id : record.label },
                ]}
                title={record.label === '' ? record.cut.id : record.label}
                recordKey={record.cut.id}
                version={record.cut.version}
                access={null}
                detail={`${record.kind.name || record.cut.kindCode} · ${record.cut.timepoint} ${record.cut.timepointTimezone} · ${record.cut.scopeCode}`}
                own={
                    <span className="flex items-center gap-2">
                        <CutStateTag state={record.state} />
                        {record.cut.outputIsIndicative && <IndicativeTag />}
                    </span>
                }
            />
            {bar}
            {tab === 'body' && <BodyTab record={record} />}
            {tab === 'authorisation' && <AuthorisationTab record={record} />}
            {tab === 'runs' && <RunsTab record={record} />}
            {tab === 'history' && (
                <RecordTimeline entityType={CUT_ENTITY_TYPE} entityId={record.cut.id} />
            )}
        </div>
    );
}

/** One card of the body: what it is, and the reference the cut records for it. */
function Card({
    title,
    lead,
    children,
}: {
    readonly title: string;
    readonly lead: string;
    readonly children: ReactNode;
}): ReactNode {
    return (
        <section className="rounded-md border border-line bg-surface-raised p-4">
            <h2 className="text-sm font-semibold">{title}</h2>
            <p className="mt-1 text-xs text-ink-muted">{lead}</p>
            <dl className="mt-3 grid grid-cols-1 gap-4 sm:grid-cols-2">{children}</dl>
        </section>
    );
}

/** The four inputs the body fixed, and the identity a person quotes it by. */
function BodyTab({ record }: { readonly record: CutRecord }): ReactNode {
    const { t } = useTranslation();
    const body: CutBody = record.cut;
    return (
        <div className="space-y-4">
            {record.label === '' && (
                <Notice tone="warn">
                    {t('cuts.labelKindGone', { code: record.cut.kindCode })}
                </Notice>
            )}
            <div className="flex flex-wrap items-center gap-3 text-sm text-ink-muted">
                <span className="flex items-center gap-2">
                    <CutStateTag state={record.state} />
                    <StateBasis facts={factsOfRecord(record)} />
                </span>
                {body.outputIsIndicative && <IndicativeTag />}
                <Link to={cutTypePath(record.kind.code)} className="text-accent">
                    {record.kind.name}
                </Link>
            </div>

            {record.gaps.length > 0 && <Gaps record={record} />}

            <Card title={t('cuts.body.identity')} lead={t('cuts.body.identityLead')}>
                <Reference label={t('cuts.body.label')} value={record.label} />
                <Reference label={t('cuts.columns.kind')} value={record.kind.code} />
                <Reference label={t('cuts.columns.scope')} value={body.scopeCode} />
                <Reference label={t('cuts.columns.centre')} value={body.businessCentreCode} />
                <Reference
                    label={t('cuts.columns.timepoint')}
                    value={`${body.timepoint} (${body.timepointTimezone})`}
                />
                <Reference label={t('cuts.body.takeOrdinal')} value={String(body.takeOrdinal)} />
                <Reference label={t('cuts.body.cutTime')} value={body.cutTimeCode} />
                <Reference
                    label={t('cuts.body.retention')}
                    value={t('cuts.body.retentionValue', {
                        months: String(body.retentionMonths),
                        destination: body.retentionDestinationCode,
                    })}
                />
                <div className="min-w-0">
                    <dt className="text-xs text-ink-faint">{t('cuts.body.legalHold')}</dt>
                    <dd className="mt-0.5 text-sm">
                        {body.legalHold ? (
                            <Tag tone="warn">{t('cuts.body.held')}</Tag>
                        ) : (
                            <span className="text-ink-muted">{t('cuts.body.notHeld')}</span>
                        )}
                    </dd>
                </div>
            </Card>

            <Card title={t('cuts.body.deals')} lead={t('cuts.body.dealsLead')}>
                <Reference label={t('cuts.body.population')} value={body.dealPopulationId} />
                <div className="min-w-0">
                    <dt className="text-xs text-ink-faint">{t('cuts.body.dealCount')}</dt>
                    <dd className="mt-0.5 text-sm text-ink-faint">{t('cuts.body.dealCountGap')}</dd>
                </div>
            </Card>

            <Card title={t('cuts.body.marketData')} lead={t('cuts.body.marketDataLead')}>
                <Reference label={t('cuts.body.snapshot')} value={body.marketDataSnapshotId} />
                <div className="min-w-0">
                    <dt className="text-xs text-ink-faint">{t('cuts.body.sets')}</dt>
                    <dd className="mt-0.5 text-sm text-ink-faint">{t('cuts.body.setsGap')}</dd>
                </div>
            </Card>

            <Card title={t('cuts.body.referenceData')} lead={t('cuts.body.referenceDataLead')}>
                <Reference label={t('cuts.body.version')} value={body.referenceDataVersion} />
            </Card>

            <Card title={t('cuts.body.engine')} lead={t('cuts.body.engineLead')}>
                <Reference label={t('cuts.body.engineVersion')} value={body.engineVersion} />
                <Reference label={t('cuts.body.codeVersion')} value={body.codeVersion} />
            </Card>

            <Card title={t('cuts.body.record')} lead={t('cuts.body.recordLead')}>
                <Reference label={t('cuts.body.modifiedBy')} value={body.modifiedBy} />
                <Reference label={t('cuts.body.performedBy')} value={body.performedBy} />
                <div className="min-w-0">
                    <dt className="text-xs text-ink-faint">{t('cuts.body.recordedAt')}</dt>
                    <dd className="mt-0.5 text-sm">
                        <RelativeTime at={body.recordedAt} />
                    </dd>
                </div>
            </Card>
        </div>
    );
}

/**
 * The parts the assembly could not fill.
 *
 * A body that could not be assembled is a fact a person acts on, and silence
 * would make the cut look smaller rather than incomplete. Each row states the
 * part, the item and why it stands.
 */
function Gaps({ record }: { readonly record: CutRecord }): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="rounded-md border border-down/50 bg-down/10 p-4">
            <h2 className="text-sm font-semibold">
                {t('cuts.gaps.title', { count: String(record.gaps.length) })}
            </h2>
            <p className="mt-1 text-xs text-ink-muted">{t('cuts.gaps.lead')}</p>
            <table className="mt-3 w-full text-left text-sm">
                <thead>
                    <tr className="text-xs uppercase tracking-wide text-ink-faint">
                        <th className="py-1 pr-3 font-medium">{t('cuts.gaps.part')}</th>
                        <th className="py-1 pr-3 font-medium">{t('cuts.gaps.item')}</th>
                        <th className="py-1 font-medium">{t('cuts.gaps.reason')}</th>
                    </tr>
                </thead>
                <tbody>
                    {record.gaps.map((gap) => (
                        <tr key={gap.id} className="border-t border-down/30">
                            <td className="py-1 pr-3 font-mono text-xs">{gap.partCode}</td>
                            <td className="py-1 pr-3 font-mono text-xs break-all">{gap.item}</td>
                            <td className="py-1">{gap.reason}</td>
                        </tr>
                    ))}
                </tbody>
            </table>
        </section>
    );
}

/** Whether the kind required a signature, and the record of the one it got. */
function AuthorisationTab({ record }: { readonly record: CutRecord }): ReactNode {
    const { t } = useTranslation();
    const kind: CutKindRow = record.kind;
    const { ready, can } = usePermissions();
    const mayTake = ready && can({ all: [PERMISSION.cutsTake] });
    return (
        <div className="space-y-4">
            {record.cut.requiresAuthorisation ? (
                <Notice tone="info">{t('cuts.auth.required', { kind: kind.name })}</Notice>
            ) : (
                <Notice tone="success">{t('cuts.auth.notRequired', { kind: kind.name })}</Notice>
            )}

            <section className="rounded-md border border-line bg-surface-raised p-4">
                <h2 className="text-sm font-semibold">{t('cuts.auth.acts')}</h2>
                <dl className="mt-3 grid grid-cols-1 gap-4 sm:grid-cols-2">
                    <div className="min-w-0">
                        <dt className="text-xs text-ink-faint">{t('cuts.auth.taken')}</dt>
                        <dd className="mt-0.5 text-sm">
                            {record.cut.takenBy === '' ? (
                                <Absent />
                            ) : record.cut.takenAt === null ? (
                                record.cut.takenBy
                            ) : (
                                <span className="flex flex-col">
                                    <span>{record.cut.takenBy}</span>
                                    <span className="text-xs text-ink-faint">
                                        <RelativeTime at={record.cut.takenAt} />
                                    </span>
                                </span>
                            )}
                        </dd>
                    </div>
                    <div className="min-w-0">
                        <dt className="text-xs text-ink-faint">{t('cuts.auth.decision')}</dt>
                        <dd className="mt-0.5 text-sm">
                            {record.cut.refusedAt !== null ? (
                                t('cuts.auth.refusedAt', {
                                    when: record.cut.refusedAt.slice(0, 10),
                                })
                            ) : record.cut.approvedBy === '' ? (
                                <span className="text-ink-muted">{t('cuts.auth.pending')}</span>
                            ) : (
                                <span className="flex flex-col">
                                    <span>{record.cut.approvedBy}</span>
                                    {record.cut.approvedAt !== null && (
                                        <span className="text-xs text-ink-faint">
                                            <RelativeTime at={record.cut.approvedAt} />
                                        </span>
                                    )}
                                </span>
                            )}
                        </dd>
                    </div>
                    <div className="min-w-0">
                        <dt className="text-xs text-ink-faint">{t('cuts.auth.request')}</dt>
                        <dd className="mt-0.5 text-sm">
                            {record.cut.approvalRequestId === null ||
                            record.cut.approvalRequestId === '' ? (
                                <Absent />
                            ) : (
                                <Link
                                    to={requestPath(record.cut.approvalRequestId)}
                                    className="text-accent"
                                >
                                    {t('cuts.auth.openRequest')}
                                </Link>
                            )}
                        </dd>
                    </div>
                    <div className="min-w-0">
                        <dt className="text-xs text-ink-faint">{t('cuts.auth.decider')}</dt>
                        <dd className="mt-0.5 text-sm">
                            {kind.signed ? (
                                `${kind.approvalKindName ?? kind.approvalKindCode ?? ''}${
                                    kind.decidePermissionCode === null
                                        ? ''
                                        : ` · ${kind.decidePermissionCode}`
                                }`
                            ) : (
                                <CutAuthorisationLabel />
                            )}
                        </dd>
                    </div>
                </dl>
            </section>

            {record.cut.refusalComment !== '' && (
                <Notice tone="error">
                    {t('cuts.auth.refusal', { reason: record.cut.refusalComment })}
                </Notice>
            )}

            {/*
             * The design's remedy for a refused body is a new take of the same
             * occasion, offered only to a person who may take one.
             */}
            {record.state === 'refused' && mayTake && (
                <Notice tone="info">
                    {t('cuts.auth.takeAgainLead')}{' '}
                    <Link
                        to={takeCutPath({
                            kindCode: record.cut.kindCode,
                            scopeCode: record.cut.scopeCode,
                            centreCode: record.cut.businessCentreCode,
                        })}
                        className="text-accent"
                    >
                        {t('cuts.auth.takeAgain')}
                    </Link>
                </Notice>
            )}

            {record.detail !== null && (
                <section className="rounded-md border border-line bg-surface-raised p-4">
                    <h2 className="text-sm font-semibold">{t('cuts.auth.subject')}</h2>
                    <p className="mt-1 text-xs text-ink-muted">{t('cuts.auth.subjectLead')}</p>
                    <dl className="mt-3 grid grid-cols-1 gap-4 sm:grid-cols-2">
                        <div className="min-w-0">
                            <dt className="text-xs text-ink-faint">{t('cuts.auth.deals')}</dt>
                            <dd className="mt-0.5 font-mono text-sm tabular-nums">
                                {record.detail.dealCount}
                            </dd>
                        </div>
                        <div className="min-w-0">
                            <dt className="text-xs text-ink-faint">{t('cuts.auth.sets')}</dt>
                            <dd className="mt-0.5 font-mono text-sm tabular-nums">
                                {record.detail.marketDataSetCount}
                            </dd>
                        </div>
                        <div className="min-w-0">
                            <dt className="text-xs text-ink-faint">{t('cuts.auth.gaps')}</dt>
                            <dd className="mt-0.5 font-mono text-sm tabular-nums">
                                {record.detail.gapCount}
                            </dd>
                        </div>
                    </dl>
                    {/*
                     * The apply is the reporting side's reconciliation of an
                     * approved request, and it does not exist yet: nothing
                     * writes the cut's authorisation when the request is
                     * approved, so the panel states that instead of drawing an
                     * apply nobody made.
                     */}
                    <p className="mt-3 text-xs text-ink-muted">{t('cuts.auth.applyLater')}</p>
                </section>
            )}

            <Notice tone="info">{t('cuts.auth.fourEyes')}</Notice>
        </div>
    );
}

/**
 * The runs that priced from this cut.
 *
 * A run records the cut its report's selection resolved to, so the tab reads the
 * runs by that column: the run, the state it reached, the instant it started and
 * where it priced. Whether one run reproduces another is the audit journey's
 * comparison, which reads the two runs' results; this tab names the runs and
 * points at them.
 *
 * A body no run may price from has none, and the tab says why rather than
 * drawing an empty table.
 */
function RunsTab({ record }: { readonly record: CutRecord }): ReactNode {
    const { t } = useTranslation();
    const runs = useQuery({
        queryKey: ['cuts', 'cut', record.cut.id, 'runs'],
        queryFn: () => api.cutRuns(record.cut.id),
    });
    const notConsumable =
        record.state === 'draft' ||
        record.state === 'incomplete' ||
        record.state === 'taken' ||
        record.state === 'refused';

    if (runs.isPending) {
        return (
            <p className="text-sm text-ink-muted" role="status">
                {t('common.loading')}
            </p>
        );
    }
    if (runs.isError) {
        return <Notice tone="error">{runs.error.message}</Notice>;
    }
    const rows = runs.data.runs;
    if (rows.length === 0) {
        return (
            <Notice tone="info">
                {notConsumable ? t('cuts.runs.notConsumable') : t('cuts.runs.none')}
            </Notice>
        );
    }
    return (
        <div className="space-y-4">
            {record.state === 'superseded' && (
                <Notice tone="warn">
                    {t('cuts.runs.superseded', {
                        count: String(rows.length),
                        label: record.supersededByLabel ?? t('cuts.runs.aLaterCut'),
                    })}
                </Notice>
            )}
            <section className="rounded-md border border-line bg-surface-raised p-4">
                <h2 className="text-sm font-semibold">{t('cuts.runs.title')}</h2>
                <p className="mt-1 text-xs text-ink-muted">{t('cuts.runs.lead')}</p>
                <table className="mt-3 w-full text-left text-sm">
                    <thead>
                        <tr className="text-xs uppercase tracking-wide text-ink-faint">
                            <th className="py-1 pr-3 font-medium">{t('cuts.runs.run')}</th>
                            <th className="py-1 pr-3 font-medium">{t('cuts.runs.report')}</th>
                            <th className="py-1 pr-3 font-medium">{t('cuts.runs.state')}</th>
                            <th className="py-1 pr-3 font-medium">{t('cuts.runs.started')}</th>
                            <th className="py-1 font-medium">{t('cuts.runs.priced')}</th>
                        </tr>
                    </thead>
                    <tbody>
                        {rows.map((run) => (
                            <RunLine key={run.id} run={run} />
                        ))}
                    </tbody>
                </table>
            </section>
            <Notice tone="info">{t('cuts.runs.comparisonGap')}</Notice>
        </div>
    );
}

/** One run of a cut: the run, its report, the state it reached and when. */
function RunLine({ run }: { readonly run: CutRun }): ReactNode {
    const { t } = useTranslation();
    return (
        <tr className="border-t border-line">
            <td className="py-1 pr-3">
                <Link
                    to={reportRunPath(run.id)}
                    className="font-mono text-xs text-accent break-all"
                >
                    {run.id}
                </Link>
            </td>
            <td className="py-1 pr-3 text-sm">{run.name === '' ? <Absent /> : run.name}</td>
            <td className="py-1 pr-3 text-sm">
                <HealthTag health={run.health} stateName={run.stateName} hasRun />
            </td>
            <td className="py-1 pr-3 text-sm">
                {run.startedAt === null ? <Absent /> : <RelativeTime at={run.startedAt} />}
            </td>
            <td className="py-1 text-sm">
                {run.pricedAt === null ? <Absent /> : <RelativeTime at={run.pricedAt} />}
            </td>
        </tr>
    );
}
