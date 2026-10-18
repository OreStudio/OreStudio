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
 * One report definition: what it is, how its latest run went, and the fields a
 * person may change.
 *
 * The form offers six of the record's fields and the BFF rebuilds the write
 * from the read it makes at write time, so a field this screen does not show
 * keeps its stored value. The version the form was filled from travels with the
 * write, so a definition somebody else changed in between is a conflict rather
 * than a silent overwrite. A successful save reads the definition again and
 * states what the server now holds.
 */

import { useState, type ReactNode } from 'react';
import { useParams } from 'react-router';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import type { ReportDefinitionView, ReportDetail } from '@ores/wire-protocol/browser';
import { api, type ReportEdit } from '../api/client.js';
import { useHolds } from '../access/holds.js';
import { PERMISSION } from '../access/permissions.js';
import { useTranslation } from '../i18n/Provider.js';
import { Crumbs } from '../refdata/shared.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { PATHS } from '../shell/paths.js';
import {
    Button,
    Detail,
    Field,
    Input,
    Notice,
    PageHeader,
    Tag,
    Textarea,
} from '../ui/Primitives.js';
import { formatDateTime } from '../ui/Time.js';
import { HealthTag, ProblemCounts, formatDuration, runDuration } from './shared.js';

/** The address one definition's row and record are read from. */
export function reportKey(name: string): readonly unknown[] {
    return ['reporting', 'report', name];
}

/**
 * Why an edit to a definition is made, from the seeded reasons.
 *
 * `common.non_material_update` is the reason that applies to an amend and needs
 * no commentary, which is what a screen writing one field has to say.
 */
const EDIT_REASON = 'common.non_material_update';

/** The parts of a definition the screen states but does not edit. */
function ReadOnlyFields({ definition }: { readonly definition: ReportDefinitionView }): ReactNode {
    const { t, language } = useTranslation();
    return (
        <dl className="grid gap-x-6 gap-y-3 sm:grid-cols-3">
            <Detail label={t('reporting.detail.name')} value={definition.name} mono />
            <Detail
                label={t('reporting.detail.description')}
                value={definition.description === '' ? '—' : definition.description}
            />
            <Detail label={t('reporting.detail.reportType')} value={definition.reportType} mono />
            <Detail label={t('reporting.detail.party')} value={definition.partyId} mono />
            <Detail
                label={t('reporting.detail.concurrencyPolicy')}
                value={definition.concurrencyPolicy}
                mono
            />
            <Detail
                label={t('reporting.detail.schedulerJob')}
                value={definition.schedulerJobId ?? t('reporting.cell.notStated')}
                mono={definition.schedulerJobId !== null}
            />
            <Detail
                label={t('reporting.detail.runGrant')}
                value={definition.runGrantId ?? t('reporting.cell.notStated')}
                mono={definition.runGrantId !== null}
            />
            <Detail label={t('reporting.detail.version')} value={String(definition.version)} mono />
            <Detail label={t('reporting.detail.modifiedBy')} value={definition.modifiedBy} />
            <Detail label={t('reporting.detail.performedBy')} value={definition.performedBy} />
            <Detail
                label={t('reporting.detail.changeReason')}
                value={definition.changeReasonCode}
                mono
            />
            <Detail
                label={t('reporting.detail.commentary')}
                value={definition.changeCommentary === '' ? '—' : definition.changeCommentary}
            />
            <Detail
                label={t('reporting.detail.recordedAt')}
                value={formatDateTime(definition.recordedAt, language)}
            />
        </dl>
    );
}

/** The draft the form edits, taken from the record it was drawn from. */
function draftOf(definition: ReportDefinitionView): ReportEdit {
    return {
        description: definition.description,
        schedule_expression: definition.scheduleExpression,
        is_official: definition.isOfficial,
        pre_processing: definition.preProcessing,
        prepared_input_key: definition.preparedInputKey,
        post_processing: definition.postProcessing,
    };
}

/**
 * The edit form.
 *
 * It is drawn afresh from each read, so a save that changes the version
 * repopulates it with what the server now holds rather than with what was typed.
 */
function EditForm({
    detail,
    onSaved,
}: {
    readonly detail: ReportDetail;
    readonly onSaved: (version: number) => void;
}): ReactNode {
    const { t } = useTranslation();
    const client = useQueryClient();
    const [draft, setDraft] = useState<ReportEdit>(() => draftOf(detail.definition));
    const [refusal, setRefusal] = useState<string | null>(null);
    const save = useMutation({
        mutationFn: (edit: ReportEdit) =>
            api.saveReport(detail.definition.name, {
                version: detail.definition.version,
                change: edit,
                reasonCode: EDIT_REASON,
                commentary: '',
            }),
        onSuccess: async (saved) => {
            setRefusal(null);
            onSaved(saved.definition.version);
            client.setQueryData(reportKey(saved.definition.name), saved);
            await client.invalidateQueries({ queryKey: reportKey(saved.definition.name) });
        },
        onError: (error: Error) => setRefusal(error.message),
    });

    return (
        <form
            className="space-y-4"
            onSubmit={(event) => {
                event.preventDefault();
                save.mutate(draft);
            }}
        >
            {refusal !== null && <Notice tone="error">{refusal}</Notice>}
            <div className="grid gap-4 sm:grid-cols-2">
                <Field label={t('reporting.detail.description')} className="sm:col-span-2">
                    <Textarea
                        rows={3}
                        value={draft.description}
                        onChange={(event) =>
                            setDraft({ ...draft, description: event.target.value })
                        }
                    />
                </Field>
                <Field
                    label={t('reporting.detail.scheduleExpression')}
                    hint={t('reporting.detail.scheduleExpressionHint')}
                >
                    <Input
                        value={draft.schedule_expression}
                        required
                        maxLength={200}
                        onChange={(event) =>
                            setDraft({ ...draft, schedule_expression: event.target.value })
                        }
                    />
                </Field>
                <Field
                    label={t('reporting.detail.preparedInputKey')}
                    hint={t('reporting.detail.preparedInputKeyHint')}
                >
                    <Input
                        value={draft.prepared_input_key}
                        maxLength={500}
                        onChange={(event) =>
                            setDraft({ ...draft, prepared_input_key: event.target.value })
                        }
                    />
                </Field>
                <Field label={t('reporting.detail.preProcessing')}>
                    <Input
                        value={draft.pre_processing}
                        maxLength={2000}
                        onChange={(event) =>
                            setDraft({ ...draft, pre_processing: event.target.value })
                        }
                    />
                </Field>
                <Field label={t('reporting.detail.postProcessing')}>
                    <Input
                        value={draft.post_processing}
                        maxLength={2000}
                        onChange={(event) =>
                            setDraft({ ...draft, post_processing: event.target.value })
                        }
                    />
                </Field>
                <div>
                    <span className="mb-1.5 block text-sm font-medium text-ink-muted">
                        {t('reporting.detail.isOfficial')}
                    </span>
                    <label className="flex items-center gap-2 text-sm">
                        <input
                            type="checkbox"
                            checked={draft.is_official}
                            onChange={(event) =>
                                setDraft({ ...draft, is_official: event.target.checked })
                            }
                        />
                        <span>{t('reporting.detail.isOfficialHint')}</span>
                    </label>
                </div>
            </div>
            <div className="flex items-center gap-3">
                <Button type="submit" variant="primary" pending={save.isPending}>
                    {t('reporting.detail.save')}
                </Button>
                <span className="text-xs text-ink-faint">{t('reporting.detail.editLead')}</span>
            </div>
        </form>
    );
}

/**
 * The latest run, as the list states it, so the detail and the list agree.
 *
 * A page that saw no run says which it is: a definition that never ran, or a
 * latest-run read that failed, in which case nothing about the run was checked.
 */
function LatestRun({
    report,
    unread,
}: {
    readonly report: ReportDetail['report'];
    readonly unread: ReportDetail['unread'];
}): ReactNode {
    const { t, language } = useTranslation();
    if (report.latestRun === null) {
        const refusal = unread.find((entry) => entry.cell === 'latestRun');
        if (refusal !== undefined) {
            return (
                <p className="text-sm text-ink-muted">
                    {t('reporting.detail.couldNotCheck', { message: refusal.message })}
                </p>
            );
        }
        return <p className="text-sm text-ink-muted">{t('reporting.detail.neverRun')}</p>;
    }
    const ms = runDuration(report.latestRun.startedAt, report.latestRun.completedAt);
    return (
        <dl className="grid gap-x-6 gap-y-3 sm:grid-cols-4">
            <Detail
                label={t('reporting.detail.started')}
                value={
                    report.latestRun.startedAt === null
                        ? '—'
                        : formatDateTime(report.latestRun.startedAt, language)
                }
            />
            <Detail
                label={t('reporting.columns.duration')}
                value={ms === null ? '—' : formatDuration(ms, t)}
                mono
            />
            <Detail
                label={t('reporting.columns.problems')}
                value={
                    <ProblemCounts
                        errors={report.latestRun.errors}
                        warnings={report.latestRun.warnings}
                    />
                }
            />
            <Detail
                label={t('reporting.columns.schedule')}
                value={
                    report.nextFireAt === null
                        ? t('reporting.detail.noNextFire')
                        : t('reporting.cell.nextFire', {
                              at: formatDateTime(report.nextFireAt, language),
                          })
                }
            />
        </dl>
    );
}

export function ReportPage(): ReactNode {
    const { name = '' } = useParams();
    const { t } = useTranslation();
    const holds = useHolds();
    const mayEdit = holds(PERMISSION.reportDefinitionsWrite);
    const [savedVersion, setSavedVersion] = useState<number | null>(null);
    const detail = useQuery({ queryKey: reportKey(name), queryFn: () => api.report(name) });
    const trail = (
        <Crumbs
            parts={[
                { label: t('shell.menu.home'), to: '/' },
                { label: t('reporting.title'), to: PATHS.reporting },
                { label: name },
            ]}
        />
    );

    if (detail.isPending) {
        return (
            <ScreenFrame trail={trail} title={name} description={t('reporting.detail.lead')}>
                <p className="text-sm text-ink-muted" role="status">
                    {t('reporting.loading')}
                </p>
            </ScreenFrame>
        );
    }

    if (detail.isError) {
        return (
            <ScreenFrame trail={trail} title={name} description={t('reporting.detail.lead')}>
                <Notice tone="error">{detail.error.message}</Notice>
            </ScreenFrame>
        );
    }

    const { report, definition, unread } = detail.data;
    const healthChecked = !unread.some(
        (entry) => entry.cell === 'latestRun' || entry.cell === 'health',
    );
    return (
        <div className="space-y-6">
            <div>
                {trail}
                <PageHeader
                    title={report.name}
                    description={t('reporting.detail.lead')}
                    actions={
                        <HealthTag
                            health={report.health}
                            stateName={report.stateName}
                            checked={healthChecked}
                        />
                    }
                />
            </div>

            {savedVersion !== null && (
                <Notice tone="success">
                    {t('reporting.detail.saved', { version: savedVersion })}
                </Notice>
            )}

            <section className="card space-y-4 p-4">
                <header className="flex flex-wrap items-baseline justify-between gap-2">
                    <h2 className="text-sm font-medium">{t('reporting.detail.latestRun')}</h2>
                    <span className="text-xs text-ink-faint">
                        {t('reporting.detail.latestRunLead')}
                    </span>
                </header>
                <LatestRun report={report} unread={unread} />
            </section>

            <section className="card space-y-4 p-4">
                <header className="flex flex-wrap items-baseline justify-between gap-2">
                    <h2 className="text-sm font-medium">{t('reporting.detail.record')}</h2>
                    <Tag tone="muted">{t('reporting.detail.readOnly')}</Tag>
                </header>
                <ReadOnlyFields definition={definition} />
            </section>

            {mayEdit ? (
                <section className="card space-y-4 p-4">
                    <header className="flex flex-wrap items-baseline justify-between gap-2">
                        <h2 className="text-sm font-medium">{t('reporting.detail.edit')}</h2>
                        <span className="text-xs text-ink-faint">
                            {t('reporting.detail.editLead')}
                        </span>
                    </header>
                    <EditForm
                        key={definition.version}
                        detail={detail.data}
                        onSaved={setSavedVersion}
                    />
                </section>
            ) : (
                <Notice tone="info">{t('reporting.detail.mayNotEdit')}</Notice>
            )}
        </div>
    );
}
