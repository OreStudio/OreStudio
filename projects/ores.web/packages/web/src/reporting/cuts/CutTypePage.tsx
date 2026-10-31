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
 * One kind of cut: its rules and the cuts the party holds of it.
 *
 * The rules are the four decisions a firm changes and reads back: who signs a
 * cut of this kind, whether its values are indicative, the rule a report
 * resolves a selection of it by, and how long its cuts are kept. The
 * authorisation is a foreign key, so the words under it are the approval kind's
 * own and never a phrase stored on the type.
 *
 * The cuts are grouped by the period one step above the kind's own frequency,
 * which the resolution rule states: a daily kind reads by month, a monthly kind
 * by year, a yearly kind as one group, and an on-demand kind by day. The
 * grouping is this screen's: the read answers the cuts and states no period.
 */

import { useMemo, useState, type ReactNode } from 'react';
import { useQuery, useQueryClient } from '@tanstack/react-query';
import { Link, useNavigate, useParams } from 'react-router';
import type { CutKindChoices, CutKindRecord, CutKindRow, CutRow } from '@ores/wire-protocol/browser';
import { api, type RecordRow } from '../../api/client.js';
import { useHeld } from '../../access/holds.js';
import { PERMISSION } from '../../access/permissions.js';
import { useTranslation } from '../../i18n/Provider.js';
import { RecordTimeline } from '../../refdata/RecordTimeline.js';
import { Crumbs } from '../../refdata/shared.js';
import {
    RecordDialog,
    RecordHeader,
    RemoveRecordDialog,
    useRecordTabs,
    type FieldSpec,
} from '../../refdata/records.js';
import { RecordStoreProvider } from '../../refdata/store.js';
import { useAreaRoot } from '../../shell/AreaTrail.js';
import { PATHS, cutPath } from '../../shell/paths.js';
import { Button, Dialog, Notice } from '../../ui/Primitives.js';
import { RelativeTime } from '../../ui/Time.js';
import {
    Absent,
    CutStateTag,
    IndicativeTag,
    StateBasis,
    frequencyOf,
    groupsOf,
    useGroupHeading,
    type CutFrequency,
} from './shared.js';
import { CUT_KIND_RESOURCE, cutKindStore } from './cutKindStore.js';

/** The entity type the type's versions are read under. */
export const CUT_KIND_ENTITY_TYPE = 'ores.reporting.cut_kind';

export function CutTypePage(): ReactNode {
    const { t } = useTranslation();
    const { code } = useParams();
    const kind = useQuery({
        queryKey: ['cuts', 'type', code ?? ''],
        queryFn: () => api.cutKind(code ?? ''),
    });

    if (kind.isPending) {
        return (
            <p className="text-sm text-ink-muted" role="status">
                {t('common.loading')}
            </p>
        );
    }
    if (kind.isError) {
        return <Notice tone="error">{kind.error.message}</Notice>;
    }
    return (
        <RecordStoreProvider store={cutKindStore}>
            <CutTypeBody kind={kind.data} loadedAt={kind.dataUpdatedAt} />
        </RecordStoreProvider>
    );
}

function CutTypeBody({
    kind,
    loadedAt,
}: {
    readonly kind: CutKindRecord;
    readonly loadedAt: number;
}): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const queries = useQueryClient();
    const root = useAreaRoot('refdata');
    const holds = useHeld();
    const mayWrite = holds(PERMISSION.cutKindsWrite);
    const mayDelete = holds(PERMISSION.cutKindsDelete);
    const [editing, setEditing] = useState(false);
    const [removing, setRemoving] = useState(false);
    const { tab, bar } = useRecordTabs({
        label: kind.kind.code,
        tabs: ['details', 'cuts', 'history'],
    });
    const frequency = frequencyOf(kind.kind.resolutionRuleCode);

    const reload = async (): Promise<void> => {
        await queries.invalidateQueries({ queryKey: ['cuts', 'type', kind.kind.code] });
        await queries.invalidateQueries({ queryKey: ['cuts', 'types'] });
        await queries.invalidateQueries({
            queryKey: ['history', CUT_KIND_ENTITY_TYPE, kind.kind.code],
        });
    };

    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[
                    ...root,
                    { label: t('cuts.types.title'), to: PATHS.cutTypes },
                    { label: kind.kind.code },
                ]}
                title={kind.kind.name}
                recordKey={kind.kind.code}
                version={kind.kind.version}
                detail={kind.kind.familyName}
                access={mayWrite}
                {...(mayWrite ? { onEdit: () => setEditing(true) } : {})}
                {...(mayDelete ? { onDelete: () => setRemoving(true) } : {})}
            />
            {bar}
            {tab === 'details' && (
                <DetailsTab kind={kind.kind} frequency={frequency} mayWrite={mayWrite} onEdit={() => setEditing(true)} />
            )}
            {tab === 'cuts' && <CutsTab cuts={kind.cuts} partial={kind.partial} frequency={frequency} />}
            {tab === 'history' && (
                <RecordTimeline entityType={CUT_KIND_ENTITY_TYPE} entityId={kind.kind.code} />
            )}
            {editing && (
                <EditKindDialog
                    kind={kind.kind}
                    loadedAt={loadedAt}
                    onClose={() => setEditing(false)}
                    onSaved={() => void reload()}
                />
            )}
            {removing && (
                <RemoveRecordDialog
                    resource={CUT_KIND_RESOURCE}
                    title={t('cuts.types.removeTitle', { code: kind.kind.code })}
                    warning={t('cuts.types.removeWarning')}
                    recordKey={{ code: kind.kind.code }}
                    version={kind.kind.version}
                    onClose={() => setRemoving(false)}
                    onRemoved={() => {
                        void queries.invalidateQueries({ queryKey: ['cuts', 'types'] });
                        void navigate(PATHS.cutTypes);
                    }}
                />
            )}
        </div>
    );
}

/** One labelled value of the rules, as the details grid draws it. */
function Rule({
    label,
    value,
    mono = false,
}: {
    readonly label: string;
    readonly value: ReactNode;
    readonly mono?: boolean;
}): ReactNode {
    return (
        <div className="min-w-0">
            <dt className="text-xs text-ink-faint">{label}</dt>
            <dd className={(mono ? 'font-mono text-xs' : 'text-sm') + ' mt-0.5 break-words'}>
                {value}
            </dd>
        </div>
    );
}

/** The rules a cut of this kind is taken under, and the copy the take makes of them. */
function DetailsTab({
    kind,
    frequency,
    mayWrite,
    onEdit,
}: {
    readonly kind: CutKindRow;
    readonly frequency: CutFrequency;
    readonly mayWrite: boolean;
    readonly onEdit: () => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <div className="space-y-4">
            {kind.description !== '' && (
                <p className="max-w-3xl text-sm text-ink-muted">{kind.description}</p>
            )}
            <section className="rounded-md border border-line bg-surface-raised p-4">
                <div className="mb-3 flex items-center justify-between gap-3">
                    <h2 className="text-sm font-semibold">{t('cuts.types.rules')}</h2>
                    {mayWrite && (
                        <Button icon="edit" onClick={onEdit}>
                            {t('refdata.records.edit')}
                        </Button>
                    )}
                </div>
                <dl className="grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-3">
                    <Rule label={t('cuts.columns.family')} value={kind.familyName} />
                    <Rule label={t('cuts.types.repeats')} value={t(`cuts.frequency.${frequency}`)} />
                    <Rule
                        label={t('cuts.columns.signed')}
                        value={
                            kind.signed ? (
                                <span className="flex flex-col gap-0.5">
                                    <span>{kind.approvalKindName ?? kind.approvalKindCode ?? ''}</span>
                                    {kind.decidePermissionCode !== null &&
                                        kind.decidePermissionCode !== '' && (
                                            <span className="font-mono text-xs text-ink-faint">
                                                {kind.decidePermissionCode}
                                            </span>
                                        )}
                                </span>
                            ) : (
                                t('cuts.types.unsigned')
                            )
                        }
                    />
                    <Rule
                        label={t('cuts.columns.indicative')}
                        value={kind.outputIsIndicative ? <IndicativeTag /> : t('cuts.types.official')}
                    />
                    <Rule
                        label={t('cuts.types.resolutionRule')}
                        value={kind.resolutionRuleName}
                        mono
                    />
                    <Rule
                        label={t('cuts.columns.retention')}
                        value={t('cuts.types.months', { count: String(kind.retentionMonths) })}
                    />
                    <Rule
                        label={t('cuts.types.destination')}
                        value={kind.retentionDestinationCode}
                        mono
                    />
                    <Rule
                        label={t('cuts.types.legalHold')}
                        value={kind.legalHoldDefault ? t('cuts.types.held') : t('cuts.types.notHeld')}
                    />
                    <Rule label={t('cuts.types.order')} value={String(kind.displayOrder)} mono />
                </dl>
            </section>
            <Notice tone="info">{t('cuts.types.copyNote')}</Notice>
        </div>
    );
}

/** One row of a group's table: the cut's label and what a reader asks of it. */
function CutLine({ row }: { readonly row: CutRow }): ReactNode {
    const { t } = useTranslation();
    return (
        <tr className="border-t border-line">
            <td className="py-2 pr-3">
                <Link to={cutPath(row.id)} className="font-mono text-xs text-accent">
                    {row.label === '' ? t('cuts.labelMissing') : row.label}
                </Link>
            </td>
            <td className="py-2 pr-3">
                <span className="flex flex-col gap-1">
                    <span className="flex items-center gap-1.5">
                        <CutStateTag state={row.state} />
                        {row.outputIsIndicative && <IndicativeTag />}
                    </span>
                    <StateBasis
                        facts={{ state: row.state, approvedBy: row.approvedBy, supersededByLabel: null }}
                    />
                </span>
            </td>
            <td className="py-2 pr-3 font-mono text-xs">
                <RelativeTime at={row.timepoint} />
            </td>
            <td className="py-2 pr-3 text-sm">{row.takenBy === '' ? <Absent /> : row.takenBy}</td>
            <td className="py-2 pr-3 text-sm">
                {row.approvedBy === '' ? <Absent /> : row.approvedBy}
            </td>
            <td className="py-2 pr-3 text-right font-mono text-xs tabular-nums">{row.runCount}</td>
        </tr>
    );
}

/**
 * The cuts of one kind, under the period one step above the kind's frequency.
 *
 * The group names the period in the language the person reads, so a daily kind
 * reads "October 2026" and a monthly one "Year 2026".
 */
function CutsTab({
    cuts,
    partial,
    frequency,
}: {
    readonly cuts: readonly CutRow[];
    readonly partial: boolean;
    readonly frequency: CutFrequency;
}): ReactNode {
    const { t } = useTranslation();
    const heading = useGroupHeading();
    if (cuts.length === 0) {
        return <Notice tone="info">{t('cuts.types.noCuts')}</Notice>;
    }
    const groups = groupsOf(cuts, frequency);
    return (
        <div className="space-y-6">
            <p className="text-sm text-ink-muted">
                {t('cuts.types.grouping', { frequency: t(`cuts.frequency.${frequency}`) })}
            </p>
            {partial && <Notice tone="warn">{t('cuts.partialPage')}</Notice>}
            {groups.map((group) => (
                <section key={group.key} className="rounded-md border border-line">
                    <h2 className="border-b border-line px-4 py-2 text-sm font-semibold">
                        {heading(group.key, frequency)}
                        <span className="ml-2 text-xs font-normal text-ink-faint">
                            {t('cuts.cutCount', { count: String(group.rows.length) })}
                        </span>
                    </h2>
                    <table className="w-full px-4 text-left">
                        <thead>
                            <tr className="text-xs uppercase tracking-wide text-ink-faint">
                                <th className="px-4 py-2 font-medium">{t('cuts.columns.cut')}</th>
                                <th className="py-2 pr-3 font-medium">{t('cuts.columns.state')}</th>
                                <th className="py-2 pr-3 font-medium">
                                    {t('cuts.columns.timepoint')}
                                </th>
                                <th className="py-2 pr-3 font-medium">{t('cuts.columns.taker')}</th>
                                <th className="py-2 pr-3 font-medium">
                                    {t('cuts.columns.authoriser')}
                                </th>
                                <th className="py-2 pr-4 text-right font-medium">
                                    {t('cuts.columns.runs')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {group.rows.map((row) => (
                                <CutLine key={row.id} row={row} />
                            ))}
                        </tbody>
                    </table>
                </section>
            ))}
        </div>
    );
}

/**
 * The fields the type's form offers.
 *
 * The retention destination is not one of them. It is a tier the store keeps and
 * no lookup states the tiers, so the form states the value the kind holds and
 * offers no other; the write keeps it whatever a caller sends.
 */
export function specsOf(choices: CutKindChoices): readonly FieldSpec[] {
    return [
        {
            field: 'code',
            history: 'code',
            kind: { kind: 'text', max: 100 },
            fixed: true,
            labelKey: 'cuts.columns.type',
        },
        {
            field: 'name',
            history: 'name',
            kind: { kind: 'text', max: 200 },
            labelKey: 'cuts.columns.name',
        },
        {
            field: 'description',
            history: 'description',
            kind: { kind: 'text', max: 2000 },
            blank: true,
            labelKey: 'cuts.types.description',
        },
        {
            field: 'family_code',
            history: 'family_code',
            kind: { kind: 'pick', choices: asChoices(choices.families) },
            labelKey: 'cuts.columns.family',
        },
        {
            field: 'approval_kind_code',
            history: 'approval_kind_code',
            kind: { kind: 'pick', choices: asChoices(choices.approvalKinds) },
            optional: true,
            labelKey: 'cuts.columns.signed',
            help: 'cuts.types.unsignedHelp',
        },
        {
            field: 'output_is_indicative',
            history: 'output_is_indicative',
            kind: { kind: 'bool' },
            labelKey: 'cuts.types.indicativeValues',
        },
        {
            field: 'resolution_rule_code',
            history: 'resolution_rule_code',
            kind: { kind: 'pick', choices: asChoices(choices.resolutionRules) },
            labelKey: 'cuts.types.resolutionRule',
        },
        {
            field: 'retention_months',
            history: 'retention_months',
            kind: { kind: 'int', min: 0, max: 1200 },
            labelKey: 'cuts.types.retentionMonths',
        },
        {
            field: 'legal_hold_default',
            history: 'legal_hold_default',
            kind: { kind: 'bool' },
            labelKey: 'cuts.types.legalHoldOnTake',
        },
        {
            field: 'display_order',
            history: 'display_order',
            kind: { kind: 'int', min: 0, max: 1000000 },
            labelKey: 'cuts.types.order',
        },
    ];
}

/** One lookup of the kind's choices, as a picker reads it. */
function asChoices(
    entries: readonly { readonly code: string; readonly name: string }[],
): readonly { readonly value: string; readonly label: string }[] {
    return entries.map((entry) => ({ value: entry.code, label: entry.name }));
}

/**
 * The kind's own values, under the names the field table reads.
 *
 * The shared dialog reads a row's fields by their server names, so a kind of cut
 * is handed to it in the shape the write uses and in no other.
 */
export function formRowOf(kind: CutKindRow): RecordRow {
    return {
        code: kind.code,
        name: kind.name,
        description: kind.description,
        family_code: kind.familyCode,
        approval_kind_code: kind.approvalKindCode,
        output_is_indicative: kind.outputIsIndicative,
        resolution_rule_code: kind.resolutionRuleCode,
        retention_months: kind.retentionMonths,
        legal_hold_default: kind.legalHoldDefault,
        display_order: kind.displayOrder,
        version: kind.version,
    };
}

/**
 * The naming and the rules of a kind, in the shared record dialog.
 *
 * The dialog is the standard's, not this screen's: the form, the change reason,
 * the version the write claims and the closing guard all come with it, and the
 * store it reads is the one the page named.
 */
function EditKindDialog({
    kind,
    loadedAt,
    onClose,
    onSaved,
}: {
    readonly kind: CutKindRow;
    readonly loadedAt: number;
    readonly onClose: () => void;
    readonly onSaved: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const choices = useQuery({ queryKey: ['cuts', 'choices'], queryFn: () => api.cutKindChoices() });
    const specs = useMemo(
        () => (choices.data === undefined ? [] : specsOf(choices.data)),
        [choices.data],
    );
    if (choices.isPending) {
        return (
            <Dialog title={t('cuts.types.editTitle', { code: kind.code })} onClose={onClose}>
                <p className="text-sm text-ink-muted" role="status">
                    {t('common.loading')}
                </p>
            </Dialog>
        );
    }
    if (choices.isError) {
        return (
            <Dialog title={t('cuts.types.editTitle', { code: kind.code })} onClose={onClose}>
                <Notice tone="error">{choices.error.message}</Notice>
            </Dialog>
        );
    }
    return (
        <RecordDialog
            title={t('cuts.types.editTitle', { code: kind.code })}
            resource={CUT_KIND_RESOURCE}
            specs={specs}
            row={formRowOf(kind)}
            loadedAt={loadedAt}
            onClose={onClose}
            onSaved={onSaved}
            note={() => (
                <p className="text-xs text-ink-muted">
                    {t('cuts.types.destinationNote', { destination: kind.retentionDestinationCode })}
                </p>
            )}
        />
    );
}
