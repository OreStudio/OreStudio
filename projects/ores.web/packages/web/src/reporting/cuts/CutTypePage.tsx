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
 * The cuts are grouped by the period one step above the type's frequency: the
 * group is the month of the instant for a kind taken every day, the year for one
 * taken every month, and the day for one whose instant answers to no schedule.
 * The period is read from the family the kind belongs to, and the grouping is
 * this screen's.
 */

import { useState, type ReactNode } from 'react';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { Link, useNavigate, useParams } from 'react-router';
import type { CutKindChoices, CutKindRecord, CutKindRow, CutRow } from '@ores/wire-protocol/browser';
import { api } from '../../api/client.js';
import { useHeld } from '../../access/holds.js';
import { useTranslation } from '../../i18n/Provider.js';
import { RecordTimeline } from '../../refdata/RecordTimeline.js';
import { Crumbs, ReasonFields, useReason } from '../../refdata/shared.js';
import {
    DeleteDialog,
    RecordHeader,
    useRecordTabs,
} from '../../refdata/records.js';
import { useAreaRoot } from '../../shell/AreaTrail.js';
import { PATHS, cutPath } from '../../shell/paths.js';
import { Button, Dialog, Field, Input, Notice, Select, Textarea } from '../../ui/Primitives.js';
import { RelativeTime } from '../../ui/Time.js';
import { Absent, CutStateTag, IndicativeTag, StateBasis, byMonth } from './shared.js';

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
    return <CutTypeBody kind={kind.data} />;
}

function CutTypeBody({ kind }: { readonly kind: CutKindRecord }): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const queries = useQueryClient();
    const root = useAreaRoot('refdata');
    const holds = useHeld();
    const mayWrite = holds('reporting::cut_kinds:write');
    const mayDelete = holds('reporting::cut_kinds:delete');
    const [editing, setEditing] = useState(false);
    const [removing, setRemoving] = useState(false);
    const { tab, bar } = useRecordTabs({
        label: kind.kind.code,
        tabs: ['rules', 'cuts', 'history'],
    });

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
                access={mayWrite}
                {...(mayWrite ? { onEdit: () => setEditing(true) } : {})}
                {...(mayDelete ? { onDelete: () => setRemoving(true) } : {})}
            />
            {bar}
            {tab === 'rules' && (
                <RulesTab kind={kind.kind} mayWrite={mayWrite} onEdit={() => setEditing(true)} />
            )}
            {tab === 'cuts' && <CutsTab cuts={kind.cuts} partial={kind.partial} />}
            {tab === 'history' && (
                <RecordTimeline entityType={CUT_KIND_ENTITY_TYPE} entityId={kind.kind.code} />
            )}
            {editing && (
                <EditKindDialog
                    kind={kind.kind}
                    onClose={() => setEditing(false)}
                    onSaved={async () => {
                        setEditing(false);
                        await reload();
                    }}
                />
            )}
            {removing && (
                <DeleteDialog
                    title={t('cuts.types.removeTitle', { code: kind.kind.code })}
                    warning={t('cuts.types.removeWarning')}
                    onClose={() => setRemoving(false)}
                    onRemoved={() => void navigate(PATHS.cutTypes)}
                    remove={async (intent) => {
                        await api.removeCutKind(kind.kind.code, {
                            version: kind.kind.version,
                            ...intent,
                        });
                        await queries.invalidateQueries({ queryKey: ['cuts', 'types'] });
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
function RulesTab({
    kind,
    mayWrite,
    onEdit,
}: {
    readonly kind: CutKindRow;
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
                        label={t('cuts.types.approvalsRequired')}
                        value={
                            kind.approvalsRequired === null ? (
                                <Absent />
                            ) : (
                                String(kind.approvalsRequired)
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
                    <CutStateTag state={row.state} />
                    <StateBasis row={row} />
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
 * The cuts of one kind, under the period above the kind's frequency.
 *
 * The screen reads the family the kind belongs to for the period: a kind taken
 * every day is read by month, one taken every month by year, and one whose
 * instant answers to no schedule by day.
 */
function CutsTab({
    cuts,
    partial,
}: {
    readonly cuts: readonly CutRow[];
    readonly partial: boolean;
}): ReactNode {
    const { t } = useTranslation();
    if (cuts.length === 0) {
        return <Notice tone="info">{t('cuts.types.noCuts')}</Notice>;
    }
    const groups = byMonth(cuts);
    return (
        <div className="space-y-6">
            {partial && <Notice tone="warn">{t('cuts.partialPage')}</Notice>}
            {groups.map((group) => (
                <section key={group.month} className="rounded-md border border-line">
                    <h2 className="border-b border-line px-4 py-2 text-sm font-semibold">
                        {t('cuts.monthHeading', { month: group.month })}
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

/** The fields the form offers, held as text so a half-typed number is still a form. */
interface KindForm {
    readonly name: string;
    readonly description: string;
    readonly familyCode: string;
    readonly approvalKindCode: string;
    readonly outputIsIndicative: boolean;
    readonly resolutionRuleCode: string;
    readonly retentionMonths: string;
    readonly legalHoldDefault: boolean;
    readonly displayOrder: string;
}

function formOf(kind: CutKindRow): KindForm {
    return {
        name: kind.name,
        description: kind.description,
        familyCode: kind.familyCode,
        approvalKindCode: kind.approvalKindCode ?? '',
        outputIsIndicative: kind.outputIsIndicative,
        resolutionRuleCode: kind.resolutionRuleCode,
        retentionMonths: String(kind.retentionMonths),
        legalHoldDefault: kind.legalHoldDefault,
        displayOrder: String(kind.displayOrder),
    };
}

/**
 * The naming and the rules of a kind, as one form.
 *
 * The retention destination is not offered. It is a tier the store knows by a
 * check and no lookup states the tiers yet, so a form that sent one would be
 * inventing the store's vocabulary; the write keeps the value the kind holds.
 */
function EditKindDialog({
    kind,
    onClose,
    onSaved,
}: {
    readonly kind: CutKindRow;
    readonly onClose: () => void;
    readonly onSaved: () => Promise<void>;
}): ReactNode {
    const { t } = useTranslation();
    const [form, setForm] = useState<KindForm>(formOf(kind));
    const [commentary, setCommentary] = useState('');
    const choices = useQuery({ queryKey: ['cuts', 'choices'], queryFn: () => api.cutKindChoices() });
    const reason = useReason('amend', true);
    const needsCommentary = reason.needsCommentary && commentary.trim() === '';
    const months = Number.parseInt(form.retentionMonths, 10);
    const order = Number.parseInt(form.displayOrder, 10);
    const invalid =
        form.name.trim() === '' ||
        form.familyCode === '' ||
        form.resolutionRuleCode === '' ||
        Number.isNaN(months) ||
        months < 0 ||
        Number.isNaN(order) ||
        order < 0;

    const save = useMutation({
        mutationFn: () =>
            api.saveCutKind(kind.code, {
                version: kind.version,
                change: {
                    name: form.name.trim(),
                    description: form.description,
                    familyCode: form.familyCode,
                    approvalKindCode: form.approvalKindCode === '' ? null : form.approvalKindCode,
                    outputIsIndicative: form.outputIsIndicative,
                    resolutionRuleCode: form.resolutionRuleCode,
                    retentionMonths: months,
                    legalHoldDefault: form.legalHoldDefault,
                    displayOrder: order,
                },
                reasonCode: reason.code,
                commentary: commentary.trim(),
            }),
        onSuccess: onSaved,
    });

    return (
        <Dialog
            title={t('cuts.types.editTitle', { code: kind.code })}
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" onClick={onClose}>
                        {t('common.cancel')}
                    </Button>
                    <Button
                        onClick={() => save.mutate()}
                        disabled={invalid || needsCommentary || save.isPending}
                    >
                        {t('common.save')}
                    </Button>
                </>
            }
        >
            <div className="space-y-3">
                {save.isError && <Notice tone="error">{save.error.message}</Notice>}
                <Field label={t('cuts.columns.name')}>
                    <Input
                        value={form.name}
                        onChange={(event) => setForm({ ...form, name: event.target.value })}
                    />
                </Field>
                <Field label={t('cuts.types.description')}>
                    <Textarea
                        value={form.description}
                        onChange={(event) => setForm({ ...form, description: event.target.value })}
                    />
                </Field>
                <Field label={t('cuts.columns.family')}>
                    <Select
                        value={form.familyCode}
                        onChange={(event) => setForm({ ...form, familyCode: event.target.value })}
                    >
                        <ChoiceList choices={choices.data} of="families" />
                    </Select>
                </Field>
                <Field label={t('cuts.columns.signed')}>
                    <Select
                        value={form.approvalKindCode}
                        onChange={(event) =>
                            setForm({ ...form, approvalKindCode: event.target.value })
                        }
                    >
                        <option value="">{t('cuts.types.unsigned')}</option>
                        <ChoiceList choices={choices.data} of="approvalKinds" />
                    </Select>
                </Field>
                <label className="flex items-center gap-2 text-sm">
                    <input
                        type="checkbox"
                        checked={form.outputIsIndicative}
                        onChange={(event) =>
                            setForm({ ...form, outputIsIndicative: event.target.checked })
                        }
                    />
                    {t('cuts.types.indicativeValues')}
                </label>
                <Field label={t('cuts.types.resolutionRule')}>
                    <Select
                        value={form.resolutionRuleCode}
                        onChange={(event) =>
                            setForm({ ...form, resolutionRuleCode: event.target.value })
                        }
                    >
                        <ChoiceList choices={choices.data} of="resolutionRules" />
                    </Select>
                </Field>
                <Field label={t('cuts.types.retentionMonths')}>
                    <Input
                        inputMode="numeric"
                        value={form.retentionMonths}
                        onChange={(event) =>
                            setForm({ ...form, retentionMonths: event.target.value })
                        }
                    />
                </Field>
                <label className="flex items-center gap-2 text-sm">
                    <input
                        type="checkbox"
                        checked={form.legalHoldDefault}
                        onChange={(event) =>
                            setForm({ ...form, legalHoldDefault: event.target.checked })
                        }
                    />
                    {t('cuts.types.legalHoldOnTake')}
                </label>
                <Field label={t('cuts.types.order')}>
                    <Input
                        inputMode="numeric"
                        value={form.displayOrder}
                        onChange={(event) => setForm({ ...form, displayOrder: event.target.value })}
                    />
                </Field>
                <p className="text-xs text-ink-muted">{t('cuts.types.destinationNote')}</p>
                <ReasonFields
                    reason={reason}
                    commentary={commentary}
                    onCommentary={setCommentary}
                    missing={needsCommentary}
                />
            </div>
        </Dialog>
    );
}

/** The options one lookup offers, or nothing while it is still being read. */
function ChoiceList({
    choices,
    of,
}: {
    readonly choices: CutKindChoices | undefined;
    readonly of: 'families' | 'resolutionRules' | 'approvalKinds';
}): ReactNode {
    const list = choices?.[of] ?? [];
    return (
        <>
            {list.map((choice) => (
                <option key={choice.code} value={choice.code}>
                    {choice.name}
                </option>
            ))}
        </>
    );
}
