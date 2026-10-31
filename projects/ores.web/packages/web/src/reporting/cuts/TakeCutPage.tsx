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
 * Take a cut: the kind and the instant, the body with its gaps, the review
 * against the previous cut, and the press that takes and raises.
 *
 * The take is four acts over one body and not one edit, which is why it earns a
 * rail. The assembly is the server's: one call answers every part with the state
 * it reached and every gap with the item, the reason and the remedy, and the same
 * call writes the body when the person presses. The screen therefore holds no
 * rule about what a body must contain — it draws what the service answered.
 *
 * A gap stops the press. The server refuses a take that carries one and keeps the
 * attempt as an incomplete body, so the screen offers *Save as a draft* in place
 * of *Take the cut* and states why, rather than letting a person press a button
 * that would be refused.
 */

import { useMutation, useQuery } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { Link, useNavigate } from 'react-router';
import type { CutKindRow, CutTake } from '@ores/wire-protocol/browser';
import { api } from '../../api/client.js';
import { useTranslation } from '../../i18n/Provider.js';
import { Crumbs } from '../../refdata/shared.js';
import { useAreaRoot } from '../../shell/AreaTrail.js';
import { PATHS, cutPath } from '../../shell/paths.js';
import { Button, Field, Input, Notice, Select, Tag } from '../../ui/Primitives.js';
import { CutAuthorisationLabel, CutStateTag, IndicativeTag } from './shared.js';

/** The four steps of the rail, in the order the person walks them. */
const STEPS = ['kind', 'body', 'review', 'take'] as const;
type Step = (typeof STEPS)[number];

/** The kinds the take does not offer, and the reason the first step says so. */
const NOT_TAKABLE = 'consumer';

/** How many kinds the first step reads, which is a firm's whole lookup. */
const KINDS_PAGE = { offset: 0, limit: 200, search: '', sort: '', descending: false };

/** The instant a date and a time state, as the wire writes it: ISO 8601 UTC. */
function instantOf(date: string, time: string): string {
    if (date === '' || time === '') {
        return '';
    }
    return `${date}T${time}:00.000000Z`;
}

/** The scope a body fixes. It is a fact of the books and not a choice of the take. */
function ScopeFields({
    scopeCode,
    centreCode,
    centres,
    onScope,
    onCentre,
}: {
    readonly scopeCode: string;
    readonly centreCode: string;
    readonly centres: readonly { readonly code: string; readonly name: string }[];
    readonly onScope: (value: string) => void;
    readonly onCentre: (value: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <>
            <Field label={t('cuts.take.scope')} hint={t('cuts.take.scopeHint')}>
                <Input
                    value={scopeCode}
                    onChange={(event) => onScope(event.target.value)}
                    placeholder="ACME-LONDON"
                    aria-label={t('cuts.take.scope')}
                />
            </Field>
            <Field label={t('cuts.take.centre')} hint={t('cuts.take.centreHint')}>
                <Select
                    value={centreCode}
                    onChange={(event) => onCentre(event.target.value)}
                    aria-label={t('cuts.take.centre')}
                >
                    <option value="">{t('cuts.take.chooseCentre')}</option>
                    {centres.map((centre) => (
                        <option key={centre.code} value={centre.code}>
                            {centre.code} · {centre.name}
                        </option>
                    ))}
                </Select>
            </Field>
        </>
    );
}

/** The rail: the four steps, the one the person stands on marked. */
function Rail({
    step,
    reached,
    onStep,
}: {
    readonly step: Step;
    readonly reached: ReadonlySet<Step>;
    readonly onStep: (step: Step) => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <ol className="flex flex-wrap items-center gap-2 text-xs" aria-label={t('cuts.take.rail')}>
            {STEPS.map((each, index) => {
                const here = each === step;
                const open = reached.has(each);
                return (
                    <li key={each} className="flex items-center gap-2">
                        <button
                            type="button"
                            disabled={!open}
                            onClick={() => onStep(each)}
                            className={
                                here
                                    ? 'rounded-full border border-accent bg-accent/10 px-3 py-1 font-semibold text-accent'
                                    : open
                                      ? 'rounded-full border border-line px-3 py-1 text-ink-muted hover:text-ink'
                                      : 'rounded-full border border-line/60 px-3 py-1 text-ink-faint'
                            }
                        >
                            {index + 1}. {t(`cuts.take.steps.${each}`)}
                        </button>
                        {index < STEPS.length - 1 && <span className="text-ink-faint">→</span>}
                    </li>
                );
            })}
        </ol>
    );
}

/** What the server answered about the body, part by part. */
function BodyTable({ answer }: { readonly answer: CutTake }): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="rounded-md border border-line bg-surface-raised p-4">
            <h2 className="text-sm font-semibold">{t('cuts.take.bodyTitle')}</h2>
            <p className="mt-1 text-xs text-ink-muted">{t('cuts.take.bodyLead')}</p>
            <table className="mt-3 w-full text-left text-sm">
                <thead>
                    <tr className="text-xs uppercase tracking-wide text-ink-faint">
                        <th className="py-1 pr-3 font-medium">{t('cuts.take.part')}</th>
                        <th className="py-1 pr-3 font-medium">{t('cuts.take.resolved')}</th>
                        <th className="py-1 pr-3 font-medium">{t('cuts.take.partState')}</th>
                        <th className="py-1 font-medium">{t('cuts.take.reference')}</th>
                    </tr>
                </thead>
                <tbody>
                    {answer.parts.map((part) => (
                        <tr key={part.partCode} className="border-t border-line align-top">
                            <td className="py-1 pr-3 font-mono text-xs">{part.partCode}</td>
                            <td className="py-1 pr-3">
                                <span className="block">{part.summary}</span>
                                {part.snapshotNote !== '' && (
                                    <span className="mt-0.5 block text-xs text-ink-faint">
                                        {part.snapshotNote}
                                    </span>
                                )}
                            </td>
                            <td className="py-1 pr-3">
                                {part.state === 'closed' ? (
                                    <Tag tone="up">{t('cuts.take.closed')}</Tag>
                                ) : (
                                    <Tag tone="down">{t('cuts.take.gapped')}</Tag>
                                )}
                            </td>
                            <td className="py-1 font-mono text-xs break-all">
                                {part.reference === '' ? (
                                    <span className="text-ink-faint">{t('cuts.take.noReference')}</span>
                                ) : (
                                    part.reference
                                )}
                            </td>
                        </tr>
                    ))}
                </tbody>
            </table>
        </section>
    );
}

/** The parts the body could not close, with what closes each one. */
function GapTable({ answer }: { readonly answer: CutTake }): ReactNode {
    const { t } = useTranslation();
    if (answer.gaps.length === 0) {
        return (
            <Notice tone="success">
                {t('cuts.take.noGaps', {
                    deals: String(answer.dealCount),
                    sets: String(answer.marketDataSetCount),
                })}
            </Notice>
        );
    }
    return (
        <section className="rounded-md border border-down/50 bg-down/10 p-4">
            <h2 className="text-sm font-semibold">
                {t('cuts.gaps.title', { count: String(answer.gaps.length) })}
            </h2>
            <p className="mt-1 text-xs text-ink-muted">{t('cuts.take.gapLead')}</p>
            <table className="mt-3 w-full text-left text-sm">
                <thead>
                    <tr className="text-xs uppercase tracking-wide text-ink-faint">
                        <th className="py-1 pr-3 font-medium">{t('cuts.gaps.part')}</th>
                        <th className="py-1 pr-3 font-medium">{t('cuts.gaps.item')}</th>
                        <th className="py-1 pr-3 font-medium">{t('cuts.gaps.reason')}</th>
                        <th className="py-1 font-medium">{t('cuts.take.remedy')}</th>
                    </tr>
                </thead>
                <tbody>
                    {answer.gaps.map((gap) => (
                        <tr key={gap.id === '' ? `${gap.partCode}.${gap.item}` : gap.id}>
                            <td className="py-1 pr-3 font-mono text-xs">{gap.partCode}</td>
                            <td className="py-1 pr-3 font-mono text-xs break-all">{gap.item}</td>
                            <td className="py-1 pr-3">{gap.reason}</td>
                            <td className="py-1 text-sm">
                                {answer.parts.find((part) => part.partCode === gap.partCode)
                                    ?.remedy ?? ''}
                            </td>
                        </tr>
                    ))}
                </tbody>
            </table>
        </section>
    );
}

/** What the press did, in the shape the kind decides it takes. */
function PressResult({ answer }: { readonly answer: CutTake }): ReactNode {
    const { t } = useTranslation();
    if (answer.gaps.length > 0) {
        return (
            <div className="space-y-3">
                <Notice tone="warn">{t('cuts.take.savedDraft', { count: String(answer.gaps.length) })}</Notice>
                <Link to={cutPath(answer.cutId)} className="text-sm text-accent">
                    {answer.cutLabel}
                </Link>
            </div>
        );
    }
    return (
        <div className="space-y-3">
            <Notice tone={answer.requiresAuthorisation ? 'info' : 'success'}>
                {answer.requiresAuthorisation
                    ? t('cuts.take.takenWaiting', {
                          label: answer.cutLabel,
                          kind: answer.approvalKindName,
                          permission: answer.decidePermissionCode,
                      })
                    : t('cuts.take.takenInUse', { label: answer.cutLabel })}
            </Notice>
            {!answer.requiresAuthorisation && (
                <Notice tone="info">{t('cuts.take.noAuthorisation')}</Notice>
            )}
            {answer.requiresAuthorisation && (
                <Notice tone="info">
                    {t('cuts.take.fourEyes', { label: answer.cutLabel })}{' '}
                    <Link to={cutPath(answer.cutId)} className="text-accent">
                        {t('cuts.take.openRecord')}
                    </Link>
                </Notice>
            )}
            {answer.outputIsIndicative && (
                <p className="text-xs text-ink-muted">{t('cuts.take.indicative')}</p>
            )}
            <p className="text-xs text-ink-faint">
                {t('cuts.take.retention', {
                    months: String(answer.retentionMonths),
                    destination: answer.retentionDestinationCode,
                })}
            </p>
        </div>
    );
}

export function TakeCutPage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const root = useAreaRoot('reporting');
    const trail = [
        ...root,
        { label: t('cuts.title'), to: PATHS.cuts },
        { label: t('cuts.take.title') },
    ];

    const [step, setStep] = useState<Step>('kind');
    const [kindCode, setKindCode] = useState('');
    const [date, setDate] = useState(() => new Date().toISOString().slice(0, 10));
    const [time, setTime] = useState('17:00');
    const [timezone, setTimezone] = useState('Europe/London');
    const [scopeCode, setScopeCode] = useState('');
    const [centreCode, setCentreCode] = useState('');
    const [reason, setReason] = useState('');
    const [answer, setAnswer] = useState<CutTake | null>(null);

    const choices = useQuery({
        queryKey: ['cuts', 'take-choices'],
        queryFn: () => api.cutKindChoices(),
    });
    const kinds = useQuery({
        queryKey: ['cuts', 'types', 'take'],
        queryFn: () => api.cutKinds(KINDS_PAGE),
    });

    /** The kinds the step offers: every kind whose instant a run does not supply. */
    const offered: readonly CutKindRow[] = (kinds.data?.rows ?? []).filter(
        (kind) => kind.familyCode !== NOT_TAKABLE,
    );
    const chosen = offered.find((kind) => kind.code === kindCode);
    const timepoint = instantOf(date, time);

    const assemble = useMutation({
        mutationFn: (preview: boolean) =>
            api.takeCut({
                kindCode,
                scopeCode,
                businessCentreCode: centreCode,
                timepoint,
                timepointTimezone: timezone,
                cutTimeCode: '',
                reason,
                preview,
            }),
        onSuccess: (result, preview) => {
            setAnswer(result);
            setStep(preview ? 'body' : 'take');
        },
    });

    const ready = kindCode !== '' && scopeCode !== '' && centreCode !== '' && timepoint !== '';

    if (kinds.isPending || choices.isPending) {
        return (
            <p className="text-sm text-ink-muted" role="status">
                {t('common.loading')}
            </p>
        );
    }
    if (kinds.isError) {
        return <Notice tone="error">{kinds.error.message}</Notice>;
    }
    if (choices.isError) {
        return <Notice tone="error">{choices.error.message}</Notice>;
    }

    const reached = new Set<Step>(['kind']);
    if (answer !== null) {
        reached.add('body');
        reached.add('review');
    }
    if (answer !== null && answer.cutId !== '') {
        reached.add('take');
    }

    return (
        <div className="space-y-4">
            <div>
                <Crumbs parts={trail} />
                <h1 className="mt-1 text-lg font-semibold">{t('cuts.take.title')}</h1>
                <p className="mt-1 text-sm text-ink-muted">{t('cuts.take.lead')}</p>
            </div>
            <Rail step={step} reached={reached} onStep={setStep} />

            {step === 'kind' && (
                <section className="space-y-4 rounded-md border border-line bg-surface-raised p-4">
                    <h2 className="text-sm font-semibold">{t('cuts.take.kindTitle')}</h2>
                    <p className="text-xs text-ink-muted">{t('cuts.take.kindLead')}</p>
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="text-xs uppercase tracking-wide text-ink-faint">
                                <th className="py-1 pr-3 font-medium">{t('cuts.columns.kind')}</th>
                                <th className="py-1 pr-3 font-medium">{t('cuts.take.family')}</th>
                                <th className="py-1 pr-3 font-medium">{t('cuts.take.basis')}</th>
                                <th className="py-1 pr-3 font-medium">{t('cuts.take.retentionShort')}</th>
                                <th className="py-1 font-medium">{t('cuts.take.indicativeShort')}</th>
                            </tr>
                        </thead>
                        <tbody>
                            {offered.map((kind) => (
                                <tr
                                    key={kind.code}
                                    className={
                                        kind.code === kindCode
                                            ? 'cursor-pointer border-t border-line bg-accent/10'
                                            : 'cursor-pointer border-t border-line hover:bg-surface'
                                    }
                                    onClick={() => setKindCode(kind.code)}
                                >
                                    <td className="py-1 pr-3">
                                        <label className="flex items-center gap-2">
                                            <input
                                                type="radio"
                                                name="cut-kind"
                                                checked={kind.code === kindCode}
                                                onChange={() => setKindCode(kind.code)}
                                                aria-label={kind.name}
                                            />
                                            <span className="font-mono text-xs">{kind.code}</span>
                                            <span>{kind.name}</span>
                                        </label>
                                    </td>
                                    <td className="py-1 pr-3 text-sm">{kind.familyName}</td>
                                    <td className="py-1 pr-3 text-sm">
                                        {kind.signed ? (
                                            <span>
                                                {kind.approvalKindName ?? kind.approvalKindCode}
                                                {kind.decidePermissionCode === null
                                                    ? ''
                                                    : ` · ${kind.decidePermissionCode}`}
                                            </span>
                                        ) : (
                                            <CutAuthorisationLabel />
                                        )}
                                    </td>
                                    <td className="py-1 pr-3 text-sm">
                                        {t('cuts.take.retention', {
                                            months: String(kind.retentionMonths),
                                            destination: kind.retentionDestinationCode,
                                        })}
                                    </td>
                                    <td className="py-1 text-sm">
                                        {kind.outputIsIndicative ? (
                                            <IndicativeTag />
                                        ) : (
                                            <span className="text-ink-faint">—</span>
                                        )}
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                    <Notice tone="info">{t('cuts.take.consumerNote')}</Notice>

                    <div className="grid grid-cols-1 gap-4 sm:grid-cols-2">
                        <Field label={t('cuts.take.timepoint')} hint={t('cuts.take.timepointHint')}>
                            <div className="flex gap-2">
                                <Input
                                    type="date"
                                    value={date}
                                    onChange={(event) => setDate(event.target.value)}
                                    aria-label={t('cuts.take.date')}
                                />
                                <Input
                                    type="time"
                                    value={time}
                                    onChange={(event) => setTime(event.target.value)}
                                    aria-label={t('cuts.take.time')}
                                />
                            </div>
                        </Field>
                        <Field label={t('cuts.take.timezone')} hint={t('cuts.take.timezoneHint')}>
                            <Input
                                value={timezone}
                                onChange={(event) => setTimezone(event.target.value)}
                                aria-label={t('cuts.take.timezone')}
                            />
                        </Field>
                        <ScopeFields
                            scopeCode={scopeCode}
                            centreCode={centreCode}
                            centres={choices.data.businessCentres}
                            onScope={setScopeCode}
                            onCentre={setCentreCode}
                        />
                        <Field label={t('cuts.take.reason')} hint={t('cuts.take.reasonHint')}>
                            <Input
                                value={reason}
                                onChange={(event) => setReason(event.target.value)}
                                aria-label={t('cuts.take.reason')}
                            />
                        </Field>
                    </div>

                    <div className="text-sm text-ink-muted">
                        {t('cuts.take.label')}{' '}
                        <span className="font-mono text-xs">
                            {kindCode === '' ? t('cuts.take.labelPending') : `${kindCode}.${scopeCode === '' ? '…' : scopeCode}.${date}`}
                        </span>
                    </div>
                    {chosen !== undefined && (
                        <p className="text-xs text-ink-muted">
                            {t('cuts.take.labelLead', { kind: chosen.name })}
                        </p>
                    )}

                    <div className="flex flex-wrap items-center justify-end gap-2">
                        <Button variant="secondary" onClick={() => void navigate(PATHS.cuts)}>
                            {t('common.cancel')}
                        </Button>
                        <Button
                            variant="primary"
                            disabled={!ready || assemble.isPending}
                            pending={assemble.isPending}
                            onClick={() => assemble.mutate(true)}
                        >
                            {t('cuts.take.assemble')}
                        </Button>
                    </div>
                    {assemble.isError && <Notice tone="error">{assemble.error.message}</Notice>}
                </section>
            )}

            {step === 'body' && answer !== null && (
                <div className="space-y-4">
                    <BodyTable answer={answer} />
                    <GapTable answer={answer} />
                    <div className="flex flex-wrap items-center justify-between gap-2">
                        <Button variant="secondary" onClick={() => setStep('kind')}>
                            {t('common.back')}
                        </Button>
                        <Button variant="primary" onClick={() => setStep('review')}>
                            {t('cuts.take.toReview')}
                        </Button>
                    </div>
                </div>
            )}

            {step === 'review' && answer !== null && (
                <div className="space-y-4">
                    <section className="rounded-md border border-line bg-surface-raised p-4">
                        <h2 className="text-sm font-semibold">{t('cuts.take.reviewTitle')}</h2>
                        <p className="mt-1 text-xs text-ink-muted">{t('cuts.take.reviewLead')}</p>
                        <dl className="mt-3 grid grid-cols-1 gap-4 sm:grid-cols-2">
                            <Line label={t('cuts.columns.kind')} value={chosen?.name ?? kindCode} />
                            <Line
                                label={t('cuts.columns.timepoint')}
                                value={`${timepoint} (${timezone})`}
                            />
                            <Line label={t('cuts.columns.scope')} value={scopeCode} />
                            <Line label={t('cuts.columns.centre')} value={centreCode} />
                            <Line
                                label={t('cuts.take.deals')}
                                value={String(answer.dealCount)}
                            />
                            <Line
                                label={t('cuts.take.sets')}
                                value={String(answer.marketDataSetCount)}
                            />
                            <Line
                                label={t('cuts.take.referenceData')}
                                value={answer.referenceDataVersion}
                            />
                            <Line
                                label={t('cuts.take.engine')}
                                value={answer.engineVersion}
                            />
                            <Line
                                label={t('cuts.take.codeVersion')}
                                value={
                                    answer.codeVersion === ''
                                        ? t('cuts.take.codeVersionGap')
                                        : answer.codeVersion
                                }
                            />
                        </dl>
                        <p className="mt-3 text-xs text-ink-muted">{t('cuts.take.reviewSentences')}</p>
                    </section>
                    <div className="flex flex-wrap items-center justify-between gap-2">
                        <Button variant="secondary" onClick={() => setStep('body')}>
                            {t('common.back')}
                        </Button>
                        <Button
                            variant="primary"
                            disabled={assemble.isPending}
                            pending={assemble.isPending}
                            onClick={() => assemble.mutate(false)}
                        >
                            {answer.gaps.length > 0
                                ? t('cuts.take.saveDraft')
                                : t('cuts.take.take')}
                        </Button>
                    </div>
                    {assemble.isError && <Notice tone="error">{assemble.error.message}</Notice>}
                </div>
            )}

            {step === 'take' && answer !== null && (
                <section className="space-y-4 rounded-md border border-line bg-surface-raised p-4">
                    <div className="flex items-center gap-2">
                        <h2 className="text-sm font-semibold">{t('cuts.take.pressTitle')}</h2>
                        {answer.cutState !== '' && <CutStateTag state={answer.cutState} />}
                    </div>
                    <PressResult answer={answer} />
                    <div className="flex flex-wrap items-center justify-end gap-2">
                        <Button variant="secondary" onClick={() => void navigate(PATHS.cuts)}>
                            {t('cuts.take.toList')}
                        </Button>
                        {answer.cutId !== '' && (
                            <Button
                                variant="primary"
                                onClick={() => void navigate(cutPath(answer.cutId))}
                            >
                                {t('cuts.take.openRecord')}
                            </Button>
                        )}
                    </div>
                </section>
            )}
        </div>
    );
}

/** One fact of the review. */
function Line({ label, value }: { readonly label: string; readonly value: string }): ReactNode {
    return (
        <div className="min-w-0">
            <dt className="text-xs text-ink-faint">{label}</dt>
            <dd className="mt-0.5 text-sm break-all">{value === '' ? '—' : value}</dd>
        </div>
    );
}
