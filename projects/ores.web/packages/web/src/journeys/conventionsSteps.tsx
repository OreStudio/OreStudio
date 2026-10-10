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
 * The steps a convention walk runs.
 *
 * The person chooses the instrument family, chooses or starts a convention of
 * it, authors its terms, reads the one review, and confirms. The confirm is one
 * write and records a refusal against the step to walk back to. Twenty-five
 * families have twenty-five shapes, and the terms of two are drawn in full, so
 * the terms step reads a table of term specs and a family with no table is
 * listed and read, and says its terms are not drawn.
 *
 * The steps are data, which is why this is a function and not a component.
 */

import { useEffect, useState, type ReactNode } from 'react';
import { Button, Field, Input, Notice, Select, Tag } from '../ui/Primitives.js';
import { HistoryPanel } from '../refdata/HistoryPanel.js';
import { Picker } from './pickers.js';
import {
    entityTypeOf,
    specsOf,
    termProblems,
    writePlan,
    type ConventionRefusal,
    type ConventionTerms,
    type PickList,
    type TermSpec,
} from './conventionsState.js';
import type { JourneyStep, StepId } from './runtime.js';
import type {
    ConventionFamilyCard,
    ConventionPickLists,
    ConventionRow,
    ConventionsServer,
} from './conventionsServer.js';
import type { Translator } from '../i18n/translate.js';

/** The steps in the order the rail draws them, and the index each one sits at. */
export const CONVENTION_STEP_IDS = [
    'instrument',
    'convention',
    'terms',
    'review',
    'outcome',
    'history',
] as const;

export function conventionStepIndex(id: StepId): number {
    const index = CONVENTION_STEP_IDS.indexOf(id as (typeof CONVENTION_STEP_IDS)[number]);
    return index < 0 ? 0 : index;
}

export interface ConventionStepsInput {
    readonly t: Translator['t'];
    readonly server: ConventionsServer;
    readonly state: ConventionTerms;
    readonly pickLists: ConventionPickLists | undefined;
    readonly pickFailure: string | undefined;
    readonly reasons: readonly {
        readonly code: string;
        readonly description: string;
        readonly requiresCommentary: boolean;
    }[];
    readonly onMove: (index: number) => void;
    readonly onFinished: () => void;
}

function refusalText(t: Translator['t'], refusal: ConventionRefusal): string {
    const fields = refusal.fields
        .map((failure) => `${failure.field}: ${failure.message}`)
        .join('; ');
    return [
        t('journey.conventions.refusal.heading'),
        refusal.subject,
        refusal.message,
        fields,
        t('journey.conventions.refusal.kept'),
    ]
        .filter((part) => part !== '')
        .join(' ');
}

/** The picker rows a term draws from, as value and label. */
function optionsFor(
    lists: ConventionPickLists | undefined,
    pick: PickList,
): readonly { readonly value: string; readonly label: string }[] {
    if (lists === undefined) {
        return [];
    }
    switch (pick) {
        case 'calendars':
            return lists.calendars.map((row) => ({ value: row.code, label: row.code }));
        case 'paymentFrequencies':
            return lists.paymentFrequencies.map((row) => ({ value: row.code, label: row.code }));
        case 'businessDayConventions':
            return lists.businessDayConventions.map((row) => ({
                value: row.code,
                label: row.code,
            }));
        case 'dayCountFractions':
            return lists.dayCountFractions.map((row) => ({ value: row.code, label: row.code }));
        case 'floatingIndices':
            return lists.floatingIndices.map((row) => ({ value: row.code, label: row.code }));
        case 'subPeriodsCouponTypes':
            return lists.subPeriodsCouponTypes.map((row) => ({ value: row.code, label: row.code }));
    }
}

/** A one-line summary of a convention's terms. */
function summaryOf(family: string, row: ConventionRow): string {
    const specs = specsOf(family);
    if (specs.length === 0) {
        return row.id;
    }
    return specs
        .map((spec) => row[spec.column])
        .filter((value) => value !== null && value !== undefined && value !== '')
        .map(String)
        .slice(0, 4)
        .join(' · ');
}

/** Stands above every step once a family is chosen, so the record stays in view. */
export function conventionHeader(
    t: Translator['t'],
    state: ConventionTerms,
): ReactNode | undefined {
    if (state.family === '') {
        return undefined;
    }
    return (
        <div className="flex flex-wrap items-baseline justify-between gap-3 text-sm">
            <span className="font-medium">{t(`journey.conventions.families.${state.family}`)}</span>
            <span className="flex items-center gap-2 text-xs text-ink-faint">
                {state.authoring && (
                    <span className="font-mono">
                        {state.opened === undefined
                            ? t('journey.conventions.new')
                            : state.conventionId.slice(0, 8)}
                    </span>
                )}
                {state.opened !== undefined && <span>v{state.opened.version}</span>}
            </span>
        </div>
    );
}

/* --------------------------------------------------------------- instrument */

function InstrumentStep({
    t,
    server,
    state,
    onMove,
}: {
    readonly t: Translator['t'];
    readonly server: ConventionsServer;
    readonly state: ConventionTerms;
    readonly onMove: (index: number) => void;
}): ReactNode {
    const [cards, setCards] = useState<readonly ConventionFamilyCard[]>();
    const [failure, setFailure] = useState<string>();
    const [query, setQuery] = useState('');
    useEffect(() => {
        let cancelled = false;
        const load = async (): Promise<void> => {
            try {
                const read = await server.families();
                if (!cancelled) {
                    setCards(read);
                    setFailure(undefined);
                }
            } catch (error) {
                if (!cancelled) {
                    setFailure(error instanceof Error ? error.message : String(error));
                }
            }
        };
        void load();
        return () => {
            cancelled = true;
        };
    }, [server]);
    const needle = query.trim().toLowerCase();
    const shown = (cards ?? []).filter(
        (card) =>
            needle === '' ||
            card.key.includes(needle) ||
            card.entity.includes(needle) ||
            t(`journey.conventions.families.${card.key}`).toLowerCase().includes(needle),
    );
    return (
        <div className="space-y-4">
            <Input
                value={query}
                placeholder={t('journey.conventions.instrument.search')}
                onChange={(event) => setQuery(event.target.value)}
            />
            {failure !== undefined && (
                <Notice tone="error">
                    {t('journey.conventions.instrument.readFailed', { message: failure })}
                </Notice>
            )}
            <div className="grid gap-3 md:grid-cols-2 xl:grid-cols-3">
                {shown.map((card) => (
                    <button
                        key={card.key}
                        type="button"
                        className="card space-y-1 p-4 text-left hover:border-accent"
                        onClick={() => {
                            state.chooseFamily(card.key);
                            onMove(conventionStepIndex('convention'));
                        }}
                    >
                        <span className="flex items-center justify-between gap-2">
                            <span className="font-semibold">
                                {t(`journey.conventions.families.${card.key}`)}
                            </span>
                            <Tag tone={card.writable ? 'accent' : 'muted'}>
                                {card.writable
                                    ? t('journey.conventions.instrument.drawn')
                                    : t('journey.conventions.instrument.notDrawn')}
                            </Tag>
                        </span>
                        <span className="block font-mono text-xs text-ink-faint">
                            {card.entity}
                        </span>
                        <span className="block text-xs text-ink-muted">
                            {card.count === null
                                ? t('journey.conventions.instrument.countUnknown')
                                : t('journey.conventions.instrument.count', {
                                      count: String(card.count),
                                  })}
                        </span>
                    </button>
                ))}
            </div>
        </div>
    );
}

/* --------------------------------------------------------------- convention */

function ConventionStep({
    t,
    server,
    state,
    onMove,
}: {
    readonly t: Translator['t'];
    readonly server: ConventionsServer;
    readonly state: ConventionTerms;
    readonly onMove: (index: number) => void;
}): ReactNode {
    const [rows, setRows] = useState<readonly ConventionRow[]>();
    const [total, setTotal] = useState(0);
    const [failure, setFailure] = useState<string>();
    const [query, setQuery] = useState('');
    const family = state.family;
    useEffect(() => {
        if (family === '') {
            return undefined;
        }
        let cancelled = false;
        const load = async (): Promise<void> => {
            try {
                const page = await server.rowsOf(family);
                if (!cancelled) {
                    setRows(page.rows);
                    setTotal(page.total);
                    setFailure(undefined);
                }
            } catch (error) {
                if (!cancelled) {
                    setFailure(error instanceof Error ? error.message : String(error));
                }
            }
        };
        void load();
        return () => {
            cancelled = true;
        };
    }, [server, family]);
    if (family === '') {
        return <Notice tone="info">{t('journey.conventions.none')}</Notice>;
    }
    const drawn = specsOf(family).length > 0;
    const needle = query.trim().toLowerCase();
    const shown = (rows ?? []).filter(
        (row) =>
            needle === '' ||
            row.id.toLowerCase().includes(needle) ||
            summaryOf(family, row).toLowerCase().includes(needle),
    );
    return (
        <div className="space-y-4">
            <div className="flex flex-wrap items-center gap-3">
                <div className="min-w-64 flex-1">
                    <Input
                        value={query}
                        placeholder={t('journey.conventions.convention.search')}
                        onChange={(event) => setQuery(event.target.value)}
                    />
                </div>
                {drawn && (
                    <Button
                        variant="primary"
                        onClick={() => {
                            state.startConvention();
                            onMove(conventionStepIndex('terms'));
                        }}
                    >
                        {t('journey.conventions.convention.new', {
                            family: t(`journey.conventions.families.${family}`),
                        })}
                    </Button>
                )}
            </div>
            <p className="text-xs text-ink-faint">
                {t('journey.conventions.convention.searchNote')}
            </p>
            {!drawn && <Notice tone="info">{t('journey.conventions.convention.notDrawn')}</Notice>}
            {failure !== undefined && (
                <Notice tone="error">
                    {t('journey.conventions.convention.readFailed', { message: failure })}
                </Notice>
            )}
            <div className="overflow-x-auto rounded-md border border-line">
                <table className="w-full text-left text-sm">
                    <thead>
                        <tr className="border-b border-line text-xs text-ink-muted">
                            {['id', 'terms', 'modifiedBy', 'version'].map((column) => (
                                <th key={column} className="px-3 py-2 font-medium">
                                    {t(`journey.conventions.convention.columns.${column}`)}
                                </th>
                            ))}
                        </tr>
                    </thead>
                    <tbody>
                        {rows?.length === 0 && (
                            <tr>
                                <td colSpan={4} className="px-3 py-3 text-ink-muted">
                                    {t('journey.conventions.convention.empty')}
                                </td>
                            </tr>
                        )}
                        {shown.map((row) => (
                            <tr
                                key={row.id}
                                className="cursor-pointer border-b border-line-subtle last:border-b-0 hover:bg-surface-hover"
                                onClick={() => {
                                    state.openConvention(row);
                                    onMove(conventionStepIndex('terms'));
                                }}
                            >
                                <td className="px-3 py-2 font-mono text-xs">
                                    {row.id.slice(0, 8)}
                                </td>
                                <td className="px-3 py-2">{summaryOf(family, row)}</td>
                                <td className="px-3 py-2 text-xs text-ink-muted">
                                    {String(row['modified_by'] ?? '')}
                                </td>
                                <td className="px-3 py-2 text-xs">{row.version}</td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            </div>
            {rows !== undefined && (
                <p className="text-xs text-ink-faint">
                    {t('journey.conventions.convention.total', {
                        shown: String(shown.length),
                        total: String(total),
                    })}
                </p>
            )}
        </div>
    );
}

/* -------------------------------------------------------------------- terms */

function TermField({
    t,
    spec,
    value,
    lists,
    onChange,
}: {
    readonly t: Translator['t'];
    readonly spec: TermSpec;
    readonly value: string;
    readonly lists: ConventionPickLists | undefined;
    readonly onChange: (value: string) => void;
}): ReactNode {
    const label = t(`journey.conventions.terms.columns.${spec.column}`);
    const badge = (
        <Tag tone={spec.kind === 'pick' ? 'accent' : 'muted'}>
            {spec.kind === 'pick'
                ? t('journey.conventions.terms.fromList')
                : t('journey.conventions.terms.freeText')}
        </Tag>
    );
    if (spec.kind === 'pick' && spec.pick !== undefined) {
        return (
            <div className="space-y-1">
                <Picker
                    label={spec.required ? `${label} *` : label}
                    value={value}
                    options={optionsFor(lists, spec.pick)}
                    empty={t('journey.conventions.terms.none')}
                    onChange={onChange}
                />
                {badge}
            </div>
        );
    }
    if (spec.kind === 'flag') {
        return (
            <label className="flex items-center gap-2 text-sm">
                <input
                    type="checkbox"
                    checked={value === 'true'}
                    onChange={(event) => onChange(event.target.checked ? 'true' : 'false')}
                />
                {label}
            </label>
        );
    }
    if (spec.kind === 'tristate') {
        return (
            <Field label={label}>
                <Select value={value} onChange={(event) => onChange(event.target.value)}>
                    <option value="">{t('journey.conventions.terms.none')}</option>
                    <option value="true">{t('journey.conventions.terms.yes')}</option>
                    <option value="false">{t('journey.conventions.terms.no')}</option>
                </Select>
            </Field>
        );
    }
    return (
        <div className="space-y-1">
            <Field label={spec.required ? `${label} *` : label}>
                <Input
                    value={value}
                    inputMode={spec.kind === 'integer' ? 'numeric' : undefined}
                    onChange={(event) => onChange(event.target.value)}
                />
            </Field>
            {badge}
        </div>
    );
}

function TermsStep({
    t,
    state,
    pickLists,
    pickFailure,
}: {
    readonly t: Translator['t'];
    readonly state: ConventionTerms;
    readonly pickLists: ConventionPickLists | undefined;
    readonly pickFailure: string | undefined;
}): ReactNode {
    const specs = specsOf(state.family);
    if (!state.authoring) {
        return <Notice tone="info">{t('journey.conventions.terms.open')}</Notice>;
    }
    const groups = (['general', 'fixed', 'floating', 'settlement'] as const).filter((group) =>
        specs.some((spec) => spec.group === group),
    );
    const problems = termProblems(state);
    return (
        <div className="space-y-5">
            {pickFailure !== undefined && (
                <Notice tone="warn">
                    {t('journey.conventions.pickFailed', { message: pickFailure })}
                </Notice>
            )}
            {state.family === 'swap' && (
                <p className="text-xs text-ink-faint">{t('journey.conventions.terms.legNote')}</p>
            )}
            {groups.map((group) => (
                <section key={group} className="card space-y-3 p-4">
                    <h3 className="font-semibold">
                        {t(`journey.conventions.terms.groups.${group}`)}
                    </h3>
                    <div className="grid gap-4 md:grid-cols-2">
                        {specs
                            .filter((spec) => spec.group === group)
                            .map((spec) => (
                                <TermField
                                    key={spec.column}
                                    t={t}
                                    spec={spec}
                                    value={state.terms[spec.column] ?? ''}
                                    lists={pickLists}
                                    onChange={(value) => state.setTerm(spec.column, value)}
                                />
                            ))}
                    </div>
                </section>
            ))}
            {problems.length > 0 && (
                <Notice tone="warn">
                    {t('journey.conventions.terms.lacking', { columns: problems.join(', ') })}
                </Notice>
            )}
        </div>
    );
}

/* ------------------------------------------------------------------- review */

function ReviewStep({
    t,
    state,
    reasons,
    onMove,
}: {
    readonly t: Translator['t'];
    readonly state: ConventionTerms;
    readonly reasons: ConventionStepsInput['reasons'];
    readonly onMove: (index: number) => void;
}): ReactNode {
    const chosen = reasons.find((reason) => reason.code === state.reasonCode);
    return (
        <div className="space-y-5">
            {state.refusal !== undefined && (
                <Notice tone="error">
                    <p className="font-semibold">{t('journey.conventions.refusal.heading')}</p>
                    <p className="text-sm">{state.refusal.subject}</p>
                    <p className="text-sm">{state.refusal.message}</p>
                    <ul className="text-sm">
                        {state.refusal.fields.map((failure) => (
                            <li key={`${failure.field}:${failure.code}`}>
                                {failure.field}: {failure.message}
                            </li>
                        ))}
                    </ul>
                    <p className="mt-2 text-sm">{t('journey.conventions.refusal.kept')}</p>
                    <Button
                        size="sm"
                        className="mt-2"
                        onClick={() => onMove(conventionStepIndex(state.refusal?.step ?? 'terms'))}
                    >
                        {t('journey.conventions.refusal.walkBack')}
                    </Button>
                </Notice>
            )}
            {state.changes.length === 0 ? (
                <Notice tone="info">{t('journey.conventions.review.nothing')}</Notice>
            ) : (
                <>
                    <p className="text-sm text-ink-muted">
                        {t('journey.conventions.review.count', {
                            count: String(state.changes.length),
                        })}
                    </p>
                    <div className="overflow-x-auto rounded-md border border-line">
                        <table className="w-full text-left text-sm">
                            <thead>
                                <tr className="border-b border-line text-xs text-ink-muted">
                                    {['term', 'before', 'after'].map((column) => (
                                        <th key={column} className="px-3 py-2 font-medium">
                                            {t(`journey.conventions.review.columns.${column}`)}
                                        </th>
                                    ))}
                                </tr>
                            </thead>
                            <tbody>
                                {state.changes.map((change) => (
                                    <tr
                                        key={change.id}
                                        className="border-b border-line-subtle last:border-b-0"
                                    >
                                        <td className="px-3 py-2">
                                            {t(
                                                `journey.conventions.terms.columns.${change.column}`,
                                            )}
                                        </td>
                                        <td className="px-3 py-2 text-ink-muted">
                                            {change.before}
                                        </td>
                                        <td className="px-3 py-2">{change.after}</td>
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    </div>
                </>
            )}
            <div className="grid gap-4 md:grid-cols-2">
                <Field label={t('journey.conventions.review.reason')}>
                    <Select
                        value={state.reasonCode}
                        onChange={(event) => state.setReason(event.target.value)}
                    >
                        {!reasons.some((reason) => reason.code === state.reasonCode) && (
                            <option value={state.reasonCode}>{state.reasonCode}</option>
                        )}
                        {reasons.map((reason) => (
                            <option key={reason.code} value={reason.code}>
                                {reason.description}
                            </option>
                        ))}
                    </Select>
                </Field>
                <Field
                    label={t('journey.conventions.review.commentary')}
                    {...(chosen?.requiresCommentary === true
                        ? { hint: t('journey.conventions.review.commentaryRequired') }
                        : {})}
                >
                    <Input
                        value={state.commentary}
                        onChange={(event) => state.setCommentary(event.target.value)}
                    />
                </Field>
            </div>
        </div>
    );
}

/* ------------------------------------------------------------------ outcome */

function OutcomeStep({
    t,
    state,
    onMove,
    onFinished,
}: {
    readonly t: Translator['t'];
    readonly state: ConventionTerms;
    readonly onMove: (index: number) => void;
    readonly onFinished: () => void;
}): ReactNode {
    if (state.written === undefined) {
        return <Notice tone="info">{t('journey.conventions.none')}</Notice>;
    }
    return (
        <div className="space-y-4">
            <Notice tone="success">
                {t('journey.conventions.outcome.written', {
                    version: String(state.written.version),
                })}
            </Notice>
            <p className="text-sm text-ink-muted">{t('journey.conventions.outcome.reads')}</p>
            <div className="grid gap-3 md:grid-cols-3">
                <button
                    type="button"
                    className="card p-4 text-left hover:border-accent"
                    onClick={() => onMove(conventionStepIndex('history'))}
                >
                    <span className="font-semibold">
                        {t('journey.conventions.outcome.history')}
                    </span>
                </button>
                <button
                    type="button"
                    className="card p-4 text-left hover:border-accent"
                    onClick={() => onMove(conventionStepIndex('convention'))}
                >
                    <span className="font-semibold">{t('journey.conventions.outcome.list')}</span>
                </button>
                <button
                    type="button"
                    className="card p-4 text-left hover:border-accent"
                    onClick={() => onMove(conventionStepIndex('instrument'))}
                >
                    <span className="font-semibold">
                        {t('journey.conventions.outcome.another')}
                    </span>
                </button>
            </div>
            <Button variant="ghost" onClick={onFinished}>
                {t('journey.conventions.outcome.done')}
            </Button>
        </div>
    );
}

/* ------------------------------------------------------------------ history */

function HistoryStep({
    t,
    state,
    entity,
}: {
    readonly t: Translator['t'];
    readonly state: ConventionTerms;
    readonly entity: string | undefined;
}): ReactNode {
    const row = state.written ?? state.opened;
    if (row === undefined || entity === undefined) {
        return <Notice tone="info">{t('journey.conventions.history.none')}</Notice>;
    }
    return (
        <div className="space-y-4">
            <p className="text-xs text-ink-faint">{t('journey.conventions.history.note')}</p>
            <HistoryPanel entityType={entityTypeOf(entity)} entityId={row.id} />
        </div>
    );
}

/* -------------------------------------------------------------- the step list */

export function conventionSteps(input: ConventionStepsInput): readonly JourneyStep<ReactNode>[] {
    const { t, server, state, pickLists, pickFailure, reasons } = input;
    const problems = termProblems(state);
    const chosen = reasons.find((reason) => reason.code === state.reasonCode);
    const commentaryMissing = chosen?.requiresCommentary === true && state.commentary.trim() === '';
    const ready =
        state.authoring && state.changes.length > 0 && problems.length === 0 && !commentaryMissing;
    // The entity name is the family key with its words joined, which the card carried from the server.
    const entity =
        state.family === '' ? undefined : `${state.family.replaceAll('-', '_')}_convention`;

    const confirm = async (): Promise<void> => {
        const plan = writePlan(state);
        if (plan === undefined) {
            return;
        }
        state.clearRefusal();
        const outcome = await server.write(plan.family, plan.write, plan.version, plan.intent);
        if (!outcome.success || outcome.row === undefined) {
            const refusal: ConventionRefusal = {
                step: 'terms',
                subject: `refdata.v1.${plan.family}_conventions.put`,
                code: outcome.code,
                message: outcome.message,
                fields: outcome.fields,
            };
            state.recordRefusal(refusal);
            throw new Error(refusalText(t, refusal));
        }
        state.recordWritten(outcome.row);
    };

    return [
        {
            id: 'instrument',
            title: t('journey.conventions.instrument.title'),
            lead: t('journey.conventions.instrument.lead'),
            body: <InstrumentStep t={t} server={server} state={state} onMove={input.onMove} />,
        },
        {
            id: 'convention',
            title: t('journey.conventions.convention.title'),
            lead: t('journey.conventions.convention.lead'),
            body: <ConventionStep t={t} server={server} state={state} onMove={input.onMove} />,
        },
        {
            id: 'terms',
            title: t('journey.conventions.terms.title'),
            lead: t('journey.conventions.terms.lead'),
            body: <TermsStep t={t} state={state} pickLists={pickLists} pickFailure={pickFailure} />,
            next: {
                label: t('common.continue'),
                enabled: state.authoring && problems.length === 0,
            },
        },
        {
            id: 'review',
            title: t('journey.conventions.review.title'),
            lead: t('journey.conventions.review.lead'),
            body: <ReviewStep t={t} state={state} reasons={reasons} onMove={input.onMove} />,
            next: { label: t('journey.conventions.review.confirm'), enabled: ready, run: confirm },
            final: state.written !== undefined,
        },
        {
            id: 'outcome',
            title: t('journey.conventions.outcome.title'),
            lead: t('journey.conventions.outcome.lead'),
            final: true,
            body: (
                <OutcomeStep
                    t={t}
                    state={state}
                    onMove={input.onMove}
                    onFinished={input.onFinished}
                />
            ),
        },
        {
            id: 'history',
            title: t('journey.conventions.history.title'),
            lead: t('journey.conventions.history.lead'),
            body: <HistoryStep t={t} state={state} entity={entity} />,
            next: {
                label: t('journey.conventions.outcome.done'),
                enabled: true,
                run: input.onFinished,
            },
        },
    ];
}
