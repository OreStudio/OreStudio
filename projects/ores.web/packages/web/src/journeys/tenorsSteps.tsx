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
 * The steps a tenor walk runs.
 *
 * The person opens or starts a tenor, opens or starts a convention of one, says
 * which tenors that convention resolves and from where each is measured, reads
 * the date the rule as it stands resolves to, reads the one review, and
 * confirms. The confirm is a sequence of writes and records a refusal naming
 * the step to walk back to.
 *
 * The convention is the second half of the same rail rather than a journey of
 * its own, because a tenor and the rule that resolves it are read together: the
 * review draws one difference list over both.
 *
 * The steps are data, which is why this is a function and not a component.
 */

import { useState, type ReactNode } from 'react';
import { Button, Detail, Field, Input, Notice, Select, Tag } from '../ui/Primitives.js';
import { HistoryPanel } from '../refdata/HistoryPanel.js';
import { Picker } from './pickers.js';
import {
    changeText,
    CONVENTION_ENTITY_TYPE,
    resolutionDraftOf,
    TENOR_ENTITY_TYPE,
    tenorProblems,
    writePlan,
    type ConventionField,
    type TenorDetails,
    type ChangeSide,
    type TenorField,
    type TenorRefusal,
} from './tenorsState.js';
import type { JourneyStep, StepId } from './runtime.js';
import type { TenorCatalogue, TenorPickLists, TenorsServer } from './tenorsServer.js';
import type { Translator } from '../i18n/translate.js';

/** The steps in the order the rail draws them, and the index each one sits at. */
export const TENOR_STEP_IDS = [
    'choose',
    'tenor',
    'convention',
    'schedule',
    'test',
    'review',
    'outcome',
    'history',
] as const;

export function tenorStepIndex(id: StepId): number {
    const index = TENOR_STEP_IDS.indexOf(id as (typeof TENOR_STEP_IDS)[number]);
    return index < 0 ? 0 : index;
}

export interface TenorStepsInput {
    readonly t: Translator['t'];
    readonly server: TenorsServer;
    readonly state: TenorDetails;
    readonly catalogue: TenorCatalogue | undefined;
    readonly catalogueFailure: string | undefined;
    readonly pickLists: TenorPickLists | undefined;
    readonly pickFailure: string | undefined;
    readonly reasons: readonly {
        readonly code: string;
        readonly description: string;
        readonly requiresCommentary: boolean;
    }[];
    readonly onMove: (index: number) => void;
    readonly onFinished: () => void;
}

/** The one line the review's refusal notice draws, and the error the page raises. */
function refusalText(t: Translator['t'], refusal: TenorRefusal): string {
    const fields = refusal.fields
        .map((failure) => `${failure.field}: ${failure.message}`)
        .join('; ');
    return [
        t('journey.tenors.refusal.heading'),
        refusal.subject,
        refusal.message,
        fields,
        t('journey.tenors.refusal.kept'),
    ]
        .filter((part) => part !== '')
        .join(' ');
}

/** The convention code a membership write names: the one read, or the one being authored. */
function conventionCodeOf(state: TenorDetails): string {
    return state.conventionOpened?.code ?? state.convention.code.trim();
}

/** Stands above every step once something is being shaped, so the record stays in view. */
export function tenorHeader(t: Translator['t'], state: TenorDetails): ReactNode | undefined {
    if (!state.tenorActive && !state.conventionActive) {
        return undefined;
    }
    const name = state.tenorActive
        ? (state.tenorOpened?.code ?? t('journey.tenors.new'))
        : (state.conventionOpened?.code ?? t('journey.tenors.new'));
    return (
        <div className="flex flex-wrap items-baseline justify-between gap-3 text-sm">
            <span className="font-medium">{name}</span>
            <span className="flex items-center gap-2 text-xs text-ink-faint">
                {state.tenorOpened !== undefined && <span>v{state.tenorOpened.version}</span>}
                {state.conventionOpened !== undefined && (
                    <span>v{state.conventionOpened.version}</span>
                )}
                {state.written && <Tag tone="accent">{t('journey.tenors.outcome.title')}</Tag>}
            </span>
        </div>
    );
}

/** A pick list's rows as a picker's options. */
function optionsOf(
    rows: readonly { readonly code: string; readonly name?: string }[] | undefined,
): readonly { readonly value: string; readonly label: string }[] {
    return (rows ?? []).map((row) => ({ value: row.code, label: row.name ?? row.code }));
}

/* ------------------------------------------------------------------- choose */

function ChooseStep({
    t,
    server,
    state,
    catalogue,
    failure,
    onMove,
}: {
    readonly t: Translator['t'];
    readonly server: TenorsServer;
    readonly state: TenorDetails;
    readonly catalogue: TenorCatalogue | undefined;
    readonly failure: string | undefined;
    readonly onMove: (index: number) => void;
}): ReactNode {
    const [query, setQuery] = useState('');
    const [readFailure, setReadFailure] = useState<string>();
    const needle = query.trim().toLowerCase();
    const matches = (value: string): boolean =>
        needle === '' || value.toLowerCase().includes(needle);

    const tenors = (catalogue?.tenors ?? []).filter(
        (row) => matches(row.code) || matches(row.display_name),
    );
    const conventions = (catalogue?.conventions ?? []).filter(
        (row) => matches(row.code) || matches(row.description),
    );
    const tenorCodes = (catalogue?.tenors ?? []).map((row) => row.code);

    const openConvention = async (code: string): Promise<void> => {
        const row = (catalogue?.conventions ?? []).find((candidate) => candidate.code === code);
        if (row === undefined) {
            return;
        }
        try {
            const resolutions = await server.resolutions(code);
            state.openConvention(row, resolutions, tenorCodes);
            onMove(tenorStepIndex('convention'));
        } catch (error) {
            setReadFailure(error instanceof Error ? error.message : String(error));
        }
    };

    return (
        <div className="space-y-4">
            <div className="flex flex-wrap items-center gap-3">
                <div className="min-w-64 flex-1">
                    <Input
                        value={query}
                        placeholder={t('journey.tenors.choose.search')}
                        onChange={(event) => setQuery(event.target.value)}
                    />
                </div>
                <Button
                    variant="primary"
                    onClick={() => {
                        state.startTenor();
                        onMove(tenorStepIndex('tenor'));
                    }}
                >
                    {t('journey.tenors.choose.startTenor')}
                </Button>
                <Button
                    onClick={() => {
                        state.startConvention(tenorCodes);
                        onMove(tenorStepIndex('convention'));
                    }}
                >
                    {t('journey.tenors.choose.startConvention')}
                </Button>
            </div>
            {failure !== undefined && (
                <Notice tone="error">
                    {t('journey.tenors.choose.readFailed', { message: failure })}
                </Notice>
            )}
            {readFailure !== undefined && (
                <Notice tone="error">
                    {t('journey.tenors.choose.readFailed', { message: readFailure })}
                </Notice>
            )}
            <section className="space-y-2">
                <h3 className="font-semibold">{t('journey.tenors.choose.tenors')}</h3>
                <div className="overflow-x-auto rounded-md border border-line">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                {['code', 'kind', 'unit', 'version'].map((column) => (
                                    <th key={column} className="px-3 py-2 font-medium">
                                        {t(`journey.tenors.choose.columns.${column}`)}
                                    </th>
                                ))}
                            </tr>
                        </thead>
                        <tbody>
                            {tenors.length === 0 && (
                                <tr>
                                    <td colSpan={4} className="px-3 py-3 text-ink-muted">
                                        {t('journey.tenors.choose.emptyTenors')}
                                    </td>
                                </tr>
                            )}
                            {tenors.map((row) => (
                                <tr
                                    key={row.code}
                                    className="cursor-pointer border-b border-line-subtle last:border-b-0 hover:bg-surface-hover"
                                    onClick={() => {
                                        state.openTenor(row);
                                        onMove(tenorStepIndex('tenor'));
                                    }}
                                >
                                    <td className="px-3 py-2 font-mono text-xs">{row.code}</td>
                                    <td className="px-3 py-2">{row.kind}</td>
                                    <td className="px-3 py-2">{row.unit}</td>
                                    <td className="px-3 py-2 text-xs">{row.version}</td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            </section>
            <section className="space-y-2">
                <h3 className="font-semibold">{t('journey.tenors.choose.conventions')}</h3>
                <div className="overflow-x-auto rounded-md border border-line">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                {['code', 'algorithm', 'version'].map((column) => (
                                    <th key={column} className="px-3 py-2 font-medium">
                                        {t(`journey.tenors.choose.columns.${column}`)}
                                    </th>
                                ))}
                            </tr>
                        </thead>
                        <tbody>
                            {conventions.length === 0 && (
                                <tr>
                                    <td colSpan={3} className="px-3 py-3 text-ink-muted">
                                        {t('journey.tenors.choose.emptyConventions')}
                                    </td>
                                </tr>
                            )}
                            {conventions.map((row) => (
                                <tr
                                    key={row.code}
                                    className="cursor-pointer border-b border-line-subtle last:border-b-0 hover:bg-surface-hover"
                                    onClick={() => {
                                        void openConvention(row.code);
                                    }}
                                >
                                    <td className="px-3 py-2 font-mono text-xs">{row.code}</td>
                                    <td className="px-3 py-2">{row.resolution_algorithm}</td>
                                    <td className="px-3 py-2 text-xs">{row.version}</td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            </section>
        </div>
    );
}

/* -------------------------------------------------------------------- tenor */

function TenorFieldRow({
    t,
    state,
    name,
    options,
}: {
    readonly t: Translator['t'];
    readonly state: TenorDetails;
    readonly name: TenorField;
    readonly options?: readonly { readonly value: string; readonly label: string }[];
}): ReactNode {
    const label = t(`journey.tenors.tenor.fields.${name}`);
    if (options !== undefined) {
        return (
            <Picker
                label={label}
                value={state.tenor[name]}
                options={options}
                empty={t('journey.tenors.convention.anchorNone')}
                onChange={(value) => state.setTenorField(name, value)}
            />
        );
    }
    return (
        <Field label={label}>
            <Input
                value={state.tenor[name]}
                onChange={(event) => state.setTenorField(name, event.target.value)}
            />
        </Field>
    );
}

function TenorStep({
    t,
    state,
    pickLists,
    pickFailure,
}: {
    readonly t: Translator['t'];
    readonly state: TenorDetails;
    readonly pickLists: TenorPickLists | undefined;
    readonly pickFailure: string | undefined;
}): ReactNode {
    if (!state.tenorActive) {
        return <Notice tone="info">{t('journey.tenors.tenor.open')}</Notice>;
    }
    const problems = tenorProblems(state);
    return (
        <div className="space-y-5">
            {pickFailure !== undefined && (
                <Notice tone="warn">
                    {t('journey.tenors.pickFailed', { message: pickFailure })}
                </Notice>
            )}
            <div className="grid gap-4 md:grid-cols-2">
                <TenorFieldRow t={t} state={state} name="code" />
                <TenorFieldRow t={t} state={state} name="displayName" />
                <TenorFieldRow t={t} state={state} name="description" />
                <TenorFieldRow t={t} state={state} name="sortOrder" />
                <TenorFieldRow
                    t={t}
                    state={state}
                    name="kind"
                    options={optionsOf(pickLists?.kinds)}
                />
                <TenorFieldRow
                    t={t}
                    state={state}
                    name="unit"
                    options={optionsOf(pickLists?.units)}
                />
                <TenorFieldRow t={t} state={state} name="multiplier" />
            </div>
            {problems.length > 0 && (
                <Notice tone="warn">
                    {t('journey.tenors.problems.header')}{' '}
                    {problems.map((problem) => t(problem.what)).join(' ')}
                </Notice>
            )}
        </div>
    );
}

/* --------------------------------------------------------------- convention */

function ConventionFieldRow({
    t,
    state,
    name,
    options,
}: {
    readonly t: Translator['t'];
    readonly state: TenorDetails;
    readonly name: ConventionField;
    readonly options?: readonly { readonly value: string; readonly label: string }[];
}): ReactNode {
    const label = t(`journey.tenors.convention.fields.${name}`);
    if (options !== undefined) {
        return (
            <Picker
                label={label}
                value={state.convention[name]}
                options={options}
                empty={t('journey.tenors.convention.anchorNone')}
                onChange={(value) => state.setConventionField(name, value)}
            />
        );
    }
    return (
        <Field label={label}>
            <Input
                value={state.convention[name]}
                onChange={(event) => state.setConventionField(name, event.target.value)}
            />
        </Field>
    );
}

function ConventionStep({
    t,
    state,
    pickLists,
    pickFailure,
}: {
    readonly t: Translator['t'];
    readonly state: TenorDetails;
    readonly pickLists: TenorPickLists | undefined;
    readonly pickFailure: string | undefined;
}): ReactNode {
    if (!state.conventionActive) {
        return <Notice tone="info">{t('journey.tenors.convention.open')}</Notice>;
    }
    const problems = tenorProblems(state);
    const anchorOptions = optionsOf(pickLists?.anchors);
    return (
        <div className="space-y-5">
            {pickFailure !== undefined && (
                <Notice tone="warn">
                    {t('journey.tenors.pickFailed', { message: pickFailure })}
                </Notice>
            )}
            <div className="grid gap-4 md:grid-cols-2">
                <ConventionFieldRow t={t} state={state} name="code" />
                <ConventionFieldRow t={t} state={state} name="description" />
                <ConventionFieldRow
                    t={t}
                    state={state}
                    name="measuredFrom"
                    options={anchorOptions}
                />
                <ConventionFieldRow
                    t={t}
                    state={state}
                    name="resolutionAlgorithm"
                    options={optionsOf(pickLists?.algorithms)}
                />
                <ConventionFieldRow
                    t={t}
                    state={state}
                    name="rollRule"
                    options={optionsOf(pickLists?.rollRules)}
                />
                <ConventionFieldRow t={t} state={state} name="oresmdUri" />
            </div>
            <section className="space-y-3">
                <h3 className="font-semibold">{t('journey.tenors.convention.members')}</h3>
                <p className="text-xs text-ink-faint">
                    {t('journey.tenors.convention.memberNote')}
                </p>
                <div className="overflow-x-auto rounded-md border border-line">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                <th className="px-3 py-2 font-medium">
                                    {t('journey.tenors.choose.columns.code')}
                                </th>
                                <th className="px-3 py-2 font-medium">
                                    {t('journey.tenors.convention.inSet')}
                                </th>
                                <th className="px-3 py-2 font-medium">
                                    {t('journey.tenors.convention.anchorOverride')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {state.members.length === 0 && (
                                <tr>
                                    <td colSpan={3} className="px-3 py-3 text-ink-muted">
                                        {t('journey.tenors.choose.emptyTenors')}
                                    </td>
                                </tr>
                            )}
                            {state.members.map((member) => (
                                <tr
                                    key={member.tenorCode}
                                    className="border-b border-line-subtle last:border-b-0"
                                >
                                    <td className="px-3 py-2 font-mono text-xs">
                                        {member.tenorCode}
                                    </td>
                                    <td className="px-3 py-2">
                                        <label className="flex items-center gap-2">
                                            <input
                                                type="checkbox"
                                                checked={member.wanted}
                                                onChange={() =>
                                                    state.toggleMember(member.tenorCode)
                                                }
                                            />
                                            <span className="text-xs">
                                                {member.wanted
                                                    ? t('journey.tenors.convention.inSet')
                                                    : t('journey.tenors.convention.notInSet')}
                                            </span>
                                        </label>
                                    </td>
                                    <td className="px-3 py-2">
                                        {member.wanted ? (
                                            <Picker
                                                label={t(
                                                    'journey.tenors.convention.anchorOverride',
                                                )}
                                                value={member.anchorOverride}
                                                options={anchorOptions}
                                                empty={t('journey.tenors.convention.anchorNone')}
                                                onChange={(value) =>
                                                    state.setAnchor(member.tenorCode, value)
                                                }
                                            />
                                        ) : (
                                            <span className="text-xs text-ink-faint">
                                                {t('journey.tenors.convention.notInSet')}
                                            </span>
                                        )}
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            </section>
            {problems.length > 0 && (
                <Notice tone="warn">
                    {t('journey.tenors.problems.header')}{' '}
                    {problems.map((problem) => t(problem.what)).join(' ')}
                </Notice>
            )}
        </div>
    );
}

/* ----------------------------------------------------------------- schedule */

/**
 * The schedule axes a convention may roll on.
 *
 * A schedule axis is what turns a period into dated rolls, so a person choosing
 * a convention is choosing how it rolls. The axis is read from the catalogue and
 * named here; expanding it into its dated rolls needs the schedule-date operation
 * the server does not serve yet, so this step names the axis and its source
 * rather than drawing the dates.
 */
function ScheduleStep({
    t,
    state,
    catalogue,
}: {
    readonly t: Translator['t'];
    readonly state: TenorDetails;
    readonly catalogue: TenorCatalogue | undefined;
}): ReactNode {
    const [chosen, setChosen] = useState('');
    if (!state.conventionActive) {
        return <Notice tone="info">{t('journey.tenors.schedule.open')}</Notice>;
    }
    const schedules = catalogue?.schedules ?? [];
    const memberSchedules = new Set(
        state.members
            .filter((member) => member.wanted)
            .map((member) => member.original?.schedule_code ?? '')
            .filter((code) => code !== ''),
    );
    const selected = chosen !== '' ? chosen : ([...memberSchedules][0] ?? '');
    return (
        <div className="space-y-5">
            <p className="text-xs text-ink-faint">{t('journey.tenors.schedule.note')}</p>
            {schedules.length === 0 ? (
                <Notice tone="info">{t('journey.tenors.schedule.empty')}</Notice>
            ) : (
                <div className="grid gap-3 md:grid-cols-2">
                    {schedules.map((schedule) => (
                        <button
                            key={schedule.code}
                            type="button"
                            className={`card space-y-1 p-4 text-left hover:border-accent ${
                                selected === schedule.code ? 'border-accent' : ''
                            }`}
                            onClick={() => setChosen(schedule.code)}
                        >
                            <span className="flex items-center justify-between gap-2">
                                <span className="font-mono text-xs">{schedule.code}</span>
                                {memberSchedules.has(schedule.code) && (
                                    <Tag tone="accent">{t('journey.tenors.schedule.inUse')}</Tag>
                                )}
                            </span>
                            <span className="block font-semibold">{schedule.name}</span>
                            <span className="block text-xs text-ink-muted">
                                {schedule.description}
                            </span>
                            <span className="block font-mono text-xs text-ink-faint">
                                {schedule.schedule_source}
                                {schedule.calendar_code === null
                                    ? ''
                                    : ` · ${schedule.calendar_code}`}
                            </span>
                        </button>
                    ))}
                </div>
            )}
        </div>
    );
}

/* --------------------------------------------------------------------- test */

/**
 * Resolves the rule as the form holds it.
 *
 * The answer describes the draft, not the saved row, so a person reads the date
 * before they write it. A refusal is an answer too and carries the dates it
 * reached, so the step shows it beside them and keeps every field it held.
 */
function TestStep({
    t,
    server,
    state,
    pickLists,
}: {
    readonly t: Translator['t'];
    readonly server: TenorsServer;
    readonly state: TenorDetails;
    readonly pickLists: TenorPickLists | undefined;
}): ReactNode {
    const [busy, setBusy] = useState(false);
    if (!state.tenorActive && !state.conventionActive) {
        return <Notice tone="info">{t('journey.tenors.test.open')}</Notice>;
    }
    const ready = state.testHorizon.trim() !== '' && state.testCalendar !== '';
    const run = async (): Promise<void> => {
        setBusy(true);
        try {
            state.recordTestPreview(await server.previewResolution(resolutionDraftOf(state)));
        } catch (error) {
            state.recordTestFailure(error instanceof Error ? error.message : String(error));
        } finally {
            setBusy(false);
        }
    };
    const preview = state.testPreview;
    const refused = preview !== undefined && preview.result.outcome !== 'ok';
    return (
        <div className="space-y-5">
            <p className="text-xs text-ink-faint">{t('journey.tenors.test.note')}</p>
            <div className="grid gap-4 md:grid-cols-3">
                <Field label={t('journey.tenors.test.horizon')}>
                    <Input
                        type="date"
                        value={state.testHorizon}
                        onChange={(event) => state.setTestHorizon(event.target.value)}
                    />
                </Field>
                <Field
                    label={t('journey.tenors.test.spot')}
                    hint={t('journey.tenors.test.spotHint')}
                >
                    <Input
                        type="date"
                        value={state.testSpot}
                        onChange={(event) => state.setTestSpot(event.target.value)}
                    />
                </Field>
                <Picker
                    label={t('journey.tenors.test.calendar')}
                    value={state.testCalendar}
                    options={optionsOf(pickLists?.calendars)}
                    empty={t('journey.tenors.test.calendarNone')}
                    onChange={(value) => state.setTestCalendar(value)}
                />
            </div>
            <Button variant="primary" pending={busy} disabled={!ready} onClick={() => void run()}>
                {t('journey.tenors.test.run')}
            </Button>
            {state.testFailure !== undefined && (
                <Notice tone="error">
                    {t('journey.tenors.test.failed', { message: state.testFailure })}
                </Notice>
            )}
            {preview !== undefined && (
                <div className="space-y-4">
                    {refused ? (
                        <Notice tone="error">
                            <p className="font-semibold">{t('journey.tenors.test.refused')}</p>
                            <p className="text-sm">
                                <span className="font-mono text-xs">{preview.result.code}</span>{' '}
                                {preview.result.message}
                            </p>
                            {preview.result.fields.length > 0 && (
                                <ul className="text-sm">
                                    {preview.result.fields.map((failure) => (
                                        <li key={`${failure.field}:${failure.code}`}>
                                            {t('journey.tenors.test.atFault')} {failure.field}:{' '}
                                            {failure.message}
                                        </li>
                                    ))}
                                </ul>
                            )}
                        </Notice>
                    ) : (
                        <Notice tone="success">{t('journey.tenors.test.resolved')}</Notice>
                    )}
                    {/*
                     * The dates travel on a refusal too, so they are drawn
                     * whether the rule was accepted or declined.
                     */}
                    <dl className="grid gap-3 md:grid-cols-3">
                        <Detail
                            label={t('journey.tenors.test.anchor')}
                            value={preview.anchorDate}
                            mono
                        />
                        <Detail
                            label={t('journey.tenors.test.beforeRoll')}
                            value={preview.dateBeforeRoll}
                            mono
                        />
                        <Detail
                            label={t('journey.tenors.test.adjusted')}
                            value={preview.adjustedDate}
                            mono
                        />
                        <Detail
                            label={t('journey.tenors.test.rollRule')}
                            value={preview.rollRule}
                        />
                        <Detail
                            label={t('journey.tenors.test.calendar')}
                            value={preview.calendar}
                        />
                    </dl>
                </div>
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
    readonly state: TenorDetails;
    readonly reasons: TenorStepsInput['reasons'];
    readonly onMove: (index: number) => void;
}): ReactNode {
    const chosen = reasons.find((reason) => reason.code === state.reasonCode);
    const refusedStep = state.refusal?.step ?? 'review';
    return (
        <div className="space-y-5">
            {state.refusal !== undefined && (
                <Notice tone="error">
                    <p className="font-semibold">{t('journey.tenors.refusal.heading')}</p>
                    {/*
                     * The step that refused, in the words the rail uses. A person
                     * walks back to it, so the name has to be the rail's name and
                     * not the protocol subject the server answered with.
                     */}
                    <p className="text-sm">
                        {t('journey.tenors.refusal.atStep', {
                            step: t(`journey.tenors.review.stepLabel.${refusedStep}`),
                        })}
                    </p>
                    <p className="text-sm">{state.refusal.subject}</p>
                    <p className="text-sm">{state.refusal.message}</p>
                    <ul className="text-sm">
                        {state.refusal.fields.map((failure) => (
                            <li key={`${failure.field}:${failure.code}`}>
                                {failure.field}: {failure.message}
                            </li>
                        ))}
                    </ul>
                    {state.refusal.written.length > 0 && (
                        <p className="mt-2 text-sm">{t('journey.tenors.refusal.kept')}</p>
                    )}
                    <Button
                        size="sm"
                        className="mt-2"
                        onClick={() => onMove(tenorStepIndex(refusedStep))}
                    >
                        {t('journey.tenors.refusal.walkBack')}
                    </Button>
                </Notice>
            )}
            {state.changes.length === 0 ? (
                <Notice tone="info">{t('journey.tenors.review.nothing')}</Notice>
            ) : (
                <>
                    <p className="text-sm text-ink-muted">
                        {t('journey.tenors.review.count', {
                            count: String(state.changes.length),
                        })}
                    </p>
                    <div className="overflow-x-auto rounded-md border border-line">
                        <table className="w-full text-left text-sm">
                            <thead>
                                <tr className="border-b border-line text-xs text-ink-muted">
                                    {['change', 'before', 'after'].map((column) => (
                                        <th key={column} className="px-3 py-2 font-medium">
                                            {t(`journey.tenors.review.columns.${column}`)}
                                        </th>
                                    ))}
                                </tr>
                            </thead>
                            <tbody>
                                {state.changes.map((change) => {
                                    const text = changeText(change);
                                    const side = (value: ChangeSide): string =>
                                        value.value !== undefined
                                            ? value.value
                                            : t(value.key ?? '', value.params);
                                    return (
                                        <tr
                                            key={change.id}
                                            className="border-b border-line-subtle last:border-b-0"
                                        >
                                            <td className="px-3 py-2">
                                                {t(text.whatKey, text.whatParams)}
                                            </td>
                                            <td className="px-3 py-2 text-ink-muted">
                                                {side(text.before)}
                                            </td>
                                            <td className="px-3 py-2">{side(text.after)}</td>
                                        </tr>
                                    );
                                })}
                            </tbody>
                        </table>
                    </div>
                </>
            )}
            <div className="grid gap-4 md:grid-cols-2">
                <Field label={t('journey.tenors.review.reason')}>
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
                    label={t('journey.tenors.review.commentary')}
                    {...(chosen?.requiresCommentary === true
                        ? { hint: t('journey.tenors.review.commentaryRequired') }
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
    readonly state: TenorDetails;
    readonly onMove: (index: number) => void;
    readonly onFinished: () => void;
}): ReactNode {
    if (!state.written) {
        return <Notice tone="info">{t('journey.tenors.none')}</Notice>;
    }
    return (
        <div className="space-y-4">
            <Notice tone="success">{t('journey.tenors.outcome.written')}</Notice>
            <p className="text-sm text-ink-muted">{t('journey.tenors.outcome.reads')}</p>
            <div className="grid gap-3 md:grid-cols-3">
                <button
                    type="button"
                    className="card p-4 text-left hover:border-accent"
                    onClick={() => onMove(tenorStepIndex('history'))}
                >
                    <span className="font-semibold">{t('journey.tenors.outcome.history')}</span>
                </button>
                <button
                    type="button"
                    className="card p-4 text-left hover:border-accent"
                    onClick={() => onMove(tenorStepIndex('choose'))}
                >
                    <span className="font-semibold">{t('journey.tenors.outcome.list')}</span>
                </button>
                <button
                    type="button"
                    className="card p-4 text-left hover:border-accent"
                    onClick={() => onMove(tenorStepIndex('choose'))}
                >
                    <span className="font-semibold">{t('journey.tenors.outcome.another')}</span>
                </button>
            </div>
            <Button variant="ghost" onClick={onFinished}>
                {t('journey.tenors.outcome.done')}
            </Button>
        </div>
    );
}

/* ------------------------------------------------------------------ history */

function HistoryStep({
    t,
    state,
}: {
    readonly t: Translator['t'];
    readonly state: TenorDetails;
}): ReactNode {
    const row = state.tenorOpened ?? state.conventionOpened;
    if (row === undefined) {
        return <Notice tone="info">{t('journey.tenors.history.none')}</Notice>;
    }
    const entityType = state.tenorOpened !== undefined ? TENOR_ENTITY_TYPE : CONVENTION_ENTITY_TYPE;
    return (
        <div className="space-y-4">
            <p className="text-xs text-ink-faint">{t('journey.tenors.history.note')}</p>
            <HistoryPanel entityType={entityType} entityId={row.code} />
        </div>
    );
}

/* ------------------------------------------------------------- the step list */

export function tenorSteps(input: TenorStepsInput): readonly JourneyStep<ReactNode>[] {
    const { t, server, state, pickLists, pickFailure, reasons } = input;
    const problems = tenorProblems(state);
    const chosen = reasons.find((reason) => reason.code === state.reasonCode);
    const commentaryMissing = chosen?.requiresCommentary === true && state.commentary.trim() === '';
    const ready =
        (state.tenorActive || state.conventionActive) &&
        state.changes.length > 0 &&
        problems.length === 0 &&
        !commentaryMissing;

    const confirm = async (): Promise<void> => {
        const plan = writePlan(state);
        if (plan === undefined) {
            return;
        }
        const conventionCode = conventionCodeOf(state);
        state.clearRefusal();
        const written: string[] = [];
        const refuse = (
            step: string,
            subject: string,
            code: string,
            message: string,
            fields: TenorRefusal['fields'],
        ): never => {
            const refusal: TenorRefusal = { step, subject, code, message, fields, written };
            state.recordRefusal(refusal);
            throw new Error(refusalText(t, refusal));
        };

        if (plan.tenor !== undefined) {
            const outcome = await server.writeTenor(
                plan.tenor.write,
                plan.tenor.version,
                plan.intent,
            );
            if (!outcome.success) {
                refuse(
                    'tenor',
                    'refdata.v1.tenors.put',
                    outcome.code,
                    outcome.message,
                    outcome.fields,
                );
            }
            written.push('refdata.v1.tenors.put');
        }
        if (plan.convention !== undefined) {
            const outcome = await server.writeConvention(
                plan.convention.write,
                plan.convention.version,
                plan.intent,
            );
            if (!outcome.success) {
                refuse(
                    'convention',
                    'refdata.v1.tenor_conventions.put',
                    outcome.code,
                    outcome.message,
                    outcome.fields,
                );
            }
            written.push('refdata.v1.tenor_conventions.put');
        }
        for (const link of plan.links) {
            const outcome = await server.writeMembership(
                conventionCode,
                link.tenorCode,
                link.anchorOverride,
                link.version,
                plan.intent,
            );
            if (!outcome.success) {
                refuse(
                    'convention',
                    'refdata.v1.tenor_convention_resolutions.put',
                    outcome.code,
                    outcome.message,
                    outcome.fields,
                );
            }
            written.push('refdata.v1.tenor_convention_resolutions.put');
        }
        for (const unlink of plan.unlinks) {
            const outcome = await server.removeMembership(
                conventionCode,
                unlink.tenorCode,
                unlink.version,
                plan.intent,
            );
            if (!outcome.success) {
                refuse(
                    'convention',
                    'refdata.v1.tenor_convention_resolutions.delete',
                    outcome.code,
                    outcome.message,
                    outcome.fields,
                );
            }
            written.push('refdata.v1.tenor_convention_resolutions.delete');
        }
        state.recordWritten();
    };

    return [
        {
            id: 'choose',
            title: t('journey.tenors.choose.title'),
            lead: t('journey.tenors.choose.lead'),
            body: (
                <ChooseStep
                    t={t}
                    server={server}
                    state={state}
                    catalogue={input.catalogue}
                    failure={input.catalogueFailure}
                    onMove={input.onMove}
                />
            ),
        },
        {
            id: 'tenor',
            title: t('journey.tenors.tenor.title'),
            lead: t('journey.tenors.tenor.lead'),
            body: <TenorStep t={t} state={state} pickLists={pickLists} pickFailure={pickFailure} />,
            next: {
                label: t('common.continue'),
                enabled: state.tenorActive && tenorProblems(state).length === 0,
            },
        },
        {
            id: 'convention',
            title: t('journey.tenors.convention.title'),
            lead: t('journey.tenors.convention.lead'),
            body: (
                <ConventionStep
                    t={t}
                    state={state}
                    pickLists={pickLists}
                    pickFailure={pickFailure}
                />
            ),
            next: {
                label: t('common.continue'),
                enabled: state.conventionActive && tenorProblems(state).length === 0,
            },
        },
        {
            id: 'schedule',
            title: t('journey.tenors.schedule.title'),
            lead: t('journey.tenors.schedule.lead'),
            body: <ScheduleStep t={t} state={state} catalogue={input.catalogue} />,
            next: { label: t('common.continue'), enabled: state.conventionActive },
        },
        {
            id: 'test',
            title: t('journey.tenors.test.title'),
            lead: t('journey.tenors.test.lead'),
            body: <TestStep t={t} server={server} state={state} pickLists={pickLists} />,
            next: { label: t('common.continue'), enabled: true },
        },
        {
            id: 'review',
            title: t('journey.tenors.review.title'),
            lead: t('journey.tenors.review.lead'),
            body: <ReviewStep t={t} state={state} reasons={reasons} onMove={input.onMove} />,
            next: { label: t('journey.tenors.review.confirm'), enabled: ready, run: confirm },
            final: state.written,
        },
        {
            id: 'outcome',
            title: t('journey.tenors.outcome.title'),
            lead: t('journey.tenors.outcome.lead'),
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
            title: t('journey.tenors.history.title'),
            lead: t('journey.tenors.history.lead'),
            body: <HistoryStep t={t} state={state} />,
            next: {
                label: t('journey.tenors.outcome.done'),
                enabled: true,
                run: input.onFinished,
            },
        },
    ];
}
