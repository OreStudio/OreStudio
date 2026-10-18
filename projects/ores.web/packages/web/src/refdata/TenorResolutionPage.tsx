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
 * The read-only front door to tenor resolution.
 *
 * A trader asks what date a tenor gives on a calendar, under a convention, and
 * reads the answer. The question names the rules as the store holds them, so
 * the screen writes nothing: there is no save, no change reason and no
 * membership row. The journey that authors those rules is offered from here as
 * a workflow, so reading a date never becomes authoring one by accident.
 *
 * The date is asked for as soon as the picks can answer it. A button would only
 * ask the person to confirm the four things they just chose, and the screen's
 * whole job is the answer.
 */

import { useQuery } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { tenors, type TenorResolveQuery } from '../api/tenors.js';
import { useTranslation } from '../i18n/Provider.js';
import { WorkflowMenu } from '../shell/WorkflowMenu.js';
import { Button, Detail, Field, Input, Notice, Select } from '../ui/Primitives.js';
import { loadedIso } from '../ui/loadedLabel.js';
import {
    answerOf,
    canResolve,
    fieldLabelKey,
    reachedDates,
    type ReachedDates,
    type ResolveAnswer,
} from './tenorResolution.js';
import type { Translator } from '../i18n/translate.js';

const CATALOGUE_KEY = ['tenors', 'catalogue'] as const;
const PICK_LISTS_KEY = ['tenors', 'pick-lists'] as const;

/** The address of one convention's set of tenors, as the cache knows it. */
function resolutionsKey(conventionCode: string): readonly unknown[] {
    return ['tenors', 'conventions', conventionCode, 'resolutions'];
}

/**
 * Today, as a date input writes it.
 *
 * The local day, not UTC: a trader east of London in the evening would
 * otherwise start from yesterday. `loadedIso` already reads the local clock.
 */
function today(): string {
    return loadedIso(Date.now()).slice(0, 10);
}

/** The three dates a resolution reports, each drawn only when it was reached. */
function DateDetails({
    dates,
    t,
}: {
    readonly dates: ReachedDates;
    readonly t: Translator['t'];
}): ReactNode {
    return (
        <>
            {dates.anchor !== '' && (
                <Detail label={t('refdata.tenors.anchor')} value={dates.anchor} mono />
            )}
            {dates.beforeRoll !== '' && (
                <Detail label={t('refdata.tenors.beforeRoll')} value={dates.beforeRoll} mono />
            )}
            {dates.adjusted !== '' && (
                <Detail label={t('refdata.tenors.adjusted')} value={dates.adjusted} mono />
            )}
        </>
    );
}

/** A read that failed, in the server's words, with the one action that helps. */
function Failed({
    message,
    onRetry,
    t,
}: {
    readonly message: string;
    readonly onRetry: () => void;
    readonly t: Translator['t'];
}): ReactNode {
    return (
        <Notice tone="error">
            <p>{message}</p>
            <Button size="sm" onClick={onRetry}>
                {t('unavailable.retry')}
            </Button>
        </Notice>
    );
}

/**
 * One answer, drawn by its kind.
 *
 * The page renders this, and the tests render it directly because this package
 * has no DOM to click through.
 *
 * A refusal and a failed request read differently on purpose: the first is a
 * rule that declined to resolve, the second is a question that never arrived.
 */
export function ResolutionAnswer({
    answer,
    onRetry,
}: {
    readonly answer: ResolveAnswer;
    readonly onRetry: () => void;
}): ReactNode {
    const { t } = useTranslation();
    if (answer.kind === 'answered') {
        const dates = reachedDates(answer.resolution);
        return (
            <div className="space-y-4">
                <Notice tone="success">{t('refdata.tenors.answered')}</Notice>
                {dates !== undefined && (
                    <dl className="grid gap-3 md:grid-cols-3">
                        <DateDetails dates={dates} t={t} />
                        <Detail
                            label={t('refdata.tenors.rollRule')}
                            value={answer.resolution.rollRule}
                        />
                        <Detail
                            label={t('refdata.tenors.calendar')}
                            value={answer.resolution.calendar}
                        />
                    </dl>
                )}
            </div>
        );
    }
    if (answer.kind === 'refused') {
        const dates = reachedDates(answer.resolution);
        return (
            <div className="space-y-4">
                <Notice tone="error">
                    <p className="font-semibold">{t('refdata.tenors.refused')}</p>
                    <p>{answer.resolution.result.message}</p>
                    {answer.resolution.result.fields.length > 0 && (
                        <ul>
                            {answer.resolution.result.fields.map((failure) => {
                                const label = fieldLabelKey(failure.field);
                                return (
                                    <li key={`${failure.field}:${failure.code}`}>
                                        {label === undefined
                                            ? failure.message
                                            : `${t(label)}: ${failure.message}`}
                                    </li>
                                );
                            })}
                        </ul>
                    )}
                </Notice>
                {/* A refusal may have walked part of the way, so what it reached is kept. */}
                {dates !== undefined && (
                    <dl className="grid gap-3 md:grid-cols-3">
                        <DateDetails dates={dates} t={t} />
                    </dl>
                )}
            </div>
        );
    }
    if (answer.kind === 'unreachable') {
        return <Failed message={answer.message} onRetry={onRetry} t={t} />;
    }
    return null;
}

export function TenorResolutionPage(): ReactNode {
    const { t } = useTranslation();
    const [conventionCode, setConventionCode] = useState('');
    const [tenorCode, setTenorCode] = useState('');
    const [calendarCode, setCalendarCode] = useState('');
    const [horizon, setHorizon] = useState(today());
    const [spot, setSpot] = useState('');

    const catalogue = useQuery({ queryKey: CATALOGUE_KEY, queryFn: tenors.catalogue });
    const pickLists = useQuery({ queryKey: PICK_LISTS_KEY, queryFn: tenors.pickLists });
    const members = useQuery({
        queryKey: resolutionsKey(conventionCode),
        queryFn: () => tenors.resolutions(conventionCode),
        enabled: conventionCode !== '',
    });

    /*
     * A rule measured from spot cannot resolve without a spot date, and the
     * engine refuses rather than assume the pair's spot lag. The member row may
     * override the anchor the convention sets, and the server reads the
     * override first, so the question goes to the row that will answer it.
     */
    const chosenConvention = (catalogue.data?.conventions ?? []).find(
        (row) => row.code === conventionCode,
    );
    const chosenMember = (members.data ?? []).find((row) => row.tenor_code === tenorCode);
    const anchor = chosenMember?.anchor_override ?? chosenConvention?.measured_from ?? '';
    const needsSpot = anchor === 'SPOT';

    const picks = { conventionCode, tenorCode, calendarCode, horizon, spot };
    const ready = canResolve(picks, needsSpot);
    const query: TenorResolveQuery = { ...picks, spot: spot.trim() === '' ? null : spot };

    const resolution = useQuery({
        queryKey: ['tenors', 'resolve', query],
        queryFn: () => tenors.resolve(query),
        enabled: ready,
    });

    const displayNames = new Map(
        (catalogue.data?.tenors ?? []).map((tenor) => [tenor.code, tenor.display_name]),
    );
    const offered = (members.data ?? []).map((member) => ({
        code: member.tenor_code,
        name: displayNames.get(member.tenor_code) ?? member.tenor_code,
    }));

    const readFailure = catalogue.error ?? pickLists.error ?? members.error;
    const retryReads = (): void => {
        void catalogue.refetch();
        void pickLists.refetch();
        if (conventionCode !== '') {
            void members.refetch();
        }
    };
    const answer = answerOf(
        resolution.data,
        resolution.error ?? null,
        resolution.isFetching && resolution.data === undefined,
    );
    const loading = catalogue.isPending || pickLists.isPending;

    return (
        <div className="space-y-5">
            <div className="flex justify-end">
                <WorkflowMenu on="tenors" />
            </div>
            {loading && <Notice tone="info">{t('refdata.tenors.loading')}</Notice>}
            {readFailure !== null && (
                <Failed message={readFailure.message} onRetry={retryReads} t={t} />
            )}
            <div className="grid gap-4 md:grid-cols-2">
                <Field label={t('refdata.tenors.convention')}>
                    <Select
                        value={conventionCode}
                        onChange={(event) => {
                            setConventionCode(event.target.value);
                            setTenorCode('');
                        }}
                    >
                        <option value="">{t('refdata.tenors.choose')}</option>
                        {(catalogue.data?.conventions ?? []).map((convention) => (
                            <option key={convention.code} value={convention.code}>
                                {convention.code}
                            </option>
                        ))}
                    </Select>
                </Field>
                <Field label={t('refdata.tenors.tenor')}>
                    <Select
                        value={tenorCode}
                        disabled={conventionCode === ''}
                        onChange={(event) => setTenorCode(event.target.value)}
                    >
                        <option value="">{t('refdata.tenors.choose')}</option>
                        {offered.map((tenor) => (
                            <option key={tenor.code} value={tenor.code}>
                                {tenor.name}
                            </option>
                        ))}
                    </Select>
                </Field>
                <Field label={t('refdata.tenors.calendar')}>
                    <Select
                        value={calendarCode}
                        onChange={(event) => setCalendarCode(event.target.value)}
                    >
                        <option value="">{t('refdata.tenors.choose')}</option>
                        {(pickLists.data?.calendars ?? []).map((calendar) => (
                            <option key={calendar.code} value={calendar.code}>
                                {calendar.name ?? calendar.code}
                            </option>
                        ))}
                    </Select>
                </Field>
                <Field label={t('refdata.tenors.horizon')}>
                    <Input
                        type="date"
                        value={horizon}
                        onChange={(event) => setHorizon(event.target.value)}
                    />
                </Field>
                <Field label={t('refdata.tenors.spot')} hint={t('refdata.tenors.spotHint')}>
                    <Input
                        type="date"
                        value={spot}
                        onChange={(event) => setSpot(event.target.value)}
                    />
                </Field>
            </div>
            {resolution.isFetching && resolution.data !== undefined && (
                <Notice tone="info">{t('refdata.tenors.loading')}</Notice>
            )}
            <ResolutionAnswer answer={answer} onRetry={() => void resolution.refetch()} />
        </div>
    );
}
