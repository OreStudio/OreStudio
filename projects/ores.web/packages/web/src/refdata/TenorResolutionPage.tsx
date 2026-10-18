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
 */

import { useMutation, useQuery } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { tenors, type TenorResolution, type TenorResolveQuery } from '../api/tenors.js';
import { useTranslation } from '../i18n/Provider.js';
import { loadedIso } from '../ui/loadedLabel.js';
import { WorkflowMenu } from '../shell/WorkflowMenu.js';
import { Button, Detail, Field, Input, Notice, Select } from '../ui/Primitives.js';
import {
    answerOf,
    canResolve,
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

/** The three dates a resolution reports, drawn the same way wherever they appear. */
function DateDetails({
    dates,
    t,
}: {
    readonly dates: ReachedDates;
    readonly t: Translator['t'];
}): ReactNode {
    return (
        <>
            <Detail label={t('refdata.tenors.anchor')} value={dates.anchor} mono />
            <Detail label={t('refdata.tenors.beforeRoll')} value={dates.beforeRoll} mono />
            <Detail label={t('refdata.tenors.adjusted')} value={dates.adjusted} mono />
        </>
    );
}

/**
 * One answer, drawn by its kind.
 *
 * A refusal and a failed request read differently on purpose: the first is a
 * rule that declined to resolve, the second is a question that never arrived.
 */
export function ResolutionAnswer({ answer }: { readonly answer: ResolveAnswer }): ReactNode {
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
                    <p className="text-sm">
                        <span className="font-mono text-xs">{answer.resolution.result.code}</span>{' '}
                        {answer.resolution.result.message}
                    </p>
                    {answer.resolution.result.fields.length > 0 && (
                        <ul className="text-sm">
                            {answer.resolution.result.fields.map((failure) => (
                                <li key={`${failure.field}:${failure.code}`}>
                                    {failure.field}: {failure.message}
                                </li>
                            ))}
                        </ul>
                    )}
                </Notice>
                {/* A refusal may still have walked part of the way, so what it reached is shown. */}
                {dates !== undefined && (
                    <dl className="grid gap-3 md:grid-cols-3">
                        <DateDetails dates={dates} t={t} />
                    </dl>
                )}
            </div>
        );
    }
    if (answer.kind === 'unreachable') {
        return (
            <Notice tone="error">
                {t('refdata.tenors.unreachable', { message: answer.message })}
            </Notice>
        );
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
    const resolve = useMutation({
        mutationFn: (query: TenorResolveQuery) => tenors.resolve(query),
    });

    const displayNames = new Map(
        (catalogue.data?.tenors ?? []).map((tenor) => [tenor.code, tenor.display_name]),
    );
    const offered = (members.data ?? []).map((member) => ({
        code: member.tenor_code,
        name: displayNames.get(member.tenor_code) ?? member.tenor_code,
    }));
    const needsSpot =
        (catalogue.data?.conventions ?? []).find((row) => row.code === conventionCode)
            ?.measured_from === 'SPOT';
    const ready = canResolve({ conventionCode, tenorCode, calendarCode, horizon, spot }, needsSpot);
    const readFailure = catalogue.error ?? pickLists.error ?? members.error;
    const answer = answerOf(resolve.data, resolve.error ?? null, resolve.isPending);

    return (
        <div className="space-y-5">
            <div className="flex justify-end">
                <WorkflowMenu on="tenors" />
            </div>
            {readFailure !== null && (
                <Notice tone="error">
                    {t('refdata.tenors.readFailed', { message: readFailure.message })}
                </Notice>
            )}
            <div className="grid gap-4 md:grid-cols-2">
                <Field label={t('refdata.tenors.convention')}>
                    <Select
                        value={conventionCode}
                        onChange={(event) => {
                            setConventionCode(event.target.value);
                            setTenorCode('');
                            resolve.reset();
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
                        onChange={(event) => {
                            setTenorCode(event.target.value);
                            resolve.reset();
                        }}
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
                        onChange={(event) => {
                            setCalendarCode(event.target.value);
                            resolve.reset();
                        }}
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
                        onChange={(event) => {
                            setHorizon(event.target.value);
                            resolve.reset();
                        }}
                    />
                </Field>
                <Field label={t('refdata.tenors.spot')} hint={t('refdata.tenors.spotHint')}>
                    <Input
                        type="date"
                        value={spot}
                        onChange={(event) => {
                            setSpot(event.target.value);
                            resolve.reset();
                        }}
                    />
                </Field>
            </div>
            <Button
                variant="primary"
                pending={resolve.isPending}
                disabled={!ready}
                onClick={() => {
                    resolve.mutate({
                        tenorCode,
                        conventionCode,
                        calendarCode,
                        horizon,
                        spot: spot.trim() === '' ? null : spot,
                    });
                }}
            >
                {t('refdata.tenors.run')}
            </Button>
            <ResolutionAnswer answer={answer} />
        </div>
    );
}
