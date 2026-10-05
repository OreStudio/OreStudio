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

import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { Link, useNavigate, useParams } from 'react-router';
import type { HistoryVersion } from '@ores/wire-protocol/browser';
import { api, type CalendarDay, type RecordRow } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { useFlags } from '../images/flags.js';
import { Button, Field, Input, Notice } from '../ui/Primitives.js';
import { HistoryPanel } from './HistoryPanel.js';
import { RecordList, useRecordSource } from './RecordList.js';
import { RecordPicker } from './RecordPicker.js';
import {
    ClassifiedValue,
    PartsPanel,
    RecordDetails,
    RecordDialog,
    RecordGate,
    RecordHeader,
    RemoveRecordDialog,
    RevertDialog,
    show,
    useRecord,
    useRecordPermissions,
    useRecordTabs,
    useRecords,
    type FieldSpec,
} from './records.js';

const CALENDARS = 'calendars';
const RULES = 'calendar-rules';
const EXCEPTIONS = 'calendar-exceptions';
const EVENTS = 'calendar-events';

const CALENDAR_FIELDS: readonly FieldSpec[] = [
    {
        field: 'code',
        history: 'Code',
        kind: { kind: 'text', max: 100 },
        fixed: true,
        flag: 'calendar',
    },
    { field: 'name', history: 'Name', kind: { kind: 'text' } },
    {
        field: 'calendar_type',
        history: 'Calendar Type',
        kind: { kind: 'classification', list: 'calendar-type' },
    },
    {
        field: 'country_code',
        history: 'Country Code',
        kind: { kind: 'record', resource: 'countries', value: 'alpha2_code', label: 'name' },
        flag: 'country',
    },
    { field: 'image_id', history: 'Image ID', kind: { kind: 'image' }, optional: true },
];

/** The calendar fields the server requires that the form does not edit. */
const CALENDAR_KEPT = ['source', 'is_editable', 'base_calendar_code'];

const RULE_KINDS = ['fixed_date', 'nth_weekday_of_month', 'last_weekday_of_month', 'easter_offset'];

const RULE_FIELDS: readonly FieldSpec[] = [
    { field: 'kind', history: 'Kind', kind: { kind: 'choice', options: RULE_KINDS } },
    {
        field: 'month',
        history: 'Month',
        kind: {
            kind: 'choice',
            options: ['1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12'],
            numeric: true,
        },
        when: (values) => values['kind'] !== 'easter_offset',
    },
    {
        field: 'day',
        history: 'Day',
        kind: { kind: 'int', min: 1, max: 31 },
        when: (values) => values['kind'] === 'fixed_date',
    },
    {
        field: 'occurrence',
        history: 'Occurrence',
        kind: { kind: 'choice', options: ['1', '2', '3', '4'], numeric: true },
        when: (values) => values['kind'] === 'nth_weekday_of_month',
    },
    {
        field: 'weekday',
        history: 'Weekday',
        kind: { kind: 'choice', options: ['1', '2', '3', '4', '5', '6', '0'], numeric: true },
        when: (values) =>
            values['kind'] === 'nth_weekday_of_month' || values['kind'] === 'last_weekday_of_month',
    },
    {
        field: 'day_offset',
        history: 'Day Offset',
        kind: { kind: 'int', min: -366, max: 366 },
        when: (values) => values['kind'] === 'easter_offset',
    },
    {
        field: 'shift',
        history: 'Shift',
        kind: { kind: 'choice', options: ['none', 'nearest_weekday', 'roll_forward_to_monday'] },
    },
    {
        field: 'effective_from',
        history: 'Effective From',
        kind: { kind: 'int', min: 1900, max: 2200 },
        optional: true,
    },
    {
        field: 'effective_to',
        history: 'Effective To',
        kind: { kind: 'int', min: 1900, max: 2200 },
        optional: true,
    },
];

const EXCEPTION_FIELDS: readonly FieldSpec[] = [
    { field: 'exception_date', history: 'Exception Date', kind: { kind: 'date' } },
    { field: 'is_business_day', history: 'Is Business Day', kind: { kind: 'bool' } },
    { field: 'description', history: 'Description', kind: { kind: 'text' }, optional: true },
];

const EVENT_FIELDS: readonly FieldSpec[] = [
    { field: 'event_date', history: 'Event Date', kind: { kind: 'date' } },
    {
        field: 'diary_entry_type',
        history: 'Diary Entry Type',
        kind: { kind: 'classification', list: 'diary-entry-type' },
    },
    { field: 'name', history: 'Name', kind: { kind: 'text' } },
    { field: 'description', history: 'Description', kind: { kind: 'text' }, optional: true },
];

/**
 * What a calendar is made of, which decides the panels it shows.
 *
 * A QuantLib calendar takes its holidays from QuantLib and is read only. A
 * derived calendar is a base calendar with exceptions on top. A bespoke
 * calendar is made of its own rules and exceptions.
 */
type Shape = 'quantlib' | 'derived' | 'bespoke';

function shapeOf(calendar: RecordRow): Shape {
    if (calendar['is_editable'] !== true) {
        return 'quantlib';
    }
    return show(calendar['base_calendar_code']) === '' ? 'bespoke' : 'derived';
}

const TABS: Readonly<Record<Shape, readonly string[]>> = {
    quantlib: ['details', 'events', 'history'],
    derived: ['details', 'exceptions', 'events', 'days', 'history'],
    bespoke: ['details', 'rules', 'exceptions', 'events', 'days', 'history'],
};

export function calendarPath(code?: string): string {
    return code === undefined
        ? '/refdata/calendars'
        : `/refdata/calendars/${encodeURIComponent(code)}`;
}

/** A new user calendar: bespoke when it has no base, derived when it has one. */
function userCalendar(base: string | null): Readonly<Record<string, unknown>> {
    return { image_id: null, source: 'user', is_editable: true, base_calendar_code: base };
}

/** The tenant's calendars: one page at a time, searched and sorted on the server. */
export function CalendarsPage(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const [adding, setAdding] = useState(false);
    const source = useRecordSource(CALENDARS);
    return (
        <>
            <RecordList
                source={source}
                title={t('refdata.calendars.title')}
                lead={t('refdata.calendars.lead')}
                crumbs={[
                    { label: t('refdata.area.title'), to: '/refdata' },
                    { label: t('refdata.calendars.title') },
                ]}
                pathOf={(row) => calendarPath(show(row['code']))}
                addLabel={t('refdata.calendars.add')}
                onAdd={() => setAdding(true)}
                columns={[
                    {
                        id: 'code',
                        header: t('refdata.fields.code'),
                        cell: (row) => show(row['code']),
                        mono: true,
                        sort: 'code',
                        flag: { source: 'calendar', code: (row) => show(row['code']) },
                    },
                    {
                        id: 'name',
                        header: t('refdata.fields.name'),
                        cell: (row) => show(row['name']),
                        sort: 'name',
                    },
                    {
                        id: 'calendar_type',
                        header: t('refdata.fields.calendar_type'),
                        cell: (row) => (
                            <ClassifiedValue
                                list="calendar-type"
                                code={show(row['calendar_type'])}
                            />
                        ),
                        sort: 'calendar_type',
                    },
                    {
                        id: 'country_code',
                        header: t('refdata.fields.country_code'),
                        cell: (row) => show(row['country_code']),
                        mono: true,
                        sort: 'country_code',
                        flag: { source: 'country', code: (row) => show(row['country_code']) },
                    },
                    {
                        id: 'made',
                        header: t('refdata.calendars.made'),
                        cell: (row) => <MadeOf calendar={row} />,
                    },
                ]}
            />
            {adding && (
                <RecordDialog
                    title={t('refdata.calendars.addTitle')}
                    resource={CALENDARS}
                    specs={CALENDAR_FIELDS}
                    row={undefined}
                    given={userCalendar(null)}
                    onClose={() => setAdding(false)}
                    onSaved={(write) => void navigate(calendarPath(show(write['code'])))}
                />
            )}
        </>
    );
}

/** Where a calendar's holidays come from, in words. */
function MadeOf({ calendar }: { readonly calendar: RecordRow }): ReactNode {
    const { t } = useTranslation();
    const shape = shapeOf(calendar);
    if (shape === 'derived') {
        return (
            <>
                {t('refdata.calendars.derivedFrom', { base: show(calendar['base_calendar_code']) })}
            </>
        );
    }
    return <>{t(`refdata.calendars.shape.${shape}`)}</>;
}

/** One calendar: its row, the inputs that make its business days, the days, and the history. */
export function CalendarPage(): ReactNode {
    const { code } = useParams();
    return (
        <RecordGate resource={CALENDARS} recordKey={code ?? ''} listPath={calendarPath()}>
            {(calendar) => <CalendarBody key={show(calendar['code'])} calendar={calendar} />}
        </RecordGate>
    );
}

function CalendarBody({ calendar }: { readonly calendar: RecordRow }): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const code = show(calendar['code']);
    const shape = shapeOf(calendar);
    const editable = shape !== 'quantlib';
    const may = useRecordPermissions(CALENDARS);
    const { tab, bar } = useRecordTabs({ label: code, tabs: TABS[shape] });
    const rules = useRecords(RULES, code);
    const exceptions = useRecords(EXCEPTIONS, code);
    const events = useRecords(EVENTS, code);
    const [editing, setEditing] = useState(false);
    const [deriving, setDeriving] = useState(false);
    const [removing, setRemoving] = useState(false);
    const [reverting, setReverting] = useState<HistoryVersion | null>(null);
    const baseCode = show(calendar['base_calendar_code']);
    const baseRecord = useQuery({
        queryKey: ['records', CALENDARS, 'key', baseCode],
        queryFn: () => api.record(CALENDARS, baseCode),
        enabled: baseCode !== '',
    });
    const base = baseCode === '' ? undefined : baseRecord.data;

    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[
                    { label: t('refdata.area.title'), to: '/refdata' },
                    { label: t('refdata.calendars.title'), to: calendarPath() },
                    { label: code },
                ]}
                title={show(calendar['name'])}
                recordKey={code}
                version={calendar.version}
                flag="calendar"
                own={
                    may.write ? (
                        <Button icon="add" onClick={() => setDeriving(true)}>
                            {t('refdata.calendars.derive')}
                        </Button>
                    ) : undefined
                }
                onEdit={may.write && editable ? () => setEditing(true) : undefined}
                onDelete={may.remove && editable ? () => setRemoving(true) : undefined}
            />
            {bar}
            {tab === 'details' && (
                <div className="space-y-4">
                    <RecordDetails
                        specs={CALENDAR_FIELDS}
                        row={calendar}
                        extra={[
                            [
                                t('refdata.calendars.made'),
                                base === undefined ? (
                                    <MadeOf calendar={calendar} />
                                ) : (
                                    <Link
                                        className="text-accent hover:underline"
                                        to={calendarPath(show(base['code']))}
                                    >
                                        <MadeOf calendar={calendar} />
                                    </Link>
                                ),
                            ],
                        ]}
                    />
                    {shape === 'quantlib' && (
                        <Notice tone="info">{t('refdata.calendars.quantlibNote')}</Notice>
                    )}
                </div>
            )}
            {tab === 'rules' && (
                <PartsPanel
                    resource={RULES}
                    titles={{
                        add: t('refdata.calendars.addPart.calendar-rules'),
                        one: t('refdata.calendars.editPart.calendar-rules'),
                        remove: t('refdata.calendars.removePart.calendar-rules'),
                    }}
                    entityType="ores.refdata.calendar_rule"
                    specs={RULE_FIELDS}
                    parentField="calendar_code"
                    parent={code}
                    rows={rules.data ?? []}
                    error={rules.error?.message}
                    initial={{ kind: 'fixed_date', shift: 'none' }}
                    columns={[
                        {
                            header: t('refdata.calendars.rule'),
                            cell: (row) => <RuleWords rule={row} />,
                        },
                        {
                            header: t('refdata.fields.shift'),
                            cell: (row) => t(`refdata.choices.shift.${show(row['shift'])}`),
                        },
                        {
                            header: t('refdata.calendars.years'),
                            cell: (row) => years(row),
                        },
                    ]}
                    sort={(row) =>
                        `${show(row['month']).padStart(2, '0')}${show(row['day']).padStart(2, '0')}`
                    }
                />
            )}
            {tab === 'exceptions' && (
                <PartsPanel
                    resource={EXCEPTIONS}
                    titles={{
                        add: t('refdata.calendars.addPart.calendar-exceptions'),
                        one: t('refdata.calendars.editPart.calendar-exceptions'),
                        remove: t('refdata.calendars.removePart.calendar-exceptions'),
                    }}
                    entityType="ores.refdata.calendar_exception"
                    specs={EXCEPTION_FIELDS}
                    parentField="calendar_code"
                    parent={code}
                    rows={exceptions.data ?? []}
                    error={exceptions.error?.message}
                    initial={{ is_business_day: 'false' }}
                    lead={t('refdata.calendars.exceptionsLead')}
                    columns={[
                        {
                            header: t('refdata.fields.exception_date'),
                            cell: (row) => show(row['exception_date']),
                            mono: true,
                        },
                        {
                            header: t('refdata.calendars.effect'),
                            cell: (row) =>
                                row['is_business_day'] === true
                                    ? t('refdata.calendars.businessDay')
                                    : t('refdata.calendars.holiday'),
                        },
                        {
                            header: t('refdata.fields.description'),
                            cell: (row) => show(row['description']),
                        },
                    ]}
                    sort={(row) => show(row['exception_date'])}
                />
            )}
            {tab === 'events' && (
                <PartsPanel
                    resource={EVENTS}
                    titles={{
                        add: t('refdata.calendars.addPart.calendar-events'),
                        one: t('refdata.calendars.editPart.calendar-events'),
                        remove: t('refdata.calendars.removePart.calendar-events'),
                    }}
                    entityType="ores.refdata.calendar_event"
                    specs={EVENT_FIELDS}
                    parentField="calendar_code"
                    parent={code}
                    rows={events.data ?? []}
                    error={events.error?.message}
                    keep={['source']}
                    lead={t('refdata.calendars.eventsLead')}
                    columns={[
                        {
                            header: t('refdata.fields.event_date'),
                            cell: (row) => show(row['event_date']),
                            mono: true,
                        },
                        {
                            header: t('refdata.fields.diary_entry_type'),
                            cell: (row) => (
                                <ClassifiedValue
                                    list="diary-entry-type"
                                    code={show(row['diary_entry_type'])}
                                />
                            ),
                        },
                        { header: t('refdata.fields.name'), cell: (row) => show(row['name']) },
                    ]}
                    sort={(row) => show(row['event_date'])}
                />
            )}
            {tab === 'days' && (
                <DaysPanel calendar={calendar} base={base} exceptions={exceptions.data ?? []} />
            )}
            {tab === 'history' && (
                <HistoryPanel
                    entityType="ores.refdata.calendar"
                    entityId={code}
                    {...(may.write && editable ? { onRevert: setReverting } : {})}
                />
            )}
            {editing && (
                <RecordDialog
                    title={t('refdata.records.editTitle', { code })}
                    resource={CALENDARS}
                    specs={CALENDAR_FIELDS}
                    row={calendar}
                    keep={CALENDAR_KEPT}
                    onClose={() => setEditing(false)}
                />
            )}
            {deriving && (
                <RecordDialog
                    title={t('refdata.calendars.deriveTitle', { base: code })}
                    resource={CALENDARS}
                    specs={CALENDAR_FIELDS}
                    row={undefined}
                    given={userCalendar(code)}
                    initial={{
                        calendar_type: show(calendar['calendar_type']),
                        country_code: show(calendar['country_code']),
                    }}
                    onClose={() => setDeriving(false)}
                    onSaved={(write) => void navigate(calendarPath(show(write['code'])))}
                />
            )}
            {removing && (
                <RemoveRecordDialog
                    resource={CALENDARS}
                    title={t('refdata.records.deleteTitle', { code })}
                    warning={t('refdata.calendars.removeWarning')}
                    recordKey={{ code }}
                    version={calendar.version}
                    before={async (intent) => {
                        const parts: (readonly [string, readonly RecordRow[]])[] = [
                            [RULES, rules.data ?? []],
                            [EXCEPTIONS, exceptions.data ?? []],
                            [EVENTS, events.data ?? []],
                        ];
                        for (const [resource, rows] of parts) {
                            for (const row of rows) {
                                await api.removeRecord(resource, {
                                    key: { id: show(row['id']) },
                                    ...intent,
                                });
                            }
                        }
                    }}
                    onClose={() => setRemoving(false)}
                    onRemoved={() => void navigate(calendarPath())}
                />
            )}
            {reverting !== null && (
                <RevertDialog
                    resource={CALENDARS}
                    specs={CALENDAR_FIELDS}
                    row={calendar}
                    version={reverting}
                    keep={CALENDAR_KEPT}
                    onClose={() => setReverting(null)}
                />
            )}
        </div>
    );
}

/** The years a rule applies in, open at either end. */
function years(rule: RecordRow): string {
    const from = show(rule['effective_from']);
    const to = show(rule['effective_to']);
    if (from === '' && to === '') {
        return '—';
    }
    return `${from === '' ? '…' : from} – ${to === '' ? '…' : to}`;
}

/** A rule in words, such as "3rd Monday of January"; a rule has no name of its own. */
function RuleWords({ rule }: { readonly rule: RecordRow }): ReactNode {
    const { t, language } = useTranslation();
    const month = Number(rule['month']);
    const monthName =
        Number.isInteger(month) && month >= 1
            ? new Intl.DateTimeFormat(language, { month: 'long', timeZone: 'UTC' }).format(
                  new Date(Date.UTC(2001, month - 1, 1)),
              )
            : '';
    const weekday = t(`refdata.choices.weekday.${show(rule['weekday'])}`);
    switch (rule['kind']) {
        case 'fixed_date':
            return (
                <>
                    {t('refdata.calendars.rules.fixed', {
                        day: show(rule['day']),
                        month: monthName,
                    })}
                </>
            );
        case 'nth_weekday_of_month':
            return (
                <>
                    {t('refdata.calendars.rules.nth', {
                        occurrence: t(`refdata.choices.occurrence.${show(rule['occurrence'])}`),
                        weekday,
                        month: monthName,
                    })}
                </>
            );
        case 'last_weekday_of_month':
            return <>{t('refdata.calendars.rules.last', { weekday, month: monthName })}</>;
        default:
            const offset = Number(rule['day_offset']);
            return (
                <>
                    {t('refdata.calendars.rules.easter', {
                        offset: offset > 0 ? `+${String(offset)}` : String(offset),
                    })}
                </>
            );
    }
}

const ENTRY_STYLE = {
    here: '',
    other: 'text-ink-muted italic',
    unbuilt: 'text-warn',
} as const;

function isWeekend(date: string): boolean {
    const day = new Date(`${date}T00:00:00Z`).getUTCDay();
    return day === 0 || day === 6;
}

/** A day that differs from the plain weekday rule: a weekday holiday, or a weekend business day. */
function isNotable(day: CalendarDay): boolean {
    return day.businessDay === isWeekend(day.date);
}

/**
 * The business days a calendar produces in one year, one month per block, and
 * the rebuild. A second calendar can be laid beside it: the union of the two
 * is the business-day definition a pair settles on.
 */
function DaysPanel({
    calendar,
    base,
    exceptions,
}: {
    readonly calendar: RecordRow;
    readonly base: RecordRow | undefined;
    readonly exceptions: readonly RecordRow[];
}): ReactNode {
    const { t, language } = useTranslation();
    const flags = useFlags();
    const calendars = useRecords(CALENDARS);
    const code = show(calendar['code']);
    const [year, setYear] = useState(new Date().getUTCFullYear());
    const [other, setOther] = useState('');
    const days = useQuery({
        queryKey: ['calendar-days', code, year],
        queryFn: () => api.calendarDays(code, year),
    });
    const otherDays = useQuery({
        queryKey: ['calendar-days', other, year],
        queryFn: () => api.calendarDays(other, year),
        enabled: other !== '',
    });
    const byDate = new Map(exceptions.map((row) => [show(row['exception_date']), row]));
    const otherHolidays = new Set(
        (otherDays.data ?? [])
            .filter((day) => !day.businessDay && !isWeekend(day.date))
            .map((day) => day.date),
    );
    const notable = (days.data ?? []).filter(isNotable);
    const holidays = new Set(notable.filter((day) => !day.businessDay).map((day) => day.date));
    const onlyOther = [...otherHolidays].filter((date) => !holidays.has(date));
    const businessDays = (days.data ?? []).filter(
        (day) => day.businessDay && !otherHolidays.has(day.date),
    ).length;
    const monthName = (month: number): string =>
        new Intl.DateTimeFormat(language, { month: 'long', timeZone: 'UTC' }).format(
            new Date(Date.UTC(2001, month, 1)),
        );
    const cause = (day: CalendarDay): string => {
        const exception = byDate.get(day.date);
        if (exception !== undefined) {
            return show(exception['description']) || t('refdata.calendars.cause.exception');
        }
        return base === undefined
            ? t('refdata.calendars.cause.rule')
            : t('refdata.calendars.cause.base', { base: show(base['code']) });
    };
    const built = new Map((days.data ?? []).map((day) => [day.date, day.businessDay]));
    const unbuilt = exceptions.filter((row) => {
        const businessDay = built.get(show(row['exception_date']));
        return businessDay !== undefined && businessDay !== row['is_business_day'];
    });
    const entries: readonly {
        readonly date: string;
        readonly words: string;
        readonly from: 'here' | 'other' | 'unbuilt';
    }[] = [
        ...notable.map((day) => ({
            date: day.date,
            words: day.businessDay ? t('refdata.calendars.businessDay') : cause(day),
            from: 'here' as const,
        })),
        ...onlyOther.map((date) => ({
            date,
            words: t('refdata.calendars.cause.other', { other }),
            from: 'other' as const,
        })),
        ...unbuilt.map((row) => ({
            date: show(row['exception_date']),
            words: t('refdata.calendars.cause.unbuilt', {
                words: show(row['description']) || t('refdata.calendars.cause.exception'),
            }),
            from: 'unbuilt' as const,
        })),
    ].sort((a, b) => a.date.localeCompare(b.date));

    return (
        <section className="space-y-4">
            <div className="flex flex-wrap items-end gap-3">
                <div className="flex items-center gap-1">
                    <Button
                        size="sm"
                        variant="ghost"
                        aria-label={t('refdata.calendars.previousYear')}
                        onClick={() => setYear(year - 1)}
                    >
                        ‹
                    </Button>
                    <span className="w-14 text-center text-sm font-medium">{year}</span>
                    <Button
                        size="sm"
                        variant="ghost"
                        aria-label={t('refdata.calendars.nextYear')}
                        onClick={() => setYear(year + 1)}
                    >
                        ›
                    </Button>
                </div>
                <Field label={t('refdata.calendars.compare')}>
                    <div className="w-72">
                        <RecordPicker
                            value={other}
                            optional
                            choices={(calendars.data ?? [])
                                .filter((candidate) => candidate['code'] !== code)
                                .map((candidate) => ({
                                    value: show(candidate['code']),
                                    label: show(candidate['name']),
                                    image: flags.flag('calendar', show(candidate['code'])),
                                }))}
                            onChange={setOther}
                            placeholder="—"
                            label={t('refdata.calendars.compare')}
                        />
                    </div>
                </Field>
            </div>
            {base !== undefined && shapeOf(base) === 'quantlib' && (
                <Notice tone="warn">
                    {t('refdata.calendars.quantlibBaseWarning', { base: show(base['code']) })}
                </Notice>
            )}
            {days.isError && <Notice tone="error">{days.error.message}</Notice>}
            {days.isSuccess && days.data.length === 0 ? (
                <Notice tone="info">
                    {t('refdata.calendars.notBuilt', { year: String(year) })}
                </Notice>
            ) : (
                days.isSuccess && (
                    <>
                        <p className="text-sm text-ink-muted">
                            {t('refdata.calendars.summary', {
                                business: String(businessDays),
                                holidays: String(holidays.size + onlyOther.length),
                                year: String(year),
                            })}
                        </p>
                        <div className="grid gap-3 sm:grid-cols-2 lg:grid-cols-3">
                            {Array.from({ length: 12 }, (_, month) => {
                                const inMonth = entries.filter(
                                    (entry) => Number(entry.date.slice(5, 7)) === month + 1,
                                );
                                return (
                                    <div key={month} className="rounded-md border border-line p-3">
                                        <h4 className="text-sm font-medium capitalize">
                                            {monthName(month)}
                                        </h4>
                                        {inMonth.length === 0 ? (
                                            <p className="mt-1 text-xs text-ink-faint">
                                                {t('refdata.calendars.noHolidays')}
                                            </p>
                                        ) : (
                                            <ul className="mt-1 space-y-0.5">
                                                {inMonth.map((entry) => (
                                                    <li
                                                        key={`${entry.date}-${entry.from}`}
                                                        className="flex gap-2 text-xs"
                                                    >
                                                        <span className="font-mono text-ink-muted">
                                                            {entry.date.slice(8)}
                                                        </span>
                                                        <span className={ENTRY_STYLE[entry.from]}>
                                                            {entry.words}
                                                        </span>
                                                    </li>
                                                ))}
                                            </ul>
                                        )}
                                    </div>
                                );
                            })}
                        </div>
                    </>
                )
            )}
            <RebuildForm calendar={code} year={year} />
        </section>
    );
}

/** Builds the calendar's business days up to a year. */
function RebuildForm({
    calendar,
    year,
}: {
    readonly calendar: string;
    readonly year: number;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const may = useRecordPermissions(CALENDARS);
    const [endYear, setEndYear] = useState(String(Math.max(year, new Date().getUTCFullYear() + 1)));
    const valid = /^[0-9]{4}$/.test(endYear);
    const rebuild = useMutation({
        mutationFn: () => api.rebuildCalendar(calendar, Number(endYear)),
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['calendar-days', calendar] });
        },
    });
    if (!may.write) {
        return null;
    }
    return (
        <div className="space-y-2 rounded-md border border-line p-3">
            <div className="flex flex-wrap items-end gap-2">
                <Field label={t('refdata.calendars.rebuildTo')}>
                    <Input
                        value={endYear}
                        inputMode="numeric"
                        maxLength={4}
                        onChange={(event) => setEndYear(event.target.value)}
                    />
                </Field>
                <Button
                    disabled={!valid}
                    pending={rebuild.isPending}
                    onClick={() => rebuild.mutate()}
                >
                    {t('refdata.calendars.rebuild')}
                </Button>
            </div>
            <p className="text-xs text-ink-faint">{t('refdata.calendars.rebuildNote')}</p>
            {rebuild.isSuccess && (
                <Notice tone="success">
                    {t('refdata.calendars.rebuilt', { count: String(rebuild.data) })}
                </Notice>
            )}
            {rebuild.isError && <Notice tone="error">{rebuild.error.message}</Notice>}
        </div>
    );
}
