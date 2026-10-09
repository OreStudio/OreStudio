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

import { Fragment, type ReactNode } from 'react';
import {
    TIMELINE_PROVENANCE_FIELDS,
    fieldValue,
    type Timeline as Stream,
    type TimelineEvent,
    type TimelineField,
} from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { formatDateTime, parseTimestamp } from '../ui/Time.js';
import { DiffLines } from '../ui/Diff.js';
import { Tag } from '../ui/Primitives.js';

/**
 * One subject's story, drawn as a rail of dated entries, newest first.
 *
 * The stream is not the person's and not the request's: the same component
 * draws either, because an entry is an entry — a row that was written, by
 * somebody, at a moment. The subject's own header is the caller's, because a
 * person has a face and a request has a state, and neither belongs here.
 *
 * The shape is a pull request's conversation: a time down the left, a rail with
 * a dot an entry, and the entry's own words beside it. An entry that changed a
 * field carries the green and red widget; an entry that changed nothing says
 * what happened in the same rail and is drawn quieter, because a sign-in is a
 * fact about the person rather than a field of a record.
 *
 * Deciding which is which needs the entry before it of the same entity, so the
 * entries are paired here rather than by the server: the server states what it
 * knows, and the pairing is a reading of the stream.
 *
 * This component writes nothing and knows nothing about writes. What may be
 * done about an entry is the caller's, through `renderActions`; and where a
 * caller offers nothing for an entry that is a record of something that
 * happened, the entry says so rather than showing a control that cannot work.
 */
export function Timeline({
    timeline,
    renderActions,
    renderHead,
}: {
    readonly timeline: Stream;
    readonly renderActions?: (event: TimelineEvent) => ReactNode;
    readonly renderHead?: (event: TimelineEvent) => ReactNode;
}): ReactNode {
    const { t } = useTranslation();
    if (timeline.events.length === 0) {
        return <p className="text-sm text-ink-muted">{t('timeline.empty')}</p>;
    }
    const previous = previousVersions(timeline.events);
    return (
        <ol className="grid list-none gap-0 p-0">
            {timeline.events.map((event, index) => (
                <Fragment key={entryKey(event, index)}>
                    {startsADay(event, timeline.events[index - 1]) && <DayDivider at={event.at} />}
                    <Entry
                        event={event}
                        before={previous.get(earlier(event))}
                        actions={renderActions?.(event)}
                        head={renderHead?.(event)}
                    />
                </Fragment>
            ))}
        </ol>
    );
}

/** What names one entry in the stream: no two entries come from one version twice. */
function entryKey(event: TimelineEvent, index: number): string {
    return `${event.entityType}#${event.entityId}#${String(event.version)}#${String(index)}`;
}

/** The key of the version one before this one, which is what an entry is read against. */
function earlier(event: TimelineEvent): string {
    return `${event.entityType}#${event.entityId}#${String(event.version - 1)}`;
}

/** Every entry by the version it states, so an entry can find the one before it. */
function previousVersions(events: readonly TimelineEvent[]): ReadonlyMap<string, TimelineEvent> {
    const byVersion = new Map<string, TimelineEvent>();
    for (const event of events) {
        byVersion.set(`${event.entityType}#${event.entityId}#${String(event.version)}`, event);
    }
    return byVersion;
}

/** The day an entry belongs to, as the reader's own calendar states it. */
function dayOf(at: string): string {
    const date = parseTimestamp(at);
    if (Number.isNaN(date.getTime())) {
        return at;
    }
    return `${String(date.getFullYear())}-${String(date.getMonth())}-${String(date.getDate())}`;
}

/** Whether an entry is the first of its day, which is where a divider belongs. */
function startsADay(event: TimelineEvent, before: TimelineEvent | undefined): boolean {
    return before === undefined || dayOf(before.at) !== dayOf(event.at);
}

/** The column the rail sits in, which every row of the stream shares. */
const ROW = 'grid grid-cols-[5.6rem_1.6rem_1fr]';

function DayDivider({ at }: { readonly at: string }): ReactNode {
    const { language } = useTranslation();
    const date = parseTimestamp(at);
    const label = Number.isNaN(date.getTime())
        ? at
        : new Intl.DateTimeFormat(language, { dateStyle: 'full' }).format(date);
    return (
        <li className={`${ROW} mt-1.5`}>
            <span className="col-start-2 col-end-4 border-b border-line-subtle px-0 pb-1 pl-1.5 pt-0.5 text-[0.72rem] tracking-[0.06em] text-ink-faint uppercase">
                {label}
            </span>
        </li>
    );
}

/** The time of day, which is what the rail's gutter carries. */
function timeOf(at: string, language: string): string {
    const date = parseTimestamp(at);
    if (Number.isNaN(date.getTime())) {
        return '';
    }
    return new Intl.DateTimeFormat(language, {
        hour: '2-digit',
        minute: '2-digit',
        second: '2-digit',
        hour12: false,
    }).format(date);
}

/** The entity an entry came from, as a reader names it: the last part of its type. */
function entityOf(event: TimelineEvent): string {
    const parts = event.entityType.split('.');
    return parts[parts.length - 1] ?? event.entityType;
}

/** The short form of a row, because a uuid says nothing a reader can use. */
function shortId(id: string): string {
    return id.length > 8 ? id.slice(0, 8) : id;
}

/**
 * What an entry is, for the colour of its dot and its badge.
 *
 * A change is the accent, a grant is the one that succeeds, an answer or a
 * closing is the one to look at, and a fact about the subject is faint. A
 * subject that draws its own header tints from here too, so one act does not
 * read as two colours on two screens.
 */
export function kindTone(kind: string): 'accent' | 'up' | 'warn' | 'muted' | 'neutral' {
    if (kind === 'granted') return 'up';
    if (kind === 'decided') return 'warn';
    if (kind === 'changed' || kind === 'raised' || kind === 'asked') return 'accent';
    return 'muted';
}

/** The dot's own colours, which carry a border as well as a fill. */
const DOT: Readonly<Record<string, string>> = {
    accent: 'border-accent bg-accent/15',
    up: 'border-up bg-up/15',
    warn: 'border-warn bg-warn/15',
    muted: 'border-line bg-surface-overlay',
    neutral: 'border-line bg-surface-overlay',
};

/** Whether an entry is a record of something that happened rather than of a row written. */
function isAnAct(kind: string): boolean {
    return !['raised', 'changed', 'asked', 'decided', 'granted'].includes(kind);
}

/**
 * Whether an entry is a field of a record that moved.
 *
 * A change is drawn with the difference against the version before it, and on a
 * stream whose subject offers no write it is drawn with the reason there is no
 * control, rather than silently.
 */
function isAChange(kind: string): boolean {
    return kind === 'changed';
}

function Entry({
    event,
    before,
    actions,
    head,
}: {
    readonly event: TimelineEvent;
    readonly before: TimelineEvent | undefined;
    readonly actions: ReactNode;
    readonly head: ReactNode;
}): ReactNode {
    const { t, language } = useTranslation();
    const changed = changedFields(event, before);
    const quiet = isAnAct(event.kind);
    return (
        <li className={ROW}>
            <div className="pr-2.5 pt-[9px] text-right font-mono text-[0.72rem] text-ink-faint">
                {timeOf(event.at, language)}
            </div>
            <div className="relative flex justify-center">
                <span className="absolute inset-y-0 w-px bg-line" aria-hidden />
                <span
                    className={`relative z-10 mt-[9px] h-[9px] w-[9px] rounded-full border ${DOT[kindTone(event.kind)] ?? DOT['neutral']}`}
                    aria-hidden
                />
            </div>
            <div className={`grid min-w-0 gap-1.5 pb-3.5 pl-1.5 pt-1 ${quiet ? 'opacity-90' : ''}`}>
                {head ?? <DefaultHead event={event} changed={changed} />}
                {event.commentary !== '' && (
                    <p className="text-[0.8rem] text-ink-muted italic">{event.commentary}</p>
                )}
                {changed.length > 0 && before !== undefined ? (
                    <ChangeTable changed={changed} before={before} />
                ) : (
                    <Facts event={event} before={before} />
                )}
                {actions}
                {actions === undefined && isAnAct(event.kind) && (
                    <p className="text-[0.78rem] text-ink-faint">{t('timeline.notRevertible')}</p>
                )}
                {actions === undefined && isAChange(event.kind) && (
                    <p className="text-[0.78rem] text-ink-faint">
                        {t('timeline.changeNotOffered')}
                    </p>
                )}
            </div>
        </li>
    );
}

/**
 * The header a stream draws when its subject brings none.
 *
 * A subject that names its own entries draws its own, because a request has a
 * kind and a person has a face and neither belongs in the other's header. What
 * stays here is what every entry has: what it was, the row it came from, who
 * wrote it, and the sentence that says what they did.
 */
function DefaultHead({
    event,
    changed,
}: {
    readonly event: TimelineEvent;
    readonly changed: readonly TimelineField[];
}): ReactNode {
    const { t, language } = useTranslation();
    const badge = [entityOf(event), event.entityId === '' ? '' : shortId(event.entityId)]
        .filter((part) => part !== '')
        .join(' ');
    return (
        <div className="flex flex-wrap items-baseline gap-2">
            <Tag tone={kindTone(event.kind)}>{t(`timeline.kind.${event.kind}`)}</Tag>
            <span className="font-mono text-[0.72rem] text-ink-faint">
                {badge}
                {event.version > 0 && ` v${String(event.version)}`}
            </span>
            {event.reasonCode !== '' && (
                <span className="rounded border border-warn/40 bg-warn/10 px-1.5 font-mono text-[0.68rem] text-warn">
                    {event.reasonCode}
                </span>
            )}
            {event.actor !== '' && (
                <span className="text-sm font-semibold text-ink">{event.actor}</span>
            )}
            <span className="text-sm text-ink-muted">{sentenceOf(event, changed)}</span>
            <span className="ml-auto text-[0.72rem] text-ink-faint">
                {formatDateTime(event.at, language)}
            </span>
        </div>
    );
}

/**
 * What the entry did, in one phrase.
 *
 * A change names the fields that moved, because that is the one thing a reader
 * scanning the rail wants and the diff table below says it again in full. Every
 * other entry is its own kind's sentence.
 */
function sentenceOf(event: TimelineEvent, changed: readonly TimelineField[]): string {
    if (changed.length > 0) {
        return changed.map((field) => field.name).join(', ');
    }
    if (event.kind === 'granted' || event.kind === 'asked') {
        return fieldValue(event.fields, 'Role');
    }
    if (event.kind === 'raised') {
        return '';
    }
    return fieldValue(event.fields, 'Detail') || fieldValue(event.fields, 'Event');
}

/** The fields an entry changed from the version before it, in the order the record holds them. */
function changedFields(
    event: TimelineEvent,
    before: TimelineEvent | undefined,
): readonly TimelineField[] {
    if (before === undefined) {
        return [];
    }
    return event.fields.filter(
        (field) =>
            !TIMELINE_PROVENANCE_FIELDS.has(field.name) &&
            fieldValue(before.fields, field.name) !== field.value,
    );
}

/** The fields worth drawing for an entry, which are the record's own rather than its provenance. */
function ownFields(event: TimelineEvent): readonly TimelineField[] {
    return event.fields.filter((field) => !TIMELINE_PROVENANCE_FIELDS.has(field.name));
}

/**
 * A field that changed, from the value before to the value after.
 *
 * The widget is the product's own, so a reader who has read the history of a
 * refdata record reads this the same way.
 */
function ChangeTable({
    changed,
    before,
}: {
    readonly changed: readonly TimelineField[];
    readonly before: TimelineEvent;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <table className="w-full border-collapse text-left text-[0.84rem]">
            <thead>
                <tr>
                    <th className="border-b border-line-subtle px-2 py-[3px] text-[0.68rem] font-semibold tracking-[0.06em] text-ink-faint uppercase">
                        {t('timeline.field')}
                    </th>
                    <th className="border-b border-line-subtle px-2 py-[3px] text-[0.68rem] font-semibold tracking-[0.06em] text-ink-faint uppercase">
                        {t('history.valueDiff')}
                    </th>
                </tr>
            </thead>
            <tbody>
                {changed.map((field) => (
                    <tr key={field.name} className="align-top">
                        <td className="border-b border-line-subtle py-[3px] pl-2.5 pr-2 shadow-[inset_3px_0_0] shadow-warn">
                            {field.name}
                        </td>
                        <td className="border-b border-line-subtle px-2 py-[3px]">
                            <DiffLines
                                before={fieldValue(before.fields, field.name)}
                                after={field.value}
                            />
                        </td>
                    </tr>
                ))}
            </tbody>
        </table>
    );
}

/** What an entry says when there is no field difference to draw. */
function Facts({
    event,
    before,
}: {
    readonly event: TimelineEvent;
    readonly before: TimelineEvent | undefined;
}): ReactNode {
    const { t } = useTranslation();
    const fields = ownFields(event);
    if (fields.length === 0) {
        return null;
    }
    return (
        <div className="ml-0.5 grid gap-0.5 border-l-2 border-line-subtle pl-2.5 text-[0.8rem]">
            {before === undefined && (
                <span className="text-[0.72rem] text-ink-faint">{t('history.initial')}</span>
            )}
            <dl className="grid gap-0.5">
                {fields.map((field) => (
                    <div key={field.name} className="flex flex-wrap gap-2">
                        <dt className="min-w-[15rem] font-mono text-[0.72rem] text-ink-faint">
                            {field.name}
                        </dt>
                        <dd className="text-ink-muted">{field.value}</dd>
                    </div>
                ))}
            </dl>
        </div>
    );
}
