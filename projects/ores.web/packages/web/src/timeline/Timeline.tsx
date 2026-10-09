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
    type TimelineGap,
} from '@ores/wire-protocol/browser';
import { useTranslation } from '../i18n/Provider.js';
import { formatDateTime, parseTimestamp } from '../ui/Time.js';
import { DiffLines } from '../ui/Diff.js';
import { Tag } from '../ui/Primitives.js';

/**
 * One subject's story, drawn as one dated stream, newest first.
 *
 * The stream is not the person's and not the request's: the same component
 * draws either, because an entry is an entry — a row that was written, by
 * somebody, at a moment. The subject's own header is the caller's, because a
 * person has a face and a request has a state, and neither belongs here.
 *
 * An entry that changed a field carries the green and red widget; an entry that
 * changed nothing says what happened in the same stream and is drawn quieter,
 * because a sign-in is a fact about the person rather than a field of a record.
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
        /*
         * An empty stream still carries its gaps. A subject whose every source
         * refused has nothing to draw and everything to explain, and saying
         * only that nothing happened would be the one lie the gaps exist to
         * prevent.
         */
        return (
            <section className="space-y-4">
                <p className="text-sm text-ink-muted">{t('timeline.empty')}</p>
                <Gaps gaps={timeline.gaps} />
            </section>
        );
    }
    const previous = previousVersions(timeline.events);
    return (
        <section className="space-y-4">
            <ol className="list-none space-y-1 p-0">
                {timeline.events.map((event, index) => (
                    <li key={entryKey(event, index)}>
                        {startsADay(event, timeline.events[index - 1]) && (
                            <DayDivider at={event.at} />
                        )}
                        <Entry
                            event={event}
                            before={previous.get(earlier(event))}
                            actions={renderActions?.(event)}
                            head={renderHead?.(event)}
                        />
                    </li>
                ))}
            </ol>
            <Gaps gaps={timeline.gaps} />
        </section>
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

function DayDivider({ at }: { readonly at: string }): ReactNode {
    const { language } = useTranslation();
    const date = parseTimestamp(at);
    const label = Number.isNaN(date.getTime())
        ? at
        : new Intl.DateTimeFormat(language, { dateStyle: 'full' }).format(date);
    return (
        <div className="sticky top-0 z-10 mb-2 mt-4 bg-bg-primary/95 py-1 first:mt-0">
            <span className="text-xs font-medium tracking-wide text-ink-faint uppercase">
                {label}
            </span>
        </div>
    );
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

/** The dot's colour, which has no neutral: a dot that is there is a dot. */
function toneOf(kind: string): 'accent' | 'up' | 'warn' | 'muted' {
    const tone = kindTone(kind);
    return tone === 'neutral' ? 'muted' : tone;
}

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
    return (
        <div className="grid grid-cols-[0.75rem_1fr] gap-3">
            <Dot tone={toneOf(event.kind)} />
            <article className="mb-2 rounded-[var(--radius-card)] border border-line bg-surface-raised px-3 py-2">
                {head ?? (
                    <DefaultHead event={event} language={language} />
                )}
                {event.commentary !== '' && (
                    <p className="mt-1 text-sm text-ink-muted italic">{event.commentary}</p>
                )}
                {changed.length > 0 && before !== undefined ? (
                    <ChangeTable changed={changed} before={before} />
                ) : (
                    <Facts event={event} before={before} />
                )}
                {actions}
                {actions === undefined && isAnAct(event.kind) && (
                    <p className="mt-2 text-[11px] text-ink-faint">
                        {t('timeline.notRevertible')}
                    </p>
                )}
                {actions === undefined && isAChange(event.kind) && (
                    <p className="mt-2 text-[11px] text-ink-faint">
                        {t('timeline.changeNotOffered')}
                    </p>
                )}
            </article>
        </div>
    );
}

/**
 * The header a stream draws when its subject brings none.
 *
 * A subject that names its own entries draws its own, because a request has a
 * kind and a person has a face and neither belongs in the other's header. What
 * stays here is what every entry has: the row it came from, when, who wrote it,
 * and the reason they gave.
 */
function DefaultHead({
    event,
    language,
}: {
    readonly event: TimelineEvent;
    readonly language: string;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <header className="flex flex-wrap items-center gap-2">
            <Tag tone={toneOf(event.kind)}>
                <span className="font-mono">{entityOf(event)}</span>
                {event.entityId !== '' && (
                    <span className="ml-1 text-ink-faint">
                        {shortId(event.entityId)}
                        {event.version > 0 && ` v${String(event.version)}`}
                    </span>
                )}
            </Tag>
            <span className="font-mono text-[11px] text-ink-faint">
                {formatDateTime(event.at, language)}
            </span>
            {event.actor !== '' && (
                <span className="text-xs text-ink-muted">
                    {t('timeline.by', { who: event.actor })}
                </span>
            )}
            {event.reasonCode !== '' && (
                <span className="rounded border border-warn/40 bg-warn/10 px-1.5 font-mono text-[11px] text-warn">
                    {event.reasonCode}
                </span>
            )}
        </header>
    );
}

function Dot({ tone }: { readonly tone: 'accent' | 'up' | 'warn' | 'muted' }): ReactNode {
    const tones = {
        accent: 'bg-accent',
        up: 'bg-up',
        warn: 'bg-warn',
        muted: 'bg-line-strong',
    } as const;
    return (
        <div className="flex justify-center pt-2">
            <span className={`mt-1 block h-2.5 w-2.5 rounded-full ${tones[tone]}`} />
        </div>
    );
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
        <table className="mt-2 w-full text-left text-sm">
            <thead>
                <tr className="text-xs text-ink-faint">
                    <th className="w-48 py-1 font-normal">{t('timeline.field')}</th>
                    <th className="py-1 font-normal">{t('history.valueDiff')}</th>
                </tr>
            </thead>
            <tbody>
                {changed.map((field) => (
                    <tr key={field.name} className="border-t border-line-subtle align-top">
                        <td className="py-1 pr-3 text-ink-muted">{field.name}</td>
                        <td className="py-1">
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
        <dl className="mt-2 grid grid-cols-[12rem_1fr] gap-x-3 gap-y-0.5 text-sm">
            {before === undefined && (
                <div className="col-span-2 text-xs text-ink-faint">{t('history.initial')}</div>
            )}
            {fields.map((field) => (
                <Fragment key={field.name}>
                    <dt className="text-ink-faint">{field.name}</dt>
                    <dd className="break-words text-ink-muted">{field.value}</dd>
                </Fragment>
            ))}
        </dl>
    );
}

/**
 * What the stream cannot show, and why.
 *
 * A story that leaves out a chapter without saying so reads as though the
 * chapter never happened, so the omission is drawn as part of the answer.
 */
function Gaps({ gaps }: { readonly gaps: readonly TimelineGap[] }): ReactNode {
    const { t } = useTranslation();
    if (gaps.length === 0) {
        return null;
    }
    return (
        <section className="rounded-[var(--radius-card)] border border-line-subtle bg-surface-overlay/40 px-3 py-2">
            <h3 className="text-xs font-medium tracking-wide text-ink-faint uppercase">
                {t('timeline.gapsTitle')}
            </h3>
            <ul className="mt-1 list-none space-y-0.5 p-0 text-xs text-ink-muted">
                {gaps.map((gap) => (
                    <li key={`${gap.entity}:${gap.reason}`}>
                        <span className="font-mono text-ink-faint">{gap.entity}</span> — {gap.reason}
                    </li>
                ))}
            </ul>
        </section>
    );
}
