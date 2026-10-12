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
 * Watch a curve or surface evolve, from
 * doc/knowledge/journeys/market_data/journey_watch-a-curve-or-surface-evolve.org.
 *
 * The person names an object by its typed identity and a range of days. The
 * screen reads the aligned matrix of instants by nodes, and the object as it
 * stands now, and draws both. A node no point states at an instant is drawn as
 * a hole and never as zero. The matrix states how each node changed over the
 * range. The "now" table states how old each value is and, when asked, where it
 * came from.
 *
 * A person who may write observations can also adjust a node by hand. A manual value
 * owns its node until it is cleared, and the "now" table marks each manual node.
 *
 * An oresmd URL in the address, as `uri`, names the series to open: the screen
 * reads it through the server's parse, fills the identity from it and reads the
 * object, so a link to a URL is a link to its series.
 */

import { Fragment, useEffect, useRef, useState, type FormEvent, type ReactNode } from 'react';
import { Link, useSearchParams } from 'react-router';
import { useMutation, useQuery } from '@tanstack/react-query';
import type {
    CurveEvolution,
    CurveIdentity,
    CurveSeriesList,
    CurveSnapshot,
} from '@ores/contracts';
import { PERMISSION } from '../access/permissions.js';
import { useHolds } from '../access/holds.js';
import { curves, type CurveRange } from '../api/curves.js';
import { useTranslation } from '../i18n/Provider.js';
import { ReasonFields, useReason } from '../refdata/shared.js';
import { seriesOfUrl, seriesRoute, seriesVerdict } from '../oresmd/identity.js';
import { oresmdQueries } from '../oresmd/queries.js';
import { titleOrRaw } from '../oresmd/title.js';
import { useSeriesTitle } from '../oresmd/useSeriesTitle.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { Button, Field, Input, Notice, PageHeader, Select, Tag } from '../ui/Primitives.js';
import {
    canBeShown,
    formatAge,
    formFrom,
    identityOf,
    matrixOf,
    neverQuoted,
    nowRowsOf,
    rangeOf,
    type IdentityForm,
} from './evolutionView.js';

/** The days the range opens on: the last thirty. */
const DEFAULT_SPAN_DAYS = 30;

const STATUSES = ['persistent', 'added', 'dropped', 'intermittent', 'never_quoted'] as const;
type Status = (typeof STATUSES)[number];

const SOURCES = ['quoted', 'derived', 'manual'] as const;

const STATUS_TONE: Readonly<Record<Status, 'neutral' | 'up' | 'down' | 'warn' | 'muted'>> = {
    persistent: 'neutral',
    added: 'up',
    dropped: 'down',
    intermittent: 'warn',
    never_quoted: 'muted',
};

function isStatus(value: string): value is Status {
    return (STATUSES as readonly string[]).includes(value);
}

function day(offsetDays: number): string {
    return new Date(Date.now() - offsetDays * 86_400_000).toISOString().slice(0, 10);
}

/** What the person asked for, once the form passed its checks. */
interface Asked {
    readonly identity: CurveIdentity;
    readonly range: CurveRange;
    readonly provenance: boolean;
}

/** The instant of a column, to the minute. */
function columnLabel(asOf: string): string {
    return asOf.slice(0, 16);
}

export function MatrixPanel({ evolution }: { readonly evolution: CurveEvolution }): ReactNode {
    const { t } = useTranslation();
    const matrix = matrixOf(evolution);
    const never = neverQuoted(matrix);
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('market.evolution.matrix.title')}</h2>
                {never > 0 && (
                    <span className="text-xs text-ink-faint">
                        {t('market.evolution.matrix.neverQuoted', { count: never })}
                    </span>
                )}
            </header>
            {matrix.rows.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('market.evolution.matrix.noNodes')}</p>
            ) : matrix.columns.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('market.evolution.matrix.noInstants')}</p>
            ) : (
                <div className="overflow-x-auto">
                    <table className="w-full text-sm">
                        <thead className="text-left text-xs text-ink-faint">
                            <tr>
                                <th className="py-1 pr-4 font-normal">
                                    {t('market.evolution.matrix.node')}
                                </th>
                                <th className="py-1 pr-4 font-normal">
                                    {t('market.evolution.matrix.status')}
                                </th>
                                {matrix.columns.map((column) => (
                                    <th key={column} className="py-1 pr-4 font-normal">
                                        {columnLabel(column)}
                                    </th>
                                ))}
                            </tr>
                        </thead>
                        <tbody className="divide-y divide-line-subtle">
                            {matrix.rows.map((row, index) => (
                                <tr key={`${index}-${row.label}`}>
                                    <td className="py-2 pr-4 font-mono">{row.label}</td>
                                    <td className="py-2 pr-4">
                                        <Tag
                                            tone={
                                                isStatus(row.status)
                                                    ? STATUS_TONE[row.status]
                                                    : 'neutral'
                                            }
                                        >
                                            {isStatus(row.status)
                                                ? t(`market.evolution.status.${row.status}`)
                                                : row.status}
                                        </Tag>
                                    </td>
                                    {row.cells.map((cell, index) => (
                                        <td
                                            key={matrix.columns[index]}
                                            className="py-2 pr-4 font-mono"
                                        >
                                            {cell === '' ? (
                                                <span
                                                    className="text-ink-faint"
                                                    title={t('market.evolution.matrix.hole')}
                                                >
                                                    —
                                                </span>
                                            ) : (
                                                cell
                                            )}
                                        </td>
                                    ))}
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            )}
        </section>
    );
}

/** What the "now" table needs to let a person adjust nodes: the object, and what to do after a write. */
export interface AdjustContext {
    readonly identity: CurveIdentity;
    readonly onChanged: () => void;
}

/** Whether text is one finite number, written whole. */
function isNumber(text: string): boolean {
    return text.trim() !== '' && Number.isFinite(Number(text));
}

/** The form that keys a manual value on one node, with the reason it is keyed for. */
function NodeAdjust({
    adjust,
    coordinates,
    label,
    onClose,
}: {
    readonly adjust: AdjustContext;
    readonly coordinates: readonly string[];
    readonly label: string;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const reason = useReason('amend', true);
    const [value, setValue] = useState('');
    const [commentary, setCommentary] = useState('');
    const key = useMutation({
        mutationFn: () =>
            curves.keyPoint(
                adjust.identity,
                coordinates,
                value.trim(),
                reason.code,
                commentary.trim(),
            ),
        onSuccess: () => {
            adjust.onChanged();
            onClose();
        },
    });
    const missing = commentary.trim() === '';
    const valid = isNumber(value) && !missing && reason.code !== '';
    return (
        <form
            className="space-y-3 rounded-md border border-line p-3"
            aria-label={t('market.evolution.now.adjustAdjusting', { node: label })}
            onSubmit={(event) => {
                event.preventDefault();
                if (valid) {
                    key.mutate();
                }
            }}
        >
            <h3 className="text-sm font-medium">
                {t('market.evolution.now.adjustAdjusting', { node: label })}
            </h3>
            <p className="text-xs text-ink-faint">{t('market.evolution.now.adjustManualNote')}</p>
            <Field
                label={t('market.evolution.now.adjustNewValue')}
                {...(value !== '' && !isNumber(value)
                    ? { error: t('market.evolution.now.adjustValueInvalid') }
                    : {})}
            >
                <Input
                    value={value}
                    inputMode="decimal"
                    onChange={(event) => setValue(event.target.value)}
                />
            </Field>
            <ReasonFields
                reason={reason}
                commentary={commentary}
                onCommentary={setCommentary}
                missing={false}
            />
            {key.isError && <Notice tone="error">{key.error.message}</Notice>}
            <div className="flex items-center gap-3">
                <Button
                    type="submit"
                    variant="primary"
                    size="sm"
                    disabled={!valid}
                    pending={key.isPending}
                    pendingLabel={t('market.evolution.now.adjustSaving')}
                >
                    {t('market.evolution.now.adjustSave')}
                </Button>
                <Button type="button" size="sm" variant="ghost" onClick={onClose}>
                    {t('market.evolution.now.adjustCancel')}
                </Button>
            </div>
        </form>
    );
}

/** Clears the manual value on one node, so the feed value shows again. */
function NodeClear({
    adjust,
    coordinates,
}: {
    readonly adjust: AdjustContext;
    readonly coordinates: readonly string[];
}): ReactNode {
    const { t } = useTranslation();
    const clear = useMutation({
        mutationFn: () => curves.clearPoint(adjust.identity, coordinates),
        onSuccess: adjust.onChanged,
    });
    return (
        <>
            <Button
                type="button"
                size="sm"
                variant="danger"
                pending={clear.isPending}
                onClick={() => clear.mutate()}
            >
                {t('market.evolution.now.adjustClear')}
            </Button>
            {clear.isError && <Notice tone="error">{clear.error.message}</Notice>}
        </>
    );
}

export function NowPanel({
    snapshot,
    provenance,
    adjust,
}: {
    readonly snapshot: CurveSnapshot;
    readonly provenance: boolean;
    /** Present only for a person who may write; it adds the adjust and clear controls. */
    readonly adjust?: AdjustContext;
}): ReactNode {
    const { t } = useTranslation();
    const rows = nowRowsOf(snapshot);
    const [editing, setEditing] = useState<number | null>(null);
    return (
        <section className="card space-y-4 p-6">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-lg font-medium">{t('market.evolution.now.title')}</h2>
                <span className="text-xs text-ink-faint">
                    {t('market.evolution.now.summary', {
                        oldest: formatAge(snapshot.oldest_age_seconds),
                        spread: formatAge(snapshot.spread_seconds),
                    })}
                </span>
            </header>
            {snapshot.warning && (
                <Notice tone="warn">
                    {t('market.evolution.now.mixed', {
                        oldest: formatAge(snapshot.oldest_age_seconds),
                        spread: formatAge(snapshot.spread_seconds),
                    })}
                </Notice>
            )}
            <div className="overflow-x-auto">
                <table className="w-full text-sm">
                    <thead className="text-left text-xs text-ink-faint">
                        <tr>
                            <th className="py-1 pr-4 font-normal">
                                {t('market.evolution.matrix.node')}
                            </th>
                            <th className="py-1 pr-4 font-normal">
                                {t('market.evolution.now.value')}
                            </th>
                            <th className="py-1 pr-4 font-normal">
                                {t('market.evolution.now.age')}
                            </th>
                            {provenance && (
                                <>
                                    <th className="py-1 pr-4 font-normal">
                                        {t('market.evolution.now.source')}
                                    </th>
                                    <th className="py-1 pr-4 font-normal">
                                        {t('market.evolution.now.detail')}
                                    </th>
                                </>
                            )}
                            {adjust !== undefined && (
                                <th className="py-1 pr-4 font-normal">
                                    {t('market.evolution.now.adjustActions')}
                                </th>
                            )}
                        </tr>
                    </thead>
                    <tbody className="divide-y divide-line-subtle">
                        {rows.map((row, index) => (
                            <Fragment key={`${index}-${row.label}`}>
                                <tr>
                                    <td className="py-2 pr-4 font-mono">{row.label}</td>
                                    <td className="py-2 pr-4 font-mono">
                                        {row.value === '' ? (
                                            <span
                                                className="text-ink-faint"
                                                title={t('market.evolution.matrix.hole')}
                                            >
                                                —
                                            </span>
                                        ) : (
                                            row.value
                                        )}
                                    </td>
                                    <td className="py-2 pr-4 font-mono">
                                        {row.ageSeconds === undefined
                                            ? ''
                                            : formatAge(row.ageSeconds)}
                                    </td>
                                    {provenance && (
                                        <>
                                            <td className="py-2 pr-4">
                                                {(SOURCES as readonly string[]).includes(
                                                    row.sourceKind,
                                                ) && (
                                                    <Tag
                                                        tone={
                                                            row.sourceKind === 'quoted'
                                                                ? 'neutral'
                                                                : 'accent'
                                                        }
                                                    >
                                                        {t(
                                                            `market.evolution.source.${row.sourceKind}`,
                                                        )}
                                                    </Tag>
                                                )}
                                            </td>
                                            <td className="py-2 pr-4 font-mono text-xs">
                                                {row.detail}
                                            </td>
                                        </>
                                    )}
                                    {adjust !== undefined && (
                                        <td className="py-2 pr-4">
                                            <div className="flex flex-wrap items-center gap-2">
                                                <Button
                                                    type="button"
                                                    size="sm"
                                                    onClick={() => setEditing(index)}
                                                >
                                                    {t('market.evolution.now.adjustOpen')}
                                                </Button>
                                                {row.sourceKind === 'manual' && (
                                                    <NodeClear
                                                        adjust={adjust}
                                                        coordinates={
                                                            snapshot.nodes[index]?.coordinates ?? []
                                                        }
                                                    />
                                                )}
                                            </div>
                                        </td>
                                    )}
                                </tr>
                                {adjust !== undefined && editing === index && (
                                    <tr>
                                        <td colSpan={6} className="pb-3">
                                            <NodeAdjust
                                                adjust={adjust}
                                                coordinates={
                                                    snapshot.nodes[index]?.coordinates ?? []
                                                }
                                                label={row.label}
                                                onClose={() => setEditing(null)}
                                            />
                                        </td>
                                    </tr>
                                )}
                            </Fragment>
                        ))}
                    </tbody>
                </table>
            </div>
        </section>
    );
}

/**
 * The series the party can see, as a choice.
 *
 * A series with no declared shape is listed and cannot be chosen: it has no grid, so the
 * evolution would refuse it. An empty list says why it can be empty. A list cut at the
 * cap says so, so it is not read as complete.
 */
export function SeriesPicker({
    list,
    loading,
    failure,
    picked,
    onPick,
}: {
    readonly list: CurveSeriesList | undefined;
    readonly loading: boolean;
    readonly failure: string | undefined;
    readonly picked: string;
    readonly onPick: (seriesId: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const titled = useSeriesTitle();
    if (loading) {
        return <p className="text-sm text-ink-muted">{t('market.evolution.picker.loading')}</p>;
    }
    if (failure !== undefined) {
        return <Notice tone="error">{failure}</Notice>;
    }
    if (list === undefined) {
        return null;
    }
    if (list.series.length === 0) {
        return <Notice tone="info">{t('market.evolution.picker.empty')}</Notice>;
    }
    return (
        <div className="space-y-2">
            <Field label={t('market.evolution.picker.label')}>
                <Select value={picked} onChange={(event) => onPick(event.target.value)}>
                    <option value="">{t('market.evolution.picker.placeholder')}</option>
                    {list.series.map((listed) => (
                        <option
                            key={listed.series_id}
                            value={listed.series_id}
                            disabled={!canBeShown(listed)}
                        >
                            {canBeShown(listed)
                                ? titled(listed.identity)
                                : `${titled(listed.identity)} (${t('market.evolution.picker.noShape')})`}
                        </option>
                    ))}
                </Select>
            </Field>
            {list.truncated && (
                <Notice tone="warn">
                    {t('market.evolution.picker.truncated', { count: list.series.length })}
                </Notice>
            )}
        </div>
    );
}

/**
 * The evolution screen.
 *
 * The party is the one the session works in. A switch of party does not clear the cache, so
 * the series list is keyed by it: the list of the party left behind is never shown for the
 * party switched to.
 */
export function EvolutionPage({ partyId }: { readonly partyId: string }): ReactNode {
    const { t } = useTranslation();
    // The server checks the write again; this only decides what the screen offers.
    const mayAdjust = useHolds()(PERMISSION.observationsWrite);
    const [form, setForm] = useState<IdentityForm>({
        asset: '',
        scope: '',
        instrumentType: '',
        quoteType: '',
        fields: '',
        partyId: '',
    });
    const [picked, setPicked] = useState('');
    const [from, setFrom] = useState(day(DEFAULT_SPAN_DAYS));
    const [to, setTo] = useState(day(0));
    const [provenance, setProvenance] = useState(false);
    const [asked, setAsked] = useState<Asked | null>(null);
    const [problem, setProblem] = useState<string | null>(null);
    // Every submit gets its own number, so reading the same object again reads it afresh.
    const [reads, setReads] = useState(0);

    const series = useQuery(oresmdQueries.seriesList(partyId));

    // An oresmd URL in the address opens the series it names, once for each URL.
    // A series the party does not have, or one that declares no axis, is said to be
    // unavailable in words, and the identity stays on the form to read or change.
    const [search] = useSearchParams();
    const uri = search.get('uri') ?? '';
    const parsed = useQuery({ ...oresmdQueries.parse(uri), enabled: uri !== '' });
    const schema = useQuery({ ...oresmdQueries.schema(), enabled: uri !== '' });
    const [unavailable, setUnavailable] = useState<string | null>(null);
    const [offerSeries, setOfferSeries] = useState(false);
    // The URL and the party together say what was opened, so a change of either opens again.
    const opened = useRef('');
    const openedKey = `${partyId}|${uri}`;
    // The effect lists the form values it reads. The ref stops it opening twice, so
    // a change to them while the data loads only runs the guard.
    useEffect(() => {
        if (uri === '' || opened.current === openedKey) return;
        if (parsed.isError || schema.isError || series.isError) {
            opened.current = openedKey;
            setUnavailable(null);
            setProblem((parsed.error ?? schema.error ?? series.error)?.message ?? '');
            return;
        }
        if (parsed.data === undefined || schema.data === undefined || series.data === undefined) {
            return;
        }
        opened.current = openedKey;
        setUnavailable(null);
        setProblem(null);
        const named = seriesOfUrl(schema.data, parsed.data);
        if (!named.ok) {
            setUnavailable(
                named.reason === 'fixing'
                    ? t('market.evolution.errors.fixing')
                    : named.reason === 'unknown'
                      ? t('market.evolution.errors.unknown', { name: named.message })
                      : named.message,
            );
            return;
        }
        const verdict = seriesVerdict(series.data, named.form);
        if (verdict.kind !== 'shown') {
            setForm(named.form);
            setOfferSeries(verdict.kind === 'no-shape');
            setUnavailable(
                verdict.kind === 'not-listed'
                    ? t('market.evolution.errors.notListed')
                    : t('market.evolution.errors.noShape', {
                          series: titleOrRaw(schema.data, verdict.listed.identity),
                      }),
            );
            return;
        }
        const form = formFrom(verdict.listed);
        setPicked(verdict.listed.series_id);
        setForm(form);
        const identity = identityOf(form);
        const span = rangeOf(from, to);
        if (identity.ok && span.ok) {
            setReads((count) => count + 1);
            setAsked({ identity: identity.identity, range: span.range, provenance });
        }
    }, [
        uri,
        openedKey,
        parsed.data,
        parsed.isError,
        parsed.error,
        schema.data,
        schema.isError,
        schema.error,
        series.data,
        series.isError,
        series.error,
        from,
        to,
        provenance,
        t,
    ]);

    const pick = (seriesId: string): void => {
        // Choosing the placeholder clears the pick and leaves the form as it stands.
        setPicked(seriesId);
        const listed = series.data?.series.find((candidate) => candidate.series_id === seriesId);
        if (listed !== undefined && canBeShown(listed)) {
            setForm(formFrom(listed));
        }
    };

    const evolution = useQuery({
        queryKey: ['market-evolution', asked?.identity, asked?.range, reads],
        queryFn: () => curves.evolution(asked!.identity, asked!.range),
        enabled: asked !== null,
        retry: false,
    });
    // A person who may adjust needs to see which nodes are manual, so the source is always read.
    const showSource = (asked?.provenance ?? false) || mayAdjust;
    const snapshot = useQuery({
        queryKey: ['market-snapshot', asked?.identity, showSource, reads],
        queryFn: () => curves.snapshot(asked!.identity, showSource),
        enabled: asked !== null,
        retry: false,
    });

    const submit = (event: FormEvent): void => {
        event.preventDefault();
        const identity = identityOf(form);
        if (!identity.ok) {
            setProblem(t(`market.evolution.errors.${identity.reason}`));
            return;
        }
        const span = rangeOf(from, to);
        if (!span.ok) {
            setProblem(t(`market.evolution.errors.${span.reason === 'range' ? 'range' : 'day'}`));
            return;
        }
        setProblem(null);
        setUnavailable(null);
        setReads(reads + 1);
        setAsked({ identity: identity.identity, range: span.range, provenance });
    };

    const text = (key: keyof IdentityForm) => ({
        value: form[key],
        // Typing makes the identity the person's own, so it no longer names the party of a
        // picked series.
        onChange: (event: { target: { value: string } }) => {
            setPicked('');
            setForm({ ...form, [key]: event.target.value, partyId: '' });
        },
    });

    return (
        <div className="space-y-6">
            <div>
                <AreaTrail area="marketdata" screen={t('market.evolution.title')} />
                <PageHeader
                    title={t('market.evolution.title')}
                    description={t('market.evolution.description')}
                />
            </div>

            <form className="card space-y-4 p-6" onSubmit={submit}>
                <h2 className="text-lg font-medium">{t('market.evolution.form.title')}</h2>
                <SeriesPicker
                    list={series.data}
                    loading={series.isPending}
                    failure={series.isError ? series.error.message : undefined}
                    picked={picked}
                    onPick={pick}
                />
                <div className="grid gap-4 sm:grid-cols-2">
                    <Field label={t('market.evolution.form.asset')}>
                        <Input {...text('asset')} />
                    </Field>
                    <Field label={t('market.evolution.form.scope')}>
                        <Input {...text('scope')} />
                    </Field>
                    <Field label={t('market.evolution.form.instrumentType')}>
                        <Input {...text('instrumentType')} />
                    </Field>
                    <Field label={t('market.evolution.form.quoteType')}>
                        <Input {...text('quoteType')} />
                    </Field>
                </div>
                <Field
                    label={t('market.evolution.form.fields')}
                    hint={t('market.evolution.form.fieldsHint')}
                >
                    <textarea
                        className="w-full rounded-md border border-line bg-surface-base px-3 py-2 font-mono text-sm text-ink"
                        rows={2}
                        {...text('fields')}
                    />
                </Field>
                <div className="grid gap-4 sm:grid-cols-2">
                    <Field label={t('market.evolution.form.from')}>
                        <Input
                            type="date"
                            value={from}
                            onChange={(event) => setFrom(event.target.value)}
                        />
                    </Field>
                    <Field label={t('market.evolution.form.to')}>
                        <Input
                            type="date"
                            value={to}
                            onChange={(event) => setTo(event.target.value)}
                        />
                    </Field>
                </div>
                <label className="flex items-center gap-2 text-sm text-ink-muted">
                    <input
                        type="checkbox"
                        checked={provenance}
                        onChange={(event) => setProvenance(event.target.checked)}
                    />
                    {t('market.evolution.form.provenance')}
                </label>
                {problem !== null && <Notice tone="error">{problem}</Notice>}
                {uri === '' && series.isError && (
                    <Notice tone="error">{series.error.message}</Notice>
                )}
                {unavailable !== null && (
                    <Notice tone="warn">
                        {unavailable}
                        {offerSeries && (
                            <>
                                {' '}
                                <Link
                                    className="text-accent-bright hover:underline"
                                    to={seriesRoute(uri)}
                                >
                                    {t('market.series.openSeries')}
                                </Link>
                            </>
                        )}
                    </Notice>
                )}
                <div className="flex items-center gap-3">
                    <Button type="submit">{t('market.evolution.form.submit')}</Button>
                </div>
            </form>

            {asked !== null && (
                <>
                    {evolution.isPending ? (
                        <p className="text-sm text-ink-muted">{t('market.evolution.reading')}</p>
                    ) : evolution.isError ? (
                        <Notice tone="error">{evolution.error.message}</Notice>
                    ) : (
                        <MatrixPanel evolution={evolution.data} />
                    )}
                    {snapshot.isError ? (
                        <Notice tone="error">{snapshot.error.message}</Notice>
                    ) : (
                        snapshot.data !== undefined && (
                            <NowPanel
                                snapshot={snapshot.data}
                                provenance={showSource}
                                {...(mayAdjust
                                    ? {
                                          adjust: {
                                              identity: asked.identity,
                                              onChanged: () => setReads(reads + 1),
                                          },
                                      }
                                    : {})}
                            />
                        )
                    )}
                </>
            )}
        </div>
    );
}
