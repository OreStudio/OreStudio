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
 */

import { useQueries } from '@tanstack/react-query';
import { useRef, type ReactNode } from 'react';
import { quotes } from '../api/quotes.js';
import { useTranslation } from '../i18n/Provider.js';
import { Select, cx } from '../ui/Primitives.js';
import { LineChart } from './LineChart.js';
import {
    agreementOf,
    changeOf,
    compare,
    CONSENSUS,
    differenceOf,
    differenceText,
    gridOf,
    instantOf,
    isOutlier,
    isStale,
    newestOf,
    onTimeAxis,
    tenorsOf,
    valueText,
    type Agreement,
    type Kind,
    type Point,
    type Series,
    type Vendor,
} from './quotesView.js';
import { DATA_CELL } from './readScreen.js';

/**
 * The boards of the quotes view, after variant C of
 * doc/prototypes/market_data/read-the-quotes-by-series/: each asset class and
 * shape has the board its desk reads. Quotes are a board of series by vendor,
 * curves stand side by side by tenor, and a surface is a grid that can show the
 * mid, one vendor, the spread, or two sources compared.
 */

/** What every board reads: the vendors, the ones shown, the time, and each vendor's colour. */
export interface Watch {
    readonly vendors: readonly Vendor[];
    readonly shown: ReadonlySet<string>;
    readonly now: number;
    readonly colourOf: (vendor: string) => string;
}

const HEAD = 'px-2 py-1 text-xs font-semibold text-ink-faint whitespace-nowrap';
const HEAD_LEFT = cx(HEAD, 'text-left');
const HEAD_RIGHT = cx(HEAD, 'text-right');
const LEFT_CELL = 'px-2 py-1 whitespace-nowrap';
const ROW = 'border-t border-line/50';
const PICKABLE = 'cursor-pointer hover:bg-panel-2';
const SCROLL = 'max-h-[36rem] overflow-auto rounded-md border border-line';

function age(at: string, now: number): string {
    const seconds = Math.max(0, (now - instantOf(at)) / 1000);
    return seconds < 60 ? `${seconds.toFixed(0)}s` : `${Math.round(seconds / 60)}m`;
}

function Swatch({ colour }: { readonly colour: string }): ReactNode {
    return (
        <span
            aria-hidden="true"
            className="mr-1.5 inline-block h-2 w-2 rounded-full align-middle"
            style={{ background: colour }}
        />
    );
}

/** The vendors of a set of series, among the ones shown. */
function vendorsIn(series: readonly Series[], watch: Watch): Vendor[] {
    return watch.vendors.filter(
        (v) =>
            watch.shown.has(v.key) && series.some((s) => s.points.some((p) => p.quotes.has(v.key))),
    );
}

function VendorHeads({
    vendors,
    watch,
}: {
    readonly vendors: readonly Vendor[];
    readonly watch: Watch;
}): ReactNode {
    return vendors.map((v) => (
        <th key={v.key} className={HEAD_RIGHT}>
            <Swatch colour={watch.colourOf(v.key)} />
            {v.provider}
        </th>
    ));
}

/** One vendor's quote of a point: dimmed when stale, warned when far from the others. */
function ValueCell({
    kind,
    point,
    vendor,
    agreement,
    now,
}: {
    readonly kind: Kind;
    readonly point: Point;
    readonly vendor: Vendor;
    readonly agreement: Agreement;
    readonly now: number;
}): ReactNode {
    const { t } = useTranslation();
    const q = point.quotes.get(vendor.key);
    if (q === undefined) return <td className={cx(DATA_CELL, 'text-ink-faint')}>·</td>;
    const stale = isStale(q.at, now);
    const out = !stale && isOutlier(kind, Number(q.value), agreement.reference);
    return (
        <td
            className={cx(DATA_CELL, stale && 'text-ink-faint', out && 'text-warn')}
            title={`${vendor.provider} · ${age(q.at, now)}${out ? ` · ${t('marketdata.quotes.outlier')}` : ''}`}
        >
            {valueText(kind, Number(q.value))}
            {out && ' ⚠'}
        </td>
    );
}

function Difference({
    kind,
    delta,
    at,
}: {
    readonly kind: Kind;
    readonly delta: number;
    readonly at: number;
}): ReactNode {
    const text = differenceText(differenceOf(kind, delta, at));
    if (text === '0') return <span className="text-ink-faint">0</span>;
    return <span className={delta > 0 ? 'text-up' : 'text-down'}>{text}</span>;
}

function spreadText(kind: Kind, a: Agreement): string {
    if (a.spread === null || a.mid === null || a.count < 2) return '—';
    return differenceText(differenceOf(kind, a.spread, a.mid)).replace('+', '');
}

/** A heading with its unit, when it has one. */
function withUnit(label: string, unit: string): string {
    return unit === '' ? label : `${label} (${unit})`;
}

/** The unit a board's spreads and changes are in. */
function useUnit(kind: Kind): string {
    const { t } = useTranslation();
    if (kind === 'spot') return t('marketdata.quotes.unitPip');
    if (kind === 'rate') return t('marketdata.quotes.unitBp');
    if (kind === 'vol') return t('marketdata.quotes.unitVol');
    return '';
}

/**
 * Every vendor's quote of one point, the mid and the spread, and the last hour
 * of each on one time axis. A vendor too far from the others to share an axis
 * is listed, not drawn.
 */
export function AllQuotes({
    series,
    point,
    watch,
}: {
    readonly series: Series;
    readonly point: Point;
    readonly watch: Watch;
}): ReactNode {
    const { t, plural } = useTranslation();
    const unit = useUnit(series.kind);
    const agreement = agreementOf(series.kind, point, watch.vendors, watch.shown, watch.now);
    const quoted = watch.vendors.filter((v) => point.quotes.has(v.key));
    const drawn = quoted.filter((v) => {
        const q = point.quotes.get(v.key)!;
        return (
            watch.shown.has(v.key) && !isOutlier(series.kind, Number(q.value), agreement.reference)
        );
    });
    const from = useRef(Date.now() - 3_600_000).current;
    const histories = useQueries({
        queries: drawn.map((v) => ({
            queryKey: ['quote-history', v.provider, point.uri, from] as const,
            queryFn: () =>
                quotes.history(v.provider, series.key, point.uri, new Date(from).toISOString()),
            retry: false,
            staleTime: Infinity,
        })),
    });
    const axis = onTimeAxis(
        drawn.map((v, i) => {
            const q = point.quotes.get(v.key)!;
            const reference = agreement.reference ?? Number(q.value);
            const past = (histories[i]?.data?.ticks ?? [])
                .filter((tick) => !isOutlier(series.kind, tick.value, reference))
                .map((tick) => ({ at: instantOf(tick.at), value: tick.value }));
            return [...past, { at: instantOf(q.at), value: Number(q.value) }];
        }),
        from,
        watch.now,
        240,
    );
    const notDrawn = quoted.filter((v) => watch.shown.has(v.key) && !drawn.includes(v));
    return (
        <div className="space-y-3">
            <table className="w-full text-sm">
                <thead>
                    <tr>
                        <th className={HEAD_LEFT}>{t('marketdata.quotes.vendor')}</th>
                        <th className={HEAD_RIGHT}>{t('marketdata.quotes.value')}</th>
                        <th className={HEAD_RIGHT}>
                            {withUnit(t('marketdata.quotes.toMid'), unit)}
                        </th>
                        <th className={HEAD_RIGHT}>
                            {withUnit(t('marketdata.quotes.change'), unit)}
                        </th>
                        <th className={HEAD_RIGHT}>{t('marketdata.quotes.age')}</th>
                    </tr>
                </thead>
                <tbody>
                    {quoted.map((v) => {
                        const q = point.quotes.get(v.key)!;
                        const value = Number(q.value);
                        const stale = isStale(q.at, watch.now);
                        const hidden = !watch.shown.has(v.key);
                        const out =
                            !stale && !hidden && isOutlier(series.kind, value, agreement.reference);
                        return (
                            <tr
                                key={v.key}
                                className={cx(ROW, (stale || hidden) && 'text-ink-faint')}
                            >
                                <td className={LEFT_CELL}>
                                    <Swatch colour={watch.colourOf(v.key)} />
                                    {v.provider}
                                    {hidden && (
                                        <span className="ml-2 text-xs">
                                            {t('marketdata.quotes.leftOut')}
                                        </span>
                                    )}
                                    {stale && (
                                        <span className="ml-2 text-xs text-warn">
                                            {t('marketdata.quotes.stale')}
                                        </span>
                                    )}
                                    {out && (
                                        <span className="ml-2 text-xs text-warn">
                                            {t('marketdata.quotes.outlier')}
                                        </span>
                                    )}
                                </td>
                                <td className={cx(DATA_CELL, out && 'text-warn')}>
                                    {valueText(series.kind, value)}
                                </td>
                                <td className={DATA_CELL}>
                                    {agreement.mid === null ? (
                                        '—'
                                    ) : (
                                        <Difference
                                            kind={series.kind}
                                            delta={value - agreement.mid}
                                            at={value}
                                        />
                                    )}
                                </td>
                                <td className={DATA_CELL}>
                                    <Difference kind={series.kind} delta={changeOf(q)} at={value} />
                                </td>
                                <td className={DATA_CELL}>{age(q.at, watch.now)}</td>
                            </tr>
                        );
                    })}
                    <tr className="border-t border-line">
                        <td className={cx(LEFT_CELL, 'text-ink-muted')}>
                            {plural('marketdata.quotes.midOf', agreement.count)}
                        </td>
                        <td className={DATA_CELL}>{valueText(series.kind, agreement.mid)}</td>
                        <td className={cx(DATA_CELL, 'text-ink-muted')}>
                            {t('marketdata.quotes.spreadOf', {
                                spread: spreadText(series.kind, agreement),
                            })}
                        </td>
                        <td colSpan={2} />
                    </tr>
                </tbody>
            </table>
            {drawn.length > 0 && (
                <LineChart
                    series={drawn.map((v, i) => ({
                        points: axis.lines[i] ?? [],
                        colour: watch.colourOf(v.key),
                    }))}
                    xs={axis.xs}
                    xFormat={(x) => new Date(x).toISOString().slice(11, 16)}
                    format={(v) => valueText(series.kind, v)}
                    label={t('marketdata.quotes.lastHour', {
                        name: `${series.name} ${point.label}`.trim(),
                    })}
                    price
                />
            )}
            {notDrawn.length > 0 && (
                <p className="text-xs text-ink-faint">
                    {t('marketdata.quotes.notDrawn', {
                        vendors: notDrawn.map((v) => v.provider).join(', '),
                    })}
                </p>
            )}
        </div>
    );
}

/** A board of quotes: one row for each series, each vendor a column, and the picked one beside it. */
export function QuoteBoard({
    series,
    watch,
    picked,
    onPick,
}: {
    readonly series: readonly Series[];
    readonly watch: Watch;
    readonly picked: string;
    readonly onPick: (key: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const vendors = vendorsIn(series, watch);
    const kind = series[0]?.kind ?? 'plain';
    const unit = useUnit(kind);
    const chosen = series.find((s) => s.key === picked);
    return (
        <div className="grid grid-cols-1 gap-4 xl:grid-cols-[minmax(0,1fr)_minmax(0,1fr)]">
            <div className={SCROLL}>
                <table className="w-full text-sm">
                    <thead className="sticky top-0 bg-surface-base">
                        <tr>
                            <th className={HEAD_LEFT}>{t('marketdata.quotes.series')}</th>
                            <VendorHeads vendors={vendors} watch={watch} />
                            <th className={HEAD_RIGHT}>{t('marketdata.quotes.mid')}</th>
                            <th className={HEAD_RIGHT}>
                                {withUnit(t('marketdata.quotes.spread'), unit)}
                            </th>
                            <th className={HEAD_RIGHT}>{t('marketdata.quotes.age')}</th>
                        </tr>
                    </thead>
                    <tbody>
                        {series.map((s) => {
                            const point = s.points[0]!;
                            const agreement = agreementOf(
                                s.kind,
                                point,
                                watch.vendors,
                                watch.shown,
                                watch.now,
                            );
                            const newest = newestOf(s, watch.shown);
                            return (
                                <tr
                                    key={s.key}
                                    onClick={() => onPick(s.key)}
                                    className={cx(
                                        ROW,
                                        PICKABLE,
                                        s.key === picked && 'bg-accent/10',
                                    )}
                                >
                                    <td className={LEFT_CELL}>
                                        {s.name}
                                        {point.label !== '' && (
                                            <span className="ml-2 text-ink-faint">
                                                {point.label}
                                            </span>
                                        )}
                                    </td>
                                    {vendors.map((v) => (
                                        <ValueCell
                                            key={v.key}
                                            kind={s.kind}
                                            point={point}
                                            vendor={v}
                                            agreement={agreement}
                                            now={watch.now}
                                        />
                                    ))}
                                    <td className={DATA_CELL}>
                                        {valueText(s.kind, agreement.mid)}
                                    </td>
                                    <td className={cx(DATA_CELL, 'text-ink-muted')}>
                                        {spreadText(s.kind, agreement)}
                                    </td>
                                    <td
                                        className={cx(
                                            DATA_CELL,
                                            watch.now - newest > 60_000 && 'text-ink-faint',
                                        )}
                                    >
                                        {newest === 0
                                            ? '—'
                                            : age(new Date(newest).toISOString(), watch.now)}
                                    </td>
                                </tr>
                            );
                        })}
                    </tbody>
                </table>
            </div>
            <section className="min-w-0 overflow-x-auto rounded-md border border-line p-4">
                {chosen === undefined ? (
                    <p className="text-sm text-ink-faint">{t('marketdata.quotes.pickQuote')}</p>
                ) : (
                    <div className="space-y-3">
                        <h2 className="text-sm font-semibold">{chosen.name}</h2>
                        <AllQuotes series={chosen} point={chosen.points[0]!} watch={watch} />
                    </div>
                )}
            </section>
        </div>
    );
}

/** One curve, tenor down and each vendor across, with its curves drawn by tenor. */
function CurveStrip({
    series,
    watch,
}: {
    readonly series: Series;
    readonly watch: Watch;
}): ReactNode {
    const { t } = useTranslation();
    const unit = useUnit(series.kind);
    const vendors = vendorsIn([series], watch);
    const agreements = series.points.map((p) =>
        agreementOf(series.kind, p, watch.vendors, watch.shown, watch.now),
    );
    return (
        <div className="grid grid-cols-1 gap-4 xl:grid-cols-2">
            <div className={SCROLL}>
                <table className="w-full text-sm">
                    <thead className="sticky top-0 bg-surface-base">
                        <tr>
                            <th className={HEAD_LEFT}>{t('marketdata.quotes.tenor')}</th>
                            <VendorHeads vendors={vendors} watch={watch} />
                            <th className={HEAD_RIGHT}>{t('marketdata.quotes.mid')}</th>
                            <th className={HEAD_RIGHT}>
                                {withUnit(t('marketdata.quotes.spread'), unit)}
                            </th>
                        </tr>
                    </thead>
                    <tbody>
                        {series.points.map((p, i) => (
                            <tr key={p.uri} className={ROW}>
                                <td className={cx(LEFT_CELL, 'font-mono')}>{p.label}</td>
                                {vendors.map((v) => (
                                    <ValueCell
                                        key={v.key}
                                        kind={series.kind}
                                        point={p}
                                        vendor={v}
                                        agreement={agreements[i]!}
                                        now={watch.now}
                                    />
                                ))}
                                <td className={DATA_CELL}>
                                    {valueText(series.kind, agreements[i]!.mid)}
                                </td>
                                <td className={cx(DATA_CELL, 'text-ink-muted')}>
                                    {spreadText(series.kind, agreements[i]!)}
                                </td>
                            </tr>
                        ))}
                    </tbody>
                </table>
            </div>
            <LineChart
                categories={series.points.map((p) => p.label)}
                series={vendors.map((v) => ({
                    colour: watch.colourOf(v.key),
                    dots: true,
                    points: series.points.map((p, i) => {
                        const q = p.quotes.get(v.key);
                        const value = q === undefined ? NaN : Number(q.value);
                        return Number.isFinite(value) &&
                            !isOutlier(series.kind, value, agreements[i]!.reference)
                            ? value
                            : null;
                    }),
                }))}
                format={(v) => valueText(series.kind, v)}
                label={t('marketdata.quotes.byTenor', { name: series.name })}
                price
            />
        </div>
    );
}

/** A segmented choice, as every board draws its modes. */
function Choice<T extends string>({
    label,
    options,
    value,
    titleOf,
    onChoose,
}: {
    readonly label: string;
    readonly options: readonly T[];
    readonly value: T;
    readonly titleOf: (option: T) => string;
    readonly onChoose: (option: T) => void;
}): ReactNode {
    return (
        <div
            role="group"
            aria-label={label}
            className="inline-flex flex-wrap overflow-hidden rounded border border-line"
        >
            {options.map((o) => (
                <button
                    key={o}
                    type="button"
                    aria-pressed={o === value}
                    onClick={() => onChoose(o)}
                    className={cx(
                        'px-3 py-1 text-sm',
                        o === value ? 'bg-accent/15 text-ink' : 'text-ink-muted hover:bg-panel-2',
                    )}
                >
                    {titleOf(o)}
                </button>
            ))}
        </div>
    );
}

/** The curves of a board side by side, tenor down, for the mid or one vendor; a picked curve opens below. */
export function CurveBoard({
    series,
    watch,
    picked,
    onPick,
    source,
    onSource,
}: {
    readonly series: readonly Series[];
    readonly watch: Watch;
    readonly picked: string;
    readonly onPick: (key: string) => void;
    readonly source: string;
    readonly onSource: (source: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    const vendors = vendorsIn(series, watch);
    const options = [CONSENSUS, ...vendors.map((v) => v.key)];
    const chosenSource = options.includes(source) ? source : CONSENSUS;
    const tenors = tenorsOf(series);
    const chosen = series.find((s) => s.key === picked);
    const vendorName = (key: string): string =>
        watch.vendors.find((v) => v.key === key)?.provider ?? key;
    return (
        <div className="space-y-4">
            <div className="flex flex-wrap items-center justify-between gap-3">
                <Choice
                    label={t('marketdata.quotes.show')}
                    options={options}
                    value={chosenSource}
                    titleOf={(o) => (o === CONSENSUS ? t('marketdata.quotes.mid') : vendorName(o))}
                    onChoose={onSource}
                />
                <p className="text-xs text-ink-faint">{t('marketdata.quotes.curvesHint')}</p>
            </div>
            <div className={SCROLL}>
                <table className="w-full text-sm">
                    <thead className="sticky top-0 bg-surface-base">
                        <tr>
                            <th className={HEAD_LEFT}>{t('marketdata.quotes.tenor')}</th>
                            {series.map((s) => (
                                <th key={s.key} className={HEAD_RIGHT}>
                                    <button
                                        type="button"
                                        onClick={() => onPick(s.key)}
                                        className={cx(
                                            'whitespace-nowrap hover:text-ink',
                                            s.key === picked && 'text-accent',
                                        )}
                                    >
                                        {s.name}
                                    </button>
                                </th>
                            ))}
                        </tr>
                    </thead>
                    <tbody>
                        {tenors.map((tenor) => (
                            <tr key={tenor} className={ROW}>
                                <td className={cx(LEFT_CELL, 'font-mono')}>{tenor}</td>
                                {series.map((s) => {
                                    const p = s.points.find((x) => x.coordinates[0] === tenor);
                                    if (p === undefined)
                                        return (
                                            <td
                                                key={s.key}
                                                className={cx(DATA_CELL, 'text-ink-faint')}
                                            >
                                                ·
                                            </td>
                                        );
                                    const agreement = agreementOf(
                                        s.kind,
                                        p,
                                        watch.vendors,
                                        watch.shown,
                                        watch.now,
                                    );
                                    if (chosenSource === CONSENSUS)
                                        return (
                                            <td key={s.key} className={DATA_CELL}>
                                                {valueText(s.kind, agreement.mid)}
                                            </td>
                                        );
                                    const vendor = watch.vendors.find(
                                        (v) => v.key === chosenSource,
                                    )!;
                                    return (
                                        <ValueCell
                                            key={s.key}
                                            kind={s.kind}
                                            point={p}
                                            vendor={vendor}
                                            agreement={agreement}
                                            now={watch.now}
                                        />
                                    );
                                })}
                            </tr>
                        ))}
                    </tbody>
                </table>
            </div>
            <section className="space-y-3 rounded-md border border-line p-4">
                {chosen === undefined ? (
                    <p className="text-sm text-ink-faint">{t('marketdata.quotes.pickCurve')}</p>
                ) : (
                    <>
                        <h2 className="text-sm font-semibold">{chosen.name}</h2>
                        <CurveStrip series={chosen} watch={watch} />
                    </>
                )}
            </section>
        </div>
    );
}

/** What a surface's grid shows: the mid, one vendor, the spread, or two sources compared. */
export type SurfaceMode = string;
export const MID = 'mid';
export const SPREAD = 'spread';
export const COMPARE = 'compare';

/** The state of a comparison: the two sources and the picked cell. */
export interface Comparison {
    readonly a: string;
    readonly b: string;
    readonly row: string;
    readonly column: string;
}

/** A difference as a diverging colour: the accent where A is above B, red where below. */
function diverging(kind: Kind, difference: number | null, at: number): string | undefined {
    if (difference === null) return undefined;
    const size = Math.abs(differenceOf(kind, difference, at).amount);
    const full = kind === 'vol' ? 0.5 : kind === 'rate' ? 5 : 10;
    if (size < full / 25) return undefined;
    const share = Math.round(8 + Math.min(1, size / full) * 50);
    return `color-mix(in srgb, var(--color-${difference > 0 ? 'accent' : 'down'}) ${share}%, transparent)`;
}

/** Two sources of a surface: their differences on the grid, a column by tenor, and a cell's every source. */
function SurfaceCompare({
    series,
    watch,
    comparison,
    onCompare,
}: {
    readonly series: Series;
    readonly watch: Watch;
    readonly comparison: Comparison;
    readonly onCompare: (c: Comparison) => void;
}): ReactNode {
    const { t } = useTranslation();
    const unit = useUnit(series.kind);
    const grid = gridOf(series);
    const vendors = vendorsIn([series], watch);
    const sources = [CONSENSUS, ...vendors.map((v) => v.key)];
    const a = sources.includes(comparison.a)
        ? comparison.a
        : (vendors[1]?.key ?? vendors[0]?.key ?? CONSENSUS);
    const b = sources.includes(comparison.b) && comparison.b !== a ? comparison.b : CONSENSUS;
    const column = grid.columns.includes(comparison.column)
        ? comparison.column
        : (grid.columns[0] ?? '');
    const row = grid.rows.includes(comparison.row) ? comparison.row : '';
    const set = (next: Partial<Comparison>): void => onCompare({ a, b, row, column, ...next });
    const nameOf = (source: string, other: string): string =>
        source === CONSENSUS
            ? other === CONSENSUS
                ? t('marketdata.quotes.consensus')
                : t('marketdata.quotes.consensusOthers')
            : (watch.vendors.find((v) => v.key === source)?.provider ?? source);
    const colourOf = (source: string): string =>
        source === CONSENSUS ? 'var(--color-ink)' : watch.colourOf(source);
    const cells = grid.rows.flatMap((r) =>
        grid.columns.map((c) => {
            const p = grid.at(r, c);
            return {
                r,
                c,
                p,
                compared:
                    p === undefined
                        ? null
                        : compare(series.kind, p, a, b, watch.vendors, watch.shown, watch.now),
            };
        }),
    );
    const measured = cells.filter((x) => x.compared?.difference != null);
    const widest = measured.reduce<(typeof measured)[number] | null>(
        (w, x) =>
            w === null || Math.abs(x.compared!.difference!) > Math.abs(w.compared!.difference!)
                ? x
                : w,
        null,
    );
    const past = measured.filter((x) =>
        isOutlier(series.kind, x.compared!.a!, x.compared!.b!),
    ).length;
    const firstColumn = measured.filter((x) => x.c === grid.columns[0]);
    const firstGap =
        firstColumn.length === 0
            ? null
            : firstColumn.reduce((s, x) => s + x.compared!.difference!, 0) / firstColumn.length;
    const gap = (d: number | null, at: number): string =>
        d === null ? '—' : differenceText(differenceOf(series.kind, d, at));
    const picked = row === '' ? undefined : grid.at(row, column);
    const drawnSources = [CONSENSUS, ...vendors.map((v) => v.key)];
    return (
        <div className="space-y-3">
            <div className="flex flex-wrap items-end gap-3">
                <label className="text-sm">
                    <span className="mb-1 block text-xs text-ink-faint">
                        {t('marketdata.quotes.compareA')}
                    </span>
                    <Select value={a} onChange={(event) => set({ a: event.target.value })}>
                        {sources.map((s) => (
                            <option key={s} value={s}>
                                {nameOf(s, b)}
                            </option>
                        ))}
                    </Select>
                </label>
                <button
                    type="button"
                    className="rounded border border-line px-3 py-2 text-sm"
                    aria-label={t('marketdata.quotes.swap')}
                    title={t('marketdata.quotes.swap')}
                    onClick={() => set({ a: b, b: a })}
                >
                    ⇄
                </button>
                <label className="text-sm">
                    <span className="mb-1 block text-xs text-ink-faint">
                        {t('marketdata.quotes.compareB')}
                    </span>
                    <Select value={b} onChange={(event) => set({ b: event.target.value })}>
                        {sources
                            .filter((s) => s !== a)
                            .map((s) => (
                                <option key={s} value={s}>
                                    {nameOf(s, a)}
                                </option>
                            ))}
                    </Select>
                </label>
                <p className="max-w-md text-xs text-ink-faint">
                    {t('marketdata.quotes.compareHint', { unit })}
                </p>
            </div>
            <p className="flex flex-wrap gap-5 text-sm">
                <span>
                    <span className="text-ink-faint">
                        {t('marketdata.quotes.columnGap', { column: grid.columns[0] ?? '' })}{' '}
                    </span>
                    <span className="font-mono">
                        {gap(firstGap, firstColumn[0]?.compared?.b ?? 1)}
                    </span>
                </span>
                <span>
                    <span className="text-ink-faint">{t('marketdata.quotes.widest')} </span>
                    <span className="font-mono">
                        {widest === null
                            ? '—'
                            : `${gap(widest.compared!.difference, widest.compared!.b!)} · ${widest.r} ${widest.c}`}
                    </span>
                </span>
                <span>
                    <span className="text-ink-faint">{t('marketdata.quotes.pastLimit')} </span>
                    <span
                        className={cx('font-mono', past > 0 && 'text-warn')}
                    >{`${String(past)} / ${String(measured.length)}`}</span>
                </span>
            </p>
            <div className="grid grid-cols-1 gap-4 xl:grid-cols-[minmax(0,5fr)_minmax(0,4fr)]">
                <div className="space-y-3">
                    <div className={SCROLL}>
                        <table className="w-full text-sm">
                            <thead className="sticky top-0 bg-surface-base">
                                <tr>
                                    <th className={HEAD_LEFT}>{t('marketdata.quotes.tenor')}</th>
                                    {grid.columns.map((c) => (
                                        <th key={c} className={HEAD_RIGHT}>
                                            <button
                                                type="button"
                                                className={cx(
                                                    'hover:text-ink',
                                                    c === column && 'text-accent',
                                                )}
                                                onClick={() => set({ column: c })}
                                            >
                                                {c}
                                            </button>
                                        </th>
                                    ))}
                                </tr>
                            </thead>
                            <tbody>
                                {grid.rows.map((r) => (
                                    <tr key={r} className={ROW}>
                                        <td className={cx(LEFT_CELL, 'font-mono')}>{r}</td>
                                        {grid.columns.map((c) => {
                                            const cell = cells.find((x) => x.r === r && x.c === c)!;
                                            const d = cell.compared?.difference ?? null;
                                            const past =
                                                d !== null &&
                                                isOutlier(
                                                    series.kind,
                                                    cell.compared!.a!,
                                                    cell.compared!.b!,
                                                );
                                            const on = r === row && c === column;
                                            return (
                                                <td
                                                    key={c}
                                                    onClick={() => set({ row: r, column: c })}
                                                    className={cx(
                                                        DATA_CELL,
                                                        PICKABLE,
                                                        past && 'font-bold',
                                                        on &&
                                                            'outline-2 -outline-offset-2 outline-accent',
                                                    )}
                                                    style={{
                                                        background: diverging(
                                                            series.kind,
                                                            d,
                                                            cell.compared?.b ?? 1,
                                                        ),
                                                    }}
                                                    title={`${nameOf(a, b)} ${valueText(series.kind, cell.compared?.a ?? null)} · ${nameOf(b, a)} ${valueText(series.kind, cell.compared?.b ?? null)}`}
                                                >
                                                    {d === null ? '·' : gap(d, cell.compared!.b!)}
                                                    {past && ' ⚠'}
                                                </td>
                                            );
                                        })}
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    </div>
                    {picked !== undefined && (
                        <table className="w-full text-sm">
                            <caption className="pb-1 text-left text-sm font-semibold">{`${row} ${column}`}</caption>
                            <thead>
                                <tr>
                                    <th className={HEAD_LEFT}>{t('marketdata.quotes.source')}</th>
                                    <th className={HEAD_RIGHT}>{t('marketdata.quotes.value')}</th>
                                    <th className={HEAD_RIGHT}>
                                        {t('marketdata.quotes.toSource', { source: nameOf(b, a) })}
                                    </th>
                                    <th className={HEAD_RIGHT}>{t('marketdata.quotes.age')}</th>
                                </tr>
                            </thead>
                            <tbody>
                                {drawnSources.map((s) => {
                                    const c = compare(
                                        series.kind,
                                        picked,
                                        s,
                                        b,
                                        watch.vendors,
                                        watch.shown,
                                        watch.now,
                                    );
                                    const q = s === CONSENSUS ? undefined : picked.quotes.get(s);
                                    const label = s === b ? nameOf(b, a) : nameOf(s, b);
                                    return (
                                        <tr
                                            key={s}
                                            className={cx(
                                                ROW,
                                                q !== undefined &&
                                                    isStale(q.at, watch.now) &&
                                                    'text-ink-faint',
                                            )}
                                        >
                                            <td className={LEFT_CELL}>
                                                <Swatch colour={colourOf(s)} />
                                                {label}
                                                {s === a && ' (A)'}
                                                {s === b && ' (B)'}
                                            </td>
                                            <td className={DATA_CELL}>
                                                {valueText(series.kind, s === b ? c.b : c.a)}
                                            </td>
                                            <td className={DATA_CELL}>
                                                {s === b ? '' : gap(c.difference, c.b ?? 1)}
                                            </td>
                                            <td className={DATA_CELL}>
                                                {q === undefined ? '' : age(q.at, watch.now)}
                                            </td>
                                        </tr>
                                    );
                                })}
                            </tbody>
                        </table>
                    )}
                </div>
                <div className="space-y-2">
                    <h3 className="text-sm font-semibold">
                        {t('marketdata.quotes.termOf', { column })}
                    </h3>
                    <LineChart
                        categories={grid.rows}
                        series={drawnSources.map((s) => ({
                            colour: colourOf(s),
                            strong: s === a || s === b,
                            dots: s === a || s === b,
                            opacity: s === a || s === b ? 1 : 0.45,
                            ...(s === CONSENSUS ? { dashed: true } : {}),
                            points: grid.rows.map((r) => {
                                const p = grid.at(r, column);
                                if (p === undefined) return null;
                                const c = compare(
                                    series.kind,
                                    p,
                                    s,
                                    s === b ? a : b,
                                    watch.vendors,
                                    watch.shown,
                                    watch.now,
                                );
                                return c.a;
                            }),
                        }))}
                        format={(v) => valueText(series.kind, v)}
                        label={t('marketdata.quotes.termLabel', { name: series.name, column })}
                        price
                    />
                    <p className="flex flex-wrap gap-4 text-xs text-ink-muted">
                        {drawnSources.map((s) => (
                            <span key={s} className={s === a || s === b ? 'text-ink' : ''}>
                                <Swatch colour={colourOf(s)} />
                                {s === b ? nameOf(b, a) : nameOf(s, b)}
                                {s === a && ' (A)'}
                                {s === b && ' (B)'}
                            </span>
                        ))}
                    </p>
                </div>
            </div>
        </div>
    );
}

/** The surfaces of a board: a list beside the picked surface's grid. */
export function SurfaceBoard({
    series,
    watch,
    picked,
    onPick,
    mode,
    onMode,
    point,
    onPoint,
    comparison,
    onCompare,
}: {
    readonly series: readonly Series[];
    readonly watch: Watch;
    readonly picked: string;
    readonly onPick: (key: string) => void;
    readonly mode: SurfaceMode;
    readonly onMode: (mode: SurfaceMode) => void;
    readonly point: string;
    readonly onPoint: (uri: string) => void;
    readonly comparison: Comparison;
    readonly onCompare: (c: Comparison) => void;
}): ReactNode {
    const { t } = useTranslation();
    const chosen = series.find((s) => s.key === picked) ?? series[0];
    return (
        <div className="grid grid-cols-1 gap-4 md:grid-cols-[12rem_minmax(0,1fr)]">
            <nav
                aria-label={t('marketdata.quotes.surfaces')}
                className="max-h-[40rem] space-y-0.5 overflow-auto rounded-md border border-line p-2"
            >
                {series.map((s) => {
                    const newest = newestOf(s, watch.shown);
                    return (
                        <button
                            key={s.key}
                            type="button"
                            onClick={() => onPick(s.key)}
                            className={cx(
                                'flex w-full items-center justify-between rounded px-2 py-1.5 text-left text-sm',
                                s.key === chosen?.key
                                    ? 'bg-accent/15 text-ink'
                                    : 'hover:bg-panel-2',
                            )}
                        >
                            <span>{s.name}</span>
                            <span
                                className={cx(
                                    'text-xs',
                                    watch.now - newest > 60_000 ? 'text-warn' : 'text-ink-faint',
                                )}
                            >
                                {newest === 0
                                    ? '—'
                                    : age(new Date(newest).toISOString(), watch.now)}
                            </span>
                        </button>
                    );
                })}
            </nav>
            {chosen === undefined ? null : (
                <SurfaceGrid
                    series={chosen}
                    watch={watch}
                    mode={mode}
                    onMode={onMode}
                    point={point}
                    onPoint={onPoint}
                    comparison={comparison}
                    onCompare={onCompare}
                />
            )}
        </div>
    );
}

function SurfaceGrid({
    series,
    watch,
    mode,
    onMode,
    point,
    onPoint,
    comparison,
    onCompare,
}: {
    readonly series: Series;
    readonly watch: Watch;
    readonly mode: SurfaceMode;
    readonly onMode: (mode: SurfaceMode) => void;
    readonly point: string;
    readonly onPoint: (uri: string) => void;
    readonly comparison: Comparison;
    readonly onCompare: (c: Comparison) => void;
}): ReactNode {
    const { t } = useTranslation();
    const unit = useUnit(series.kind);
    const grid = gridOf(series);
    const vendors = vendorsIn([series], watch);
    const modes = [MID, ...vendors.map((v) => v.key), SPREAD, COMPARE];
    const chosen = modes.includes(mode) ? mode : MID;
    const vendor = watch.vendors.find((v) => v.key === chosen);
    const titleOf = (m: string): string =>
        m === MID
            ? t('marketdata.quotes.mid')
            : m === SPREAD
              ? t('marketdata.quotes.spread')
              : m === COMPARE
                ? t('marketdata.quotes.compare')
                : (watch.vendors.find((v) => v.key === m)?.provider ?? m);
    const hint =
        chosen === MID
            ? t('marketdata.quotes.midHint', { unit })
            : chosen === SPREAD
              ? t('marketdata.quotes.spreadHint', { unit })
              : t('marketdata.quotes.vendorHint', { vendor: vendor?.provider ?? '', unit });
    const pickedPoint = series.points.find((p) => p.uri === point);
    return (
        <section className="min-w-0 space-y-3 rounded-md border border-line p-4">
            <h2 className="text-sm font-semibold">{series.name}</h2>
            <Choice
                label={t('marketdata.quotes.show')}
                options={modes}
                value={chosen}
                titleOf={titleOf}
                onChoose={onMode}
            />
            {chosen === COMPARE ? (
                <SurfaceCompare
                    series={series}
                    watch={watch}
                    comparison={comparison}
                    onCompare={onCompare}
                />
            ) : (
                <>
                    <p className="text-xs text-ink-faint">{hint}</p>
                    <div className={SCROLL}>
                        <table className="w-full text-sm">
                            <thead className="sticky top-0 bg-surface-base">
                                <tr>
                                    <th className={HEAD_LEFT}>{t('marketdata.quotes.tenor')}</th>
                                    {grid.columns.map((c) => (
                                        <th key={c} className={HEAD_RIGHT}>
                                            {c}
                                        </th>
                                    ))}
                                </tr>
                            </thead>
                            <tbody>
                                {grid.rows.map((r) => (
                                    <tr key={r} className={ROW}>
                                        <td className={cx(LEFT_CELL, 'font-mono')}>{r}</td>
                                        {grid.columns.map((c) => {
                                            const p = grid.at(r, c);
                                            if (p === undefined)
                                                return (
                                                    <td
                                                        key={c}
                                                        className={cx(DATA_CELL, 'text-ink-faint')}
                                                    >
                                                        ·
                                                    </td>
                                                );
                                            const agreement = agreementOf(
                                                series.kind,
                                                p,
                                                watch.vendors,
                                                watch.shown,
                                                watch.now,
                                            );
                                            const on = p.uri === point;
                                            if (vendor !== undefined)
                                                return (
                                                    <SurfaceVendorCell
                                                        key={c}
                                                        series={series}
                                                        point={p}
                                                        vendor={vendor}
                                                        agreement={agreement}
                                                        watch={watch}
                                                        on={on}
                                                        onPoint={onPoint}
                                                    />
                                                );
                                            const spread = chosen === SPREAD;
                                            const size =
                                                agreement.spread === null || agreement.mid === null
                                                    ? 0
                                                    : Math.abs(
                                                          differenceOf(
                                                              series.kind,
                                                              agreement.spread,
                                                              agreement.mid,
                                                          ).amount,
                                                      );
                                            return (
                                                <td
                                                    key={c}
                                                    onClick={() => onPoint(p.uri)}
                                                    className={cx(
                                                        DATA_CELL,
                                                        PICKABLE,
                                                        on &&
                                                            'outline-2 -outline-offset-2 outline-accent',
                                                    )}
                                                    style={
                                                        spread
                                                            ? {
                                                                  background: diverging(
                                                                      series.kind,
                                                                      -size,
                                                                      1,
                                                                  ),
                                                              }
                                                            : undefined
                                                    }
                                                >
                                                    {spread
                                                        ? spreadText(series.kind, agreement)
                                                        : valueText(series.kind, agreement.mid)}
                                                </td>
                                            );
                                        })}
                                    </tr>
                                ))}
                            </tbody>
                        </table>
                    </div>
                    {pickedPoint !== undefined && (
                        <div className="space-y-2 border-t border-line pt-3">
                            <h3 className="text-sm font-semibold">{`${series.name} ${pickedPoint.label}`}</h3>
                            <AllQuotes series={series} point={pickedPoint} watch={watch} />
                        </div>
                    )}
                </>
            )}
        </section>
    );
}

function SurfaceVendorCell({
    series,
    point,
    vendor,
    agreement,
    watch,
    on,
    onPoint,
}: {
    readonly series: Series;
    readonly point: Point;
    readonly vendor: Vendor;
    readonly agreement: Agreement;
    readonly watch: Watch;
    readonly on: boolean;
    readonly onPoint: (uri: string) => void;
}): ReactNode {
    const q = point.quotes.get(vendor.key);
    const stale = q !== undefined && isStale(q.at, watch.now);
    const out =
        q !== undefined && !stale && isOutlier(series.kind, Number(q.value), agreement.reference);
    return (
        <td
            onClick={() => onPoint(point.uri)}
            className={cx(
                DATA_CELL,
                PICKABLE,
                stale && 'text-ink-faint',
                out && 'text-warn',
                on && 'outline-2 -outline-offset-2 outline-accent',
            )}
        >
            {q === undefined ? '·' : valueText(series.kind, Number(q.value))}
            {out && ' ⚠'}
        </td>
    );
}

/** A board of series with more coordinates than a grid holds: one row for each point, each vendor a column. */
export function DatumBoard({
    series,
    watch,
}: {
    readonly series: readonly Series[];
    readonly watch: Watch;
}): ReactNode {
    const { t } = useTranslation();
    const vendors = vendorsIn(series, watch);
    return (
        <div className={SCROLL}>
            <table className="w-full text-sm">
                <thead className="sticky top-0 bg-surface-base">
                    <tr>
                        <th className={HEAD_LEFT}>{t('marketdata.quotes.series')}</th>
                        <th className={HEAD_LEFT}>{t('marketdata.quotes.point')}</th>
                        <VendorHeads vendors={vendors} watch={watch} />
                        <th className={HEAD_RIGHT}>{t('marketdata.quotes.mid')}</th>
                    </tr>
                </thead>
                <tbody>
                    {series.flatMap((s) =>
                        s.points.map((p) => {
                            const agreement = agreementOf(
                                s.kind,
                                p,
                                watch.vendors,
                                watch.shown,
                                watch.now,
                            );
                            return (
                                <tr key={p.uri} className={ROW}>
                                    <td className={LEFT_CELL}>{s.name}</td>
                                    <td className={cx(LEFT_CELL, 'font-mono')}>{p.label}</td>
                                    {vendors.map((v) => (
                                        <ValueCell
                                            key={v.key}
                                            kind={s.kind}
                                            point={p}
                                            vendor={v}
                                            agreement={agreement}
                                            now={watch.now}
                                        />
                                    ))}
                                    <td className={DATA_CELL}>
                                        {valueText(s.kind, agreement.mid)}
                                    </td>
                                </tr>
                            );
                        }),
                    )}
                </tbody>
            </table>
        </div>
    );
}
