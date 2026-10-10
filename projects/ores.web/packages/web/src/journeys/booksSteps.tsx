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
 * The steps a book structure walk runs.
 *
 * The person sees the portfolio tree, chooses the portfolio a book sits in or
 * creates one, names the book and gives it the fields finance and the ledger
 * use, sets its status and its three classification axes, reads the rights at
 * the portfolio node, reads the one review, and confirms. The confirm writes the
 * portfolio first when the walk created one, then the book, and stops at the
 * first refusal, which it records against the step to walk back to. A book reads
 * its aggregation currency, its sandbox and its rights from its portfolio and
 * copies none of them.
 *
 * The steps are data, which is why this is a function and not a component.
 */

import { useEffect, useState, type ReactNode } from 'react';
import { Button, Field, Input, Notice, Select, Tag } from '../ui/Primitives.js';
import { HistoryPanel } from '../refdata/HistoryPanel.js';
import { Picker, optionsOf } from './pickers.js';
import {
    BOOK_ENTITY_TYPE,
    ancestryUnits,
    bookProblems,
    writePlan,
    type BookRefusal,
    type BookStructure,
} from './booksState.js';
import type { JourneyStep, StepId } from './runtime.js';
import type {
    BookPickLists,
    BookTree,
    BookWriteOutcome,
    BooksServer,
    PortfolioRights,
} from './booksServer.js';
import type { Book } from '@ores/wire-protocol/generated/refdata/domain/book';
import type { Portfolio } from '@ores/wire-protocol/generated/refdata/domain/portfolio';
import type { Translator } from '../i18n/translate.js';

/** The steps in the order the rail draws them, and the index each one sits at. */
export const BOOK_STEP_IDS = [
    'tree',
    'portfolio',
    'book',
    'classification',
    'rights',
    'review',
    'outcome',
    'history',
] as const;

export function bookStepIndex(id: StepId): number {
    const index = BOOK_STEP_IDS.indexOf(id as (typeof BOOK_STEP_IDS)[number]);
    return index < 0 ? 0 : index;
}

export interface BookStepsInput {
    readonly t: Translator['t'];
    readonly server: BooksServer;
    readonly state: BookStructure;
    /** The tenant's own party, which a written book and portfolio name. */
    readonly partyId: string;
    /** The tree, read once by the page and again after a write. */
    readonly tree: BookTree | undefined;
    readonly pickLists: BookPickLists | undefined;
    readonly pickFailure: string | undefined;
    readonly reasons: readonly {
        readonly code: string;
        readonly description: string;
        readonly requiresCommentary: boolean;
    }[];
    readonly onMove: (index: number) => void;
    /** Reads the tree again, so the outcome and the tree agree. */
    readonly onWritten: () => void;
    readonly onFinished: () => void;
}

/** What was and was not written before the refusal, stated so a partial write is not a surprise. */
function writtenText(t: Translator['t'], refusal: BookRefusal): string {
    return refusal.written.length === 0
        ? t('journey.books.refusal.nothingWritten')
        : t('journey.books.refusal.partial', { calls: refusal.written.join(', ') });
}

function refusalText(t: Translator['t'], refusal: BookRefusal): string {
    const fields = refusal.fields
        .map((failure) => `${failure.field}: ${failure.message}`)
        .join('; ');
    return [
        t('journey.books.refusal.heading'),
        refusal.subject,
        refusal.message,
        fields,
        writtenText(t, refusal),
        t('journey.books.refusal.kept'),
    ]
        .filter((part) => part !== '')
        .join(' ');
}

/** Stands above every step once a book is being shaped, so the record stays in view. */
export function bookHeader(t: Translator['t'], state: BookStructure): ReactNode | undefined {
    if (!state.shaping) {
        return undefined;
    }
    return (
        <div className="flex flex-wrap items-baseline justify-between gap-3 text-sm">
            <span className="truncate font-medium">
                {state.fields.name === '' ? t('journey.books.noName') : state.fields.name}
            </span>
            <span className="flex items-center gap-2">
                <Tag tone="neutral">
                    {state.fields.bookStatus === ''
                        ? t('journey.books.noStatus')
                        : state.fields.bookStatus}
                </Tag>
                <span className="text-xs text-ink-faint">
                    {state.opened === undefined
                        ? t('journey.books.new')
                        : `v${state.opened.version}`}
                </span>
            </span>
        </div>
    );
}

/* --------------------------------------------------------------------- tree */

function TreeStep({
    t,
    tree,
    state,
    pickFailure,
    onMove,
}: {
    readonly t: Translator['t'];
    readonly pickFailure: string | undefined;
    readonly tree: BookTree | undefined;
    readonly state: BookStructure;
    readonly onMove: (index: number) => void;
}): ReactNode {
    const [query, setQuery] = useState('');
    if (tree === undefined) {
        return pickFailure === undefined ? (
            <p className="text-sm text-ink-muted">{t('common.loading')}</p>
        ) : (
            <Notice tone="error">{t('journey.books.readFailed', { message: pickFailure })}</Notice>
        );
    }
    const needle = query.trim().toLowerCase();
    const booksOf = (portfolioId: string): readonly Book[] =>
        tree.books.filter((book) => book.parent_portfolio_id === portfolioId);
    const matches = (name: string): boolean => needle === '' || name.toLowerCase().includes(needle);
    const children = (parent: string | null): readonly Portfolio[] =>
        tree.portfolios.filter((portfolio) => portfolio.parent_portfolio_id === parent);
    const visible = (portfolio: Portfolio): boolean =>
        matches(portfolio.name) ||
        booksOf(portfolio.id).some((book) => matches(book.name)) ||
        children(portfolio.id).some(visible);

    const node = (portfolio: Portfolio, depth: number): ReactNode => (
        <li key={portfolio.id}>
            <div
                className="flex items-center justify-between gap-3 border-b border-line-subtle py-2"
                style={{ paddingLeft: `${String(depth * 1.25)}rem` }}
            >
                <button
                    type="button"
                    className="text-left font-medium hover:text-accent-bright"
                    onClick={() => {
                        state.selectPortfolio(portfolio.id);
                        state.startBook();
                        onMove(bookStepIndex('portfolio'));
                    }}
                >
                    {portfolio.name}
                </button>
                <span className="flex items-center gap-2 text-xs text-ink-faint">
                    <Tag tone="muted">{portfolio.purpose_type}</Tag>
                    {portfolio.aggregation_ccy}
                </span>
            </div>
            <ul>
                {booksOf(portfolio.id)
                    .filter((book) => matches(book.name) || matches(portfolio.name))
                    .map((book) => (
                        <li
                            key={book.id}
                            className="flex items-center justify-between gap-3 border-b border-line-subtle py-2 text-sm"
                            style={{ paddingLeft: `${String((depth + 1) * 1.25)}rem` }}
                        >
                            <button
                                type="button"
                                className="text-left hover:text-accent-bright"
                                onClick={() => {
                                    state.openBook(book);
                                    onMove(bookStepIndex('book'));
                                }}
                            >
                                {book.name}
                            </button>
                            <span className="flex items-center gap-2 text-xs text-ink-faint">
                                <Tag tone={book.book_status === 'Active' ? 'up' : 'muted'}>
                                    {book.book_status}
                                </Tag>
                                {book.regulatory_book_type} · {book.book_purpose_type}
                            </span>
                        </li>
                    ))}
                {children(portfolio.id)
                    .filter(visible)
                    .map((child) => node(child, depth + 1))}
            </ul>
        </li>
    );

    const roots = children(null).filter(visible);
    return (
        <div className="space-y-4">
            <div className="flex flex-wrap items-center gap-3">
                <div className="min-w-64 flex-1">
                    <Input
                        value={query}
                        placeholder={t('journey.books.tree.search')}
                        onChange={(event) => setQuery(event.target.value)}
                    />
                </div>
                <Button
                    variant="primary"
                    onClick={() => {
                        state.startPortfolio('');
                        state.startBook();
                        onMove(bookStepIndex('portfolio'));
                    }}
                >
                    {t('journey.books.tree.newPortfolio')}
                </Button>
            </div>
            {roots.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('journey.books.tree.empty')}</p>
            ) : (
                <ul className="rounded-md border border-line px-3">
                    {roots.map((root) => node(root, 0))}
                </ul>
            )}
            <p className="text-xs text-ink-faint">{t('journey.books.tree.hint')}</p>
        </div>
    );
}

/* ---------------------------------------------------------------- portfolio */

function PortfolioStep({
    t,
    state,
    tree,
    pickLists,
}: {
    readonly t: Translator['t'];
    readonly state: BookStructure;
    readonly tree: BookTree | undefined;
    readonly pickLists: BookPickLists | undefined;
}): ReactNode {
    const portfolios = tree?.portfolios ?? [];
    const pending = state.newPortfolio;
    const chosen = portfolios.find((portfolio) => portfolio.id === state.portfolioId);
    const units = pickLists?.businessUnits ?? [];
    const unitName = (id: string | null): string =>
        id === null ? '-' : (units.find((unit) => unit.id === id)?.unit_name ?? id);
    return (
        <div className="space-y-5">
            <Picker
                label={t('journey.books.portfolio.choose')}
                value={pending === undefined ? state.portfolioId : ''}
                options={portfolios.map((portfolio) => ({
                    value: portfolio.id,
                    label: portfolio.name,
                }))}
                empty={t('journey.books.portfolio.none')}
                onChange={(value) =>
                    value === '' ? state.startPortfolio('') : state.selectPortfolio(value)
                }
            />
            {pending !== undefined && (
                <section className="card space-y-4 p-4">
                    <h3 className="font-semibold">{t('journey.books.portfolio.newTitle')}</h3>
                    <div className="grid gap-4 md:grid-cols-2">
                        <Field label={t('journey.books.portfolio.name')}>
                            <Input
                                value={pending.name}
                                onChange={(event) =>
                                    state.setPortfolioField('name', event.target.value)
                                }
                            />
                        </Field>
                        <Picker
                            label={t('journey.books.portfolio.parent')}
                            value={pending.parentPortfolioId}
                            options={portfolios.map((portfolio) => ({
                                value: portfolio.id,
                                label: portfolio.name,
                            }))}
                            empty={t('journey.books.portfolio.noParent')}
                            onChange={(value) =>
                                state.setPortfolioField('parentPortfolioId', value)
                            }
                        />
                        <Picker
                            label={t('journey.books.portfolio.purpose')}
                            value={pending.purposeType}
                            options={optionsOf(pickLists?.purposeTypes ?? [])}
                            empty={t('journey.books.portfolio.choosePurpose')}
                            onChange={(value) => state.setPortfolioField('purposeType', value)}
                        />
                        <Picker
                            label={t('journey.books.portfolio.aggregation')}
                            value={pending.aggregationCcy}
                            options={(pickLists?.currencies ?? []).map((currency) => ({
                                value: currency.iso_code,
                                label: `${currency.iso_code} ${currency.name}`,
                            }))}
                            empty={t('journey.books.portfolio.chooseCurrency')}
                            onChange={(value) => state.setPortfolioField('aggregationCcy', value)}
                        />
                        <Picker
                            label={t('journey.books.portfolio.ownerUnit')}
                            value={pending.ownerUnitId}
                            options={units.map((unit) => ({
                                value: unit.id,
                                label: unit.unit_name,
                            }))}
                            empty={t('journey.books.portfolio.noOwner')}
                            onChange={(value) => state.setPortfolioField('ownerUnitId', value)}
                        />
                        <label className="flex items-center gap-2 self-end pb-2 text-sm">
                            <input
                                type="checkbox"
                                checked={pending.isVirtual}
                                onChange={(event) =>
                                    state.setPortfolioVirtual(event.target.checked)
                                }
                            />
                            {t('journey.books.portfolio.virtual')}
                        </label>
                    </div>
                    <Button size="sm" variant="ghost" onClick={state.cancelPortfolio}>
                        {t('journey.books.portfolio.cancel')}
                    </Button>
                </section>
            )}
            {chosen !== undefined && (
                <section className="card space-y-2 p-4">
                    <h3 className="font-semibold">{t('journey.books.portfolio.inherits')}</h3>
                    <p className="text-xs text-ink-faint">
                        {t('journey.books.portfolio.inheritsHint')}
                    </p>
                    <dl className="grid gap-2 text-sm md:grid-cols-2">
                        {(
                            [
                                ['aggregation', chosen.aggregation_ccy],
                                ['ownerUnit', unitName(chosen.owner_unit_id)],
                                ['purpose', chosen.purpose_type],
                                ['sandbox', chosen.sandbox_id ?? '-'],
                                ['status', chosen.status],
                            ] as const
                        ).map(([key, value]) => (
                            <div key={key}>
                                <dt className="text-xs text-ink-faint">
                                    {t(`journey.books.portfolio.${key}`)}
                                </dt>
                                <dd>{value}</dd>
                            </div>
                        ))}
                    </dl>
                </section>
            )}
        </div>
    );
}

/* --------------------------------------------------------------------- book */

function BookStep({
    t,
    state,
    tree,
    pickLists,
}: {
    readonly t: Translator['t'];
    readonly state: BookStructure;
    readonly tree: BookTree | undefined;
    readonly pickLists: BookPickLists | undefined;
}): ReactNode {
    const chain = ancestryUnits(state.portfolioId, tree?.portfolios ?? [], state.newPortfolio);
    const units = pickLists?.businessUnits ?? [];
    return (
        <div className="space-y-5">
            {state.opened !== undefined && (
                <Notice tone="info">{t('journey.books.book.nameFixed')}</Notice>
            )}
            <div className="grid gap-4 md:grid-cols-2">
                <Field label={t('journey.books.book.name')}>
                    <Input
                        value={state.fields.name}
                        disabled={state.opened !== undefined}
                        onChange={(event) => state.setField('name', event.target.value)}
                    />
                </Field>
                <Field label={t('journey.books.book.description')}>
                    <Input
                        value={state.fields.description}
                        onChange={(event) => state.setField('description', event.target.value)}
                    />
                </Field>
                <Picker
                    label={t('journey.books.book.currency')}
                    value={state.fields.functionalCurrency}
                    options={(pickLists?.currencies ?? []).map((currency) => ({
                        value: currency.iso_code,
                        label: `${currency.iso_code} ${currency.name}`,
                    }))}
                    empty={t('journey.books.book.chooseCurrency')}
                    onChange={(value) => state.setField('functionalCurrency', value)}
                />
                <Picker
                    label={t('journey.books.book.ratesCentre')}
                    value={state.fields.ratesCentreCode}
                    options={(pickLists?.businessCentres ?? []).map((centre) => ({
                        value: centre.code,
                        label:
                            centre.description === ''
                                ? centre.code
                                : `${centre.code} ${centre.description}`,
                    }))}
                    empty={t('journey.books.book.noRatesCentre')}
                    onChange={(value) => state.setField('ratesCentreCode', value)}
                />
                <Field label={t('journey.books.book.glAccount')}>
                    <Input
                        value={state.fields.glAccountRef}
                        onChange={(event) => state.setField('glAccountRef', event.target.value)}
                    />
                </Field>
                <Field label={t('journey.books.book.costCenter')}>
                    <Input
                        value={state.fields.costCenter}
                        onChange={(event) => state.setField('costCenter', event.target.value)}
                    />
                </Field>
                <Picker
                    label={t('journey.books.book.ownerUnit')}
                    value={state.fields.ownerUnitId}
                    options={units.map((unit) => ({
                        value: unit.id,
                        label: chain.has(unit.id)
                            ? unit.unit_name
                            : t('journey.books.book.outsideChain', { unit: unit.unit_name }),
                    }))}
                    empty={t('journey.books.book.noOwner')}
                    onChange={(value) => state.setField('ownerUnitId', value)}
                />
            </div>
            {state.fields.ownerUnitId !== '' && !chain.has(state.fields.ownerUnitId) && (
                <Notice tone="warn">{t('journey.books.book.ownerOutside')}</Notice>
            )}
        </div>
    );
}

/* ------------------------------------------------------------ classification */

function ClassificationStep({
    t,
    state,
    pickLists,
}: {
    readonly t: Translator['t'];
    readonly state: BookStructure;
    readonly pickLists: BookPickLists | undefined;
}): ReactNode {
    const closing =
        state.opened !== undefined &&
        state.fields.bookStatus !== state.opened.book_status &&
        state.fields.bookStatus.toLowerCase().startsWith('clos');
    return (
        <div className="space-y-5">
            <p className="text-xs text-ink-faint">
                {t('journey.books.classification.independent')}
            </p>
            <div className="grid gap-4 md:grid-cols-2">
                <Picker
                    label={t('journey.books.classification.status')}
                    value={state.fields.bookStatus}
                    options={optionsOf(pickLists?.bookStatuses ?? [])}
                    empty={t('journey.books.classification.choose')}
                    onChange={(value) => state.setField('bookStatus', value)}
                />
                <Picker
                    label={t('journey.books.classification.regulatory')}
                    value={state.fields.regulatoryBookType}
                    options={optionsOf(pickLists?.regulatoryBookTypes ?? [])}
                    empty={t('journey.books.classification.choose')}
                    onChange={(value) => state.setField('regulatoryBookType', value)}
                />
                <Picker
                    label={t('journey.books.classification.purpose')}
                    value={state.fields.bookPurposeType}
                    options={optionsOf(pickLists?.bookPurposeTypes ?? [])}
                    empty={t('journey.books.classification.choose')}
                    onChange={(value) => state.setField('bookPurposeType', value)}
                />
                <Picker
                    label={t('journey.books.classification.ledgerFeed')}
                    value={state.fields.ledgerFeedType}
                    options={optionsOf(pickLists?.ledgerFeedTypes ?? [])}
                    empty={t('journey.books.classification.choose')}
                    onChange={(value) => state.setField('ledgerFeedType', value)}
                />
                <label className="flex items-center gap-2 self-end pb-2 text-sm">
                    <input
                        type="checkbox"
                        checked={state.fields.isSweepable}
                        onChange={(event) => state.setSweepable(event.target.checked)}
                    />
                    {t('journey.books.classification.sweepable')}
                </label>
            </div>
            {closing && (
                <Notice tone="warn">{t('journey.books.classification.closeUnchecked')}</Notice>
            )}
        </div>
    );
}

/* ------------------------------------------------------------------- rights */

function RightsStep({
    t,
    server,
    state,
}: {
    readonly t: Translator['t'];
    readonly server: BooksServer;
    readonly state: BookStructure;
}): ReactNode {
    const [rights, setRights] = useState<PortfolioRights>();
    const [failure, setFailure] = useState<string>();
    const portfolioId = state.portfolioId;
    useEffect(() => {
        if (portfolioId === '') {
            return undefined;
        }
        let cancelled = false;
        const load = async (): Promise<void> => {
            try {
                const read = await server.rightsAt(portfolioId);
                if (!cancelled) {
                    setRights(read);
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
    }, [server, portfolioId]);
    const name = (accountId: string): string =>
        rights?.accounts.find((account) => account.id === accountId)?.username ?? accountId;
    return (
        <div className="space-y-4">
            <Notice tone="info">{t('journey.books.rights.readOnly')}</Notice>
            {portfolioId === '' && (
                <p className="text-sm text-ink-muted">{t('journey.books.rights.newPortfolio')}</p>
            )}
            {failure !== undefined && (
                <Notice tone="error">
                    {t('journey.books.rights.readFailed', { message: failure })}
                </Notice>
            )}
            {rights !== undefined && portfolioId !== '' && (
                <div className="overflow-x-auto rounded-md border border-line">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                <th className="px-3 py-2 font-medium">
                                    {t('journey.books.rights.account')}
                                </th>
                                <th className="px-3 py-2 font-medium">
                                    {t('journey.books.rights.right')}
                                </th>
                            </tr>
                        </thead>
                        <tbody>
                            {rights.rights.length === 0 && (
                                <tr>
                                    <td colSpan={2} className="px-3 py-3 text-ink-muted">
                                        {t('journey.books.rights.none')}
                                    </td>
                                </tr>
                            )}
                            {rights.rights.map((right) => (
                                <tr
                                    key={right.id}
                                    className="border-b border-line-subtle last:border-b-0"
                                >
                                    <td className="px-3 py-2">{name(right.account_id)}</td>
                                    <td className="px-3 py-2 font-mono text-xs">
                                        {right.right_code}
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
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
    readonly state: BookStructure;
    readonly reasons: BookStepsInput['reasons'];
    readonly onMove: (index: number) => void;
}): ReactNode {
    const chosen = reasons.find((reason) => reason.code === state.reasonCode);
    const problems = bookProblems(state);
    return (
        <div className="space-y-5">
            {state.refusal !== undefined && (
                <Notice tone="error">
                    <p className="font-semibold">{t('journey.books.refusal.heading')}</p>
                    <p className="text-sm">{state.refusal.subject}</p>
                    <p className="text-sm">{state.refusal.message}</p>
                    <ul className="text-sm">
                        {state.refusal.fields.map((failure) => (
                            <li key={`${failure.field}:${failure.code}`}>
                                {failure.field}: {failure.message}
                            </li>
                        ))}
                    </ul>
                    <p className="mt-2 text-sm">{writtenText(t, state.refusal)}</p>
                    <p className="text-sm">{t('journey.books.refusal.kept')}</p>
                    <Button
                        size="sm"
                        className="mt-2"
                        onClick={() => onMove(bookStepIndex(state.refusal?.step ?? 'book'))}
                    >
                        {t('journey.books.refusal.walkBack')}
                    </Button>
                </Notice>
            )}
            {problems.length > 0 && (
                <Notice tone="warn">
                    <ul>
                        {problems.map((problem) => (
                            <li key={problem}>{problem}</li>
                        ))}
                    </ul>
                </Notice>
            )}
            {state.changes.length === 0 ? (
                <Notice tone="info">{t('journey.books.review.nothing')}</Notice>
            ) : (
                <div className="overflow-x-auto rounded-md border border-line">
                    <table className="w-full text-left text-sm">
                        <thead>
                            <tr className="border-b border-line text-xs text-ink-muted">
                                {['what', 'before', 'after', 'operation'].map((column) => (
                                    <th key={column} className="px-3 py-2 font-medium">
                                        {t(`journey.books.review.columns.${column}`)}
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
                                    <td className="px-3 py-2">{change.what}</td>
                                    <td className="px-3 py-2 text-ink-muted">{change.before}</td>
                                    <td className="px-3 py-2">{change.after}</td>
                                    <td className="px-3 py-2 font-mono text-xs text-ink-faint">
                                        {change.operation}
                                    </td>
                                </tr>
                            ))}
                        </tbody>
                    </table>
                </div>
            )}
            <div className="grid gap-4 md:grid-cols-2">
                <Field label={t('journey.books.review.reason')}>
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
                    label={t('journey.books.review.commentary')}
                    {...(chosen?.requiresCommentary === true
                        ? { hint: t('journey.books.review.commentaryRequired') }
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
    readonly state: BookStructure;
    readonly onMove: (index: number) => void;
    readonly onFinished: () => void;
}): ReactNode {
    if (state.written === undefined) {
        return <Notice tone="info">{t('journey.books.none')}</Notice>;
    }
    return (
        <div className="space-y-4">
            <Notice tone="success">
                {t('journey.books.outcome.written', {
                    name: state.written.name,
                    version: String(state.written.version),
                })}
            </Notice>
            <div className="grid gap-3 md:grid-cols-3">
                <button
                    type="button"
                    className="card p-4 text-left hover:border-accent"
                    onClick={() => onMove(bookStepIndex('history'))}
                >
                    <span className="font-semibold">{t('journey.books.outcome.history')}</span>
                </button>
                <button
                    type="button"
                    className="card p-4 text-left hover:border-accent"
                    onClick={() => onMove(bookStepIndex('tree'))}
                >
                    <span className="font-semibold">{t('journey.books.outcome.tree')}</span>
                </button>
                <button
                    type="button"
                    className="card p-4 text-left hover:border-accent"
                    onClick={onFinished}
                >
                    <span className="font-semibold">{t('journey.books.outcome.done')}</span>
                </button>
            </div>
        </div>
    );
}

/* ------------------------------------------------------------------ history */

function HistoryStep({
    t,
    state,
}: {
    readonly t: Translator['t'];
    readonly state: BookStructure;
}): ReactNode {
    const book = state.written ?? state.opened;
    if (book === undefined) {
        return <Notice tone="info">{t('journey.books.history.none')}</Notice>;
    }
    return (
        <div className="space-y-4">
            <p className="text-xs text-ink-faint">{t('journey.books.history.noRevert')}</p>
            <HistoryPanel entityType={BOOK_ENTITY_TYPE} entityId={book.name} />
        </div>
    );
}

/* -------------------------------------------------------------- the step list */

export function bookSteps(input: BookStepsInput): readonly JourneyStep<ReactNode>[] {
    const { t, server, state, partyId, tree, pickLists, reasons } = input;
    const problems = bookProblems(state);
    const chosen = reasons.find((reason) => reason.code === state.reasonCode);
    const commentaryMissing = chosen?.requiresCommentary === true && state.commentary.trim() === '';
    const ready =
        state.shaping && state.changes.length > 0 && problems.length === 0 && !commentaryMissing;
    const placed = state.portfolioId !== '' || state.newPortfolio !== undefined;

    const wrote: string[] = [];

    function fail(subject: string, step: string, outcome: BookWriteOutcome<unknown>): never {
        const refusal: BookRefusal = {
            step,
            subject,
            code: outcome.code,
            message: outcome.message,
            fields: outcome.fields,
            written: [...wrote],
        };
        state.recordRefusal(refusal);
        throw new Error(refusalText(t, refusal));
    }

    const confirm = async (): Promise<void> => {
        const plan = writePlan(state, partyId);
        if (plan === undefined) {
            return;
        }
        state.clearRefusal();
        wrote.length = 0;
        if (plan.portfolio !== undefined) {
            const made = await server.writePortfolio(plan.portfolio, null, plan.intent);
            if (!made.success) {
                fail('refdata.v1.portfolios.put', 'portfolio', made);
            }
            wrote.push('refdata.v1.portfolios.put');
            state.portfolioWritten(plan.portfolio.id);
            input.onWritten();
        }
        const book = await server.writeBook(plan.book, plan.version, plan.intent);
        if (!book.success || book.row === undefined) {
            fail('refdata.v1.books.put', 'book', book);
        }
        state.recordWritten(book.row);
        input.onWritten();
    };

    return [
        {
            id: 'tree',
            title: t('journey.books.tree.title'),
            lead: t('journey.books.tree.lead'),
            body: (
                <TreeStep
                    t={t}
                    tree={tree}
                    state={state}
                    pickFailure={input.pickFailure}
                    onMove={input.onMove}
                />
            ),
        },
        {
            id: 'portfolio',
            title: t('journey.books.portfolio.title'),
            lead: t('journey.books.portfolio.lead'),
            body: <PortfolioStep t={t} state={state} tree={tree} pickLists={pickLists} />,
            next: { label: t('common.continue'), enabled: state.shaping && placed },
        },
        {
            id: 'book',
            title: t('journey.books.book.title'),
            lead: t('journey.books.book.lead'),
            body: <BookStep t={t} state={state} tree={tree} pickLists={pickLists} />,
            next: {
                label: t('common.continue'),
                enabled: state.shaping && state.fields.name.trim() !== '',
            },
        },
        {
            id: 'classification',
            title: t('journey.books.classification.title'),
            lead: t('journey.books.classification.lead'),
            body: <ClassificationStep t={t} state={state} pickLists={pickLists} />,
            next: { label: t('common.continue'), enabled: true },
        },
        {
            id: 'rights',
            title: t('journey.books.rights.title'),
            lead: t('journey.books.rights.lead'),
            body: <RightsStep t={t} server={server} state={state} />,
            next: { label: t('common.continue'), enabled: true },
        },
        {
            id: 'review',
            title: t('journey.books.review.title'),
            lead: t('journey.books.review.lead'),
            body: <ReviewStep t={t} state={state} reasons={reasons} onMove={input.onMove} />,
            next: { label: t('journey.books.review.confirm'), enabled: ready, run: confirm },
            final: state.written !== undefined,
        },
        {
            id: 'outcome',
            title: t('journey.books.outcome.title'),
            lead: t('journey.books.outcome.lead'),
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
            title: t('journey.books.history.title'),
            lead: t('journey.books.history.lead'),
            body: <HistoryStep t={t} state={state} />,
            next: { label: t('journey.books.outcome.done'), enabled: true, run: input.onFinished },
        },
    ];
}
