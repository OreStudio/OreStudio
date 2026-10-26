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

import { useQuery } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { useParams, useSearchParams } from 'react-router';
import { conventions, type ConventionDocument, type ConventionTerm } from '../api/conventions.js';
import { useTranslation } from '../i18n/Provider.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { conventionPath, PATHS } from '../shell/paths.js';
import { EditingModeToggle, useEditingMode, type EditingMode } from '../ui/EditingMode.js';
import { ParameterDocuments, ParameterFilters } from '../ui/OreParameter.js';
import { Button, Detail, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { Crumbs } from './shared.js';

/**
 * The record page of one convention, its terms drawn as the ORE element and
 * leaf screen standard draws a leaf: the label from the documentation entry,
 * the value in force, the documentation under the term in Simple or Advanced,
 * and the gaps of the contract in view.
 *
 * Every word comes from the server: the kind's name, class and element, and
 * each term's label, help, default and documentation. The page holds no text
 * about a kind or a term.
 */
export function ConventionRecordPage(): ReactNode {
    const { family = '', id = '' } = useParams();
    const [params] = useSearchParams();
    const party = params.get('party') ?? undefined;
    const { t } = useTranslation();
    const document = useQuery({
        queryKey: ['conventions', family, 'documents', id, party ?? ''],
        queryFn: () => conventions.document(family, id, party),
    });
    const [mode, setMode] = useEditingMode();
    const crumbs = [
        ...useAreaRoot('refdata'),
        { label: t('refdata.conventions.title'), to: PATHS.conventions },
        { label: t(`journey.conventions.families.${family}`), to: conventionPath(family) },
        { label: id },
    ];
    if (document.isError) {
        return (
            <div className="space-y-4">
                <Crumbs parts={crumbs} />
                <Notice tone="error">
                    <span className="mr-3">
                        {t('refdata.conventions.record.readFailed', {
                            message: document.error.message,
                        })}
                    </span>
                    <Button size="sm" onClick={() => void document.refetch()}>
                        {t('refdata.records.retry')}
                    </Button>
                </Notice>
            </div>
        );
    }
    if (document.data === undefined) {
        return (
            <div className="space-y-4">
                <Crumbs parts={crumbs} />
                <p className="text-sm text-ink-muted">{t('common.loading')}</p>
            </div>
        );
    }
    const record = document.data;
    return (
        <div className="space-y-4">
            <Crumbs parts={crumbs} />
            <PageHeader
                title={record.id}
                description={t('refdata.records.lead', {
                    code: record.id,
                    version: String(record.version ?? 0),
                })}
                subdued
                actions={<EditingModeToggle mode={mode} onChange={setMode} words="leaves" />}
            />
            {record.shared && <Notice tone="warn">{t('refdata.conventions.record.shared')}</Notice>}
            <KindCard document={record} mode={mode} />
            <TermsCard terms={record.terms} mode={mode} />
        </div>
    );
}

/** What the kind is, from the kind's own row: its name, its description and its class. */
export function KindCard({
    document,
    mode,
}: {
    readonly document: ConventionDocument;
    readonly mode: EditingMode;
}): ReactNode {
    const { t } = useTranslation();
    const { kind } = document;
    return (
        <section className="card space-y-3 p-4">
            <h2 className="text-sm font-medium">{t('refdata.conventions.record.kind')}</h2>
            <p className="text-sm">{kind.name}</p>
            <p className="text-sm text-ink-muted">{kind.description}</p>
            <dl className="grid gap-x-6 gap-y-2 sm:grid-cols-2">
                <Detail
                    label={t('refdata.conventions.assetClass')}
                    value={
                        kind.assetClass === null
                            ? t('refdata.conventions.classes.other')
                            : t(`refdata.conventions.classes.${kind.assetClass}`)
                    }
                />
                {mode === 'advanced' && (
                    <>
                        <Detail
                            label={t('refdata.conventions.record.code')}
                            value={kind.code}
                            mono
                        />
                        <Detail
                            label={t('refdata.conventions.record.oreElement')}
                            value={kind.oreElement ?? t('refdata.conventions.record.noElement')}
                            mono={kind.oreElement !== null}
                        />
                    </>
                )}
            </dl>
        </section>
    );
}

/** The terms of the convention, as rows with their documentation and their filters. */
function TermsCard({
    terms,
    mode,
}: {
    readonly terms: readonly ConventionTerm[];
    readonly mode: EditingMode;
}): ReactNode {
    const { t } = useTranslation();
    const [setOnly, setSetOnly] = useState(false);
    const [opened, setOpened] = useState<ReadonlySet<string>>(new Set());
    const noun = t('oreParameter.nouns.terms');
    // A composite lays its parts over its central row, so a column name alone may
    // name a term of two entities; the entity and the column together name one.
    const keyOf = (term: ConventionTerm): string => `${term.entity}.${term.column}`;
    const visible = terms.filter((term) => !setOnly || term.value !== null);
    const allOpen = visible.length > 0 && visible.every((term) => opened.has(keyOf(term)));
    const toggle = (key: string): void =>
        setOpened((before) => {
            const next = new Set(before);
            if (!next.delete(key)) {
                next.add(key);
            }
            return next;
        });
    return (
        <section className="card space-y-4 p-4">
            <header className="space-y-1">
                <h2 className="text-sm font-medium">{t('refdata.conventions.record.terms')}</h2>
                <p className="text-sm text-ink-muted">
                    {t('refdata.conventions.record.termsLead')}
                </p>
            </header>
            <div className="flex flex-wrap items-center gap-2">
                <ParameterFilters
                    set={setOnly}
                    onSet={() => setSetOnly((before) => !before)}
                    shown={visible.length}
                    total={terms.length}
                    noun={noun}
                />
                <span className="grow" />
                <Button
                    size="sm"
                    variant="ghost"
                    onClick={() => setOpened(allOpen ? new Set() : new Set(visible.map(keyOf)))}
                >
                    {allOpen
                        ? t('oreParameter.hideAllDocumentation')
                        : t('oreParameter.showAllDocumentation')}
                </Button>
            </div>
            {visible.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('oreParameter.filters.none', { noun })}</p>
            ) : (
                <ul className="divide-y divide-line-subtle">
                    {visible.map((term) => (
                        <TermRow
                            key={keyOf(term)}
                            term={term}
                            mode={mode}
                            open={opened.has(keyOf(term))}
                            onToggle={() => toggle(keyOf(term))}
                        />
                    ))}
                </ul>
            )}
        </section>
    );
}

/** One term: its one label, its marks, the value in force and its documentation. */
function TermRow({
    term,
    mode,
    open,
    onToggle,
}: {
    readonly term: ConventionTerm;
    readonly mode: EditingMode;
    readonly open: boolean;
    readonly onToggle: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const label = term.documented && term.label !== '' ? term.label : term.ore;
    const words = term.choices.find((choice) => choice.value === term.value)?.label;
    return (
        <li className="space-y-2 py-3">
            <div className="flex flex-wrap items-baseline justify-between gap-3">
                <div className="min-w-0">
                    <span className="text-sm font-medium">{label}</span>{' '}
                    {term.required && (
                        <Tag tone="muted" small>
                            {t('oreParameter.required')}
                        </Tag>
                    )}
                </div>
                <div className="text-sm">
                    {term.value === null ? (
                        <span className="text-ink-faint">
                            {term.documented
                                ? t('oreParameter.defaultIs', { text: term.defaultText })
                                : '—'}
                        </span>
                    ) : (
                        <span className="font-mono text-xs">
                            {words !== undefined && words !== term.value
                                ? `${words} (${term.value})`
                                : term.value}
                        </span>
                    )}
                </div>
            </div>
            <ParameterDocuments
                doc={{
                    documented: term.documented,
                    name: term.ore,
                    description: term.description,
                    defaultText: term.defaultText,
                    format: term.format,
                    introducedIn: term.introducedIn,
                    removedIn: term.removedIn,
                    xpath: term.xpath,
                }}
                mode={mode}
                open={open}
                onToggle={onToggle}
            />
        </li>
    );
}
