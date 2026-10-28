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
import { useEffect, useState, type ReactNode } from 'react';
import { useParams, useSearchParams } from 'react-router';
import { useHeld } from '../access/holds.js';
import {
    conventions,
    type ConventionDocument,
    type ConventionTerm,
    type ConventionWriteOutcome,
} from '../api/conventions.js';
import { useTranslation } from '../i18n/Provider.js';
import { useActingPartyId } from '../session/SessionProvider.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { conventionPath, PATHS } from '../shell/paths.js';
import { EditingModeToggle, useEditingMode, type EditingMode } from '../ui/EditingMode.js';
import { ParameterControl } from '../parameters/ParameterControl.js';
import { ParameterRow } from '../parameters/ParameterRow.js';
import { controlOfValueSet, shortDefault } from '../parameters/control.js';
import { ParameterDocuments, ParameterFilters } from '../ui/OreParameter.js';
import { Button, Detail, Notice, PageHeader, Tag } from '../ui/Primitives.js';
import { TabPane } from './parties.js';
import { RecordTimeline } from './RecordTimeline.js';
import { markStale, useRecordTabs } from './records.js';
import { Crumbs, ReasonFields, useReason } from './shared.js';

/** The entity type the refdata service registers a convention's history under. */
const CONVENTION_ENTITY = 'ores.refdata.convention';

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
    /*
     * The convention is read under a party, and the party is the page's, not the
     * tab's: a tab change keeps it so the record stays the one in view.
     */
    const panel = useRecordTabs({
        label: t('refdata.conventions.title'),
        tabs: ['details', 'history'],
        ...(party === undefined ? {} : { keep: { party } }),
    });
    const [seen, setSeen] = useState<ReadonlySet<string>>(() => new Set([panel.tab]));
    useEffect(() => {
        setSeen((before) => (before.has(panel.tab) ? before : new Set(before).add(panel.tab)));
    }, [panel.tab]);
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
            {panel.bar}
            <TabPane name="details" tab={panel.tab} seen={seen}>
                <div className="space-y-4">
                    <KindCard document={record} mode={mode} />
                    <TermsCard document={record} party={party} mode={mode} />
                </div>
            </TabPane>
            <TabPane name="history" tab={panel.tab} seen={seen}>
                <section className="card space-y-4 p-4">
                    <h2 className="text-sm font-medium">{t('refdata.records.tabs.history')}</h2>
                    <RecordTimeline entityType={CONVENTION_ENTITY} entityId={record.id} />
                </section>
            </TabPane>
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

/** The changed terms of an edit: each column whose text differs from the value the convention holds. */
export function changedTerms(
    terms: readonly ConventionTerm[],
    editing: Readonly<Record<string, string>>,
): Readonly<Record<string, string | null>> {
    const changes: Record<string, string | null> = {};
    for (const term of terms) {
        const text = editing[term.column] ?? '';
        if (text !== (term.value ?? '')) {
            changes[term.column] = text === '' ? null : text;
        }
    }
    return changes;
}

/**
 * The terms of the convention, as rows with their documentation and their
 * filters, read in place or changed in place. Save writes the changed terms
 * against the version the page read, with a reason; a refusal the server puts
 * on a term is shown on that term.
 */
function TermsCard({
    document,
    party,
    mode,
}: {
    readonly document: ConventionDocument;
    readonly party: string | undefined;
    readonly mode: EditingMode;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const held = useHeld();
    const { terms } = document;
    const entity = `${document.family.replaceAll('-', '_')}_convention`;
    const acting = useActingPartyId();
    // The service writes under the acting party, so a convention of another party
    // is read here and changed only after switching to it.
    const ours = document.partyId === null || document.partyId === acting;
    const mayWrite = held(`refdata::${entity}s:write`);
    const [setOnly, setSetOnly] = useState(false);
    const [opened, setOpened] = useState<ReadonlySet<string>>(new Set());
    const [editing, setEditing] = useState<Readonly<Record<string, string>> | null>(null);
    const [commentary, setCommentary] = useState('');
    const [refusal, setRefusal] = useState<ConventionWriteOutcome | null>(null);
    const noun = t('oreParameter.nouns.terms');
    // A composite lays its parts over its central row, so a column name alone may
    // name a term of two entities; the entity and the column together name one.
    const keyOf = (term: ConventionTerm): string => `${term.entity}.${term.column}`;
    const changes = editing === null ? {} : changedTerms(terms, editing);
    const changed = Object.keys(changes);
    const reason = useReason('amend', changed.length > 0);
    const missing = reason.needsCommentary && commentary.trim() === '';
    const refusedOf = new Map((refusal?.fields ?? []).map((field) => [field.field, field.message]));
    // A filter never hides a problem: a refused term stays in view.
    const visible = terms.filter(
        (term) => !setOnly || term.value !== null || refusedOf.has(term.column),
    );
    const allOpen = visible.length > 0 && visible.every((term) => opened.has(keyOf(term)));
    const toggle = (key: string): void =>
        setOpened((before) => {
            const next = new Set(before);
            if (!next.delete(key)) {
                next.add(key);
            }
            return next;
        });
    // The page has no live feed of convention changes, so the read is refetched
    // after a save rather than only marked stale.
    const reread = (): Promise<void> =>
        queries.invalidateQueries({
            queryKey: ['conventions', document.family, 'documents', document.id],
        });
    const leave = (): void => {
        setEditing(null);
        setCommentary('');
        setRefusal(null);
    };
    const save = useMutation({
        mutationFn: () =>
            conventions.saveTerms(
                document.family,
                document.id,
                party,
                document.version ?? 0,
                changes,
                { reason_code: reason.code, commentary: commentary.trim() },
            ),
        onSuccess: (outcome) => {
            if (!outcome.success) {
                setRefusal(outcome);
                return;
            }
            markStale(queries, [['conventions', document.family]]);
            void reread();
            leave();
        },
    });
    const conflict = refusal?.conflict === true;
    const ready = changed.length > 0 && !missing && reason.code !== '' && !conflict;
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
                {mayWrite && document.editable && ours && editing === null && (
                    <Button
                        size="sm"
                        icon="edit"
                        onClick={() =>
                            setEditing(
                                Object.fromEntries(
                                    terms.map((term) => [term.column, term.value ?? '']),
                                ),
                            )
                        }
                    >
                        {t('refdata.conventions.record.edit')}
                    </Button>
                )}
            </div>
            {mayWrite && !document.editable && (
                <p className="text-xs text-ink-faint">
                    {t('refdata.conventions.record.notEditable')}
                </p>
            )}
            {mayWrite && document.editable && !ours && (
                <p className="text-xs text-ink-faint">
                    {t('refdata.conventions.record.otherParty')}
                </p>
            )}
            {visible.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('oreParameter.filters.none', { noun })}</p>
            ) : (
                <ul className="divide-y divide-line-subtle">
                    {visible.map((term) => (
                        <TermRow
                            key={keyOf(term)}
                            term={term}
                            mode={mode}
                            text={editing === null ? null : (editing[term.column] ?? '')}
                            changed={term.column in changes}
                            refused={refusedOf.get(term.column) ?? null}
                            open={opened.has(keyOf(term))}
                            onToggle={() => toggle(keyOf(term))}
                            onText={(text) =>
                                editing !== null && setEditing({ ...editing, [term.column]: text })
                            }
                            onFocus={() => setOpened((before) => new Set(before).add(keyOf(term)))}
                        />
                    ))}
                </ul>
            )}
            {editing !== null && (
                <div className="space-y-3 border-t border-line pt-4">
                    {conflict && (
                        <Notice tone="error">
                            <span className="mr-2">{t('refdata.conventions.record.conflict')}</span>
                            <Button
                                size="sm"
                                onClick={() => {
                                    leave();
                                    void reread();
                                }}
                            >
                                {t('refdata.conventions.record.reload')}
                            </Button>
                        </Notice>
                    )}
                    {refusal !== null && !conflict && refusal.fields.length === 0 && (
                        <Notice tone="error">{refusal.message}</Notice>
                    )}
                    {save.isError && <Notice tone="error">{save.error.message}</Notice>}
                    <ReasonFields
                        reason={reason}
                        commentary={commentary}
                        onCommentary={setCommentary}
                        missing={missing}
                    />
                    <div className="flex flex-wrap items-center gap-2">
                        <Button
                            variant="primary"
                            icon="save"
                            disabled={!ready}
                            pending={save.isPending}
                            onClick={() => {
                                setRefusal(null);
                                save.mutate();
                            }}
                        >
                            {t('refdata.conventions.record.save')}
                        </Button>
                        <Button variant="ghost" icon="cancel" onClick={leave}>
                            {t('common.cancel')}
                        </Button>
                        <span className="text-xs text-ink-faint" aria-live="polite">
                            {changed.length === 0
                                ? t('oreParameter.noChanges')
                                : changed.length === 1
                                  ? t('oreParameter.oneChange')
                                  : t('oreParameter.changes', { count: String(changed.length) })}
                        </span>
                    </div>
                </div>
            )}
        </section>
    );
}

/**
 * One term: its one label, its marks, and the value in force or, in edit, the
 * shared control its value set calls for; then its documentation.
 */
export function TermRow({
    term,
    mode,
    text,
    changed,
    refused,
    open,
    onToggle,
    onText,
    onFocus,
}: {
    readonly term: ConventionTerm;
    readonly mode: EditingMode;
    /** The text of the control while the terms are being changed, else null. */
    readonly text: string | null;
    readonly changed: boolean;
    /** The server's words when it refused this term's value. */
    readonly refused: string | null;
    readonly open: boolean;
    readonly onToggle: () => void;
    readonly onText: (text: string) => void;
    readonly onFocus: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const label = term.documented && term.label !== '' ? term.label : term.ore;
    const words = term.choices.find((choice) => choice.value === term.value)?.label;
    const standard = shortDefault(term.defaultText);
    const problemId = `refused-${term.entity}-${term.column}`;
    const marks = (
        <>
            {term.required && (
                <Tag tone="muted" small>
                    {t('oreParameter.required')}
                </Tag>
            )}
            {changed && (
                <Tag tone="accent" small>
                    {t('oreParameter.changed')}
                </Tag>
            )}
        </>
    );
    return (
        <li className={changed ? 'space-y-2 bg-accent/5 py-3' : 'space-y-2 py-3'}>
            {text === null ? (
                <div className="flex flex-wrap items-baseline justify-between gap-3">
                    <div className="min-w-0">
                        <span className="text-sm font-medium">{label}</span> {marks}
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
            ) : (
                <>
                    {(term.required || changed) && (
                        <div className="flex flex-wrap items-baseline gap-2">{marks}</div>
                    )}
                    <ParameterRow label={label} {...(term.help === '' ? {} : { hint: term.help })}>
                        <ParameterControl
                            model={controlOfValueSet(term)}
                            label={label}
                            value={text}
                            unset={t('oreParameter.defaultOption', {
                                value: standard === '' ? '—' : standard,
                            })}
                            invalid={refused !== null}
                            {...(refused === null ? {} : { describedBy: problemId })}
                            onFocus={onFocus}
                            onChange={onText}
                        />
                    </ParameterRow>
                    {refused !== null && (
                        <p id={problemId} role="alert" className="mt-1 text-xs text-down">
                            {refused}
                        </p>
                    )}
                </>
            )}
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
