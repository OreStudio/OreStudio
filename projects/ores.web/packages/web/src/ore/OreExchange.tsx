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

import { useMutation, useQueryClient } from '@tanstack/react-query';
import { useState, type ReactNode } from 'react';
import { useNavigate } from 'react-router';
import { api, type OreImportResult, type OrePreview } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { JourneyPage } from '../journeys/JourneyPage.js';
import type { JourneyStep } from '../journeys/runtime.js';
import { markStale } from '../refdata/records.js';
import { Crumbs, ReasonFields, useReason } from '../refdata/shared.js';
import { useAreaRoot, type AreaName } from '../shell/AreaTrail.js';
import { Button, Dialog, LinkButton, Notice, PageHeader } from '../ui/Primitives.js';
import { Icon } from '../ui/Icon.js';
import { toast } from '../ui/Toasts.js';

/**
 * A kind that ORE reads as one document declares it once: the record resource
 * the pair acts on, the document's file name and root element, and the list
 * the import returns to. Everything the pair draws comes from this
 * (RS-ORE-01, RS-ORE-07).
 */
export interface OreDocumentKind {
    readonly resource: string;
    readonly fileName: string;
    readonly rootElement: string;
    readonly listPath: string;
    readonly importPath: string;
    /** The key of the kind's words: its plural, and the lead of its import. */
    readonly words: string;
}

/**
 * The standard pair on a list header: Export, which any reader may take, and
 * Import, for a writer, between Refresh and Add (RS-ORE-02, RS-ORE-03).
 */
export function OreExchangeActions({
    kind,
    mayWrite,
}: {
    readonly kind: OreDocumentKind;
    readonly mayWrite: boolean;
}): ReactNode {
    const { t } = useTranslation();
    const [exporting, setExporting] = useState(false);
    return (
        <>
            {mayWrite && (
                <LinkButton to={kind.importPath}>
                    <Icon name="import" />
                    {t('refdata.ore.import')}
                </LinkButton>
            )}
            <Button icon="export" onClick={() => setExporting(true)}>
                {t('refdata.ore.export')}
            </Button>
            {exporting && <ExportDialog kind={kind} onClose={() => setExporting(false)} />}
        </>
    );
}

/** Hands a document the server wrote to the browser, under the name the server gave. */
function save(name: string, content: string): void {
    const url = URL.createObjectURL(new Blob([content], { type: 'application/xml' }));
    const link = document.createElement('a');
    link.href = url;
    link.download = name;
    link.click();
    URL.revokeObjectURL(url);
}

/**
 * The Export operation dialog: it names the document and its scope, writes on
 * one press, and says what was saved (RS-ORE-04).
 */
function ExportDialog({
    kind,
    onClose,
}: {
    readonly kind: OreDocumentKind;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const run = useMutation({
        mutationFn: async () => {
            const result = await api.oreDocument(kind.resource);
            for (const file of result.files) {
                save(file.name, file.content);
            }
            return result;
        },
    });
    return (
        <Dialog
            title={t('refdata.ore.export')}
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" icon="cancel" onClick={onClose}>
                        {run.isSuccess ? t('refdata.ore.close') : t('refdata.records.cancel')}
                    </Button>
                    <Button
                        variant="primary"
                        icon="export"
                        disabled={run.isSuccess}
                        pending={run.isPending}
                        onClick={() => run.mutate()}
                    >
                        {t('refdata.ore.export')}
                    </Button>
                </>
            }
        >
            <div className="space-y-3">
                <p className="text-sm text-ink-muted">
                    {t('refdata.ore.exportLead', {
                        file: kind.fileName,
                        plural: t(`${kind.words}.title`).toLowerCase(),
                    })}
                </p>
                <p className="text-xs text-ink-faint">{t('refdata.ore.exportNote')}</p>
                {run.isSuccess && (
                    <Notice tone="success">
                        {t('refdata.ore.exported', {
                            file: run.data.files.map((file) => file.name).join(', '),
                            count: String(run.data.count),
                        })}
                    </Notice>
                )}
                {run.isError && <Notice tone="error">{run.error.message}</Notice>}
            </div>
        </Dialog>
    );
}

/**
 * The document one record is written as: the name the file types table gives it,
 * what it holds in words, and the write the server answers with (RS-ORE-02).
 */
export interface OreRecordDocument {
    /** The name the table gives the document, for the sentence before the press. */
    readonly fileName: string;
    /** What the document holds, in words, such as "5 products". */
    readonly what: string;
    /** Asks the server to write the document and answers the file it names. */
    readonly write: () => Promise<{ readonly name: string; readonly content: string }>;
}

/**
 * The Export of a kind whose document is one record, on the record header beside
 * Edit, drawn only for a person who may read the record (RS-ORE-02).
 */
export function OreRecordExport({ document }: { readonly document: OreRecordDocument }): ReactNode {
    const { t } = useTranslation();
    const [exporting, setExporting] = useState(false);
    return (
        <>
            <Button icon="export" onClick={() => setExporting(true)}>
                {t('refdata.ore.export')}
            </Button>
            {exporting && (
                <RecordExportDialog document={document} onClose={() => setExporting(false)} />
            )}
        </>
    );
}

/**
 * The Export dialog of one record: it states what it will write and its scope,
 * writes on one press, and saves the file under the name the server gave
 * (RS-ORE-03, RS-ORE-04). Nothing is written in the browser.
 */
export function RecordExportDialog({
    document,
    onClose,
}: {
    readonly document: OreRecordDocument;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const [saved, setSaved] = useState<readonly string[]>([]);
    const run = useMutation({
        mutationFn: async () => {
            const file = await document.write();
            save(file.name, file.content);
            return file;
        },
        onSuccess: (file) => setSaved([file.name]),
    });
    return (
        <Dialog
            title={t('refdata.ore.export')}
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" icon="cancel" onClick={onClose}>
                        {run.isSuccess ? t('refdata.ore.close') : t('refdata.records.cancel')}
                    </Button>
                    <Button
                        variant="primary"
                        icon="export"
                        disabled={run.isSuccess}
                        pending={run.isPending}
                        onClick={() => run.mutate()}
                    >
                        {t('refdata.ore.export')}
                    </Button>
                </>
            }
        >
            <div className="space-y-3">
                <p className="text-sm text-ink-muted">
                    {t('refdata.ore.recordExportLead', {
                        file: document.fileName,
                        what: document.what,
                    })}
                </p>
                <p className="text-xs text-ink-faint">{t('refdata.ore.exportNote')}</p>
                {run.isSuccess && (
                    <Notice tone="success">
                        {t('refdata.ore.recordSaved', {
                            file: saved.join(', '),
                            what: document.what,
                        })}
                    </Notice>
                )}
                {run.isError && <Notice tone="error">{run.error.message}</Notice>}
            </div>
        </Dialog>
    );
}

/** One row of the difference, as the review draws it. */
export interface OreImportReviewRow {
    /** The row's identity within its configuration; a choice names it. */
    readonly key: string;
    /** One of added, changed, removed. */
    readonly change: string;
    /** The row as the review names it; the key when the kind has no better word. */
    readonly name: string;
    /** A second name the kind reads the row by, such as a currency's name. */
    readonly detail?: string;
    readonly what: string;
}

/** One thing the file states and the system does not know. */
export interface OreImportIssue {
    readonly kind: string;
    readonly subject: string;
    readonly detail: string;
}

/** What the file holds, as the second step states it. */
export interface OreImportHolds {
    readonly counts: readonly {
        readonly id: string;
        readonly label: string;
        readonly value: number;
    }[];
    /** The products or records the file carries, one line each. */
    readonly lines: readonly string[];
    /** What the counts mean, in the kind's own words. */
    readonly note: string;
}

/** A configuration a file may be written into (RS-ORE-05, step 4). */
export interface OreImportTarget {
    readonly id: string;
    readonly name: string;
    readonly detail: string;
}

/** What the server answers a preview with, in the words the steps read. */
export interface OreImportPreview {
    readonly success: boolean;
    readonly message: string;
    /** The configuration the file was compared against, empty for none. */
    readonly targetName: string;
    readonly holds: OreImportHolds;
    readonly rows: readonly OreImportReviewRow[];
    readonly issues: readonly OreImportIssue[];
    /** The difference in one line, for the review. */
    readonly summary: string;
}

/** What the review offers for a row, the first being the default. */
export interface OreImportChoice {
    readonly id: string;
    readonly label: string;
}

/** Where a file is put: a configuration that exists, or one the import makes. */
export interface OreImportWriteTarget {
    readonly id: string | null;
    readonly newName: string;
    readonly newVariant: string;
}

/** What an import answers. */
export interface OreImportDone {
    readonly success: boolean;
    readonly message: string;
    /** The reasons the write gave, one per row it could not write. */
    readonly errors: readonly string[];
    readonly summary: string;
}

/**
 * A kind that ORE reads as one document, as the Import steps see it: the
 * document it accepts, the configurations a file may be written into, what the
 * file holds, and the two server operations behind the steps (RS-ORE-01,
 * RS-ORE-05, RS-ORE-07).
 *
 * The words are the kind's own, so the shared dialog states a configuration's
 * products where the currencies' states currencies, and no screen writes a
 * dialog of its own.
 */
export interface OreImportKind {
    /** The key of the kind's words: its plural. */
    readonly words: string;
    /** The area the kind's screens sit under, which the trail starts from. */
    readonly area: AreaName;
    readonly rootElement: string;
    readonly listPath: string;
    /**
     * What a file is written into: a configuration the person chooses, or the
     * kind's table, which is the whole document and leaves the steps at four
     * (RS-ORE-05).
     */
    readonly targetKind: 'configuration' | 'table';
    /** The configurations a file may be written into; empty until they are read. */
    readonly targets?: readonly OreImportTarget[];
    readonly preview: (content: string, targetId: string | null) => Promise<OreImportPreview>;
    /** The choices the review offers for one row; none where the file decides. */
    readonly choicesFor: (row: OreImportReviewRow) => readonly OreImportChoice[];
    readonly write: (request: {
        readonly content: string;
        readonly target: OreImportWriteTarget;
        readonly choices: Readonly<Record<string, string>>;
        readonly intent: { readonly reason_code: string; readonly commentary: string };
    }) => Promise<OreImportDone>;
}

/**
 * The Import steps of a kind, at their own address: the file, what it holds,
 * what the system does not know, the target where the kind has one, and the
 * difference with the review. The parse is the server's, and nothing is written
 * before the last step (RS-ORE-05, RS-ORE-06).
 */
export function OreImportPage({ kind }: { readonly kind: OreImportKind }): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const queries = useQueryClient();
    const crumbs = [
        ...useAreaRoot(kind.area),
        { label: t(`${kind.words}.title`), to: kind.listPath },
        { label: t('refdata.ore.import') },
    ];
    const [at, setAt] = useState(0);
    const [file, setFile] = useState<{ readonly name: string; readonly content: string }>();
    const [preview, setPreview] = useState<OreImportPreview>();
    const [choices, setChoices] = useState<Readonly<Record<string, string>>>({});
    const [commentary, setCommentary] = useState('');
    const reason = useReason('amend', true);
    const missing = reason.needsCommentary && commentary.trim() === '';

    // The target: a configuration the file would create, which writes nothing of
    // the tenant's, or one it would change in place.
    const [existing, setExisting] = useState(false);
    const [targetId, setTargetId] = useState('');
    const [newName, setNewName] = useState('');
    const [newVariant, setNewVariant] = useState('');
    const targets = kind.targets ?? [];
    const picksTarget = kind.targetKind === 'configuration';

    const choose = async (event: React.ChangeEvent<HTMLInputElement>): Promise<void> => {
        const picked = event.target.files?.[0];
        if (picked === undefined) {
            return;
        }
        setPreview(undefined);
        setChoices({});
        setFile({ name: picked.name, content: await picked.text() });
    };
    const choiceOf = (row: OreImportReviewRow): string =>
        choices[row.key] ?? kind.choicesFor(row)[0]?.id ?? '';
    // A new configuration needs a name; an existing one needs to be chosen.
    const targetComplete = !picksTarget || (existing ? targetId !== '' : newName.trim() !== '');

    const steps: readonly JourneyStep<ReactNode>[] = [
        {
            id: 'file',
            title: t('refdata.ore.steps.file'),
            lead: t('refdata.ore.steps.fileLead', { root: kind.rootElement }),
            body: (
                <div className="space-y-3">
                    <input
                        type="file"
                        accept=".xml,application/xml,text/xml"
                        onChange={(event) => void choose(event)}
                        className="block w-full text-sm text-ink-muted"
                    />
                    {file !== undefined && (
                        <p className="text-sm">
                            <span className="font-mono">{file.name}</span>
                            <span className="text-ink-faint">
                                {' · '}
                                {t('refdata.ore.fileSize', {
                                    kb: String(Math.max(1, Math.round(file.content.length / 1024))),
                                })}
                            </span>
                        </p>
                    )}
                </div>
            ),
            next: {
                label: t('refdata.ore.next'),
                enabled: file !== undefined,
                run: async () => {
                    if (file === undefined) {
                        return;
                    }
                    // The first reading states what the file holds and what the
                    // system does not know; the difference waits for the target.
                    const result = await kind.preview(file.content, null);
                    if (!result.success) {
                        throw new Error(result.message);
                    }
                    setPreview(result);
                },
            },
        },
        {
            id: 'holds',
            title: t('refdata.ore.steps.holds'),
            lead: t('refdata.ore.steps.holdsLead'),
            body: preview === undefined ? null : <OreImportHoldsBody holds={preview.holds} />,
            next: { label: t('refdata.ore.next'), enabled: preview !== undefined },
        },
        {
            id: 'unknown',
            title: t('refdata.ore.steps.unknown'),
            lead: t('refdata.ore.steps.unknownLead'),
            body:
                preview === undefined ? null : (
                    <OreImportUnknownBody
                        issues={preview.issues}
                        nothingUnknown={t('refdata.ore.nothingUnknown')}
                    />
                ),
            next: { label: t('refdata.ore.next'), enabled: preview !== undefined },
        },
        ...(!picksTarget
            ? []
            : [
                  {
                      id: 'target',
                      title: t('refdata.ore.steps.target'),
                      lead: t('refdata.ore.steps.targetLead'),
                      body: (
                          <OreImportTargetBody
                              targets={targets}
                              existing={existing}
                              onExisting={setExisting}
                              targetId={targetId}
                              onTargetId={setTargetId}
                              name={newName}
                              onName={setNewName}
                              variant={newVariant}
                              onVariant={setNewVariant}
                          />
                      ),
                      next: {
                          label: t('refdata.ore.next'),
                          enabled: targetComplete,
                          run: async () => {
                              if (file === undefined) {
                                  return;
                              }
                              // The difference is read against the target the
                              // person chose, so the target is decided before it.
                              const result = await kind.preview(
                                  file.content,
                                  existing ? targetId : null,
                              );
                              if (!result.success) {
                                  throw new Error(result.message);
                              }
                              setPreview(result);
                          },
                      },
                  },
              ]),
        {
            id: 'review',
            title: t('refdata.ore.steps.review'),
            lead: t('refdata.ore.steps.reviewLead'),
            body:
                preview === undefined ? null : (
                    <OreImportReviewBody
                        kind={kind}
                        preview={preview}
                        choices={choices}
                        onChoice={(key, id) => setChoices({ ...choices, [key]: id })}
                        existing={existing}
                        newName={newName}
                        reason={reason}
                        commentary={commentary}
                        onCommentary={setCommentary}
                        missing={missing}
                    />
                ),
            next: {
                label: t('refdata.ore.import'),
                enabled: preview !== undefined && !missing && reason.code !== '' && targetComplete,
                run: async () => {
                    if (file === undefined || preview === undefined) {
                        return;
                    }
                    const result = await kind.write({
                        content: file.content,
                        target: {
                            id: existing && targetId !== '' ? targetId : null,
                            newName: newName.trim(),
                            newVariant: newVariant.trim(),
                        },
                        choices,
                        intent: { reason_code: reason.code, commentary: commentary.trim() },
                    });
                    markStale(queries, [['records'], ['history'], ['timeline']]);
                    if (!result.success) {
                        throw new Error(
                            [result.message, ...result.errors]
                                .filter((line) => line !== '')
                                .join('\n'),
                        );
                    }
                    toast(result.summary, 'info');
                    void navigate(kind.listPath);
                },
            },
            final: true,
        },
    ];

    return (
        <div className="space-y-4">
            <div>
                <Crumbs parts={crumbs} />
                <PageHeader
                    title={t('refdata.ore.importTitle', {
                        plural: t(`${kind.words}.title`).toLowerCase(),
                    })}
                    description={t('refdata.ore.importLead')}
                />
            </div>
            <JourneyPage steps={steps} at={at} onMove={setAt} />
        </div>
    );
}

/**
 * What the file holds: the counts the server states, then the products or
 * records it carries, one line each, so the second step is a reading and not a
 * promise (RS-ORE-05, step 2).
 */
export function OreImportHoldsBody({ holds }: { readonly holds: OreImportHolds }): ReactNode {
    return (
        <div className="space-y-3">
            <div className="grid gap-3 sm:grid-cols-3 lg:grid-cols-5">
                {holds.counts.map((count) => (
                    <div key={count.id} className="rounded-md border border-line p-3">
                        <p className="text-xl font-semibold tabular-nums">{count.value}</p>
                        <p className="text-xs text-ink-muted">{count.label}</p>
                    </div>
                ))}
            </div>
            {holds.lines.length > 0 && (
                <ul className="space-y-1 text-sm text-ink-muted">
                    {holds.lines.map((line, index) => (
                        <li key={`${line}-${String(index)}`}>{line}</li>
                    ))}
                </ul>
            )}
            <p className="text-xs text-ink-faint">{holds.note}</p>
        </div>
    );
}

/**
 * What the system does not know, one list for each kind of thing, and each list
 * only when it holds something: a type with no vocabulary row, an element the
 * tables cannot hold, a row the file states twice (RS-ORE-05, step 3).
 */
export function OreImportUnknownBody({
    issues,
    nothingUnknown = 'Everything in the file is known.',
}: {
    readonly issues: readonly OreImportIssue[];
    readonly nothingUnknown?: string;
}): ReactNode {
    const { t } = useTranslation();
    if (issues.length === 0) {
        return <Notice tone="success">{nothingUnknown}</Notice>;
    }
    return (
        <div className="space-y-4">
            {[...new Set(issues.map((issue) => issue.kind))].map((kind) => (
                <section key={kind} className="space-y-2">
                    <h3 className="text-sm font-medium">{t(`refdata.ore.issues.${kind}`)}</h3>
                    <p className="text-xs text-ink-muted">{t(`refdata.ore.issues.${kind}Lead`)}</p>
                    <table className="w-full text-sm">
                        <tbody>
                            {issues
                                .filter((issue) => issue.kind === kind)
                                .map((issue, index) => (
                                    <tr
                                        key={`${issue.subject}-${String(index)}`}
                                        className="border-t border-line-subtle"
                                    >
                                        <td className="py-2 pr-4 font-mono text-xs">
                                            {issue.subject}
                                        </td>
                                        <td className="py-2 text-ink-muted">{issue.detail}</td>
                                    </tr>
                                ))}
                        </tbody>
                    </table>
                </section>
            ))}
        </div>
    );
}

/**
 * The target step: a configuration the file would create, which changes nothing
 * that exists, or one it would change in place (RS-ORE-05, step 4).
 */
export function OreImportTargetBody({
    targets,
    existing,
    onExisting,
    targetId,
    onTargetId,
    name,
    onName,
    variant,
    onVariant,
}: {
    readonly targets: readonly OreImportTarget[];
    readonly existing: boolean;
    readonly onExisting: (value: boolean) => void;
    readonly targetId: string;
    readonly onTargetId: (value: string) => void;
    readonly name: string;
    readonly onName: (value: string) => void;
    readonly variant: string;
    readonly onVariant: (value: string) => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <div className="space-y-4">
            <div className="space-y-2">
                <label className="flex items-center gap-2 text-sm">
                    <input
                        type="radio"
                        name="ore-target"
                        checked={!existing}
                        onChange={() => onExisting(false)}
                    />
                    {t('refdata.ore.steps.targetNew')}
                </label>
                {!existing && (
                    <div className="ml-6 grid gap-3 sm:grid-cols-2">
                        <label className="space-y-1 text-xs text-ink-muted">
                            {t('refdata.ore.steps.targetName')}
                            <input
                                value={name}
                                onChange={(event) => onName(event.target.value)}
                                className="block w-full rounded-md border border-line px-2 py-1 text-sm text-ink"
                            />
                        </label>
                        <label className="space-y-1 text-xs text-ink-muted">
                            {t('refdata.ore.steps.targetVariant')}
                            <input
                                value={variant}
                                onChange={(event) => onVariant(event.target.value)}
                                className="block w-full rounded-md border border-line px-2 py-1 text-sm text-ink"
                            />
                        </label>
                    </div>
                )}
            </div>
            <div className="space-y-2">
                <label className="flex items-center gap-2 text-sm">
                    <input
                        type="radio"
                        name="ore-target"
                        checked={existing}
                        onChange={() => onExisting(true)}
                    />
                    {t('refdata.ore.steps.targetExisting')}
                </label>
                {existing && (
                    <select
                        value={targetId}
                        onChange={(event) => onTargetId(event.target.value)}
                        className="ml-6 block rounded-md border border-line px-2 py-1 text-sm"
                    >
                        <option value="">{t('refdata.ore.steps.targetChoose')}</option>
                        {targets.map((target) => (
                            <option key={target.id} value={target.id}>
                                {target.detail === ''
                                    ? target.name
                                    : `${target.name} · ${target.detail}`}
                            </option>
                        ))}
                    </select>
                )}
            </div>
            <p className="text-xs text-ink-faint">{t('refdata.ore.steps.targetNote')}</p>
        </div>
    );
}

/**
 * The difference and the review: the removed rows first, each with the choice
 * the review offers and Keep it first, the reach of the write, and the reason
 * and commentary the write is made with (RS-ORE-05, step 5).
 */
export function OreImportReviewBody({
    kind,
    preview,
    choices,
    onChoice,
    existing,
    newName,
    reason,
    commentary,
    onCommentary,
    missing,
}: {
    readonly kind: OreImportKind;
    readonly preview: OreImportPreview;
    readonly choices: Readonly<Record<string, string>>;
    readonly onChoice: (key: string, choice: string) => void;
    readonly existing: boolean;
    readonly newName: string;
    readonly reason: ReturnType<typeof useReason>;
    readonly commentary: string;
    readonly onCommentary: (value: string) => void;
    readonly missing: boolean;
}): ReactNode {
    const { t } = useTranslation();
    const choiceOf = (row: OreImportReviewRow): string =>
        choices[row.key] ?? kind.choicesFor(row)[0]?.id ?? '';
    const order = ['removed', 'changed', 'added'];
    return (
        <div className="space-y-4">
            <TargetSentence
                hasTargets={kind.targetKind === 'configuration'}
                existing={existing}
                targetName={preview.targetName}
                newName={newName}
            />
            {preview.rows.length === 0 ? (
                <Notice tone="info">{t('refdata.ore.noDifference')}</Notice>
            ) : (
                <>
                    {preview.summary !== '' && (
                        <p className="text-sm text-ink-muted">{preview.summary}</p>
                    )}
                    <table className="w-full text-sm">
                        <thead>
                            <tr className="text-left text-xs text-ink-muted">
                                <th className="py-2 pr-4">{t('refdata.ore.change')}</th>
                                <th className="py-2 pr-4">{t('refdata.ore.record')}</th>
                                <th className="py-2 pr-4">{t('refdata.ore.what')}</th>
                                <th className="py-2">{t('refdata.ore.choice')}</th>
                            </tr>
                        </thead>
                        <tbody>
                            {[...preview.rows]
                                .sort((a, b) => order.indexOf(a.change) - order.indexOf(b.change))
                                .map((row) => (
                                    <tr key={row.key} className="border-t border-line-subtle">
                                        <td className="py-2 pr-4">
                                            {t(`refdata.ore.changes.${row.change}`)}
                                        </td>
                                        <td className="py-2 pr-4 font-mono text-xs">
                                            {row.name === '' ? row.key : row.name}
                                        </td>
                                        <td className="py-2 pr-4 text-ink-muted">
                                            {row.detail === undefined
                                                ? row.what
                                                : `${row.detail} · ${row.what}`}
                                        </td>
                                        <td className="py-2">
                                            <span className="flex flex-wrap gap-3">
                                                {kind.choicesFor(row).map((choice) => (
                                                    <label
                                                        key={choice.id}
                                                        className="flex items-center gap-1"
                                                    >
                                                        <input
                                                            type="radio"
                                                            name={`choice-${row.key}`}
                                                            checked={choiceOf(row) === choice.id}
                                                            onChange={() =>
                                                                onChoice(row.key, choice.id)
                                                            }
                                                        />
                                                        {choice.label}
                                                    </label>
                                                ))}
                                            </span>
                                        </td>
                                    </tr>
                                ))}
                        </tbody>
                    </table>
                </>
            )}
            <p className="text-xs text-ink-faint">{t('refdata.ore.reach')}</p>
            <ReasonFields
                reason={reason}
                commentary={commentary}
                onCommentary={onCommentary}
                missing={missing}
            />
        </div>
    );
}

/**
 * Where the file is about to be written, in one sentence: the configuration the
 * person chose, named, so the last press states its reach (RS-ORE-05).
 */
function TargetSentence({
    hasTargets,
    existing,
    targetName,
    newName,
}: {
    readonly hasTargets: boolean;
    readonly existing: boolean;
    readonly targetName: string;
    readonly newName: string;
}): ReactNode {
    const { t } = useTranslation();
    if (!hasTargets) {
        return null;
    }
    if (existing) {
        return (
            <Notice tone="warn">
                {t('refdata.ore.steps.targetExistingNote', { name: targetName })}
            </Notice>
        );
    }
    return <Notice tone="info">{t('refdata.ore.steps.targetNewNote', { name: newName })}</Notice>;
}
