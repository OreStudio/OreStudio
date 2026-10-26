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
import { useAreaRoot } from '../shell/AreaTrail.js';
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

type Choice = 'take' | 'keep' | 'delete' | 'skip';

/** What the review offers for a row of the difference, and which is the default (RS-ORE-05). */
const CHOICES: Record<string, readonly [Choice, Choice]> = {
    removed: ['keep', 'delete'],
    changed: ['take', 'keep'],
    added: ['take', 'skip'],
};

/**
 * The Import steps of a table kind, at their own address: the file, what it
 * holds, what the system does not know, and the difference with the review.
 * The parse is the server's, and nothing is written before the last step
 * (RS-ORE-05, RS-ORE-06).
 */
export function OreImportPage({ kind }: { readonly kind: OreDocumentKind }): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const queries = useQueryClient();
    const crumbs = [
        ...useAreaRoot('refdata'),
        { label: t(`${kind.words}.title`), to: kind.listPath },
        { label: t('refdata.ore.import') },
    ];
    const [at, setAt] = useState(0);
    const [file, setFile] = useState<{ readonly name: string; readonly content: string }>();
    const [preview, setPreview] = useState<OrePreview>();
    const [choices, setChoices] = useState<Readonly<Record<string, Choice>>>({});
    const [commentary, setCommentary] = useState('');
    const reason = useReason('amend', true);
    const missing = reason.needsCommentary && commentary.trim() === '';

    const choose = async (event: React.ChangeEvent<HTMLInputElement>): Promise<void> => {
        const picked = event.target.files?.[0];
        if (picked === undefined) {
            return;
        }
        setPreview(undefined);
        setChoices({});
        setFile({ name: picked.name, content: await picked.text() });
    };
    const choiceOf = (row: OrePreview['rows'][number]): Choice =>
        choices[row.iso_code] ?? CHOICES[row.change]?.[0] ?? 'take';

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
                    const result = await api.previewOreImport(kind.resource, file.content);
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
            body:
                preview === undefined ? null : (
                    <div className="space-y-3">
                        <div className="grid gap-3 sm:grid-cols-3 lg:grid-cols-5">
                            {(
                                [
                                    ['inFile', preview.in_file],
                                    ['held', preview.held],
                                    ['changed', preview.changed],
                                    ['added', preview.added],
                                    ['removed', preview.removed],
                                ] as const
                            ).map(([key, value]) => (
                                <div key={key} className="rounded-md border border-line p-3">
                                    <p className="text-xl font-semibold tabular-nums">{value}</p>
                                    <p className="text-xs text-ink-muted">
                                        {t(`refdata.ore.counts.${key}`)}
                                    </p>
                                </div>
                            ))}
                        </div>
                        <p className="text-xs text-ink-faint">{t('refdata.ore.countsNote')}</p>
                    </div>
                ),
            next: { label: t('refdata.ore.next'), enabled: preview !== undefined },
        },
        {
            id: 'unknown',
            title: t('refdata.ore.steps.unknown'),
            lead: t('refdata.ore.steps.unknownLead'),
            body:
                preview === undefined ? null : preview.issues.length === 0 ? (
                    <Notice tone="success">{t('refdata.ore.nothingUnknown')}</Notice>
                ) : (
                    <div className="space-y-4">
                        {(['value', 'dropped', 'duplicate'] as const).map((issueKind) => {
                            const issues = preview.issues.filter(
                                (issue) => issue.kind === issueKind,
                            );
                            if (issues.length === 0) {
                                return null;
                            }
                            return (
                                <section key={issueKind} className="space-y-2">
                                    <h3 className="text-sm font-medium">
                                        {t(`refdata.ore.issues.${issueKind}`)}
                                    </h3>
                                    <p className="text-xs text-ink-muted">
                                        {t(`refdata.ore.issues.${issueKind}Lead`)}
                                    </p>
                                    <table className="w-full text-sm">
                                        <tbody>
                                            {issues.map((issue, index) => (
                                                <tr
                                                    key={`${issue.subject}-${String(index)}`}
                                                    className="border-t border-line-subtle"
                                                >
                                                    <td className="py-2 pr-4 font-mono text-xs">
                                                        {issue.subject}
                                                    </td>
                                                    <td className="py-2 text-ink-muted">
                                                        {issue.detail}
                                                    </td>
                                                </tr>
                                            ))}
                                        </tbody>
                                    </table>
                                </section>
                            );
                        })}
                    </div>
                ),
            next: { label: t('refdata.ore.next'), enabled: preview !== undefined },
        },
        {
            id: 'review',
            title: t('refdata.ore.steps.review'),
            lead: t('refdata.ore.steps.reviewLead'),
            body:
                preview === undefined ? null : (
                    <div className="space-y-4">
                        {preview.rows.length === 0 ? (
                            <Notice tone="info">{t('refdata.ore.noDifference')}</Notice>
                        ) : (
                            <table className="w-full text-sm">
                                <thead>
                                    <tr className="text-left text-xs text-ink-muted">
                                        <th className="py-2 pr-4">{t('refdata.ore.change')}</th>
                                        <th className="py-2 pr-4">
                                            {t('refdata.fields.iso_code')}
                                        </th>
                                        <th className="py-2 pr-4">{t('refdata.fields.name')}</th>
                                        <th className="py-2 pr-4">{t('refdata.ore.what')}</th>
                                        <th className="py-2">{t('refdata.ore.choice')}</th>
                                    </tr>
                                </thead>
                                <tbody>
                                    {[...preview.rows]
                                        .sort(
                                            (a, b) =>
                                                ['removed', 'changed', 'added'].indexOf(a.change) -
                                                ['removed', 'changed', 'added'].indexOf(b.change),
                                        )
                                        .map((row) => (
                                            <tr
                                                key={row.iso_code}
                                                className="border-t border-line-subtle"
                                            >
                                                <td className="py-2 pr-4">
                                                    {t(`refdata.ore.changes.${row.change}`)}
                                                </td>
                                                <td className="py-2 pr-4 font-mono text-xs">
                                                    {row.iso_code}
                                                </td>
                                                <td className="py-2 pr-4">{row.name}</td>
                                                <td className="py-2 pr-4 text-ink-muted">
                                                    {row.what}
                                                </td>
                                                <td className="py-2">
                                                    <span className="flex flex-wrap gap-3">
                                                        {(CHOICES[row.change] ?? []).map(
                                                            (choice) => (
                                                                <label
                                                                    key={choice}
                                                                    className="flex items-center gap-1"
                                                                >
                                                                    <input
                                                                        type="radio"
                                                                        name={`choice-${row.iso_code}`}
                                                                        checked={
                                                                            choiceOf(row) === choice
                                                                        }
                                                                        onChange={() =>
                                                                            setChoices({
                                                                                ...choices,
                                                                                [row.iso_code]:
                                                                                    choice,
                                                                            })
                                                                        }
                                                                    />
                                                                    {t(
                                                                        `refdata.ore.choices.${choice}`,
                                                                    )}
                                                                </label>
                                                            ),
                                                        )}
                                                    </span>
                                                </td>
                                            </tr>
                                        ))}
                                </tbody>
                            </table>
                        )}
                        <p className="text-xs text-ink-faint">{t('refdata.ore.reach')}</p>
                        <ReasonFields
                            reason={reason}
                            commentary={commentary}
                            onCommentary={setCommentary}
                            missing={missing}
                        />
                    </div>
                ),
            next: {
                label: t('refdata.ore.import'),
                enabled: preview !== undefined && !missing && reason.code !== '',
                run: async () => {
                    if (file === undefined || preview === undefined) {
                        return;
                    }
                    const result: OreImportResult = await api.importOre(kind.resource, {
                        content: file.content,
                        choices: preview.rows.map((row) => ({
                            iso_code: row.iso_code,
                            action: choiceOf(row),
                        })),
                        reasonCode: reason.code,
                        commentary: commentary.trim(),
                    });
                    markStale(queries, [['records'], ['history'], ['timeline']]);
                    if (!result.success) {
                        throw new Error(
                            [
                                result.message,
                                ...result.errors.map((e) => `${e.iso_code}: ${e.message}`),
                            ]
                                .filter((line) => line !== '')
                                .join('\n'),
                        );
                    }
                    toast(
                        t('refdata.ore.imported', {
                            added: String(result.added),
                            changed: String(result.changed),
                            deleted: String(result.deleted),
                            kept: String(result.kept),
                        }),
                        'info',
                    );
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
