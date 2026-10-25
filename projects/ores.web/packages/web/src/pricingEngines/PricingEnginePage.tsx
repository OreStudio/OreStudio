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

/**
 * One pricing engine configuration, as the record screen standard draws a
 * record page: the trail, the name as the title, the key and version line, the
 * actions, the details and the products with their settings.
 *
 * A change is made in a dialog and never in a form on the page. The dialog
 * states the difference the change makes and asks for a reason, and Save writes
 * the whole document against the version the page read, so a document somebody
 * else changed is a conflict and not a silent overwrite.
 *
 * What the page cannot say, it says: no operation lists the reports that bind a
 * configuration, so the reach of a change is stated as unknown, and nothing
 * here claims a count the server did not give.
 */

import { useState, type ReactNode } from 'react';
import { Link, useNavigate, useParams } from 'react-router';
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';
import {
    pricingEngines,
    type PricingDocument,
    type PricingParameter,
    type PricingProduct,
    type PricingVocabulary,
} from '../api/pricingEngines.js';
import { ApiFailure } from '../api/transport.js';
import { useHolds } from '../access/holds.js';
import { actorPathFor } from '../access/PeoplePage.js';
import { useActorPictures, usePeopleDirectory } from '../access/PersonRef.js';
import { PERMISSION } from '../access/permissions.js';
import { useEntityChanges } from '../events/useEntityChanges.js';
import { useLoadState } from '../events/useLoadState.js';
import { useTranslation } from '../i18n/Provider.js';
import { Timeline } from '../timeline/Timeline.js';
import { orderTimeline } from '@ores/wire-protocol/browser';
import { RecordHeader } from '../refdata/records.js';
import { Crumbs } from '../refdata/shared.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { PATHS, reportPath } from '../shell/paths.js';
import { RefreshButton } from '../ui/RefreshButton.js';
import { Button, Detail, Dialog, Field, Input, Notice, Select, Tag } from '../ui/Primitives.js';
import { RelativeTime } from '../ui/Time.js';
import { useTabs } from '../ui/Tabs.js';
import { EditingModeToggle, useEditingMode, type EditingMode } from '../ui/EditingMode.js';
import {
    ParameterDocuments,
    ParameterFilters,
    type ParameterDocumentation,
} from '../ui/OreParameter.js';
import { PRICING_ENGINES_EVENTS } from './PricingEnginesPage.js';
import { ModelEnginePanels, SettingValue } from './parts.js';
import { SaveDialog } from './SaveDialog.js';
import {
    documentationOf,
    draftOf,
    historyEventsOf,
    historyNameOf,
    enginesFor,
    templateInfo,
    type KnownTemplate,
    isKnownPair,
    modelsFor,
    newId,
    newParameter,
    newProduct,
    pairChange,
    settingsFor,
    startsWith,
    EMPTY_VOCABULARY,
    parametersOf,
    settingKey,
    withPair,
    type Draft,
} from './model.js';

/** Where one configuration is cached. */
export function pricingEngineKey(id: string): readonly unknown[] {
    return ['pricing-engine', id];
}

/** Where the vocabulary is cached. It does not change while a person works. */
export const VOCABULARY_KEY = ['pricing-engine-vocabulary'] as const;

/** The five sections of the configuration page, in the order the standard fixes. */
const TABS = ['details', 'globals', 'products', 'reports', 'history'] as const;

/** The eight characters a person reads to tell one identifier from another. */
function shortId(id: string): string {
    return id.slice(0, 8);
}

/** An asset class code as a person reads it: Interest Rates, FX, Bond. */
function assetClassLabel(code: string): string {
    if (code === '') return '';
    if (code === 'fx') return 'FX';
    return code
        .split('_')
        .map((word) => (word === '' ? word : word.charAt(0).toUpperCase() + word.slice(1)))
        .join(' ');
}

/** The asset class of a product type, from the instrument it maps to. */
function assetClassOf(vocabulary: PricingVocabulary | undefined, typeCode: string): string {
    return vocabulary?.types.find((type) => type.code === typeCode)?.assetClass ?? '';
}

export function PricingEnginePage(): ReactNode {
    const { id = '' } = useParams();
    const { t } = useTranslation();
    const holds = useHolds();
    const queries = useQueryClient();
    const navigate = useNavigate();
    const area = useAreaRoot('reporting');
    const mayWrite = holds(PERMISSION.pricingEnginesWrite);
    const mayDelete = holds(PERMISSION.pricingEnginesDelete);
    const [selected, setSelected] = useState<string | null>(null);
    const [mode, setMode] = useEditingMode();
    const [dialog, setDialog] = useState<
        | { readonly kind: 'details' }
        | { readonly kind: 'add' }
        | { readonly kind: 'remove'; readonly productId: string }
        | { readonly kind: 'delete' }
        | null
    >(null);
    const tabs = useTabs({
        label: t('pricingEngines.tabs.label'),
        tabs: TABS,
        titleOf: (tab) => t(`pricingEngines.tabs.${tab}`),
    });
    const open = useQuery({
        queryKey: pricingEngineKey(id),
        queryFn: () => pricingEngines.read(id),
        meta: { quiet: true },
    });
    const vocabulary = useQuery({
        queryKey: VOCABULARY_KEY,
        queryFn: pricingEngines.vocabulary,
        staleTime: 5 * 60_000,
        meta: { quiet: true },
    });
    const load = useLoadState([['pricing-engine', id]]);
    const news = useEntityChanges([PRICING_ENGINES_EVENTS], load);
    const trail = [...area, { label: t('pricingEngines.title'), to: PATHS.pricingEngines }];

    if (open.isPending) {
        return (
            <ScreenFrame
                trail={<Crumbs parts={trail} />}
                title={shortId(id)}
                description={t('pricingEngines.lead')}
            >
                <p className="text-sm text-ink-muted" role="status">
                    {t('common.loading')}
                </p>
            </ScreenFrame>
        );
    }

    if (open.isError) {
        const missing = open.error instanceof ApiFailure && open.error.status === 404;
        return (
            <ScreenFrame
                trail={<Crumbs parts={[...trail, { label: shortId(id) }]} />}
                title={shortId(id)}
                description={t('pricingEngines.lead')}
            >
                {missing ? (
                    <div className="space-y-3">
                        <Notice tone="info">{t('refdata.records.notFound')}</Notice>
                        <Link
                            to={PATHS.pricingEngines}
                            className="text-sm text-accent hover:text-accent-bright"
                        >
                            {t('refdata.records.backToList')}
                        </Link>
                    </div>
                ) : (
                    <Notice tone="error">{open.error.message}</Notice>
                )}
            </ScreenFrame>
        );
    }

    const document = open.data;
    const draft = draftOf(document);
    const reload = (): void => {
        void queries.invalidateQueries({ queryKey: pricingEngineKey(id) });
    };
    const close = (): void => setDialog(null);
    const known: PricingVocabulary | undefined = vocabulary.data;
    const products = [...document.products].sort((a, b) => a.position - b.position);
    const chosen = products.find((product) => product.id === selected) ?? products[0];

    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[...trail, { label: document.name }]}
                title={document.name}
                recordKey={shortId(document.id)}
                version={document.version}
                subdued
                access={mayWrite}
                own={
                    <div className="flex items-center gap-2">
                        <EditingModeToggle mode={mode} onChange={setMode} />
                        <RefreshButton onClick={reload} load={load} news={news} />
                    </div>
                }
                onEdit={mayWrite ? () => setDialog({ kind: 'details' }) : undefined}
                onDelete={mayDelete ? () => setDialog({ kind: 'delete' }) : undefined}
            />
            {tabs.bar}
            {tabs.tab === 'details' && <DetailsTab document={document} />}
            {tabs.tab === 'globals' && (
                <GlobalParametersTab
                    document={document}
                    vocabulary={known}
                    mode={mode}
                    mayWrite={mayWrite}
                />
            )}
            {tabs.tab === 'products' && (
                <ProductsTab
                    document={document}
                    draft={draft}
                    vocabulary={known}
                    mayWrite={mayWrite && known !== undefined}
                    mode={mode}
                    chosen={chosen}
                    onSelect={setSelected}
                    onRemove={(productId) => setDialog({ kind: 'remove', productId })}
                    onAdd={() => setDialog({ kind: 'add' })}
                />
            )}
            {tabs.tab === 'reports' && <ReportsTab reports={document.reports} />}
            {tabs.tab === 'history' && <HistoryTab document={document} />}
            {vocabulary.isError && (
                <Notice tone="warn">
                    {t('pricingEngines.record.vocabularyUnread', {
                        message: vocabulary.error.message,
                    })}
                </Notice>
            )}
            {dialog?.kind === 'details' && (
                <DetailsDialog document={document} vocabulary={known} onClose={close} />
            )}
            {dialog?.kind === 'add' && known !== undefined && (
                <AddProductDialog document={document} vocabulary={known} onClose={close} />
            )}
            {dialog?.kind === 'remove' && known !== undefined && (
                <RemoveProductDialog
                    document={document}
                    vocabulary={known}
                    productId={dialog.productId}
                    onClose={close}
                />
            )}
            {dialog?.kind === 'delete' && (
                <DeleteDialog
                    document={document}
                    onClose={close}
                    onDeleted={() => void navigate(PATHS.pricingEngines)}
                />
            )}
        </div>
    );
}

/** The configuration's own fields, as read. */
function DetailsTab({ document }: { readonly document: PricingDocument }): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card space-y-4 p-4">
            <header>
                <h2 className="text-sm font-medium">{t('pricingEngines.record.details')}</h2>
            </header>
            <dl className="grid gap-x-6 gap-y-3 sm:grid-cols-5">
                <Detail label={t('pricingEngines.fields.name')} value={document.name} />
                <Detail
                    label={t('pricingEngines.fields.party')}
                    value={document.partyName === '' ? document.partyId : document.partyName}
                />
                <Detail
                    label={t('pricingEngines.fields.variant')}
                    value={
                        document.variant === '' ? (
                            <span className="text-ink-faint">
                                {t('pricingEngines.cell.noVariant')}
                            </span>
                        ) : (
                            <Tag>{document.variant}</Tag>
                        )
                    }
                />
                <Detail label={t('pricingEngines.fields.modifiedBy')} value={document.modifiedBy} />
                <Detail
                    label={t('pricingEngines.fields.changed')}
                    value={<RelativeTime at={document.recordedAt} />}
                />
                <div className="sm:col-span-4">
                    <Detail
                        label={t('pricingEngines.fields.description')}
                        value={
                            document.description === '' ? (
                                <span className="text-ink-faint">
                                    {t('pricingEngines.cell.noDescription')}
                                </span>
                            ) : (
                                document.description
                            )
                        }
                    />
                </div>
            </dl>
        </section>
    );
}

/** The parameters of the whole document, with their documentation under them. */
function GlobalParametersTab({
    document,
    vocabulary,
    mode,
    mayWrite,
}: {
    readonly document: PricingDocument;
    readonly vocabulary: PricingVocabulary | undefined;
    readonly mode: EditingMode;
    readonly mayWrite: boolean;
}): ReactNode {
    const { t } = useTranslation();
    const [setOnly, setSetOnly] = useState(false);
    const [numbersOnly, setNumbersOnly] = useState(false);
    const [editing, setEditing] = useState(false);
    const [values, setValues] = useState<Readonly<Record<string, string>>>({});
    const [saving, setSaving] = useState<Draft | null>(null);
    const globals = document.parameters.filter((parameter) => parameter.productId === null);
    const docOf = (name: string) =>
        (vocabulary?.globals ?? []).find((entry) => entry.name === name);
    const shown = globals.filter((parameter) => {
        if (!setOnly) return true;
        const doc = docOf(parameter.name);
        return (doc?.defaultText ?? '') === '' || parameter.value !== doc?.defaultText;
    });
    return (
        <section className="card space-y-4 p-4">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <div className="space-y-1">
                    <h2 className="text-sm font-medium">{t('pricingEngines.globals.heading')}</h2>
                    <p className="text-sm text-ink-muted">{t('pricingEngines.globals.lead')}</p>
                </div>
                {mayWrite && !editing && (
                    <Button size="sm" icon="edit" onClick={() => setEditing(true)}>
                        {t('pricingEngines.actions.edit')}
                    </Button>
                )}
            </header>
            {globals.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('pricingEngines.globals.empty')}</p>
            ) : (
                <>
                    <ParameterFilters
                        set={setOnly}
                        numbers={numbersOnly}
                        onSet={() => setSetOnly((before) => !before)}
                        onNumbers={() => setNumbersOnly((before) => !before)}
                        shown={shown.length}
                        total={globals.length}
                    />
                    <ul className="divide-y divide-line-subtle">
                        {shown.map((parameter) =>
                            editing ? (
                                <li key={parameter.id} className="space-y-2 py-3">
                                    <div className="flex flex-wrap items-baseline justify-between gap-3">
                                        <span className="text-sm font-medium">
                                            {docOf(parameter.name)?.label ?? parameter.name}
                                        </span>
                                        <div className="text-sm">
                                            {docOf(parameter.name)?.domain === 'boolean' ? (
                                                <Select
                                                    aria-label={
                                                        docOf(parameter.name)?.label ??
                                                        parameter.name
                                                    }
                                                    value={values[parameter.id] ?? parameter.value}
                                                    onChange={(event) =>
                                                        setValues({
                                                            ...values,
                                                            [parameter.id]: event.target.value,
                                                        })
                                                    }
                                                >
                                                    <option value="true">true</option>
                                                    <option value="false">false</option>
                                                </Select>
                                            ) : (
                                                <Input
                                                    aria-label={
                                                        docOf(parameter.name)?.label ??
                                                        parameter.name
                                                    }
                                                    inputMode={
                                                        docOf(parameter.name)?.domain ===
                                                            'double' ||
                                                        docOf(parameter.name)?.domain === 'integer'
                                                            ? 'numeric'
                                                            : 'text'
                                                    }
                                                    value={values[parameter.id] ?? parameter.value}
                                                    onChange={(event) =>
                                                        setValues({
                                                            ...values,
                                                            [parameter.id]: event.target.value,
                                                        })
                                                    }
                                                />
                                            )}
                                        </div>
                                    </div>
                                    <ParameterDocuments
                                        doc={{
                                            documented:
                                                (docOf(parameter.name)?.description ?? '') !== '',
                                            name: parameter.name,
                                            description: docOf(parameter.name)?.description ?? '',
                                            defaultText: docOf(parameter.name)?.defaultText ?? '',
                                            format: docOf(parameter.name)?.format ?? '',
                                            introducedIn: docOf(parameter.name)?.introducedIn ?? '',
                                            removedIn: docOf(parameter.name)?.removedIn ?? '',
                                            xpath: docOf(parameter.name)?.xpath ?? '',
                                        }}
                                        mode={mode}
                                    />
                                </li>
                            ) : (
                                <ParameterRow
                                    key={parameter.id}
                                    parameter={parameter}
                                    doc={docOf(parameter.name)}
                                    template={undefined}
                                    mode={mode}
                                />
                            ),
                        )}
                    </ul>
                    {editing && (
                        <div className="flex justify-end gap-2 border-t border-line-subtle pt-2">
                            <Button
                                variant="ghost"
                                onClick={() => {
                                    setValues({});
                                    setEditing(false);
                                }}
                            >
                                {t('common.cancel')}
                            </Button>
                            <Button
                                variant="primary"
                                icon="save"
                                onClick={() =>
                                    setSaving({
                                        ...draftOf(document),
                                        parameters: draftOf(document).parameters.map((parameter) =>
                                            values[parameter.id] === undefined
                                                ? parameter
                                                : {
                                                      ...parameter,
                                                      value:
                                                          values[parameter.id] ?? parameter.value,
                                                  },
                                        ),
                                    })
                                }
                            >
                                {t('pricingEngines.save.save')}
                            </Button>
                        </div>
                    )}
                    {saving !== null && vocabulary !== undefined && (
                        <SaveDialog
                            title={t('pricingEngines.globals.heading')}
                            document={document}
                            after={saving}
                            vocabulary={vocabulary}
                            onClose={() => setSaving(null)}
                            onSaved={() => {
                                setSaving(null);
                                setValues({});
                                setEditing(false);
                            }}
                        >
                            <p className="text-sm text-ink-muted">
                                {t('pricingEngines.change.reviewHint')}
                            </p>
                        </SaveDialog>
                    )}
                </>
            )}
        </section>
    );
}

/** Every product of the configuration, by asset class, and the product itself. */
function ProductsTab({
    document,
    draft,
    vocabulary,
    mayWrite,
    mode,
    chosen,
    onSelect,
    onRemove,
    onAdd,
}: {
    readonly document: PricingDocument;
    readonly draft: Draft;
    readonly vocabulary: PricingVocabulary | undefined;
    readonly mayWrite: boolean;
    readonly mode: EditingMode;
    readonly chosen: PricingProduct | undefined;
    readonly onSelect: (id: string) => void;
    readonly onRemove: (id: string) => void;
    readonly onAdd: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const [filter, setFilter] = useState('');
    const [editingId, setEditingId] = useState<string | null>(null);
    const [pending, setPending] = useState<string | null>(null);
    const first = [...document.products].sort((a, b) => a.position - b.position)[0];
    const [openClasses, setOpenClasses] = useState<ReadonlySet<string>>(
        () => new Set(first === undefined ? [] : [assetClassOf(vocabulary, first.typeCode)]),
    );
    const products = [...document.products].sort((a, b) => a.position - b.position);
    const needle = filter.trim().toLowerCase();
    const visible =
        needle === ''
            ? products
            : products.filter(
                  (product) =>
                      product.typeCode.toLowerCase().includes(needle) ||
                      assetClassLabel(assetClassOf(vocabulary, product.typeCode))
                          .toLowerCase()
                          .includes(needle),
              );
    const classes = [
        ...new Set(visible.map((product) => assetClassOf(vocabulary, product.typeCode))),
    ];
    return (
        <div className="grid gap-4 lg:grid-cols-[15rem_minmax(0,1fr)]">
            <aside className="card space-y-3 p-3">
                <div className="space-y-1">
                    <h2 className="text-sm font-medium">{t('pricingEngines.tabs.products')}</h2>
                    <p className="text-xs text-ink-faint">
                        {t('pricingEngines.record.products', {
                            count: String(products.length),
                        })}
                    </p>
                </div>
                {products.length > 0 && (
                    <input
                        value={filter}
                        onChange={(event) => setFilter(event.target.value)}
                        placeholder={t('pricingEngines.workbench.filter')}
                        aria-label={t('pricingEngines.workbench.filter')}
                        className="w-full rounded border border-line bg-surface-base px-2 py-1 text-sm text-ink placeholder:text-ink-faint"
                    />
                )}
                {visible.length > 0 && (
                    <button
                        type="button"
                        className="px-1 text-xs text-accent hover:text-accent-bright"
                        onClick={() =>
                            setOpenClasses(
                                classes.every((assetClass) => openClasses.has(assetClass))
                                    ? new Set()
                                    : new Set(classes),
                            )
                        }
                    >
                        {classes.every((assetClass) => openClasses.has(assetClass))
                            ? t('pricingEngines.workbench.collapseAll')
                            : t('pricingEngines.workbench.expandAll')}
                    </button>
                )}
                {products.length === 0 ? (
                    <p className="text-sm text-ink-muted">
                        {t('pricingEngines.record.noProducts')}
                    </p>
                ) : visible.length === 0 ? (
                    <p className="text-sm text-ink-muted">
                        {t('pricingEngines.workbench.noMatch')}
                    </p>
                ) : (
                    classes.map((assetClass) => {
                        const inside = visible.filter(
                            (product) => assetClassOf(vocabulary, product.typeCode) === assetClass,
                        );
                        return (
                            <details
                                key={assetClass}
                                open={openClasses.has(assetClass)}
                                onToggle={(event) => {
                                    const isOpen = event.currentTarget.open;
                                    setOpenClasses((before) => {
                                        const next = new Set(before);
                                        if (isOpen) {
                                            next.add(assetClass);
                                        } else {
                                            next.delete(assetClass);
                                        }
                                        return next;
                                    });
                                }}
                                className="group/class"
                            >
                                <summary className="flex cursor-pointer items-baseline justify-between gap-2 px-1 pt-2 text-[10px] tracking-wide text-ink-faint uppercase">
                                    <span>
                                        {assetClassLabel(assetClass) === ''
                                            ? t('pricingEngines.workbench.assetClass')
                                            : assetClassLabel(assetClass)}
                                    </span>
                                    <span>{inside.length}</span>
                                </summary>
                                <div className="space-y-1 pt-1">
                                    {inside.map((product) => (
                                        <button
                                            key={product.id}
                                            type="button"
                                            title={product.typeCode}
                                            onClick={() => {
                                                if (
                                                    editingId !== null &&
                                                    editingId !== product.id
                                                ) {
                                                    setPending(product.id);
                                                    return;
                                                }
                                                setOpenClasses((before) =>
                                                    new Set(before).add(
                                                        assetClassOf(vocabulary, product.typeCode),
                                                    ),
                                                );
                                                onSelect(product.id);
                                            }}
                                            className={
                                                product.id === chosen?.id
                                                    ? 'block w-full rounded border-l-2 border-accent bg-panel-2 px-2 py-1 text-left text-sm break-words text-ink [overflow-wrap:anywhere]'
                                                    : 'block w-full rounded border-l-2 border-transparent px-2 py-1 text-left text-sm break-words text-ink-muted hover:text-ink [overflow-wrap:anywhere]'
                                            }
                                        >
                                            {product.typeCode}
                                        </button>
                                    ))}
                                </div>
                            </details>
                        );
                    })
                )}
                {mayWrite && (
                    <Button size="sm" icon="add" onClick={onAdd}>
                        {t('pricingEngines.actions.addProduct')}
                    </Button>
                )}
            </aside>
            <div className="space-y-4">
                <Notice tone="info">
                    {t('pricingEngines.workbench.runSettings')}{' '}
                    <Link
                        to={PATHS.engineSettings}
                        className="text-accent hover:text-accent-bright"
                    >
                        {t('reporting.area.screens.engineSettings')}
                    </Link>
                </Notice>
                {chosen === undefined ? (
                    <section className="card p-4">
                        <p className="text-sm text-ink-muted">
                            {t('pricingEngines.record.noProducts')}
                        </p>
                    </section>
                ) : (
                    <Workbench
                        key={chosen.id}
                        document={document}
                        draft={draft}
                        vocabulary={vocabulary}
                        product={chosen}
                        mayWrite={mayWrite}
                        mode={mode}
                        onEditingChange={(editing) => setEditingId(editing ? chosen.id : null)}
                        onRemove={() => onRemove(chosen.id)}
                    />
                )}
            </div>
            {pending !== null && (
                <Dialog
                    title={t('pricingEngines.workbench.discardTitle')}
                    name="pricing-engine-discard"
                    onClose={() => setPending(null)}
                    footer={
                        <>
                            <Button variant="ghost" onClick={() => setPending(null)}>
                                {t('common.cancel')}
                            </Button>
                            <Button
                                variant="danger"
                                onClick={() => {
                                    const next = document.products.find(
                                        (product) => product.id === pending,
                                    );
                                    setPending(null);
                                    setEditingId(null);
                                    onSelect(pending);
                                    if (next !== undefined) {
                                        setOpenClasses((before) =>
                                            new Set(before).add(
                                                assetClassOf(vocabulary, next.typeCode),
                                            ),
                                        );
                                    }
                                }}
                            >
                                {t('pricingEngines.workbench.discard')}
                            </Button>
                        </>
                    }
                >
                    <p className="text-sm">
                        {t('pricingEngines.workbench.discardBody', {
                            type:
                                document.products.find((product) => product.id === editingId)
                                    ?.typeCode ?? '',
                        })}
                    </p>
                </Dialog>
            )}
        </div>
    );
}

/** The selected product: what it is, its model and its engine. */
function Workbench({
    document,
    draft,
    vocabulary,
    product,
    mayWrite,
    mode,
    onEditingChange,
    onRemove,
}: {
    readonly document: PricingDocument;
    readonly draft: Draft;
    readonly vocabulary: PricingVocabulary | undefined;
    readonly product: PricingProduct;
    readonly mayWrite: boolean;
    readonly mode: EditingMode;
    readonly onEditingChange: (editing: boolean) => void;
    readonly onRemove: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const [editing, setEditing] = useState(false);
    const [saving, setSaving] = useState<Draft | null>(null);
    const type = vocabulary?.types.find((candidate) => candidate.code === product.typeCode);
    const parameters = parametersOf(draft, product.id);
    const settingOf = (scope: string, name: string) =>
        vocabulary === undefined
            ? undefined
            : documentationOf(
                  vocabulary,
                  product.typeCode,
                  product.model,
                  product.engine,
                  scope,
                  name,
              );
    const model = vocabulary?.models.find((candidate) => candidate.code === product.model);
    const engine = vocabulary?.methods.find((candidate) => candidate.code === product.engine);
    const modelParameters = parameters.filter((parameter) => parameter.scope !== 'engine');
    const engineParameters = parameters.filter((parameter) => parameter.scope === 'engine');
    return (
        <div className="space-y-4">
            <section className="card space-y-2 p-4">
                <header className="flex flex-wrap items-baseline justify-between gap-2">
                    <div>
                        <h2 className="text-lg font-semibold">{product.typeCode}</h2>
                        <p className="text-xs text-ink-faint">
                            {[
                                assetClassLabel(assetClassOf(vocabulary, product.typeCode)),
                                type?.instrumentTypeCode ?? '',
                            ]
                                .filter((part) => part !== '')
                                .join(' · ')}
                        </p>
                    </div>
                    {mayWrite && !editing && (
                        <div className="flex gap-2">
                            <Button
                                size="sm"
                                icon="edit"
                                onClick={() => {
                                    setEditing(true);
                                    onEditingChange(true);
                                }}
                            >
                                {t('pricingEngines.actions.edit')}
                            </Button>
                            <Button size="sm" variant="danger" icon="delete" onClick={onRemove}>
                                {t('pricingEngines.actions.remove')}
                            </Button>
                        </div>
                    )}
                </header>
                <Docs>
                    <p className="text-sm text-ink-muted">
                        {type !== undefined && type.documentation !== ''
                            ? type.documentation
                            : t('pricingEngines.workbench.noDescription')}
                    </p>
                </Docs>
            </section>
            {editing && vocabulary !== undefined ? (
                <ProductEditor
                    document={document}
                    vocabulary={vocabulary}
                    productId={product.id}
                    mode={mode}
                    onCancel={() => {
                        setEditing(false);
                        onEditingChange(false);
                    }}
                    onSave={(after) => setSaving(after)}
                />
            ) : (
                <>
                    <RoleCard
                        mode={mode}
                        title={t('pricingEngines.fields.model')}
                        definition={t('pricingEngines.workbench.definitionModel')}
                        code={product.model}
                        description={model?.description ?? ''}
                        detail={model?.detail ?? ''}
                        source={model?.source ?? ''}
                        parameters={modelParameters}
                        settingOf={settingOf}
                        templateOf={(value) =>
                            vocabulary === undefined ? undefined : templateInfo(vocabulary, value)
                        }
                    />
                    <RoleCard
                        mode={mode}
                        title={t('pricingEngines.fields.engine')}
                        definition={t('pricingEngines.workbench.definitionEngine')}
                        code={product.engine}
                        description={engine?.description ?? ''}
                        detail={engine?.detail ?? ''}
                        source={engine?.source ?? ''}
                        parameters={engineParameters}
                        settingOf={settingOf}
                        templateOf={(value) =>
                            vocabulary === undefined ? undefined : templateInfo(vocabulary, value)
                        }
                        pairKnown={
                            vocabulary === undefined
                                ? undefined
                                : isKnownPair(
                                      vocabulary,
                                      product.typeCode,
                                      product.model,
                                      product.engine,
                                  )
                        }
                    />
                </>
            )}
            {saving !== null && vocabulary !== undefined && (
                <SaveDialog
                    title={t('pricingEngines.change.productTitle', { type: product.typeCode })}
                    document={document}
                    after={saving}
                    vocabulary={vocabulary}
                    onClose={() => setSaving(null)}
                    onSaved={() => {
                        setSaving(null);
                        setEditing(false);
                        onEditingChange(false);
                    }}
                >
                    <p className="text-sm text-ink-muted">
                        {t('pricingEngines.change.reviewHint')}
                    </p>
                </SaveDialog>
            )}
        </div>
    );
}

/** The prose documentation of a product, a model or an engine, opened like a parameter's. */
function Docs({ children }: { readonly children: ReactNode }): ReactNode {
    const { t } = useTranslation();
    const [open, setOpen] = useState(false);
    return (
        <>
            <button
                type="button"
                aria-expanded={open}
                className="text-xs text-accent hover:text-accent-bright"
                onClick={() => setOpen((before) => !before)}
            >
                {open ? t('oreParameter.hideDocumentation') : t('oreParameter.documentation')}
            </button>
            {open && <div className="space-y-1 pt-1">{children}</div>}
        </>
    );
}

/** One of the two roles a product's price comes from: the model or the engine. */
type SettingDoc = PricingVocabulary['settings'][number];

/** The documentation fields a parameter may carry, from the one contract. */
interface ParameterDoc {
    readonly label?: string | undefined;
    readonly help?: string | undefined;
    readonly description?: string | undefined;
    readonly format?: string | undefined;
    readonly defaultText?: string | undefined;
    readonly introducedIn?: string | undefined;
    readonly removedIn?: string | undefined;
    readonly xpath?: string | undefined;
    readonly required?: boolean | undefined;
}

function RoleCard({
    title,
    definition,
    code,
    description,
    detail,
    source,
    parameters,
    settingOf,
    templateOf,
    mode,
    pairKnown,
}: {
    readonly title: string;
    readonly definition: string;
    readonly code: string;
    readonly description: string;
    readonly detail: string;
    readonly source: string;
    readonly parameters: readonly PricingParameter[];
    readonly settingOf: (scope: string, name: string) => SettingDoc | undefined;
    readonly templateOf: (value: string) => KnownTemplate | undefined;
    readonly mode: EditingMode;
    readonly pairKnown?: boolean | undefined;
}): ReactNode {
    const { t } = useTranslation();
    const [setOnly, setSetOnly] = useState(false);
    const [numbersOnly, setNumbersOnly] = useState(false);
    const shown = parameters.filter((parameter) => {
        const doc = settingOf(parameter.scope, parameter.name);
        if (setOnly && (doc?.defaultText ?? '') !== '' && parameter.value === doc?.defaultText) {
            return false;
        }
        return true;
    });
    return (
        <section className="card space-y-3 p-4">
            <header className="space-y-1">
                <h3 className="text-sm font-medium">{title}</h3>
                <p className="text-xs text-ink-faint">{definition}</p>
            </header>
            <div className="space-y-1">
                <div className="font-mono text-sm">{code}</div>
                {pairKnown === false && (
                    <Notice tone="warn">{t('pricingEngines.change.unknownPair')}</Notice>
                )}
                <Docs>
                    <div className="space-y-1">
                        {description === '' ? (
                            <p className="text-sm text-ink-faint">
                                {t('pricingEngines.workbench.noDescription')}
                            </p>
                        ) : (
                            <p className="text-sm text-ink-muted">{description}</p>
                        )}
                        {detail !== '' && <p className="text-sm text-ink-muted">{detail}</p>}
                        {source !== '' && (
                            <p className="text-xs text-ink-faint">
                                {t(`pricingEngines.source.${source}`)}
                            </p>
                        )}
                    </div>
                </Docs>
            </div>
            <div>
                <h4 className="pb-1 text-xs text-ink-faint">
                    {t('pricingEngines.workbench.parameters')}
                </h4>
                {parameters.length > 0 && (
                    <div className="pb-1">
                        <ParameterFilters
                            set={setOnly}
                            numbers={numbersOnly}
                            onSet={() => setSetOnly((before) => !before)}
                            onNumbers={() => setNumbersOnly((before) => !before)}
                            shown={shown.length}
                            total={parameters.length}
                        />
                    </div>
                )}
                {parameters.length === 0 ? (
                    <p className="text-sm text-ink-faint">
                        {t('pricingEngines.workbench.noParameters')}
                    </p>
                ) : (
                    <ul className="divide-y divide-line-subtle">
                        {shown.map((parameter) => (
                            <ParameterRow
                                key={parameter.id}
                                parameter={parameter}
                                doc={settingOf(parameter.scope, parameter.name)}
                                template={templateOf(parameter.value)}
                                mode={mode}
                            />
                        ))}
                    </ul>
                )}
            </div>
        </section>
    );
}

/**
 * One parameter: the value in force, and the documentation of it from the one
 * contract under a disclosure.
 */
function ParameterRow({
    parameter,
    doc,
    template,
    mode,
}: {
    readonly parameter: PricingParameter;
    readonly doc: ParameterDoc | undefined;
    readonly template: KnownTemplate | undefined;
    readonly mode: EditingMode;
}): ReactNode {
    const label = doc?.label !== undefined && doc.label !== '' ? doc.label : parameter.name;
    const documentation: ParameterDocumentation = {
        documented: doc?.description !== undefined && doc.description !== '',
        name: parameter.name,
        description: doc?.description ?? '',
        defaultText: doc?.defaultText ?? '',
        format: doc?.format ?? '',
        introducedIn: doc?.introducedIn ?? '',
        removedIn: doc?.removedIn ?? '',
        xpath: doc?.xpath ?? '',
    };
    return (
        <li className="space-y-2 py-3">
            <div className="flex flex-wrap items-baseline justify-between gap-3">
                <div className="min-w-0">
                    <span className="text-sm font-medium">{label}</span>
                </div>
                <div className="text-sm">
                    <span className="font-mono text-xs">{parameter.value}</span>
                </div>
            </div>
            {template !== undefined && (
                <p className="text-xs text-ink-faint">
                    {template.assetClass} · {template.method} — {template.description}
                </p>
            )}
            <ParameterDocuments doc={documentation} mode={mode} />
        </li>
    );
}

/** The reports a change reaches. */
function ReportsTab({ reports }: { readonly reports: PricingDocument['reports'] }): ReactNode {
    const { t } = useTranslation();
    if (reports === null) {
        return <Notice tone="warn">{t('pricingEngines.record.reachUnknown')}</Notice>;
    }
    return (
        <section className="card space-y-3 p-4">
            <h2 className="text-sm font-medium">{t('pricingEngines.reports.heading')}</h2>
            <p className="text-sm text-ink-muted">{t('pricingEngines.reports.lead')}</p>
            {reports.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('pricingEngines.reports.empty')}</p>
            ) : (
                <ul className="space-y-2">
                    {reports.map((report) => (
                        <li key={report.id}>
                            <Link
                                to={reportPath(report.id)}
                                className="text-sm text-accent hover:text-accent-bright"
                            >
                                {report.name}
                            </Link>
                        </li>
                    ))}
                </ul>
            )}
        </section>
    );
}

/** Where the versions of one configuration are cached. */
function historyKey(id: string): readonly unknown[] {
    return ['pricing-engine-history', id];
}

/**
 * Every version of the configuration, its products and its parameters, drawn by
 * the shared timeline. A save writes all three, and a person reads one history,
 * so the versions are read together and each event names the entity it came from.
 */
function HistoryTab({ document }: { readonly document: PricingDocument }): ReactNode {
    const { t } = useTranslation();
    const holds = useHolds();
    const pictureOf = useActorPictures();
    const { isService } = usePeopleDirectory();
    const history = useQuery({
        queryKey: historyKey(document.id),
        queryFn: () => pricingEngines.history(document.id),
        meta: { quiet: true },
    });
    if (history.isPending) {
        return (
            <p className="text-sm text-ink-muted" role="status">
                {t('common.loading')}
            </p>
        );
    }
    if (history.isError) {
        return <Notice tone="error">{history.error.message}</Notice>;
    }
    const nameOf = historyNameOf(history.data);
    return (
        <Timeline
            timeline={{
                subject: 'pricing_engines',
                id: document.id,
                events: orderTimeline(historyEventsOf(history.data)),
                gaps: [],
            }}
            hideUnchanged
            recordName={(event) => nameOf.get(`${event.entityType}|${event.entityId}`)}
            actorPath={actorPathFor(holds('iam::accounts:read'))}
            actorPicture={pictureOf}
            isService={isService}
        />
    );
}

/** Changes the name, the description and the variant. */
function DetailsDialog({
    document,
    vocabulary,
    onClose,
}: {
    readonly document: PricingDocument;
    readonly vocabulary: PricingVocabulary | undefined;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const [name, setName] = useState(document.name);
    const [description, setDescription] = useState(document.description);
    const [variant, setVariant] = useState(document.variant);
    const after: Draft = { ...draftOf(document), name, description, variant };
    return (
        <SaveDialog
            title={t('pricingEngines.change.detailsTitle', { name: document.name })}
            document={document}
            after={after}
            vocabulary={vocabulary ?? EMPTY_VOCABULARY}
            onClose={onClose}
        >
            <div className="grid gap-3 sm:grid-cols-2">
                <Field label={t('pricingEngines.fields.name')}>
                    <Input value={name} maxLength={200} onChange={(e) => setName(e.target.value)} />
                </Field>
                <Field label={t('pricingEngines.fields.variant')}>
                    <Input
                        value={variant}
                        maxLength={200}
                        onChange={(e) => setVariant(e.target.value)}
                    />
                </Field>
                <div className="sm:col-span-2">
                    <Field label={t('pricingEngines.fields.description')}>
                        <Input
                            value={description}
                            maxLength={2000}
                            onChange={(e) => setDescription(e.target.value)}
                        />
                    </Field>
                </div>
            </div>
        </SaveDialog>
    );
}

/**
 * Changes how one product is priced, in place: the model, the engine and the
 * values of its settings. A setting the new pair does not take is offered for
 * dropping and a setting it usually needs is offered for adding. Nothing is
 * dropped or added until the person ticks it, and a pair not known to work is
 * marked and never refused.
 */
function ProductEditor({
    document,
    vocabulary,
    productId,
    mode,
    onCancel,
    onSave,
}: {
    readonly document: PricingDocument;
    readonly vocabulary: PricingVocabulary;
    readonly productId: string;
    readonly mode: EditingMode;
    readonly onCancel: () => void;
    readonly onSave: (after: Draft) => void;
}): ReactNode {
    const { t } = useTranslation();
    const product = document.products.find((candidate) => candidate.id === productId);
    const start = draftOf(document);
    const [model, setModel] = useState(product?.model ?? '');
    const [engine, setEngine] = useState(product?.engine ?? '');
    const [drop, setDrop] = useState<readonly string[]>([]);
    const [add, setAdd] = useState<readonly string[]>([]);
    const [values, setValues] = useState<Readonly<Record<string, string>>>({});
    if (product === undefined) {
        return null;
    }
    const change = pairChange(vocabulary, start, product, model, engine);
    const paired = withPair(vocabulary, start, productId, model, engine, { drop, add });
    const after: Draft = {
        ...paired,
        parameters: paired.parameters.map((parameter) =>
            values[parameter.id] === undefined
                ? parameter
                : { ...parameter, value: values[parameter.id] ?? parameter.value },
        ),
    };
    const known = isKnownPair(vocabulary, product.typeCode, model, engine);
    const toggle = (list: readonly string[], key: string): readonly string[] =>
        list.includes(key) ? list.filter((entry) => entry !== key) : [...list, key];
    const models = [...new Set([...modelsFor(vocabulary, product.typeCode), model])].filter(
        (entry) => entry !== '',
    );
    const engines = [
        ...new Set([...enginesFor(vocabulary, product.typeCode, model), engine]),
    ].filter((entry) => entry !== '');
    const kept = parametersOf(after, productId);

    return (
        <div className="space-y-4">
            <section className="card space-y-2 p-4">
                <div>
                    <h2 className="text-lg font-semibold">{product.typeCode}</h2>
                    <p className="text-xs text-ink-faint">
                        {[
                            assetClassLabel(assetClassOf(vocabulary, product.typeCode)),
                            vocabulary.types.find((entry) => entry.code === product.typeCode)
                                ?.instrumentTypeCode ?? '',
                        ]
                            .filter((part) => part !== '')
                            .join(' · ')}
                    </p>
                </div>
            </section>
            <section className="card space-y-3 p-4">
                <header className="space-y-1">
                    <h3 className="text-sm font-medium">{t('pricingEngines.fields.model')}</h3>
                    <p className="text-xs text-ink-faint">
                        {t('pricingEngines.workbench.definitionModel')}
                    </p>
                </header>
                <Select
                    aria-label={t('pricingEngines.fields.model')}
                    value={model}
                    onChange={(event) => {
                        const next = event.target.value;
                        setModel(next);
                        setEngine(enginesFor(vocabulary, product.typeCode, next)[0] ?? '');
                        setDrop([]);
                        setAdd([]);
                    }}
                >
                    {models.map((entry) => (
                        <option key={entry} value={entry}>
                            {entry}
                        </option>
                    ))}
                </Select>
                {model !== '' &&
                    !vocabulary.pairs.some(
                        (pair) => pair.typeCode === product.typeCode && pair.model === model,
                    ) && <Notice tone="warn">{t('pricingEngines.change.unknownPair')}</Notice>}
                {model !== '' && (
                    <p className="text-sm text-ink-muted">
                        {vocabulary.models.find((entry) => entry.code === model)?.description ??
                            t('pricingEngines.workbench.noDescription')}
                    </p>
                )}
                <ul className="divide-y divide-line-subtle">
                    {kept
                        .filter((parameter) => parameter.scope !== 'engine')
                        .map((parameter) => {
                            const doc = documentationOf(
                                vocabulary,
                                product.typeCode,
                                model,
                                engine,
                                parameter.scope,
                                parameter.name,
                            );
                            return (
                                <li key={parameter.id} className="space-y-2 py-3">
                                    <div className="flex flex-wrap items-baseline justify-between gap-3">
                                        <span className="text-sm font-medium">
                                            {doc?.label !== undefined && doc.label !== ''
                                                ? doc.label
                                                : parameter.name}
                                        </span>
                                        <div className="text-sm">
                                            <SettingValue
                                                vocabulary={vocabulary}
                                                typeCode={product.typeCode}
                                                model={model}
                                                engine={engine}
                                                name={parameter.name}
                                                scope={parameter.scope}
                                                value={parameter.value}
                                                onChange={(value) =>
                                                    setValues({ ...values, [parameter.id]: value })
                                                }
                                            />
                                        </div>
                                    </div>
                                    {doc?.kind === 'reference' &&
                                        templateInfo(vocabulary, parameter.value) !== undefined && (
                                            <p className="text-xs text-ink-faint">
                                                {
                                                    templateInfo(vocabulary, parameter.value)
                                                        ?.assetClass
                                                }{' '}
                                                ·{' '}
                                                {templateInfo(vocabulary, parameter.value)?.method}{' '}
                                                —{' '}
                                                {
                                                    templateInfo(vocabulary, parameter.value)
                                                        ?.description
                                                }
                                            </p>
                                        )}
                                    <ParameterDocuments
                                        doc={{
                                            documented: doc?.documented === true,
                                            name: parameter.name,
                                            description: doc?.description ?? '',
                                            defaultText: doc?.defaultText ?? '',
                                            format: doc?.format ?? '',
                                            introducedIn: doc?.introducedIn ?? '',
                                            removedIn: doc?.removedIn ?? '',
                                            xpath: doc?.xpath ?? '',
                                        }}
                                        mode={mode}
                                    />
                                </li>
                            );
                        })}
                </ul>
            </section>
            <section className="card space-y-3 p-4">
                <header className="space-y-1">
                    <h3 className="text-sm font-medium">{t('pricingEngines.fields.engine')}</h3>
                    <p className="text-xs text-ink-faint">
                        {t('pricingEngines.workbench.definitionEngine')}
                    </p>
                </header>
                <Select
                    aria-label={t('pricingEngines.fields.engine')}
                    value={engine}
                    onChange={(event) => {
                        setEngine(event.target.value);
                        setDrop([]);
                        setAdd([]);
                    }}
                >
                    {engines.map((entry) => (
                        <option key={entry} value={entry}>
                            {entry}
                        </option>
                    ))}
                </Select>
                {engine !== '' && (
                    <p className="text-sm text-ink-muted">
                        {vocabulary.methods.find((entry) => entry.code === engine)?.description ??
                            t('pricingEngines.workbench.noDescription')}
                    </p>
                )}
                {!known && <Notice tone="warn">{t('pricingEngines.change.unknownPair')}</Notice>}
                <ul className="divide-y divide-line-subtle">
                    {kept
                        .filter((parameter) => parameter.scope === 'engine')
                        .map((parameter) => {
                            const doc = documentationOf(
                                vocabulary,
                                product.typeCode,
                                model,
                                engine,
                                parameter.scope,
                                parameter.name,
                            );
                            return (
                                <li key={parameter.id} className="space-y-2 py-3">
                                    <div className="flex flex-wrap items-baseline justify-between gap-3">
                                        <span className="text-sm font-medium">
                                            {doc?.label !== undefined && doc.label !== ''
                                                ? doc.label
                                                : parameter.name}
                                        </span>
                                        <div className="text-sm">
                                            <SettingValue
                                                vocabulary={vocabulary}
                                                typeCode={product.typeCode}
                                                model={model}
                                                engine={engine}
                                                name={parameter.name}
                                                scope={parameter.scope}
                                                value={parameter.value}
                                                onChange={(value) =>
                                                    setValues({ ...values, [parameter.id]: value })
                                                }
                                            />
                                        </div>
                                    </div>
                                    {doc?.kind === 'reference' &&
                                        templateInfo(vocabulary, parameter.value) !== undefined && (
                                            <p className="text-xs text-ink-faint">
                                                {
                                                    templateInfo(vocabulary, parameter.value)
                                                        ?.assetClass
                                                }{' '}
                                                ·{' '}
                                                {templateInfo(vocabulary, parameter.value)?.method}{' '}
                                                —{' '}
                                                {
                                                    templateInfo(vocabulary, parameter.value)
                                                        ?.description
                                                }
                                            </p>
                                        )}
                                    <ParameterDocuments
                                        doc={{
                                            documented: doc?.documented === true,
                                            name: parameter.name,
                                            description: doc?.description ?? '',
                                            defaultText: doc?.defaultText ?? '',
                                            format: doc?.format ?? '',
                                            introducedIn: doc?.introducedIn ?? '',
                                            removedIn: doc?.removedIn ?? '',
                                            xpath: doc?.xpath ?? '',
                                        }}
                                        mode={mode}
                                    />
                                </li>
                            );
                        })}
                </ul>
            </section>
            {(change.dropped.length > 0 || change.added.length > 0) && (
                <section className="card space-y-3 p-4">
                    {change.dropped.length > 0 && (
                        <fieldset className="space-y-1">
                            <legend className="text-sm font-medium">
                                {t('pricingEngines.change.dropHeading')}
                            </legend>
                            <p className="text-xs text-ink-faint">
                                {t('pricingEngines.change.dropHint')}
                            </p>
                            {change.dropped.map((parameter) => (
                                <label
                                    key={parameter.id}
                                    className="flex items-center gap-2 text-sm"
                                >
                                    <input
                                        type="checkbox"
                                        checked={drop.includes(
                                            settingKey(parameter.scope, parameter.name),
                                        )}
                                        onChange={() =>
                                            setDrop(
                                                toggle(
                                                    drop,
                                                    settingKey(parameter.scope, parameter.name),
                                                ),
                                            )
                                        }
                                    />
                                    <span>{parameter.name}</span>
                                    <span className="font-mono text-xs text-ink-faint">
                                        {parameter.value}
                                    </span>
                                </label>
                            ))}
                        </fieldset>
                    )}
                    {change.added.length > 0 && (
                        <fieldset className="space-y-1">
                            <legend className="text-sm font-medium">
                                {t('pricingEngines.change.addHeading')}
                            </legend>
                            <p className="text-xs text-ink-faint">
                                {t('pricingEngines.change.addHint')}
                            </p>
                            {change.added.map((setting) => (
                                <label
                                    key={`${setting.scope}:${setting.name}`}
                                    className="flex items-center gap-2 text-sm"
                                >
                                    <input
                                        type="checkbox"
                                        checked={add.includes(
                                            settingKey(setting.scope, setting.name),
                                        )}
                                        onChange={() =>
                                            setAdd(
                                                toggle(
                                                    add,
                                                    settingKey(setting.scope, setting.name),
                                                ),
                                            )
                                        }
                                    />
                                    <span>{setting.name}</span>
                                    <span className="font-mono text-xs text-ink-faint">
                                        {setting.usual}
                                    </span>
                                </label>
                            ))}
                        </fieldset>
                    )}
                </section>
            )}
            <div className="flex justify-end gap-2">
                <Button variant="ghost" onClick={onCancel}>
                    {t('common.cancel')}
                </Button>
                <Button variant="primary" icon="save" onClick={() => onSave(after)}>
                    {t('pricingEngines.save.save')}
                </Button>
            </div>
        </div>
    );
}

/** Adds a product of a type the configuration does not yet price. */
function AddProductDialog({
    document,
    vocabulary,
    onClose,
}: {
    readonly document: PricingDocument;
    readonly vocabulary: PricingVocabulary;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const start = draftOf(document);
    const free = vocabulary.types.filter(
        (type) => !start.products.some((product) => product.typeCode === type.code),
    );
    const [typeCode, setTypeCode] = useState(free[0]?.code ?? '');
    const [id] = useState(newId);
    const position = Math.max(-1, ...start.products.map((entry) => entry.position)) + 1;
    const added: PricingProduct | undefined =
        typeCode === '' ? undefined : { ...newProduct(vocabulary, typeCode, position), id };
    const settings =
        added === undefined
            ? []
            : settingsFor(vocabulary, added.typeCode, added.model, added.engine);
    const after: Draft =
        added === undefined
            ? start
            : {
                  ...start,
                  products: [...start.products, added],
                  parameters: [
                      ...start.parameters,
                      ...settings
                          .filter(startsWith)
                          .map((setting, index) =>
                              newParameter(
                                  added.id,
                                  setting.scope,
                                  setting.name,
                                  setting.usual,
                                  start.parameters.length + index,
                              ),
                          ),
                  ],
              };
    return (
        <SaveDialog
            title={t('pricingEngines.change.addTitle', { name: document.name })}
            document={document}
            after={after}
            vocabulary={vocabulary}
            onClose={onClose}
        >
            {free.length === 0 ? (
                <Notice tone="info">{t('pricingEngines.change.everyTypeHeld')}</Notice>
            ) : (
                <Field label={t('pricingEngines.fields.type')}>
                    <Select value={typeCode} onChange={(event) => setTypeCode(event.target.value)}>
                        {free.map((type) => (
                            <option key={type.code} value={type.code}>
                                {type.code}
                            </option>
                        ))}
                    </Select>
                </Field>
            )}
        </SaveDialog>
    );
}

/** Removes a product and the settings it holds. */
function RemoveProductDialog({
    document,
    vocabulary,
    productId,
    onClose,
}: {
    readonly document: PricingDocument;
    readonly vocabulary: PricingVocabulary;
    readonly productId: string;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const start = draftOf(document);
    const product = start.products.find((candidate) => candidate.id === productId);
    const after: Draft = {
        ...start,
        products: start.products.filter((candidate) => candidate.id !== productId),
        parameters: start.parameters.filter((parameter) => parameter.productId !== productId),
    };
    return (
        <SaveDialog
            title={t('pricingEngines.change.removeTitle', { type: product?.typeCode ?? '' })}
            document={document}
            after={after}
            vocabulary={vocabulary}
            onClose={onClose}
        >
            <Notice tone="warn">{t('pricingEngines.change.removeHint')}</Notice>
        </SaveDialog>
    );
}

/** Removes the whole configuration. */
function DeleteDialog({
    document,
    onClose,
    onDeleted,
}: {
    readonly document: PricingDocument;
    readonly onClose: () => void;
    readonly onDeleted: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const remove = useMutation({
        mutationFn: () => pricingEngines.remove(document.id),
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['records', 'pricing-engines'] });
            onDeleted();
        },
    });
    return (
        <Dialog
            title={t('pricingEngines.change.deleteTitle', { name: document.name })}
            name="pricing-engine-delete"
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" onClick={onClose}>
                        {t('common.cancel')}
                    </Button>
                    <Button
                        variant="danger"
                        icon="delete"
                        pending={remove.isPending}
                        onClick={() => remove.mutate()}
                    >
                        {t('pricingEngines.actions.delete')}
                    </Button>
                </>
            }
        >
            <div className="space-y-3">
                {remove.isError && <Notice tone="error">{remove.error.message}</Notice>}
                <p className="text-sm">
                    {t('pricingEngines.change.deleteHint', {
                        products: String(document.products.length),
                        parameters: String(document.parameters.length),
                    })}
                </p>
            </div>
        </Dialog>
    );
}
