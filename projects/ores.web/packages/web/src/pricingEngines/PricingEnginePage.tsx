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
import { PERMISSION } from '../access/permissions.js';
import { useEntityChanges } from '../events/useEntityChanges.js';
import { useLoadState } from '../events/useLoadState.js';
import { useTranslation } from '../i18n/Provider.js';
import { HistoryPanel } from '../refdata/HistoryPanel.js';
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
import { PRICING_ENGINES_EVENTS } from './PricingEnginesPage.js';
import { ModelEnginePanels, SettingValue } from './parts.js';
import { SaveDialog } from './SaveDialog.js';
import {
    draftOf,
    enginesFor,
    isKnownPair,
    modelsFor,
    newId,
    newParameter,
    newProduct,
    pairChange,
    settingsFor,
    startsWith,
    templateInfo,
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

/** The entity type of the configuration header, for its history. */
const ENTITY_TYPE = 'ores.analytics.pricing_model_config';

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
            {tabs.tab === 'details' && (
                <DetailsTab
                    document={document}
                    mayWrite={mayWrite}
                    onEdit={() => setDialog({ kind: 'details' })}
                />
            )}
            {tabs.tab === 'globals' && (
                <GlobalParametersTab document={document} vocabulary={known} mode={mode} />
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
            {tabs.tab === 'history' && (
                <HistoryPanel entityType={ENTITY_TYPE} entityId={document.id} />
            )}
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
function DetailsTab({
    document,
    mayWrite,
    onEdit,
}: {
    readonly document: PricingDocument;
    readonly mayWrite: boolean;
    readonly onEdit: () => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card space-y-4 p-4">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-sm font-medium">{t('pricingEngines.record.details')}</h2>
                {mayWrite && (
                    <Button size="sm" icon="edit" onClick={onEdit}>
                        {t('pricingEngines.actions.change')}
                    </Button>
                )}
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
}: {
    readonly document: PricingDocument;
    readonly vocabulary: PricingVocabulary | undefined;
    readonly mode: EditingMode;
}): ReactNode {
    const { t } = useTranslation();
    const globals = document.parameters.filter((parameter) => parameter.productId === null);
    return (
        <section className="card space-y-4 p-4">
            <header className="space-y-1">
                <h2 className="text-sm font-medium">{t('pricingEngines.globals.heading')}</h2>
                <p className="text-sm text-ink-muted">{t('pricingEngines.globals.lead')}</p>
            </header>
            {globals.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('pricingEngines.globals.empty')}</p>
            ) : (
                <div>
                    {globals.map((parameter) => (
                        <ParameterRow
                            key={parameter.id}
                            parameter={parameter}
                            doc={(vocabulary?.globals ?? []).find(
                                (entry) => entry.name === parameter.name,
                            )}
                            mode={mode}
                        />
                    ))}
                </div>
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
                            <details key={assetClass} open className="group/class">
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
                                            onClick={() => onSelect(product.id)}
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
                        document={document}
                        draft={draft}
                        vocabulary={vocabulary}
                        product={chosen}
                        mayWrite={mayWrite}
                        mode={mode}
                        onRemove={() => onRemove(chosen.id)}
                    />
                )}
            </div>
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
    onRemove,
}: {
    readonly document: PricingDocument;
    readonly draft: Draft;
    readonly vocabulary: PricingVocabulary | undefined;
    readonly product: PricingProduct;
    readonly mayWrite: boolean;
    readonly mode: EditingMode;
    readonly onRemove: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const [editing, setEditing] = useState(false);
    const [saving, setSaving] = useState<Draft | null>(null);
    const type = vocabulary?.types.find((candidate) => candidate.code === product.typeCode);
    const parameters = parametersOf(draft, product.id);
    const settings = vocabulary
        ? settingsFor(vocabulary, product.typeCode, product.model, product.engine)
        : [];
    const settingOf = (scope: string, name: string) =>
        settings.find((setting) => setting.scope === scope && setting.name === name);
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
                            <Button size="sm" icon="edit" onClick={() => setEditing(true)}>
                                {t('pricingEngines.actions.edit')}
                            </Button>
                            <Button size="sm" variant="danger" icon="delete" onClick={onRemove}>
                                {t('pricingEngines.actions.remove')}
                            </Button>
                        </div>
                    )}
                </header>
                <details>
                    <summary className="cursor-pointer text-xs text-ink-faint">
                        {t('pricingEngines.workbench.documentation')}
                    </summary>
                    <p className="pt-1 text-sm text-ink-muted">
                        {type !== undefined && type.documentation !== ''
                            ? type.documentation
                            : t('pricingEngines.workbench.noDescription')}
                    </p>
                </details>
            </section>
            {editing && vocabulary !== undefined ? (
                <ProductEditor
                    document={document}
                    vocabulary={vocabulary}
                    productId={product.id}
                    onCancel={() => setEditing(false)}
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
                pairKnown={
                    vocabulary === undefined
                        ? undefined
                        : isKnownPair(vocabulary, product.typeCode, product.model, product.engine)
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
    readonly mode: EditingMode;
    readonly pairKnown?: boolean | undefined;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card space-y-3 p-4">
            <header className="space-y-1">
                <h3 className="text-sm font-medium">{title}</h3>
                <p className="text-xs text-ink-faint">{definition}</p>
            </header>
            <div className="space-y-1">
                <div className="font-mono text-sm">{code}</div>
                {description === '' ? (
                    <p className="text-sm text-ink-faint">
                        {t('pricingEngines.workbench.noDescription')}
                    </p>
                ) : (
                    <p className="text-sm text-ink-muted">{description}</p>
                )}
                {detail !== '' && <p className="text-sm text-ink-muted">{detail}</p>}
                {source !== '' && (
                    <p className="text-xs text-ink-faint">{t(`pricingEngines.source.${source}`)}</p>
                )}
                {pairKnown === false && (
                    <p className="text-xs text-ink-faint">
                        {t('pricingEngines.change.unknownPair')}
                    </p>
                )}
            </div>
            <div>
                <h4 className="pb-1 text-xs text-ink-faint">
                    {t('pricingEngines.workbench.parameters')}
                </h4>
                {parameters.length === 0 ? (
                    <p className="text-sm text-ink-faint">
                        {t('pricingEngines.workbench.noParameters')}
                    </p>
                ) : (
                    parameters.map((parameter) => (
                        <ParameterRow
                            key={parameter.id}
                            parameter={parameter}
                            doc={settingOf(parameter.scope, parameter.name)}
                            mode={mode}
                        />
                    ))
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
    mode,
}: {
    readonly parameter: PricingParameter;
    readonly doc: ParameterDoc | undefined;
    readonly mode: EditingMode;
}): ReactNode {
    const { t } = useTranslation();
    const label = doc?.label !== undefined && doc.label !== '' ? doc.label : parameter.name;
    const help = doc?.help ?? '';
    const description = doc?.description ?? '';
    return (
        <div className="border-t border-line-subtle py-2">
            <div className="flex flex-wrap items-baseline justify-between gap-2">
                <span className="text-sm">{label}</span>
                <span className="font-mono text-xs">{parameter.value}</span>
            </div>
            <div className="font-mono text-[10px] text-ink-faint">{parameter.name}</div>
            {help !== '' && <p className="pt-1 text-xs text-ink-muted">{help}</p>}
            <details className="mt-1">
                <summary className="cursor-pointer text-xs text-ink-faint">
                    {t('pricingEngines.workbench.documentation')}
                </summary>
                <div className="space-y-1 pt-1 text-xs text-ink-muted">
                    {description !== '' && description !== help && <p>{description}</p>}
                    {doc === undefined ? (
                        <p>{t('pricingEngines.globals.noDocumentation')}</p>
                    ) : (
                        <>
                            {doc.defaultText !== undefined && doc.defaultText !== '' && (
                                <p className="text-ink-faint">
                                    {t('pricingEngines.workbench.default')}:{' '}
                                    <span className="font-mono">{doc.defaultText}</span>
                                </p>
                            )}
                            {mode === 'advanced' && (
                                <>
                            {doc.format !== undefined && doc.format !== '' && (
                                <p className="text-ink-faint">
                                    {t('pricingEngines.workbench.format')}: {doc.format}
                                </p>
                            )}
                            {doc.required === true && (
                                <p className="text-ink-faint">
                                    {t('pricingEngines.workbench.required')}
                                </p>
                            )}
                            {doc.introducedIn !== undefined && doc.introducedIn !== '' && (
                                <p className="text-ink-faint">
                                    {t('pricingEngines.workbench.introduced')}: {doc.introducedIn}
                                </p>
                            )}
                            {doc.xpath !== undefined && doc.xpath !== '' && (
                                <p className="font-mono text-[10px] text-ink-faint">{doc.xpath}</p>
                            )}
                                </>
                            )}
                        </>
                    )}
                </div>
            </details>
        </div>
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
    onCancel,
    onSave,
}: {
    readonly document: PricingDocument;
    readonly vocabulary: PricingVocabulary;
    readonly productId: string;
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
        <section className="card space-y-4 p-4">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-sm font-medium">
                    {t('pricingEngines.change.productTitle', { type: product.typeCode })}
                </h2>
            </header>
            <div className="space-y-4">
                <ModelEnginePanels
                    vocabulary={vocabulary}
                    typeCode={product.typeCode}
                    model={model}
                    engine={engine}
                    models={models}
                    engines={engines}
                    onModel={(next) => {
                        setModel(next);
                        setDrop([]);
                        setAdd([]);
                    }}
                    onEngine={(next) => {
                        setEngine(next);
                        setDrop([]);
                        setAdd([]);
                    }}
                />
                {!known && <Notice tone="warn">{t('pricingEngines.change.unknownPair')}</Notice>}
                {change.dropped.length > 0 && (
                    <fieldset className="space-y-1">
                        <legend className="text-sm font-medium">
                            {t('pricingEngines.change.dropHeading')}
                        </legend>
                        <p className="text-xs text-ink-faint">
                            {t('pricingEngines.change.dropHint')}
                        </p>
                        {change.dropped.map((parameter) => (
                            <label key={parameter.id} className="flex items-center gap-2 text-sm">
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
                                    checked={add.includes(settingKey(setting.scope, setting.name))}
                                    onChange={() =>
                                        setAdd(toggle(add, settingKey(setting.scope, setting.name)))
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
                {kept.length > 0 && (
                    <fieldset className="space-y-2">
                        <legend className="text-sm font-medium">
                            {t('pricingEngines.change.valuesHeading')}
                        </legend>
                        <div className="grid gap-3 sm:grid-cols-2">
                            {kept.map((parameter) => (
                                <SettingValue
                                    key={parameter.id}
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
                            ))}
                        </div>
                    </fieldset>
                )}
            </div>
            <div className="flex justify-end gap-2 border-t border-line-subtle pt-2">
                <Button variant="ghost" onClick={onCancel}>
                    {t('common.cancel')}
                </Button>
                <Button variant="primary" icon="save" onClick={() => onSave(after)}>
                    {t('pricingEngines.save.save')}
                </Button>
            </div>
        </section>
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
