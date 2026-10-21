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
    type PricingProduct,
    type PricingVocabulary,
} from '../api/pricingEngines.js';
import { ApiFailure } from '../api/transport.js';
import { useHolds } from '../access/holds.js';
import { PERMISSION } from '../access/permissions.js';
import { useEntityChanges } from '../events/useEntityChanges.js';
import { useLoadState } from '../events/useLoadState.js';
import { useTranslation } from '../i18n/Provider.js';
import { RecordHeader } from '../refdata/records.js';
import { Crumbs } from '../refdata/shared.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { PATHS } from '../shell/paths.js';
import { RefreshButton } from '../ui/RefreshButton.js';
import { Button, Detail, Dialog, Field, Input, Notice, Select, Tag } from '../ui/Primitives.js';
import { RelativeTime } from '../ui/Time.js';
import { PRICING_ENGINES_EVENTS } from './PricingEnginesPage.js';
import { SettingValue } from './parts.js';
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

/** The eight characters a person reads to tell one identifier from another. */
function shortId(id: string): string {
    return id.slice(0, 8);
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
    const [dialog, setDialog] = useState<
        | { readonly kind: 'details' }
        | { readonly kind: 'product'; readonly productId: string }
        | { readonly kind: 'add' }
        | { readonly kind: 'remove'; readonly productId: string }
        | { readonly kind: 'delete' }
        | null
    >(null);
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

    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[...trail, { label: document.name }]}
                title={document.name}
                recordKey={shortId(document.id)}
                version={document.version}
                subdued
                access={mayWrite}
                own={<RefreshButton onClick={reload} load={load} news={news} />}
                onEdit={mayWrite ? () => setDialog({ kind: 'details' }) : undefined}
                onDelete={mayDelete ? () => setDialog({ kind: 'delete' }) : undefined}
            />
            <section className="card space-y-4 p-4">
                <h2 className="text-sm font-medium">{t('pricingEngines.record.details')}</h2>
                <dl className="grid gap-x-6 gap-y-3 sm:grid-cols-4">
                    <Detail label={t('pricingEngines.fields.name')} value={document.name} />
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
                    <Detail
                        label={t('pricingEngines.fields.modifiedBy')}
                        value={document.modifiedBy}
                    />
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
                <p className="text-xs text-ink-faint">{t('pricingEngines.record.reachUnknown')}</p>
            </section>
            <ProductsSection
                document={document}
                draft={draft}
                mayWrite={mayWrite && known !== undefined}
                onChange={(productId) => setDialog({ kind: 'product', productId })}
                onRemove={(productId) => setDialog({ kind: 'remove', productId })}
                onAdd={() => setDialog({ kind: 'add' })}
            />
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
            {dialog?.kind === 'product' && known !== undefined && (
                <ProductDialog
                    document={document}
                    vocabulary={known}
                    productId={dialog.productId}
                    onClose={close}
                />
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

/** The products with the settings each holds, and the actions on them. */
function ProductsSection({
    document,
    draft,
    mayWrite,
    onChange,
    onRemove,
    onAdd,
}: {
    readonly document: PricingDocument;
    readonly draft: Draft;
    readonly mayWrite: boolean;
    readonly onChange: (productId: string) => void;
    readonly onRemove: (productId: string) => void;
    readonly onAdd: () => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <section className="card space-y-4 p-4">
            <header className="flex flex-wrap items-baseline justify-between gap-2">
                <h2 className="text-sm font-medium">
                    {t('pricingEngines.record.products', {
                        count: String(document.products.length),
                    })}
                </h2>
                {mayWrite && (
                    <Button icon="add" size="sm" onClick={onAdd}>
                        {t('pricingEngines.actions.addProduct')}
                    </Button>
                )}
            </header>
            {document.products.length === 0 ? (
                <p className="text-sm text-ink-muted">{t('pricingEngines.record.noProducts')}</p>
            ) : (
                <ul className="divide-y divide-line-subtle">
                    {[...document.products]
                        .sort((a, b) => a.position - b.position)
                        .map((product) => (
                            <ProductRow
                                key={product.id}
                                product={product}
                                draft={draft}
                                mayWrite={mayWrite}
                                onChange={() => onChange(product.id)}
                                onRemove={() => onRemove(product.id)}
                            />
                        ))}
                </ul>
            )}
        </section>
    );
}

function ProductRow({
    product,
    draft,
    mayWrite,
    onChange,
    onRemove,
}: {
    readonly product: PricingProduct;
    readonly draft: Draft;
    readonly mayWrite: boolean;
    readonly onChange: () => void;
    readonly onRemove: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const parameters = parametersOf(draft, product.id);
    return (
        <li className="space-y-2 py-3">
            <div className="flex flex-wrap items-baseline justify-between gap-2">
                <div className="flex flex-wrap items-baseline gap-3">
                    <span className="font-medium">{product.typeCode}</span>
                    <span className="font-mono text-xs">
                        {product.model} / {product.engine}
                    </span>
                </div>
                {mayWrite && (
                    <div className="flex gap-2">
                        <Button size="sm" icon="edit" onClick={onChange}>
                            {t('pricingEngines.actions.change')}
                        </Button>
                        <Button size="sm" variant="danger" icon="delete" onClick={onRemove}>
                            {t('pricingEngines.actions.remove')}
                        </Button>
                    </div>
                )}
            </div>
            {parameters.length === 0 ? (
                <p className="text-xs text-ink-faint">{t('pricingEngines.record.noSettings')}</p>
            ) : (
                <dl className="grid gap-x-6 gap-y-1 text-xs sm:grid-cols-2 lg:grid-cols-3">
                    {parameters.map((parameter) => (
                        <div key={parameter.id} className="flex gap-2">
                            <dt className="text-ink-faint">
                                {parameter.name}
                                <span className="ml-1 text-[10px]">({parameter.scope})</span>
                            </dt>
                            <dd className="font-mono">{parameter.value}</dd>
                        </div>
                    ))}
                </dl>
            )}
        </li>
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
            vocabulary={vocabulary ?? { types: [], pairs: [], settings: [] }}
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
 * Changes how one product is priced: the model, the engine and the values of
 * its settings. A setting the new pair does not take is offered for dropping and
 * a setting it usually needs is offered for adding. Nothing is dropped or added
 * until the person ticks it, and a pair not known to work is marked and never
 * refused.
 */
function ProductDialog({
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
        <SaveDialog
            title={t('pricingEngines.change.productTitle', { type: product.typeCode })}
            document={document}
            after={after}
            vocabulary={vocabulary}
            onClose={onClose}
        >
            <div className="space-y-4">
                <p className="text-xs text-ink-faint">{t('pricingEngines.record.reachUnknown')}</p>
                <div className="grid gap-3 sm:grid-cols-2">
                    <Field label={t('pricingEngines.fields.model')}>
                        <Select
                            value={model}
                            onChange={(event) => {
                                setModel(event.target.value);
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
                    </Field>
                    <Field label={t('pricingEngines.fields.engine')}>
                        <Select
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
                    </Field>
                </div>
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
        </SaveDialog>
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
                          .filter((setting) => setting.usual !== '')
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
