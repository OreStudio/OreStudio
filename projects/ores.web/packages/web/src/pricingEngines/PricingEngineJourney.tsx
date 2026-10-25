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
 * The pricing engine authoring journey, from
 * doc/knowledge/journeys/reporting/journey_author_a_pricing_engine_configuration.org.
 *
 * A person who owns a party's pricing defines one configuration: a name, the
 * product types it prices, the model and engine of each, the settings, one check
 * and one save. The draft lives in the browser until the last step, so nothing is
 * written while the person walks the rail, and the save is the one write.
 *
 * The models and engines come from the vocabulary the server holds. A pair the
 * vocabulary does not know is marked and never refused, because a person may know
 * a pair the vocabulary has not met.
 */

import { useState, type ReactNode } from 'react';
import { useNavigate } from 'react-router';
import { useQuery, useQueryClient } from '@tanstack/react-query';
import { pricingEngines, type PricingVocabulary } from '../api/pricingEngines.js';
import { useTranslation } from '../i18n/Provider.js';
import { NEW_RECORD_REASON } from '../refdata/records.js';
import { pricingEnginePath } from '../shell/paths.js';
import { Button, Field, Input, Notice, Select, Tag } from '../ui/Primitives.js';
import { JourneyPage } from '../journeys/JourneyPage.js';
import { defineJourney, type StepAction } from '../journeys/runtime.js';
import {
    EMPTY_DRAFT,
    blocked,
    check,
    cloneDraft,
    difference,
    enginesFor,
    isKnownPair,
    missingUsual,
    modelsFor,
    nameInUse,
    parametersOf,
    usualPair,
    withModelAndEngine,
    withParameter,
    withProductTypes,
    withValue,
    withoutParameter,
    type Draft,
    EMPTY_VOCABULARY,
} from './model.js';
import { DifferenceList, FindingsList, ModelEnginePanels, SettingValue } from './parts.js';
import { typeName } from './model.js';
import { writeOf } from './SaveDialog.js';

/** Where the list of configurations is cached, which the name check and the copy read. */
export const HELD_KEY = ['records', 'pricing-engines', 'held'] as const;

export function PricingEngineJourney(): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const queries = useQueryClient();
    const [at, setAt] = useState(0);
    const [draft, setDraft] = useState<Draft>(EMPTY_DRAFT);
    const [copied, setCopied] = useState('');
    const [copyFailure, setCopyFailure] = useState<string>();
    const vocabulary = useQuery({
        queryKey: ['pricing-engine-vocabulary'],
        queryFn: pricingEngines.vocabulary,
        staleTime: 5 * 60_000,
        meta: { quiet: true },
    });
    const held = useQuery({
        queryKey: HELD_KEY,
        queryFn: pricingEngines.list,
        meta: { quiet: true },
    });
    const known = vocabulary.data ?? EMPTY_VOCABULARY;
    const rows = held.data ?? [];

    const copyFrom = async (id: string): Promise<void> => {
        setCopied(id);
        setCopyFailure(undefined);
        if (id === '') {
            setDraft(EMPTY_DRAFT);
            return;
        }
        try {
            const source = await pricingEngines.read(id);
            setDraft(
                cloneDraft(source, t('pricingEngines.author.name.copyOf', { name: source.name })),
            );
        } catch (error) {
            setCopyFailure(error instanceof Error ? error.message : String(error));
        }
    };

    if (vocabulary.isPending || held.isPending) {
        return (
            <p className="text-sm text-ink-muted" role="status">
                {t('common.loading')}
            </p>
        );
    }
    if (vocabulary.isError) {
        return <Notice tone="error">{vocabulary.error.message}</Notice>;
    }

    const findings = check(draft, known);
    const taken = nameInUse(rows, draft.name);
    const typeCodes = draft.products.map((product) => product.typeCode);
    const next = (label: string, enabled: boolean): StepAction => ({
        label,
        enabled,
    });

    const steps = defineJourney<ReactNode>([
        {
            id: 'name',
            title: t('pricingEngines.author.steps.name'),
            lead: t('pricingEngines.author.name.lead'),
            body: (
                <NameStep
                    draft={draft}
                    onDraft={setDraft}
                    rows={rows}
                    copied={copied}
                    onCopy={(id) => void copyFrom(id)}
                    taken={taken}
                    failure={copyFailure}
                />
            ),
            next: next(t('pricingEngines.author.next'), draft.name.trim() !== '' && !taken),
        },
        {
            id: 'products',
            title: t('pricingEngines.author.steps.products'),
            lead: t('pricingEngines.author.products.lead'),
            body: (
                <ProductsStep
                    vocabulary={known}
                    selected={typeCodes}
                    onSelect={(codes) => setDraft(withProductTypes(known, draft, codes))}
                />
            ),
            next: next(t('pricingEngines.author.next'), draft.products.length > 0),
        },
        {
            id: 'engines',
            title: t('pricingEngines.author.steps.engines'),
            lead: t('pricingEngines.author.engines.lead'),
            body: <EnginesStep vocabulary={known} draft={draft} onDraft={setDraft} />,
            next: next(
                t('pricingEngines.author.next'),
                draft.products.every((product) => product.model !== '' && product.engine !== ''),
            ),
        },
        {
            id: 'settings',
            title: t('pricingEngines.author.steps.settings'),
            lead: t('pricingEngines.author.settings.lead'),
            body: <SettingsStep vocabulary={known} draft={draft} onDraft={setDraft} />,
            next: next(t('pricingEngines.author.next'), true),
        },
        {
            id: 'check',
            title: t('pricingEngines.author.steps.check'),
            lead: t('pricingEngines.author.check.lead'),
            body: <FindingsList findings={findings} />,
            next: next(t('pricingEngines.author.check.next'), !blocked(findings)),
        },
        {
            id: 'save',
            title: t('pricingEngines.author.steps.save'),
            lead: t('pricingEngines.author.save.lead'),
            body: <SaveStep draft={draft} />,
            final: true,
            next: {
                label: t('pricingEngines.author.save.next'),
                enabled: !blocked(findings) && !taken,
                run: async () => {
                    const created = await pricingEngines.create(
                        writeOf(draft, { reason_code: NEW_RECORD_REASON, commentary: '' }),
                    );
                    await queries.invalidateQueries({ queryKey: ['records', 'pricing-engines'] });
                    await navigate(pricingEnginePath(created.id));
                },
            },
        },
    ]);

    return <JourneyPage steps={steps} at={at} onMove={setAt} />;
}

/** Step 1: the name, the description and the variant, and whether to start from a copy. */
function NameStep({
    draft,
    onDraft,
    rows,
    copied,
    onCopy,
    taken,
    failure,
}: {
    readonly draft: Draft;
    readonly onDraft: (draft: Draft) => void;
    readonly rows: readonly { readonly id: string; readonly name: string }[];
    readonly copied: string;
    readonly onCopy: (id: string) => void;
    readonly taken: boolean;
    readonly failure: string | undefined;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <div className="space-y-4">
            {failure !== undefined && <Notice tone="error">{failure}</Notice>}
            {rows.length > 0 && (
                <Field label={t('pricingEngines.author.name.start')}>
                    <Select value={copied} onChange={(event) => onCopy(event.target.value)}>
                        <option value="">{t('pricingEngines.author.name.startEmpty')}</option>
                        {rows.map((row) => (
                            <option key={row.id} value={row.id}>
                                {t('pricingEngines.author.name.startCopy', { name: row.name })}
                            </option>
                        ))}
                    </Select>
                </Field>
            )}
            <Field
                label={t('pricingEngines.fields.name')}
                {...(taken ? { error: t('pricingEngines.author.name.inUse') } : {})}
            >
                <Input
                    value={draft.name}
                    maxLength={200}
                    onChange={(event) => onDraft({ ...draft, name: event.target.value })}
                />
            </Field>
            <Field label={t('pricingEngines.fields.variant')}>
                <Input
                    value={draft.variant}
                    maxLength={200}
                    onChange={(event) => onDraft({ ...draft, variant: event.target.value })}
                />
            </Field>
            <Field label={t('pricingEngines.fields.description')}>
                <Input
                    value={draft.description}
                    maxLength={2000}
                    onChange={(event) => onDraft({ ...draft, description: event.target.value })}
                />
            </Field>
        </div>
    );
}

/** Step 2: the product types, ticked from the vocabulary. */
function ProductsStep({
    vocabulary,
    selected,
    onSelect,
}: {
    readonly vocabulary: PricingVocabulary;
    readonly selected: readonly string[];
    readonly onSelect: (codes: readonly string[]) => void;
}): ReactNode {
    const { t } = useTranslation();
    const [search, setSearch] = useState('');
    const needle = search.trim().toLowerCase();
    const shown = vocabulary.types.filter(
        (type) =>
            needle === '' ||
            type.code.toLowerCase().includes(needle) ||
            type.description.toLowerCase().includes(needle),
    );
    const toggle = (code: string): void =>
        onSelect(
            selected.includes(code)
                ? selected.filter((entry) => entry !== code)
                : [...selected, code],
        );
    return (
        <div className="space-y-3">
            <Field label={t('pricingEngines.author.products.search')}>
                <Input value={search} onChange={(event) => setSearch(event.target.value)} />
            </Field>
            <p className="text-xs text-ink-faint">
                {t('pricingEngines.author.products.selected', { count: String(selected.length) })}
            </p>
            <ul className="grid gap-1 sm:grid-cols-2">
                {shown.map((type) => (
                    <li key={type.code}>
                        <label className="flex items-center gap-2 text-sm">
                            <input
                                type="checkbox"
                                checked={selected.includes(type.code)}
                                onChange={() => toggle(type.code)}
                            />
                            <span className="font-medium">{type.code}</span>
                            <span className="truncate text-xs text-ink-faint">
                                {type.description}
                            </span>
                        </label>
                    </li>
                ))}
            </ul>
        </div>
    );
}

/** Step 3: the model and engine of each product, with the usual pair one click away. */
function EnginesStep({
    vocabulary,
    draft,
    onDraft,
}: {
    readonly vocabulary: PricingVocabulary;
    readonly draft: Draft;
    readonly onDraft: (draft: Draft) => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <ul className="divide-y divide-line-subtle">
            {draft.products.map((product) => {
                const usual = usualPair(vocabulary, product.typeCode);
                const known = isKnownPair(
                    vocabulary,
                    product.typeCode,
                    product.model,
                    product.engine,
                );
                const models = [
                    ...new Set([...modelsFor(vocabulary, product.typeCode), product.model]),
                ].filter((entry) => entry !== '');
                const engines = [
                    ...new Set([
                        ...enginesFor(vocabulary, product.typeCode, product.model),
                        product.engine,
                    ]),
                ].filter((entry) => entry !== '');
                return (
                    <li key={product.id} className="space-y-2 py-3">
                        <div className="flex flex-wrap items-baseline justify-between gap-2">
                            <span className="font-medium">
                                {typeName(vocabulary, product.typeCode)}
                            </span>
                            {!known && product.model !== '' && product.engine !== '' && (
                                <Tag tone="warn">{t('pricingEngines.author.engines.unknown')}</Tag>
                            )}
                            {usual !== undefined &&
                                (usual.model !== product.model ||
                                    usual.engine !== product.engine) && (
                                    <Button
                                        size="sm"
                                        onClick={() =>
                                            onDraft(
                                                withModelAndEngine(
                                                    draft,
                                                    product.id,
                                                    usual.model,
                                                    usual.engine,
                                                ),
                                            )
                                        }
                                    >
                                        {t('pricingEngines.author.engines.usual', {
                                            pair: `${usual.model} / ${usual.engine}`,
                                        })}
                                    </Button>
                                )}
                        </div>
                        <ModelEnginePanels
                            vocabulary={vocabulary}
                            typeCode={product.typeCode}
                            model={product.model}
                            engine={product.engine}
                            models={models}
                            engines={engines}
                            allowBlank
                            onModel={(model) =>
                                onDraft(
                                    withModelAndEngine(
                                        draft,
                                        product.id,
                                        model,
                                        enginesFor(vocabulary, product.typeCode, model)[0] ?? '',
                                    ),
                                )
                            }
                            onEngine={(engine) =>
                                onDraft(
                                    withModelAndEngine(draft, product.id, product.model, engine),
                                )
                            }
                        />
                    </li>
                );
            })}
        </ul>
    );
}

/** Step 4: the settings of each product, the usual ones offered and any other one added by hand. */
function SettingsStep({
    vocabulary,
    draft,
    onDraft,
}: {
    readonly vocabulary: PricingVocabulary;
    readonly draft: Draft;
    readonly onDraft: (draft: Draft) => void;
}): ReactNode {
    const { t } = useTranslation();
    return (
        <ul className="divide-y divide-line-subtle">
            {draft.products.map((product) => (
                <SettingsOfProduct
                    key={product.id}
                    vocabulary={vocabulary}
                    draft={draft}
                    productId={product.id}
                    onDraft={onDraft}
                    emptyText={t('pricingEngines.author.settings.none')}
                />
            ))}
        </ul>
    );
}

function SettingsOfProduct({
    vocabulary,
    draft,
    productId,
    onDraft,
    emptyText,
}: {
    readonly vocabulary: PricingVocabulary;
    readonly draft: Draft;
    readonly productId: string;
    readonly onDraft: (draft: Draft) => void;
    readonly emptyText: string;
}): ReactNode {
    const { t } = useTranslation();
    const product = draft.products.find((candidate) => candidate.id === productId);
    const [scope, setScope] = useState('engine');
    const [name, setName] = useState('');
    const [value, setValue] = useState('');
    if (product === undefined) {
        return null;
    }
    const held = parametersOf(draft, productId);
    const lacking = missingUsual(vocabulary, draft, product);
    const addable = name.trim() !== '' && value.trim() !== '';
    return (
        <li className="space-y-3 py-3">
            <div className="flex flex-wrap items-baseline justify-between gap-2">
                <span className="font-medium">{typeName(vocabulary, product.typeCode)}</span>
                <span className="font-mono text-xs">
                    {product.model} / {product.engine}
                </span>
            </div>
            {held.length === 0 && <p className="text-xs text-ink-faint">{emptyText}</p>}
            <div className="grid gap-3 sm:grid-cols-2">
                {held.map((parameter) => (
                    <div key={parameter.id} className="flex items-end gap-2">
                        <div className="grow">
                            <SettingValue
                                vocabulary={vocabulary}
                                typeCode={product.typeCode}
                                model={product.model}
                                engine={product.engine}
                                name={parameter.name}
                                scope={parameter.scope}
                                value={parameter.value}
                                onChange={(next) => onDraft(withValue(draft, parameter.id, next))}
                            />
                        </div>
                        <Button
                            size="sm"
                            variant="ghost"
                            icon="remove"
                            aria-label={t('pricingEngines.author.settings.remove', {
                                name: parameter.name,
                            })}
                            onClick={() => onDraft(withoutParameter(draft, parameter.id))}
                        />
                    </div>
                ))}
            </div>
            {lacking.length > 0 && (
                <Button
                    size="sm"
                    onClick={() =>
                        onDraft(
                            lacking.reduce(
                                (next, setting) =>
                                    withParameter(
                                        next,
                                        productId,
                                        setting.scope,
                                        setting.name,
                                        setting.usual,
                                    ),
                                draft,
                            ),
                        )
                    }
                >
                    {t('pricingEngines.author.settings.addUsual', {
                        names: lacking.map((setting) => setting.name).join(', '),
                    })}
                </Button>
            )}
            <div className="grid items-end gap-2 sm:grid-cols-[8rem_1fr_1fr_auto]">
                <Field label={t('pricingEngines.author.settings.scope')}>
                    <Select value={scope} onChange={(event) => setScope(event.target.value)}>
                        <option value="model">model</option>
                        <option value="engine">engine</option>
                    </Select>
                </Field>
                <Field label={t('pricingEngines.author.settings.name')}>
                    <Input value={name} onChange={(event) => setName(event.target.value)} />
                </Field>
                <Field label={t('pricingEngines.author.settings.value')}>
                    <Input value={value} onChange={(event) => setValue(event.target.value)} />
                </Field>
                <Button
                    icon="add"
                    disabled={!addable}
                    onClick={() => {
                        onDraft(withParameter(draft, productId, scope, name.trim(), value.trim()));
                        setName('');
                        setValue('');
                    }}
                >
                    {t('pricingEngines.author.settings.add')}
                </Button>
            </div>
        </li>
    );
}

/** Step 6: what the save writes. */
function SaveStep({ draft }: { readonly draft: Draft }): ReactNode {
    const { t } = useTranslation();
    return (
        <div className="space-y-4">
            <p className="text-sm">
                {t('pricingEngines.author.save.summary', {
                    name: draft.name,
                    products: String(draft.products.length),
                    parameters: String(draft.parameters.length),
                })}
            </p>
            <DifferenceList changes={difference(EMPTY_DRAFT, draft)} />
            <p className="text-xs text-ink-faint">{t('pricingEngines.record.reachUnknown')}</p>
        </div>
    );
}
