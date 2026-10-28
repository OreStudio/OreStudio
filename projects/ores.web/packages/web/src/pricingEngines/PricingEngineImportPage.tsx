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
 * The Import steps of a pricing engine configuration, at their own address
 * (RS-ORE-05).
 *
 * The document is one configuration, so the pair's Import stands on the list
 * header and the steps are five: a configuration is not a single table row, and
 * a file has to say which configuration it is written into. The browser never
 * reads the XML: the server parses it and answers what it holds.
 */

import { useQuery } from '@tanstack/react-query';
import type { ReactNode } from 'react';
import { pricingEngines } from '../api/pricingEngines.js';
import { useTranslation } from '../i18n/Provider.js';
import {
    OreImportPage,
    type OreImportChoice,
    type OreImportKind,
    type OreImportTarget,
} from '../ore/OreExchange.js';
import { PATHS } from '../shell/paths.js';

/** What the review offers for a row, the first being the default (RS-ORE-05). */
const IMPORT_CHOICES: Record<string, readonly string[]> = {
    removed: ['keep', 'delete'],
};

/** The name a configuration is offered under: its variant, and what it holds. */
function targetDetail(
    variant: string,
    products: number,
    settings: number,
    t: (key: string, values?: Record<string, string>) => string,
): string {
    return t('pricingEngines.importTargetDetail', {
        variant: variant === '' ? t('pricingEngines.cell.noVariant') : variant,
        products: String(products),
        settings: String(settings),
    });
}

export function PricingEngineImportPage(): ReactNode {
    const { t } = useTranslation();
    const configurations = useQuery({
        queryKey: ['pricing-engines', 'import-targets'],
        queryFn: () => pricingEngines.list(),
    });
    const targets: readonly OreImportTarget[] = (configurations.data ?? []).map((row) => ({
        id: row.id,
        name: row.name,
        detail: targetDetail(row.variant, row.products, row.parameters, t),
    }));

    const choicesOf = (change: string): readonly OreImportChoice[] =>
        (IMPORT_CHOICES[change] ?? []).map((id) => ({
            id,
            label: t(`refdata.ore.choices.${id}`),
        }));

    const kind: OreImportKind = {
        words: 'pricingEngines',
        area: 'reporting',
        targetKind: 'configuration',
        rootElement: 'PricingEngines',
        listPath: PATHS.pricingEngines,
        targets,
        choicesFor: (row) => choicesOf(row.change),
        preview: async (content, targetId) => {
            const result = await pricingEngines.previewOreImport(content, targetId);
            return {
                success: result.success,
                message: result.message,
                targetName: result.target_name,
                holds: {
                    counts: [
                        {
                            id: 'products',
                            label: t('pricingEngines.importCounts.products'),
                            value: result.in_file_products,
                        },
                        {
                            id: 'settings',
                            label: t('pricingEngines.importCounts.settings'),
                            value: result.in_file_parameters,
                        },
                    ],
                    lines: [
                        ...result.products.map((product) =>
                            t('pricingEngines.importProduct', {
                                type: product.type_code,
                                model: product.model,
                                engine: product.engine,
                                settings: String(product.parameters),
                            }),
                        ),
                        ...result.global_parameters.map((name) =>
                            t('pricingEngines.importGlobal', { name }),
                        ),
                    ],
                    note: t('pricingEngines.importCountsNote'),
                },
                rows: result.rows.map((row) => ({
                    key: row.key,
                    change: row.change,
                    // A product is named by its type and its place among the
                    // products of that type; a setting by the product it
                    // belongs to and its own name.
                    name: row.entity === 'parameter' ? `${row.owner} · ${row.name}` : row.key,
                    what: row.what,
                })),
                issues: result.issues,
                summary: t('pricingEngines.importSummary', {
                    added: String(result.added),
                    changed: String(result.changed),
                    removed: String(result.removed),
                    held: String(result.held),
                }),
            };
        },
        write: async ({ content, target, choices, intent }) => {
            const result = await pricingEngines.importOre({
                content,
                configurationId: target.id,
                newName: target.newName,
                newVariant: target.newVariant,
                removalsKept: Object.entries(choices)
                    .filter(([, choice]) => choice === 'keep')
                    .map(([key]) => key),
                intent,
            });
            return {
                success: result.success,
                message: result.message,
                errors: result.errors.map((error) =>
                    error.key === '' ? error.message : `${error.key}: ${error.message}`,
                ),
                summary: t('pricingEngines.imported', {
                    name: result.configuration_name,
                    added: String(result.added),
                    changed: String(result.changed),
                    deleted: String(result.deleted),
                    kept: String(result.kept),
                }),
            };
        },
    };

    return <OreImportPage kind={kind} />;
}
