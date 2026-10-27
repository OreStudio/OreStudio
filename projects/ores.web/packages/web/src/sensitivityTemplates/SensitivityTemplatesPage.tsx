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
import type { SensitivityTemplate } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { RecordList, type ListSource } from '../refdata/RecordList.js';
import { LeavingFooter, useCloseGuard } from '../refdata/records.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { sensitivityTemplatePath } from '../shell/paths.js';
import { Button, Dialog, Field, Input, Notice } from '../ui/Primitives.js';
import { useChoices, useSensitivityTemplatePermissions } from './choices.js';
import { MethodCode, methodCodeOf, TemplateFields } from './TemplateForm.js';

/** The reason a new template is written with; no other reason applies to a new record. */
const NEW_RECORD_REASON = 'system.new_record';

/**
 * The sensitivity templates: the keys a pricing engine configuration puts on a
 * product, with the asset class and the method each key names.
 *
 * A row opens its own page. The list searches and orders on the server, so both
 * reach every key and not the page on the screen.
 */
export function SensitivityTemplatesPage(): ReactNode {
    const { t } = useTranslation();
    const root = useAreaRoot('reporting');
    const may = useSensitivityTemplatePermissions();
    const [adding, setAdding] = useState(false);

    const source: ListSource<SensitivityTemplate> = {
        key: 'sensitivity-templates',
        read: (page) => api.sensitivityTemplates(page),
        search: true,
        // The store orders by the values it holds. The two lookups read as the
        // words they hold, and those live in another table, so the two columns
        // that show them are not offered as orders the server cannot keep.
        sortable: ['code', 'description'],
        mayAdd: may.write,
    };

    return (
        <>
            <RecordList
                source={source}
                title={t('sensitivityTemplates.title')}
                lead={t('sensitivityTemplates.lead')}
                crumbs={[...root, { label: t('sensitivityTemplates.title') }]}
                pathOf={(row) => sensitivityTemplatePath(row.code)}
                keyOf={(row) => row.code}
                notes={
                    may.write ? [] : [{ tone: 'info', text: t('sensitivityTemplates.readerOnly') }]
                }
                addLabel={t('sensitivityTemplates.add')}
                onAdd={() => setAdding(true)}
                columns={[
                    {
                        id: 'code',
                        header: t('sensitivityTemplates.code'),
                        cell: (row) => row.code,
                        mono: true,
                        sort: 'code',
                    },
                    {
                        id: 'asset_class',
                        header: t('sensitivityTemplates.assetClass'),
                        cell: (row) => row.assetClassName,
                    },
                    {
                        id: 'method',
                        header: t('sensitivityTemplates.method'),
                        cell: (row) => row.methodName,
                    },
                    {
                        id: 'description',
                        header: t('sensitivityTemplates.description'),
                        cell: (row) => row.description,
                        sort: 'description',
                    },
                ]}
            />
            {adding && <AddTemplateDialog onClose={() => setAdding(false)} />}
        </>
    );
}

/**
 * Adds a key. The code is stated once and never changed, and the method code
 * follows from the method rather than being typed.
 */
export function AddTemplateDialog({ onClose }: { readonly onClose: () => void }): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const queries = useQueryClient();
    const choices = useChoices();
    const [code, setCode] = useState('');
    const [methodCode, setMethodCode] = useState(choices?.methods[0]?.code ?? '');
    const [assetClassCode, setAssetClassCode] = useState(choices?.assetClasses[0]?.code ?? '');
    const [description, setDescription] = useState('');
    const guard = useCloseGuard(code.trim() !== '' || description.trim() !== '', onClose);
    const add = useMutation({
        mutationFn: () =>
            api.addSensitivityTemplate({
                code: code.trim(),
                methodCode,
                assetClassCode: assetClassCode === '' ? null : assetClassCode,
                description: description.trim(),
                reasonCode: NEW_RECORD_REASON,
                commentary: '',
            }),
        onSuccess: async (added) => {
            await queries.invalidateQueries({ queryKey: ['sensitivity-templates'] });
            void navigate(sensitivityTemplatePath(added.code));
        },
    });

    return (
        <Dialog
            title={t('sensitivityTemplates.addTitle')}
            onClose={guard.close}
            footer={
                guard.leaving ? (
                    <LeavingFooter onStay={guard.stay} onLeave={onClose} />
                ) : (
                    <>
                        <Button variant="ghost" icon="cancel" onClick={guard.close}>
                            {t('refdata.records.cancel')}
                        </Button>
                        <Button
                            variant="primary"
                            icon="add"
                            pending={add.isPending}
                            disabled={code.trim() === '' || methodCode === ''}
                            onClick={() => add.mutate()}
                        >
                            {t('refdata.records.add')}
                        </Button>
                    </>
                )
            }
        >
            <div className="space-y-3">
                <Field
                    label={t('sensitivityTemplates.code')}
                    hint={t('sensitivityTemplates.codeHelp')}
                >
                    <Input
                        value={code}
                        maxLength={100}
                        onChange={(event) => setCode(event.target.value)}
                    />
                </Field>
                <TemplateFields
                    methodCode={methodCode}
                    assetClassCode={assetClassCode}
                    description={description}
                    onMethod={setMethodCode}
                    onAssetClass={setAssetClassCode}
                    onDescription={setDescription}
                />
                <MethodCode code={methodCodeOf(choices, methodCode)} />
                <p className="text-xs text-ink-faint">{t('sensitivityTemplates.newRecordNote')}</p>
                {add.isError && <Notice tone="error">{add.error.message}</Notice>}
                {guard.leaving && <Notice tone="warn">{t('refdata.records.unsaved')}</Notice>}
            </div>
        </Dialog>
    );
}
