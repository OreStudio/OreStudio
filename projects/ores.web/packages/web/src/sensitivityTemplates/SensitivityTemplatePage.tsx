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
import { useNavigate, useParams } from 'react-router';
import type { HistoryVersion, SensitivityTemplate } from '@ores/wire-protocol/browser';
import { api } from '../api/client.js';
import { useTranslation } from '../i18n/Provider.js';
import { fieldValue } from '../refdata/HistoryPanel.js';
import { RecordTimeline } from '../refdata/RecordTimeline.js';
import {
    DeleteDialog,
    LeavingFooter,
    RecordDetails,
    RecordHeader,
    RevertVersionDialog,
    useCloseGuard,
    useRecordTabs,
} from '../refdata/records.js';
import { ReasonFields, useReason } from '../refdata/shared.js';
import { useAreaRoot } from '../shell/AreaTrail.js';
import { PATHS } from '../shell/paths.js';
import { Button, Dialog, Notice } from '../ui/Primitives.js';
import {
    ENTITY_TYPE,
    useChoices,
    useSensitivityTemplate,
    useSensitivityTemplatePermissions,
} from './choices.js';
import { MethodCode, methodCodeOf, TemplateFields } from './TemplateForm.js';

/**
 * One sensitivity template: the key, the asset class and the method it names,
 * and its description, with its history as the second tab.
 *
 * The page says what it does not decide, because the shifts a key implies are
 * the sensitivity configuration's and a person looking for them arrives here.
 */
export function SensitivityTemplatePage(): ReactNode {
    const { t } = useTranslation();
    const { code } = useParams();
    const template = useSensitivityTemplate(code ?? '');

    if (template.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    if (template.isError) {
        return <Notice tone="error">{template.error.message}</Notice>;
    }
    return <TemplateBody template={template.data} />;
}

function TemplateBody({ template }: { readonly template: SensitivityTemplate }): ReactNode {
    const { t } = useTranslation();
    const navigate = useNavigate();
    const queries = useQueryClient();
    const root = useAreaRoot('reporting');
    const may = useSensitivityTemplatePermissions();
    const [editing, setEditing] = useState(false);
    const [removing, setRemoving] = useState(false);
    const [reverting, setReverting] = useState<HistoryVersion | null>(null);
    const { tab, bar } = useRecordTabs({
        label: template.code,
        tabs: ['details', 'history'],
    });
    /** Re-reads the key and its history after a correction. */
    const reload = async (): Promise<void> => {
        await queries.invalidateQueries({ queryKey: ['sensitivity-templates'] });
        await queries.invalidateQueries({ queryKey: ['history', ENTITY_TYPE, template.code] });
    };

    /**
     * Forgets the key that was just removed. A read of it now answers 404, so
     * the page would report a missing record for a key it removed on purpose;
     * the list stays to be re-read, and the page leaves before that matters.
     */
    const forget = async (): Promise<void> => {
        queries.removeQueries({ queryKey: ['sensitivity-templates', 'key', template.code] });
        await queries.invalidateQueries({ queryKey: ['sensitivity-templates'] });
    };

    return (
        <div className="space-y-4">
            <RecordHeader
                crumbs={[
                    ...root,
                    { label: t('sensitivityTemplates.title'), to: PATHS.sensitivityTemplates },
                    { label: template.code },
                ]}
                title={template.code}
                recordKey={template.code}
                version={template.version}
                access={may.write}
                onEdit={may.write ? () => setEditing(true) : undefined}
                onDelete={may.remove ? () => setRemoving(true) : undefined}
            />
            {bar}
            {tab === 'details' && (
                <div className="space-y-4">
                    <p className="max-w-3xl text-sm text-ink-muted">
                        {t('sensitivityTemplates.decides')}
                    </p>
                    <Details template={template} />
                    {!may.write && (
                        <Notice tone="info">{t('sensitivityTemplates.readerOnly')}</Notice>
                    )}
                </div>
            )}
            {tab === 'history' && (
                <RecordTimeline
                    entityType={ENTITY_TYPE}
                    entityId={template.code}
                    {...(may.write
                        ? { onRevert: (version: HistoryVersion) => setReverting(version) }
                        : {})}
                />
            )}
            {editing && (
                <EditTemplateDialog template={template} onClose={() => setEditing(false)} />
            )}
            {removing && (
                <DeleteDialog
                    title={t('refdata.records.deleteTitle', { code: template.code })}
                    warning={t('sensitivityTemplates.removeWarning')}
                    onClose={() => setRemoving(false)}
                    onRemoved={() => void navigate(PATHS.sensitivityTemplates)}
                    remove={async (intent) => {
                        await api.removeSensitivityTemplate(template.code, {
                            version: template.version,
                            ...intent,
                        });
                        await forget();
                    }}
                />
            )}
            {reverting !== null && (
                <RevertVersionDialog
                    current={template.version}
                    version={reverting}
                    changes={[
                        [t('sensitivityTemplates.assetClass'), template.assetClass, 'Asset Class'],
                        [t('sensitivityTemplates.method'), template.method, 'Method'],
                        [
                            t('sensitivityTemplates.description'),
                            template.description,
                            'Description',
                        ],
                    ]
                        .map(([label, from, name]) => ({
                            label: label ?? '',
                            from: from ?? '',
                            to: fieldValue(reverting, name ?? ''),
                        }))
                        .filter((change) => change.from !== change.to)}
                    onClose={() => setReverting(null)}
                    write={async (intent) => {
                        await api.correctSensitivityTemplate(template.code, {
                            assetClass: fieldValue(reverting, 'Asset Class'),
                            method: fieldValue(reverting, 'Method'),
                            description: fieldValue(reverting, 'Description'),
                            version: template.version,
                            ...intent,
                        });
                        await reload();
                    }}
                />
            )}
        </div>
    );
}

/** The key's values, in the order a person reads them, then its provenance. */
function Details({ template }: { readonly template: SensitivityTemplate }): ReactNode {
    const { t } = useTranslation();
    const text = (value: string): ReactNode =>
        value === '' ? <span className="text-ink-faint">—</span> : value;
    return (
        <RecordDetails
            specs={[]}
            extra={[
                [
                    t('sensitivityTemplates.assetClass'),
                    <span className="font-mono text-xs">{text(template.assetClass)}</span>,
                ],
                [t('sensitivityTemplates.method'), text(template.method)],
                [
                    t('sensitivityTemplates.methodCode'),
                    <span className="font-mono text-xs">{text(template.methodCode)}</span>,
                    t('sensitivityTemplates.methodCodeHelp'),
                ],
                [t('sensitivityTemplates.description'), text(template.description)],
            ]}
            row={{
                version: template.version,
                modified_by: template.modifiedBy,
                performed_by: template.performedBy,
                recorded_at: template.recordedAt,
                change_reason_code: template.reasonCode,
                change_commentary: template.commentary,
            }}
        />
    );
}

/**
 * Corrects a template against the version read. The code is fixed once the key
 * exists, so it stands as words rather than a field, and the method code follows
 * from the method.
 */
export function EditTemplateDialog({
    template,
    onClose,
}: {
    readonly template: SensitivityTemplate;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const choices = useChoices();
    const [assetClass, setAssetClass] = useState(template.assetClass);
    const [method, setMethod] = useState(template.method);
    const [description, setDescription] = useState(template.description);
    const [commentary, setCommentary] = useState('');
    const changed =
        assetClass !== template.assetClass ||
        method !== template.method ||
        description.trim() !== template.description;
    const reason = useReason('amend', changed);
    const guard = useCloseGuard(changed || commentary.trim() !== '', onClose);
    const missing = reason.needsCommentary && commentary.trim() === '';
    const save = useMutation({
        mutationFn: () =>
            api.correctSensitivityTemplate(template.code, {
                assetClass,
                method,
                description: description.trim(),
                version: template.version,
                reasonCode: reason.code,
                commentary: commentary.trim(),
            }),
        onSuccess: async () => {
            await queries.invalidateQueries({ queryKey: ['sensitivity-templates'] });
            await queries.invalidateQueries({
                queryKey: ['history', ENTITY_TYPE, template.code],
            });
            onClose();
        },
    });

    return (
        <Dialog
            title={t('refdata.records.editTitle', { code: template.code })}
            onClose={guard.close}
            wide
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
                            icon="save"
                            pending={save.isPending}
                            disabled={!changed || reason.code === '' || missing}
                            onClick={() => save.mutate()}
                        >
                            {t('refdata.records.save')}
                        </Button>
                    </>
                )
            }
        >
            <div className="space-y-3">
                <div>
                    <span className="block text-sm font-medium text-ink-muted">
                        {t('sensitivityTemplates.code')}
                    </span>
                    <p className="mt-0.5 font-mono text-xs">{template.code}</p>
                    <p className="mt-0.5 text-xs text-ink-faint">
                        {t('refdata.records.fixedHint')}
                    </p>
                </div>
                <TemplateFields
                    assetClass={assetClass}
                    method={method}
                    description={description}
                    onAssetClass={setAssetClass}
                    onMethod={setMethod}
                    onDescription={setDescription}
                />
                <MethodCode code={methodCodeOf(choices, method)} />
                <ReasonFields
                    reason={reason}
                    commentary={commentary}
                    onCommentary={setCommentary}
                    missing={missing}
                />
                {save.isError && <Notice tone="error">{save.error.message}</Notice>}
                {guard.leaving && <Notice tone="warn">{t('refdata.records.unsaved')}</Notice>}
            </div>
        </Dialog>
    );
}
