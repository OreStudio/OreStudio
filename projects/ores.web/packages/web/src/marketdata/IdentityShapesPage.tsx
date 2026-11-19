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
import { Fragment, useState, type ReactNode } from 'react';
import type { SessionMode } from '@ores/wire-protocol/browser';
import { useHeld } from '../access/holds.js';
import {
    REQUIREMENTS,
    familyOf,
    identityShapes,
    type Requirement,
    type Shape,
    type ShapeKey,
    type ShapeResult,
} from '../api/identityShapes.js';
import { useEntityChanges } from '../events/useEntityChanges.js';
import { useTranslation } from '../i18n/Provider.js';
import { NEW_RECORD_REASON } from '../refdata/records.js';
import { ReasonFields, useReason } from '../refdata/shared.js';
import { AreaTrail } from '../shell/AreaTrail.js';
import { ScreenFrame } from '../shell/ScreenFrame.js';
import {
    Button,
    Dialog,
    Field,
    Input,
    Notice,
    PageHeader,
    Select,
    Tag,
    Textarea,
} from '../ui/Primitives.js';
import { RefreshButton } from '../ui/RefreshButton.js';
import { RelativeTime } from '../ui/Time.js';

const WRITE = 'marketdata::series_identity_shapes:write';
const DELETE = 'marketdata::series_identity_shapes:delete';

const SHAPES_KEY = ['series-identity-shapes'] as const;

/** One family and its rows, in the order the screen lists them. */
export interface ShapeFamily {
    readonly name: string;
    readonly rows: readonly Shape[];
}

/**
 * The rows grouped by family: asset class, series subclass, then index family,
 * with the fields of a family in name order.
 */
export function familiesOf(rows: readonly Shape[]): readonly ShapeFamily[] {
    const byFamily = new Map<string, Shape[]>();
    const ordered = [...rows].sort(
        (left, right) =>
            left.asset_class.localeCompare(right.asset_class) ||
            left.series_subclass.localeCompare(right.series_subclass) ||
            left.index_family.localeCompare(right.index_family) ||
            left.field.localeCompare(right.field),
    );
    for (const row of ordered) {
        const name = familyOf(row);
        const family = byFamily.get(name) ?? [];
        family.push(row);
        byFamily.set(name, family);
    }
    return [...byFamily].map(([name, familyRows]) => ({ name, rows: familyRows }));
}

function keyOf(row: ShapeKey): ShapeKey {
    return {
        asset_class: row.asset_class,
        series_subclass: row.series_subclass,
        index_family: row.index_family,
        field: row.field,
    };
}

function sameKey(left: ShapeKey, right: ShapeKey): boolean {
    return (
        left.asset_class === right.asset_class &&
        left.series_subclass === right.series_subclass &&
        left.index_family === right.index_family &&
        left.field === right.field
    );
}

function failed(result: ShapeResult): string {
    return result.outcome === 'ok' ? '' : result.message || result.outcome;
}

/**
 * The identity shapes: for each market series family, the fields it requires
 * and the fields it forbids. The writer holds every identity it projects to
 * these rows, so a person reads here why a series was refused.
 *
 * The shared catalogue is the system tenant's and every tenant reads it beside
 * its own rows. A tenant changes only its own rows, so the screen offers a
 * change of a shared row only to the system tenant.
 */
export function IdentityShapesPage({ mode }: { readonly mode: SessionMode }): ReactNode {
    const { t } = useTranslation();
    const held = useHeld();
    const shapes = useQuery({ queryKey: SHAPES_KEY, queryFn: identityShapes.list });
    const [adding, setAdding] = useState(false);
    const [editing, setEditing] = useState<Shape | undefined>();
    const [removing, setRemoving] = useState<Shape | undefined>();
    const [selected, setSelected] = useState<ShapeKey | undefined>();
    const load = { dataUpdatedAt: shapes.dataUpdatedAt, isFetching: shapes.isFetching };
    const news = useEntityChanges(
        [{ component: 'marketdata', entity: 'series_identity_shapes' }],
        load,
    );
    const mayWrite = held(WRITE);
    const mayRemove = held(DELETE);
    const system = mode === 'system-administration';
    const mayChange = (row: Shape): boolean => !row.system || system;
    const trail = <AreaTrail area="marketdata" screen={t('marketdata.identityShapes.title')} />;

    if (shapes.data === undefined) {
        return (
            <ScreenFrame
                trail={trail}
                title={t('marketdata.identityShapes.title')}
                description={t('marketdata.identityShapes.lead')}
            >
                {shapes.isError ? (
                    <Notice tone="error">{shapes.error.message}</Notice>
                ) : (
                    <p className="text-sm text-ink-muted">{t('common.loading')}</p>
                )}
            </ScreenFrame>
        );
    }
    return (
        <div className="space-y-6">
            <div>
                {trail}
                <PageHeader
                    title={t('marketdata.identityShapes.title')}
                    description={t('marketdata.identityShapes.lead')}
                    actions={
                        <>
                            <RefreshButton
                                onClick={() => void shapes.refetch()}
                                load={load}
                                news={news}
                            />
                            {mayWrite && (
                                <Button variant="primary" onClick={() => setAdding(true)}>
                                    {t('marketdata.identityShapes.add')}
                                </Button>
                            )}
                        </>
                    }
                />
            </div>
            {shapes.data.length === 0 ? (
                <Notice>{t('marketdata.identityShapes.none')}</Notice>
            ) : (
                <div className="space-y-6">
                    {familiesOf(shapes.data).map((family) => (
                        <section key={family.name} aria-label={family.name} className="space-y-2">
                            <h2 className="font-mono text-sm text-ink">{family.name}</h2>
                            <table className="w-full text-sm">
                                <thead className="text-left text-xs text-ink-muted">
                                    <tr>
                                        <th className="w-48 py-1">
                                            {t('marketdata.identityShapes.field')}
                                        </th>
                                        <th className="w-28 py-1">
                                            {t('marketdata.identityShapes.requirement')}
                                        </th>
                                        <th className="py-1">
                                            {t('marketdata.identityShapes.description')}
                                        </th>
                                        <th className="w-24 py-1">
                                            {t('marketdata.identityShapes.owner')}
                                        </th>
                                        <th className="w-56 py-1" />
                                    </tr>
                                </thead>
                                <tbody>
                                    {family.rows.map((row) => {
                                        const open =
                                            selected !== undefined && sameKey(selected, row);
                                        return (
                                            <Fragment key={`${row.field}-${String(row.system)}`}>
                                                <tr className="border-t border-line align-top">
                                                    <td className="py-2 font-mono">{row.field}</td>
                                                    <td className="py-2">
                                                        <RequirementTag
                                                            requirement={row.requirement}
                                                        />
                                                    </td>
                                                    <td className="py-2 text-ink-muted">
                                                        {row.description}
                                                    </td>
                                                    <td className="py-2">
                                                        <Tag
                                                            small
                                                            tone={row.system ? 'muted' : 'accent'}
                                                        >
                                                            {row.system
                                                                ? t(
                                                                      'marketdata.identityShapes.shared',
                                                                  )
                                                                : t(
                                                                      'marketdata.identityShapes.own',
                                                                  )}
                                                        </Tag>
                                                    </td>
                                                    <td className="py-2 text-right">
                                                        <span className="inline-flex gap-1">
                                                            <Button
                                                                size="sm"
                                                                variant="ghost"
                                                                aria-pressed={open}
                                                                onClick={() =>
                                                                    setSelected(
                                                                        open
                                                                            ? undefined
                                                                            : keyOf(row),
                                                                    )
                                                                }
                                                            >
                                                                {t(
                                                                    'marketdata.identityShapes.history',
                                                                )}
                                                            </Button>
                                                            {mayWrite && mayChange(row) && (
                                                                <Button
                                                                    size="sm"
                                                                    variant="ghost"
                                                                    onClick={() => setEditing(row)}
                                                                >
                                                                    {t(
                                                                        'marketdata.identityShapes.edit',
                                                                    )}
                                                                </Button>
                                                            )}
                                                            {mayRemove && mayChange(row) && (
                                                                <Button
                                                                    size="sm"
                                                                    variant="ghost"
                                                                    onClick={() => setRemoving(row)}
                                                                >
                                                                    {t(
                                                                        'marketdata.identityShapes.remove',
                                                                    )}
                                                                </Button>
                                                            )}
                                                        </span>
                                                    </td>
                                                </tr>
                                                {open && (
                                                    <tr>
                                                        <td colSpan={5} className="pb-3">
                                                            <ShapeHistory shapeKey={keyOf(row)} />
                                                        </td>
                                                    </tr>
                                                )}
                                            </Fragment>
                                        );
                                    })}
                                </tbody>
                            </table>
                        </section>
                    ))}
                </div>
            )}
            {adding && <ShapeDialog row={undefined} onClose={() => setAdding(false)} />}
            {editing !== undefined && (
                <ShapeDialog row={editing} onClose={() => setEditing(undefined)} />
            )}
            {removing !== undefined && (
                <RemoveShapeDialog row={removing} onClose={() => setRemoving(undefined)} />
            )}
        </div>
    );
}

function RequirementTag({ requirement }: { readonly requirement: Requirement }): ReactNode {
    const { t } = useTranslation();
    return (
        <Tag small tone={requirement === 'required' ? 'up' : 'warn'}>
            {t(`marketdata.identityShapes.requirements.${requirement}`)}
        </Tag>
    );
}

/** The versions of one row, newest first, each with who changed it and why. */
function ShapeHistory({ shapeKey }: { readonly shapeKey: ShapeKey }): ReactNode {
    const { t } = useTranslation();
    const versions = useQuery({
        queryKey: [...SHAPES_KEY, 'versions', shapeKey],
        queryFn: () => identityShapes.versions(shapeKey),
    });
    if (versions.isError) {
        return <Notice tone="error">{versions.error.message}</Notice>;
    }
    if (versions.isPending) {
        return <p className="text-sm text-ink-muted">{t('common.loading')}</p>;
    }
    return (
        <table
            className="w-full rounded-md bg-surface-raised text-xs"
            aria-label={t('marketdata.identityShapes.history')}
        >
            <thead className="text-left text-ink-muted">
                <tr>
                    <th className="px-2 py-1">{t('marketdata.identityShapes.version')}</th>
                    <th className="px-2 py-1">{t('marketdata.identityShapes.requirement')}</th>
                    <th className="px-2 py-1">{t('marketdata.identityShapes.description')}</th>
                    <th className="px-2 py-1">{t('marketdata.identityShapes.changedBy')}</th>
                    <th className="px-2 py-1">{t('marketdata.identityShapes.reason')}</th>
                    <th className="px-2 py-1">{t('marketdata.identityShapes.recorded')}</th>
                </tr>
            </thead>
            <tbody>
                {versions.data.map((version) => (
                    <tr key={version.version} className="border-t border-line align-top">
                        <td className="px-2 py-1 font-mono">{version.version}</td>
                        <td className="px-2 py-1">
                            <RequirementTag requirement={version.requirement} />
                        </td>
                        <td className="px-2 py-1">{version.description}</td>
                        <td className="px-2 py-1">{version.modified_by}</td>
                        <td className="px-2 py-1">
                            {version.change_reason_code}
                            {version.change_commentary !== '' && `: ${version.change_commentary}`}
                        </td>
                        <td className="px-2 py-1">
                            {version.recorded_at !== '' && (
                                <RelativeTime at={version.recorded_at} />
                            )}
                        </td>
                    </tr>
                ))}
            </tbody>
        </table>
    );
}

/** Adds a row, or changes the requirement and description of one. A row's key is never changed. */
function ShapeDialog({
    row,
    onClose,
}: {
    readonly row: Shape | undefined;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const [key, setKey] = useState<ShapeKey>(
        row === undefined
            ? { asset_class: '', series_subclass: '', index_family: '', field: '' }
            : keyOf(row),
    );
    const [requirement, setRequirement] = useState<Requirement>(row?.requirement ?? 'required');
    const [description, setDescription] = useState(row?.description ?? '');
    const [commentary, setCommentary] = useState('');
    const [failure, setFailure] = useState('');
    const changed =
        row === undefined || row.requirement !== requirement || row.description !== description;
    const reason = useReason('amend', changed);
    const missingCommentary =
        row !== undefined && reason.needsCommentary && commentary.trim() === '';
    const complete =
        key.asset_class.trim() !== '' &&
        key.series_subclass.trim() !== '' &&
        key.field.trim() !== '' &&
        description.trim() !== '';
    const save = useMutation({
        mutationFn: () =>
            identityShapes.put(
                { ...key, requirement, description },
                row?.version ?? null,
                row === undefined
                    ? { reason_code: NEW_RECORD_REASON, commentary: '' }
                    : { reason_code: reason.code, commentary },
            ),
        onSuccess: async (result) => {
            const why = failed(result);
            if (why !== '') {
                setFailure(why);
                return;
            }
            await queries.invalidateQueries({ queryKey: SHAPES_KEY });
            onClose();
        },
        onError: (error: Error) => setFailure(error.message),
    });
    const keyField = (part: keyof ShapeKey, label: string): ReactNode => (
        <Field label={label}>
            <Input
                value={key[part]}
                disabled={row !== undefined}
                className="font-mono"
                onChange={(event) => setKey({ ...key, [part]: event.target.value.trim() })}
            />
        </Field>
    );
    return (
        <Dialog
            title={
                row === undefined
                    ? t('marketdata.identityShapes.addTitle')
                    : t('marketdata.identityShapes.editTitle', {
                          family: familyOf(row),
                          field: row.field,
                      })
            }
            name="series-identity-shape"
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" onClick={onClose}>
                        {t('common.cancel')}
                    </Button>
                    <Button
                        variant="primary"
                        disabled={!complete || !changed || missingCommentary}
                        pending={save.isPending}
                        onClick={() => save.mutate()}
                    >
                        {t('common.save')}
                    </Button>
                </>
            }
        >
            <div className="space-y-3">
                {failure !== '' && <Notice tone="error">{failure}</Notice>}
                <div className="grid gap-3 md:grid-cols-2">
                    {keyField('asset_class', t('marketdata.identityShapes.assetClass'))}
                    {keyField('series_subclass', t('marketdata.identityShapes.seriesSubclass'))}
                    {keyField('index_family', t('marketdata.identityShapes.indexFamily'))}
                    {keyField('field', t('marketdata.identityShapes.field'))}
                </div>
                <Field label={t('marketdata.identityShapes.requirement')}>
                    <Select
                        value={requirement}
                        onChange={(event) => setRequirement(event.target.value as Requirement)}
                    >
                        {REQUIREMENTS.map((choice) => (
                            <option key={choice} value={choice}>
                                {t(`marketdata.identityShapes.requirements.${choice}`)}
                            </option>
                        ))}
                    </Select>
                </Field>
                <Field label={t('marketdata.identityShapes.description')}>
                    <Textarea
                        value={description}
                        onChange={(event) => setDescription(event.target.value)}
                    />
                </Field>
                {row !== undefined && (
                    <ReasonFields
                        reason={reason}
                        commentary={commentary}
                        onCommentary={setCommentary}
                        missing={missingCommentary}
                    />
                )}
            </div>
        </Dialog>
    );
}

function RemoveShapeDialog({
    row,
    onClose,
}: {
    readonly row: Shape;
    readonly onClose: () => void;
}): ReactNode {
    const { t } = useTranslation();
    const queries = useQueryClient();
    const reason = useReason('delete');
    const [commentary, setCommentary] = useState('');
    const [failure, setFailure] = useState('');
    const missingCommentary = reason.needsCommentary && commentary.trim() === '';
    const remove = useMutation({
        mutationFn: () =>
            identityShapes.remove(keyOf(row), row.version, {
                reason_code: reason.code,
                commentary,
            }),
        onSuccess: async (result) => {
            const why = failed(result);
            if (why !== '') {
                setFailure(why);
                return;
            }
            await queries.invalidateQueries({ queryKey: SHAPES_KEY });
            onClose();
        },
        onError: (error: Error) => setFailure(error.message),
    });
    return (
        <Dialog
            title={t('marketdata.identityShapes.removeTitle', {
                family: familyOf(row),
                field: row.field,
            })}
            name="series-identity-shape-removal"
            onClose={onClose}
            footer={
                <>
                    <Button variant="ghost" onClick={onClose}>
                        {t('common.cancel')}
                    </Button>
                    <Button
                        variant="danger"
                        disabled={missingCommentary}
                        pending={remove.isPending}
                        onClick={() => remove.mutate()}
                    >
                        {t('marketdata.identityShapes.remove')}
                    </Button>
                </>
            }
        >
            <div className="space-y-3">
                {failure !== '' && <Notice tone="error">{failure}</Notice>}
                <p className="text-sm text-ink-muted">
                    {t('marketdata.identityShapes.removeLead')}
                </p>
                <ReasonFields
                    reason={reason}
                    commentary={commentary}
                    onCommentary={setCommentary}
                    missing={missingCommentary}
                />
            </div>
        </Dialog>
    );
}
