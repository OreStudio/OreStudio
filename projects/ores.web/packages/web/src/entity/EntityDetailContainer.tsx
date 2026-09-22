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

import { useMemo, useState, type ReactNode } from 'react';
import { useNavigate, useParams } from 'react-router';
import { ChangeReasonDialog, type ChangeReasonResult } from './ChangeReasonDialog.js';
import { ConfirmDialog } from './ConfirmDialog.js';
import { EntityDetailPage, type DetailMode } from './EntityDetailPage.js';
import { entityBasePath, entityRecordPath } from './entityPaths.js';
import { useDeleteEntity, useEntity, useSaveEntity } from './useEntity.js';
import { useChangeReasons } from '../api/changeReasons.js';
import { useTranslation } from '../i18n/Provider.js';
import { usePageCrumbLabel } from '../components/PageCrumb.js';
import type { ColumnStyle, FieldControl, FieldGroup } from '../ui-contract.js';
import type { EntityDescriptor } from './descriptor.js';

/**
 * One record of any entity: reading it, amending it, or creating it.
 *
 * The form, the tabs and the validation are the shared ones. What is here is the
 * wiring the descriptor implies: which fields are required, what a blank record
 * looks like, and where Save, Delete and Cancel lead.
 *
 * The record is fetched by its natural key rather than found in the list's
 * cache, so a deep link into a collection larger than one page still resolves.
 */
export function EntityDetailContainer({
  descriptor,
  mode,
}: {
  readonly descriptor: EntityDescriptor;
  readonly mode: DetailMode;
}): ReactNode {
  const params = useParams();
  const key = params[descriptor.keyParam];
  const { t } = useTranslation();
  const navigate = useNavigate();
  const base = entityBasePath(descriptor);

  const query = useEntity(descriptor, mode === 'create' ? undefined : key);
  const reasons = useChangeReasons();
  const save = useSaveEntity(descriptor);
  const remove = useDeleteEntity(descriptor);

  const record = query.data;

  /*
   * One group when the model declares none.
   *
   * Grouping fields into tabs is a domain judgement the generated metadata
   * deliberately does not carry, so the shared screen takes the groups from the
   * declaration and falls back to the single group that an absent declaration
   * means rather than a defect.
   */
  const groups = useMemo<readonly FieldGroup[]>(
    () =>
      descriptor.fieldGroups ?? [
        {
          id: 'general',
          titleKey: 'entity.general',
          fields: descriptor.meta.fields.map((field) => field.name),
        },
      ],
    [descriptor],
  );

  const [values, setValues] = useState<Record<string, unknown>>(() => blankValues(descriptor));
  const [touched, setTouched] = useState(false);
  const [validation, setValidation] = useState<Record<string, string>>({});
  const [failure, setFailure] = useState<string | undefined>(undefined);
  const [stage, setStage] = useState<'reason' | 'delete' | 'delete-reason' | undefined>(undefined);

  /*
   * Seed from the record as it arrives.
   *
   * Keyed on the version as well as the identity, because a save produces a new
   * version of the same record and the form has to follow it. Identity alone is
   * unchanged by a save, so it cannot tell the record before the save from the
   * record after it, and the form would keep showing the earlier values.
   *
   * Guarded by `touched` so a fetch cannot overwrite what somebody has typed.
   */
  const seedKey =
    record === undefined ? undefined : `${String(key)}:${String(record['version'] ?? 0)}`;
  /**
   * The version the screen read, which a change states back.
   *
   * Absent when there is no record to have read, which is a create: the
   * request then states the absence of a row rather than a version, and the
   * store refuses it over a live row instead of replacing it.
   */
  const recordVersion = useMemo(() => {
    const raw = record?.['version'];
    const value = typeof raw === 'number' ? raw : Number(raw);
    return Number.isFinite(value) ? value : undefined;
  }, [record]);
  const [seeded, setSeeded] = useState<string | undefined>(undefined);
  if (!touched && seedKey !== undefined && seeded !== seedKey) {
    setSeeded(seedKey);
    setValues({ ...blankValues(descriptor), ...record });
  }

  // The record's own idea of its name, for the heading and the breadcrumb.
  const display = String(values[descriptor.meta.displayField] ?? '');
  usePageCrumbLabel(mode === 'create' ? undefined : display);

  function change(name: string, value: unknown): void {
    setTouched(true);
    setValues((previous) => ({ ...previous, [name]: value }));
    // A field being corrected should not keep showing its old complaint.
    setValidation((previous) => {
      if (previous[name] === undefined) return previous;
      const next = { ...previous };
      delete next[name];
      return next;
    });
    setFailure(undefined);
  }

  /** Which fields differ from the record, which decides the reasons offered. */
  const changedFields = useMemo(() => {
    if (mode === 'create' || record === undefined) return true;
    return descriptor.meta.fields.some(
      (field) => String(values[field.name] ?? '') !== String(record[field.name] ?? ''),
    );
  }, [mode, record, values, descriptor]);

  function validate(): boolean {
    const errors: Record<string, string> = {};
    for (const field of descriptor.meta.fields) {
      if (!field.required) continue;
      if (String(values[field.name] ?? '').trim().length === 0) {
        errors[field.name] = t('validation.required');
      }
    }
    setValidation(errors);
    return Object.keys(errors).length === 0;
  }

  function submit(result: ChangeReasonResult): void {
    setFailure(undefined);
    /*
     * The record carries the entity's own members and nothing else. The reason
     * and the commentary are the intent, and the version the screen read is the
     * precondition: the request states those separately, and the record states
     * neither, because the service derives the audit tail and the store decides
     * the version.
     *
     * The members come from the descriptor's write record rather than from the
     * row, so a field the form does not show is not sent back as whatever the
     * row happened to hold.
     */
    const data: Record<string, unknown> = {};
    for (const name of descriptor.writeFields) {
      data[name] = values[name];
    }
    for (const field of descriptor.meta.fields) {
      data[field.name] = values[field.name];
    }

    save.mutate(
      {
        data,
        intent: { reason_code: result.reasonCode, commentary: result.commentary },
        version: mode === 'create' ? undefined : recordVersion,
      },
      {
        onSuccess: () => {
          setStage(undefined);
          setTouched(false);
          navigate(entityRecordPath(descriptor, String(data[descriptor.meta.keyField] ?? '')));
        },
        onError: (error: unknown) => {
          setFailure(error instanceof Error ? error.message : t('feedback.saveFailed'));
          setStage(undefined);
        },
      },
    );
  }

  function confirmDelete(result: ChangeReasonResult): void {
    setFailure(undefined);
    remove.mutate(
      {
        key: String(values[descriptor.meta.keyField] ?? ''),
        intent: {
          reason_code: result.reasonCode,
          commentary: result.commentary,
        },
        version: recordVersion,
      },
      {
        onSuccess: () => {
          setStage(undefined);
          navigate(base);
        },
        onError: (error: unknown) => {
          setFailure(error instanceof Error ? error.message : t('feedback.deleteFailed'));
          setStage(undefined);
        },
      },
    );
  }

  if (mode === 'read' && query.isSuccess && record === undefined) {
    return (
      <div className="mx-auto max-w-[680px] px-5 py-16 text-center">
        <h1 className="text-lg font-semibold tracking-tight">{String(key ?? '')}</h1>
        <p className="mt-2 text-sm text-ink-muted">{t('feedback.notFound')}</p>
      </div>
    );
  }

  const title =
    mode === 'create'
      ? t(`${descriptor.entity}.newTitle`)
      : display.length > 0
        ? display
        : t(`${descriptor.entity}.singular`);

  return (
    <>
      <EntityDetailPage
        meta={descriptor.meta}
        groups={groups}
        values={values}
        mode={mode}
        title={title}
        {...(mode === 'read' ? {} : { subtitle: String(values[descriptor.meta.keyField] ?? '') })}
        validationErrors={validation}
        {...(failure === undefined ? {} : { failureMessage: failure })}
        pending={save.isPending || remove.isPending}
        onChange={change}
        onSave={() => {
          if (validate()) setStage('reason');
        }}
        {...(descriptor.capabilities.edit
          ? { onEdit: () => navigate(`${entityRecordPath(descriptor, String(key ?? ''))}/edit`) }
          : {})}
        onCancel={() => {
          setTouched(false);
          navigate(mode === 'create' ? base : entityRecordPath(descriptor, String(key ?? '')));
        }}
        {...(descriptor.capabilities.remove ? { onDelete: () => setStage('delete') } : {})}
        {...(descriptor.capabilities.history
          ? {
              onHistory: () =>
                navigate(`${entityRecordPath(descriptor, String(key ?? ''))}/history`),
            }
          : {})}
      />

      {stage === 'reason' && (
        <ChangeReasonDialog
          operation={mode === 'create' ? 'create' : 'amend'}
          hasChanges={changedFields}
          reasons={reasons.data ?? []}
          pending={save.isPending}
          onConfirm={submit}
          onCancel={() => setStage(undefined)}
        />
      )}

      {stage === 'delete' && (
        <ConfirmDialog
          title={t('confirmation.deleteTitle', { singular: t(`${descriptor.entity}.singular`) })}
          body={t('confirmation.deleteBody', {
            singular: t(`${descriptor.entity}.singular`),
            name: String(values[descriptor.meta.keyField] ?? ''),
          })}
          confirmLabel={t('entity.delete')}
          pending={false}
          onCancel={() => setStage(undefined)}
          onConfirm={() => setStage('delete-reason')}
        />
      )}

      {stage === 'delete-reason' && (
        <ChangeReasonDialog
          operation="delete"
          hasChanges={false}
          reasons={reasons.data ?? []}
          pending={remove.isPending}
          onConfirm={confirmDelete}
          onCancel={() => setStage(undefined)}
        />
      )}
    </>
  );
}

/**
 * The record a create starts from.
 *
 * The form edits the declared fields, but a save replaces the whole record, so
 * the rest of the shape has to be present too. What the declaration knows about
 * it is the columns: a numeric cell starts at zero and any other starts empty.
 * Provenance the service stamps is left for the service.
 */
function blankValues(descriptor: EntityDescriptor): Record<string, unknown> {
  const values: Record<string, unknown> = {
    version: 0,
    change_reason_code: '',
    change_commentary: '',
  };
  for (const field of descriptor.meta.fields) {
    if (!(field.name in values)) {
      values[field.name] = blankField(field.control);
    }
  }
  for (const column of descriptor.meta.columns) {
    if (!(column.name in values)) {
      values[column.name] = isNumericColumn(column.style) ? 0 : '';
    }
  }
  return values;
}

/** The empty value a control's type implies. */
function blankField(control: FieldControl): unknown {
  if (control === 'check_box') return false;
  if (control === 'spin_box') return 0;
  return '';
}

/**
 * Whether a column holds a number.
 *
 * The contract has no numeric style, so this reads the monospaced numeric
 * conventions the model uses for counts, versions and orders. A style that is
 * not listed starts as text, which the server will parse or refuse.
 */
function isNumericColumn(style: ColumnStyle): boolean {
  return style === 'mono_center' || style === 'mono_right' || style === 'mono_bold_center';
}
