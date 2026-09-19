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
import { EntityDetailPage, type DetailMode } from '../../../../entity/EntityDetailPage.js';
import { ConfirmDialog } from '../../../../entity/ConfirmDialog.js';
import {
  ChangeReasonDialog,
  type ChangeReasonResult,
} from '../../../../entity/ChangeReasonDialog.js';
import { countryMeta } from '../../../../generated/refdata/ui/country_ui.js';
import { countryFieldGroups } from './country_field_groups.js';
import { useCountries, useDeleteCountry, useSaveCountry } from '../../../../api/countries.js';
import { useChangeReasons } from '../../../../api/changeReasons.js';
import { useTranslation } from '../../../../i18n/Provider.js';
import { usePageCrumbLabel } from '../../../PageCrumb.js';
import { applyEdit, newCountry, type WireCountry } from '@ores/wire-protocol/browser';

/**
 * One country: reading it, amending it, or creating it.
 *
 * The record comes from the list query's cache, because the list has already
 * fetched the page it came from and a second request would be a second chance to
 * disagree. A deep link finds the record on the first page, which is large enough
 * for a reference-data set.
 *
 * The form is the shared one. What is here is the wiring: which fields are
 * editable, what a valid record looks like, and what happens when Save or Delete
 * is pressed.
 */
export function CountryDetailPage({ mode }: { readonly mode: DetailMode }): ReactNode {
  const { id } = useParams<{ id: string }>();
  const { t } = useTranslation();
  const navigate = useNavigate();

  const query = useCountries({ page: 1, pageSize: 500 });
  const reasons = useChangeReasons();
  const save = useSaveCountry();
  const remove = useDeleteCountry();

  const current = query.data?.rows.find((row) => row['id'] === id);
  const wire = current?.['wire'] as WireCountry | undefined;

  // The blank record a create starts from. Its identity is the one field a
  // person chooses, so it is empty until they do.
  const empty = useMemo(
    () => ({
      alpha2_code: '',
      alpha3_code: '',
      numeric_code: '',
      name: '',
      official_name: '',
      version: 0,
      change_reason_code: '',
      change_commentary: '',
      modified_by: '',
      performed_by: '',
      recorded_at: '',
    }),
    [],
  );

  const [values, setValues] = useState<Record<string, unknown>>(() => ({ ...empty }));
  const [touched, setTouched] = useState(false);
  const [validation, setValidation] = useState<Record<string, string>>({});
  const [failure, setFailure] = useState<string | undefined>(undefined);
  const [stage, setStage] = useState<'reason' | 'delete' | 'delete-reason' | undefined>(undefined);

  /*
   * Seed from the record as it arrives.
   *
   * Keyed on the version as well as the identity, because a save produces a new
   * version of the same record and the form has to follow it. Keying on identity
   * alone left the screen showing the values from before the save, which reads as
   * a save that did not happen.
   *
   * Guarded by `touched` so a fetch cannot overwrite what somebody has typed.
   */
  const seedKey = current === undefined ? undefined : `${String(id)}:${wire?.version ?? 0}`;
  const [seeded, setSeeded] = useState<string | undefined>(undefined);
  if (!touched && seedKey !== undefined && seeded !== seedKey) {
    setSeeded(seedKey);
    setValues(recordFrom(wire));
  }

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
    if (mode === 'create' || wire === undefined) return true;
    return (
      String(values['alpha3_code'] ?? '') !== wire.alpha3_code ||
      String(values['numeric_code'] ?? '') !== wire.numeric_code ||
      String(values['name'] ?? '') !== wire.name ||
      String(values['official_name'] ?? '') !== wire.official_name
    );
  }, [mode, wire, values]);

  function validate(): boolean {
    const errors: Record<string, string> = {};
    for (const field of countryMeta.fields) {
      if (!field.required) continue;
      if (String(values[field.name] ?? '').trim().length === 0) {
        errors[field.name] = t('validation.required');
      }
    }
    // The ISO codes are what the record is indexed by, so their shape matters
    // more than most fields.
    const alpha2 = String(values['alpha2_code'] ?? '').trim();
    if (mode === 'create' && !/^[A-Za-z]{2}$/.test(alpha2)) {
      errors['alpha2_code'] = t('country.invalidAlpha2');
    }
    if (String(values['alpha3_code'] ?? '').trim().length > 0 && !/^[A-Za-z]{3}$/.test(String(values['alpha3_code']).trim())) {
      errors['alpha3_code'] = t('country.invalidAlpha3');
    }
    if (String(values['numeric_code'] ?? '').trim().length > 0 && !/^[0-9]{3}$/.test(String(values['numeric_code']).trim())) {
      errors['numeric_code'] = t('country.invalidNumeric');
    }
    setValidation(errors);
    return Object.keys(errors).length === 0;
  }

  function submit(result: ChangeReasonResult): void {
    setFailure(undefined);
    const payload =
      mode === 'create' || wire === undefined
        ? newCountry({
            alpha2Code: String(values['alpha2_code'] ?? '').trim().toUpperCase(),
            alpha3Code: String(values['alpha3_code'] ?? '').trim().toUpperCase(),
            numericCode: String(values['numeric_code'] ?? '').trim(),
            name: String(values['name'] ?? '').trim(),
            officialName: String(values['official_name'] ?? '').trim(),
            changeReasonCode: result.reasonCode,
            changeCommentary: result.commentary,
          })
        : applyEdit(wire, {
            alpha3Code: String(values['alpha3_code'] ?? '').trim().toUpperCase(),
            numericCode: String(values['numeric_code'] ?? '').trim(),
            name: String(values['name'] ?? '').trim(),
            officialName: String(values['official_name'] ?? '').trim(),
            version: wire.version,
            changeReasonCode: result.reasonCode,
            changeCommentary: result.commentary,
            imageId: String(values['image_id'] ?? '') || null,
          });

    save.mutate(
      { data: payload, reasonCode: result.reasonCode, commentary: result.commentary },
      {
        onSuccess: () => {
          setStage(undefined);
          setTouched(false);
          navigate(`/refdata/country/${String(payload.alpha2_code)}`);
        },
        onError: (error: unknown) => {
          setFailure(error instanceof Error ? error.message : t('feedback.saveFailed'));
          setStage(undefined);
        },
      },
    );
  }

  function confirmDelete(): void {
    const code = wire?.alpha2_code ?? '';
    remove.mutate(code, {
      onSuccess: () => {
        setStage(undefined);
        navigate('/refdata/country');
      },
      onError: (error: unknown) => {
        setFailure(error instanceof Error ? error.message : t('feedback.deleteFailed'));
        setStage(undefined);
      },
    });
  }

  /*
   * What the breadcrumb calls this page, above every return.
   *
   * It was below the not-found return, so a render that found the record ran one
   * more hook than a render that did not — and React refuses that outright. The
   * screen went blank at the moment it had something to show, which is the worst
   * possible moment for it.
   */
  usePageCrumbLabel(mode === 'create' ? undefined : String(values['name'] ?? ''));

  if (mode === 'read' && query.isSuccess && current === undefined) {
    return (
      <div className="mx-auto max-w-[680px] px-5 py-16 text-center">
        <h1 className="text-lg font-semibold tracking-tight">{id}</h1>
        <p className="mt-2 text-sm text-ink-muted">{t('feedback.notFound')}</p>
      </div>
    );
  }

  const title =
    mode === 'create'
      ? t('country.newTitle')
      : String(values['name'] ?? '') || t('country.singular');

  return (
    <>
      <EntityDetailPage
        meta={countryMeta}
        groups={countryFieldGroups}
        values={values}
        mode={mode}
        title={title}
        {...(mode === 'read' ? {} : { subtitle: String(values['alpha2_code'] ?? '') })}
        validationErrors={validation}
        {...(failure === undefined ? {} : { failureMessage: failure })}
        pending={save.isPending || remove.isPending}
        onChange={change}
        onSave={() => {
          if (validate()) setStage('reason');
        }}
        onEdit={() => navigate(`/refdata/country/${String(id ?? '')}/edit`)}
        onCancel={() => {
          setTouched(false);
          navigate(`/refdata/country/${String(id ?? '')}`);
        }}
        image={{
          imageId: String(values['image_id'] ?? ''),
          /*
           * Flags are not tagged by kind, but they follow a convention: the
           * description reads "Flag of xx" and the key is the country code. That
           * is enough to keep a flag picker from offering a staff photograph.
           */
          filter: 'flag of',
          onPick: (chosen) => change('image_id', chosen ?? ''),
        }}
        onDelete={() => setStage('delete')}
        onHistory={() => navigate(`/refdata/country/${String(id ?? '')}/history`)}
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
          title={t('confirmation.deleteTitle', { singular: t('country.singular') })}
          body={t('confirmation.deleteBody', {
            singular: t('country.singular'),
            name: String(values['alpha2_code'] ?? ''),
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

/** The record as the form works with it, by wire field name. */
function recordFrom(wire: WireCountry | undefined): Record<string, unknown> {
  if (wire === undefined) return {};
  return {
    version: wire.version,
    alpha2_code: wire.alpha2_code,
    alpha3_code: wire.alpha3_code,
    numeric_code: wire.numeric_code,
    name: wire.name,
    official_name: wire.official_name,
    modified_by: wire.modified_by,
    performed_by: wire.performed_by,
    recorded_at: wire.recorded_at,
    image_id: wire.image_id ?? '',
    change_reason_code: wire.change_reason_code,
    change_commentary: wire.change_commentary,
  };
}
