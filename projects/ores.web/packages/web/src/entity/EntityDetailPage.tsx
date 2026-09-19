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

import { useEffect, useMemo, useState, type ReactNode } from 'react';
import { FieldControl } from './FieldControl.js';
import { FlagEditor } from './FlagEditor.js';
import { useTranslation } from '../i18n/Provider.js';
import { Button, Notice, cx } from '../ui/Primitives.js';
import { MaskIcon } from '../ui/icons/MaskIcon.js';
import type { EntityMeta, FieldGroup } from '../ui-contract.js';

/**
 * The detail screen for every entity.
 *
 * Tabs come from the hand-written field grouping, fields from the generated
 * declaration, and the controls from each field's declared type. Nothing here
 * knows what an entity is, which is what makes the hundredth detail screen free.
 *
 * Reading, creating and editing render the same form, because a form that
 * rearranges itself when you press Edit is a form you have to read twice. What
 * changes is which fields accept input and which actions appear.
 */
export type DetailMode = 'read' | 'edit' | 'create';

export interface EntityDetailPageProps {
  readonly meta: EntityMeta;
  readonly groups: readonly FieldGroup[];
  /** The editable values, by wire field name. */
  readonly values: Readonly<Record<string, unknown>>;
  readonly mode: DetailMode;
  readonly title: string;
  /** The record's own idea of its name, shown while editing. */
  readonly subtitle?: string;
  /**
   * The image this entity carries, when it carries one.
   *
   * Some entities have a picture rather than a field for one, and it is a shared
   * concern: the same editor serves a country's flag and a party's logo. Absent
   * means the entity has no image, and nothing is rendered.
   */
  readonly image?: {
    readonly imageId: string | undefined;
    /** What to narrow the choices to, when the entity's images share a
     *  convention the picker can match on. */
    readonly filter?: string;
    readonly onPick: (imageId: string | null) => void;
  };
  readonly validationErrors?: Readonly<Record<string, string>>;
  readonly failureMessage?: string;
  readonly pending?: boolean;
  readonly onChange: (name: string, value: unknown) => void;
  readonly onSave?: () => void;
  readonly onEdit?: () => void;
  readonly onDelete?: () => void;
  readonly onCancel?: () => void;
  readonly onHistory?: () => void;
}

export function EntityDetailPage({
  meta,
  groups,
  values,
  mode,
  title,
  subtitle,
  image,
  validationErrors,
  failureMessage,
  pending = false,
  onChange,
  onSave,
  onEdit,
  onDelete,
  onCancel,
  onHistory,
}: EntityDetailPageProps): ReactNode {
  const { t } = useTranslation();
  const [activeGroup, setActiveGroup] = useState(groups[0]?.id ?? 'general');
  const [showProvenance, setShowProvenance] = useState(false);

  // A group that disappears must not stay selected, which happens when a group
  // is only rendered for some modes.
  useEffect(() => {
    if (!groups.some((group) => group.id === activeGroup)) {
      setActiveGroup(groups[0]?.id ?? 'general');
    }
  }, [groups, activeGroup]);

  const editable = mode !== 'read';
  const group = groups.find((g) => g.id === activeGroup) ?? groups[0];

  // Provenance reads from the same values, since they carry the whole record.
  const provenanceRows = useMemo(
    () =>
      [
        [t('account.fldVersion'), values['version']],
        [t('account.fldModifiedBy'), values['modified_by']],
        [t('account.fldPerformedBy'), values['performed_by']],
        [t('account.fldRecordedAt'), values['recorded_at']],
        [t('account.fldChangeReason'), values['change_reason_code']],
        [t('account.fldCommentary'), values['change_commentary']],
      ] as const,
    [values, t],
  );

  return (
    <div className="mx-auto max-w-[860px] px-5 py-7">
      <header className="mb-5 flex items-start gap-4">
        <div className="min-w-0">
          <h1 className="truncate text-xl font-semibold tracking-tight">{title}</h1>
          {subtitle !== undefined && subtitle.length > 0 && (
            <p className="mt-1 truncate text-sm text-ink-muted">{subtitle}</p>
          )}
        </div>

        <div className="ml-auto flex shrink-0 items-center gap-2">
          {/*
            One primary action at a time, and only the ones that apply. Reading
            offers Edit; editing offers Save and Cancel; neither offers Delete
            until the record exists.
          */}
          {mode === 'read' && onEdit !== undefined && (
            <Button variant="primary" size="sm" onClick={onEdit}>
              <MaskIcon name="edit" className="size-3.5" />
              {t('entity.edit')}
            </Button>
          )}
          {editable && onSave !== undefined && (
            <>
              {onCancel !== undefined && (
                <Button variant="ghost" size="sm" onClick={onCancel} disabled={pending}>
                  {t('entity.cancel')}
                </Button>
              )}
              <Button
                variant="primary"
                size="sm"
                onClick={onSave}
                pending={pending}
                pendingLabel={t('entity.saving')}
              >
                <MaskIcon name="save" className="size-3.5" />
                {t('entity.save')}
              </Button>
            </>
          )}
          {mode === 'edit' && onDelete !== undefined && (
            <Button variant="ghost" size="sm" onClick={onDelete} disabled={pending} title={t('entity.delete')}>
              <MaskIcon name="delete" className="size-3.5 text-red-400" />
            </Button>
          )}
        </div>
      </header>

      {failureMessage !== undefined && (
        <div className="mb-4">
          <Notice tone="error">{failureMessage}</Notice>
        </div>
      )}

      {image !== undefined && (
        <FlagEditor
          imageId={image.imageId}
          editable={editable}
          {...(image.filter === undefined ? {} : { filter: image.filter })}
          onPick={image.onPick}
        />
      )}

      <div className="mb-5 flex flex-wrap gap-1 border-b border-line" role="tablist">
        {groups.map((g) => (
          <button
            key={g.id}
            type="button"
            role="tab"
            aria-selected={g.id === group?.id && !showProvenance}
            onClick={() => {
              setShowProvenance(false);
              setActiveGroup(g.id);
            }}
            className={cx(
              '-mb-px border-b-2 px-3 py-2 text-sm transition-colors',
              g.id === group?.id && !showProvenance
                ? 'border-accent text-ink'
                : 'border-transparent text-ink-muted hover:text-ink',
            )}
          >
            {t(g.titleKey)}
          </button>
        ))}
        <button
          type="button"
          role="tab"
          aria-selected={showProvenance}
          // Provenance is read-only and only meaningful once a record exists.
          disabled={mode === 'create'}
          onClick={() => setShowProvenance(true)}
          className={cx(
            '-mb-px border-b-2 px-3 py-2 text-sm transition-colors disabled:opacity-40',
            showProvenance
              ? 'border-accent text-ink'
              : 'border-transparent text-ink-muted hover:text-ink',
          )}
        >
          {t('entity.provenance')}
        </button>
        {onHistory !== undefined && (
          <button
            type="button"
            onClick={onHistory}
            className="-mb-px ml-auto flex items-center gap-1.5 border-b-2 border-transparent px-3 py-2 text-sm text-ink-muted hover:text-ink"
          >
            <MaskIcon name="history" className="size-3.5" />
            {t('entity.history')}
          </button>
        )}
      </div>

      {showProvenance ? (
        <dl className="divide-y divide-line overflow-hidden rounded-[var(--radius-card)] border border-line bg-bg-secondary">
          {provenanceRows.map(([label, value]) => (
            <div key={label} className="flex items-baseline gap-4 px-3.5 py-2">
              <dt className="w-36 shrink-0 text-xs text-ink-faint">{label}</dt>
              <dd className="min-w-0 flex-1 text-sm text-ink-muted">
                {/* A blank is said rather than shown, so it is not mistaken for
                    a failure to load. */}
                {value === null || value === undefined || String(value).length === 0 ? (
                  <span className="text-ink-faint">{t('account.notRecorded')}</span>
                ) : (
                  String(value)
                )}
              </dd>
            </div>
          ))}
        </dl>
      ) : (
        <div className="grid gap-4 sm:grid-cols-2">
          {(group?.fields ?? []).map((name) => {
            const field = meta.fields.find((f) => f.name === name);
            if (field === undefined) return null;
            /*
             * A key is typed when creating and fixed afterwards: the username,
             * the code, the natural key. Changing it later would change what the
             * record is, which is why the declaration says so and the mode
             * decides when.
             */
            const locked = (field.isKey || field.readOnlyAfterCreate === true) && mode !== 'create';
            return (
              <FieldControl
                key={name}
                field={field}
                value={values[name]}
                disabled={!editable || locked}
                onChange={onChange}
                {...(validationErrors?.[name] === undefined
                  ? {}
                  : { error: validationErrors[name] })}
              />
            );
          })}
        </div>
      )}
    </div>
  );
}
