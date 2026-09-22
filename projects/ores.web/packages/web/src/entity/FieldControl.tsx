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

import type { ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { cx } from '../ui/Primitives.js';
import type { FieldMeta } from '../ui-contract.js';
import { useLookupOptions } from './useLookup.js';

/**
 * One form control, chosen by the field's declaration.
 *
 * The declaration says which control a field needs, so the form renders without
 * knowing the entity. Nine control types cover every entity the models declare,
 * and a tenth means changing the contract rather than adding a branch here.
 *
 * Labels come from translation keys, so the form is translated like everything
 * else and a new entity adds words to a catalogue rather than strings to code.
 */
export interface FieldControlProps {
  readonly field: FieldMeta;
  readonly value: unknown;
  readonly onChange: (name: string, value: unknown) => void;
  readonly disabled?: boolean;
  readonly error?: string;
  readonly onBlur?: () => void;
}

export function FieldControl({
  field,
  value,
  onChange,
  disabled = false,
  error,
  onBlur,
}: FieldControlProps): ReactNode {
  const { t } = useTranslation();
  const label = t(field.labelKey);
  /*
   * Read-only is decided by the screen, not by the field.
   *
   * `readOnlyAfterCreate` is a property of the entity, not of this render: the
   * same field is editable when creating and fixed afterwards. Reading it here
   * made a key field uneditable in the one mode where it must be typed.
   */
  const readOnly = disabled;
  const describedBy = error === undefined ? undefined : `${field.name}-error`;

  return (
    <div>
      <label htmlFor={field.name} className="mb-1.5 flex items-center gap-1.5 text-xs font-medium text-ink-muted">
        {label}
        {field.required && (
          // Marked, and the mark is in words for a screen reader rather than a
          // bare asterisk.
          <span className="text-ink-faint" title={t('audit.required')}>
            *
          </span>
        )}
      </label>

      <Control
        field={field}
        value={value}
        onChange={(next) => onChange(field.name, next)}
        readOnly={readOnly}
        invalid={error !== undefined}
        {...(onBlur === undefined ? {} : { onBlur })}
        {...(describedBy === undefined ? {} : { describedBy })}
      />

      {error !== undefined && (
        <p id={describedBy} className="mt-1 text-xs text-red-400">
          {error}
        </p>
      )}
    </div>
  );
}

function Control({
  field,
  value,
  onChange,
  readOnly,
  invalid,
  onBlur,
  describedBy,
}: {
  readonly field: FieldMeta;
  readonly value: unknown;
  readonly onChange: (value: unknown) => void;
  readonly readOnly: boolean;
  readonly invalid: boolean;
  readonly onBlur?: () => void;
  readonly describedBy?: string;
}): ReactNode {
  const { t } = useTranslation();
  // A hook may not be called conditionally, and the control is chosen by the
  // field's type, so a fetched combo's choices are read before the branch that
  // uses them rather than inside it.
  const lookupOptions = useLookupOptions(field.lookup);
  const text = value === null || value === undefined ? '' : String(value);
  const base = cx(
    'w-full rounded-md border bg-bg-secondary px-2.5 text-sm text-ink placeholder:text-ink-faint focus:outline-none',
    invalid ? 'border-red-500/60' : 'border-line focus:border-line-strong',
    // Read-only is visibly read-only rather than disabled: a disabled control is
    // hard to read and implies it might become editable.
    readOnly && 'cursor-not-allowed bg-surface-overlay text-ink-muted',
  );

  switch (field.control) {
    case 'text_edit':
      return (
        <textarea
          id={field.name}
          value={text}
          readOnly={readOnly}
          rows={3}
          onBlur={onBlur}
          aria-describedby={describedBy}
          onChange={(event) => onChange(event.target.value)}
          className={cx(base, 'py-2')}
        />
      );

    case 'static_combo':
    case 'dynamic_combo':
    case 'flagged_combo': {
      /*
       * A declared option list, or the choices a fetched one names. A field
       * with neither renders an empty list rather than a free-text box, so the
       * value always comes from the set the model allows.
       */
      const options =
        field.options ??
        lookupOptions.map((option) => ({ value: option.value, labelKey: option.label }));
      return (
        <select
          id={field.name}
          value={text}
          disabled={readOnly}
          onBlur={onBlur}
          aria-describedby={describedBy}
          onChange={(event) => onChange(event.target.value)}
          className={cx(base, 'h-9', readOnly && 'cursor-not-allowed')}
        >
          <option value="">—</option>
          {options.map((option) => (
            <option key={option.value} value={option.value}>
              {/*
               * A fetched option's label is the row's own display text, which
               * is not a translation key; a declared one's is, and is looked
               * up. The two are told apart by which list supplied it.
               */}
              {field.options === undefined ? option.labelKey : t(option.labelKey)}
            </option>
          ))}
        </select>
      );
    }

    case 'check_box':
      return (
        <label className="flex h-9 items-center gap-2 text-sm text-ink">
          <input
            id={field.name}
            type="checkbox"
            checked={value === true}
            disabled={readOnly}
            onBlur={onBlur}
            onChange={(event) => onChange(event.target.checked)}
            className="size-4 accent-[var(--color-accent)]"
          />
          {field.nullable === true && (
            <button
              type="button"
              disabled={readOnly}
              onClick={() => onChange(null)}
              className="text-xs text-ink-faint hover:text-ink"
            >
              {t('account.notSet')}
            </button>
          )}
        </label>
      );

    case 'spin_box':
      return (
        <input
          id={field.name}
          type="number"
          value={text}
          readOnly={readOnly}
          onBlur={onBlur}
          aria-describedby={describedBy}
          {...(field.min === undefined ? {} : { min: field.min })}
          {...(field.max === undefined ? {} : { max: field.max })}
          onChange={(event) => onChange(event.target.value === '' ? null : Number(event.target.value))}
          className={cx(base, 'h-9 tabular-nums')}
        />
      );

    case 'colour':
      return (
        <input
          id={field.name}
          type="color"
          value={text.length === 0 ? '#000000' : text}
          disabled={readOnly}
          onChange={(event) => onChange(event.target.value)}
          className="h-9 w-16 rounded-md border border-line bg-bg-secondary"
        />
      );

    case 'date':
      return (
        <input
          id={field.name}
          type="date"
          value={text}
          readOnly={readOnly}
          onBlur={onBlur}
          aria-describedby={describedBy}
          onChange={(event) => onChange(event.target.value)}
          className={cx(base, 'h-9 font-mono tabular-nums')}
        />
      );

    default:
      return (
        <input
          id={field.name}
          type="text"
          value={text}
          readOnly={readOnly}
          onBlur={onBlur}
          aria-describedby={describedBy}
          {...(field.placeholderKey === undefined
            ? {}
            : { placeholder: t(field.placeholderKey) })}
          {...(field.maxLength === undefined ? {} : { maxLength: field.maxLength })}
          onChange={(event) => onChange(event.target.value)}
          className={cx(base, 'h-9')}
        />
      );
  }
}
