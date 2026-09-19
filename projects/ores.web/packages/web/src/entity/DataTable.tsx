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

import { useState, type ReactNode } from 'react';
import { MaskIcon } from '../ui/icons/MaskIcon.js';
import type { IconName } from '../ui/icons/index.js';
import { useTranslation } from '../i18n/Provider.js';
import { cx } from '../ui/Primitives.js';
import type { ColumnMeta, ColumnStyle } from '../ui-contract.js';

/**
 * A table driven by a column declaration.
 *
 * The declaration is the single source: the header, the alignment, the font and
 * whether a column is shown all come from it. That is what stops a column being
 * styled differently from how it was declared, which is the drift that costs when
 * there are a hundred entities.
 *
 * Cells are rendered by style rather than by a per-entity component, so a new
 * entity adds no rendering code.
 */
/** One entry in a row's action menu. */
export interface RowAction<Row> {
  readonly id: string;
  readonly label: string;
  readonly icon?: IconName;
  /** Rendered in the danger colour, for anything destructive. */
  readonly danger?: boolean;
  readonly onSelect: (row: Row) => void;
}

export interface DataTableProps<Row> {
  readonly columns: readonly ColumnMeta[];
  readonly rows: readonly Row[];
  readonly rowKey: (row: Row) => string;
  readonly onOpen?: (row: Row) => void;
  /**
   * Actions offered per row, in a menu at the end of the row.
   *
   * A menu rather than a toolbar of icons, which is what the web does and what a
   * person expects: the actions belong to the row they act on, they do not need
   * to be on screen when nothing is selected, and a menu can be labelled in
   * words where a toolbar had room only for a glyph.
   */
  readonly rowActions?: readonly RowAction<Row>[];
  /**
   * The rows that changed on the last reload, by row key.
   *
   * A set rather than a flag, because a whole table flashing says nothing: the
   * four rows that changed say everything.
   */
  readonly changed?: ReadonlySet<string>;
  readonly loading?: boolean;
  readonly emptyMessage: string;
}

export function DataTable<Row extends Record<string, unknown>>({
  columns,
  rows,
  rowKey,
  onOpen,
  rowActions,
  changed,
  loading = false,
  emptyMessage,
}: DataTableProps<Row>): ReactNode {
  const { t } = useTranslation();
  // Audit columns are hidden by default and offered through the header menu.
  const [shown, setShown] = useState<ReadonlySet<string>>(
    () => new Set(columns.filter((c) => !c.hidden || c.name === columns[0]?.name).map((c) => c.name)),
  );
  const [menuOpen, setMenuOpen] = useState(false);
  // Which row's action menu is open, if any. One at a time, so two menus cannot
  // be open at once and clicking elsewhere closes whichever is.
  const [openRow, setOpenRow] = useState<string | undefined>(undefined);

  const visible = columns.filter((column) => shown.has(column.name));

  function toggle(name: string): void {
    setShown((current) => {
      const next = new Set(current);
      if (next.has(name)) next.delete(name);
      else next.add(name);
      return next;
    });
  }

  if (rows.length === 0 && !loading) {
    return (
      <div className="rounded-[var(--radius-card)] border border-line bg-bg-secondary px-4 py-10 text-center text-sm text-ink-muted">
        {emptyMessage}
      </div>
    );
  }

  return (
    <div className="overflow-hidden rounded-[var(--radius-card)] border border-line bg-bg-secondary">
      <div className="overflow-x-auto">
        <table className="w-full border-collapse text-sm">
          <thead>
            <tr className="border-b border-line">
              {visible.map((column) => (
                <th
                  key={column.name}
                  scope="col"
                  style={column.width === undefined ? undefined : { width: column.width }}
                  className={cx(
                    'px-3 py-2 text-xs font-medium text-ink-muted',
                    alignment(column.style),
                  )}
                >
                  {t(column.headerKey)}
                </th>
              ))}
              <th scope="col" className="w-8 px-2 py-2">
                <button
                  type="button"
                  onClick={() => setMenuOpen((open) => !open)}
                  aria-label={t('table.chooseColumns')}
                  aria-expanded={menuOpen}
                  className="rounded p-0.5 text-ink-faint hover:text-ink"
                >
                  <svg viewBox="0 0 16 16" className="size-3.5" aria-hidden>
                    <path d="M2 4h12M4 8h8M6 12h4" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" fill="none" />
                  </svg>
                </button>
              </th>
              {rowActions !== undefined && rowActions.length > 0 && <th scope="col" className="w-8 px-1 py-2" />}
            </tr>
          </thead>
          <tbody>
            {rows.map((row) => (
              <tr
                key={rowKey(row)}
                onClick={onOpen === undefined ? undefined : () => onOpen(row)}
                className={cx(
                  'border-b border-line/60 last:border-0',
                  onOpen !== undefined && 'cursor-pointer hover:bg-surface-overlay',
                  /*
                   * A row that changed is tinted and given a leading edge, and
                   * the tint fades while the edge stays. A permanent tint becomes
                   * decoration; a fading one stays news, and the edge is what
                   * says it is still true.
                   */
                  changed?.has(rowKey(row)) === true &&
                    'row-changed shadow-[inset_2px_0_0_0_var(--color-accent)]',
                )}
              >
                {visible.map((column) => (
                  <td key={column.name} className={cx('px-3 py-2', alignment(column.style))}>
                    <Cell
                      column={column}
                      value={row[column.name]}
                      {...(typeof row['image_id'] === 'string' ? { flagId: row['image_id'] } : {})}
                    />
                  </td>
                ))}
                <td />
                {rowActions !== undefined && rowActions.length > 0 && (
                  <td className="px-1 py-1">
                    <RowMenu
                      open={openRow === rowKey(row)}
                      onToggle={() =>
                        setOpenRow(openRow === rowKey(row) ? undefined : rowKey(row))
                      }
                      onClose={() => setOpenRow(undefined)}
                      actions={rowActions}
                      row={row}
                      label={t('table.rowActions')}
                    />
                  </td>
                )}
              </tr>
            ))}
          </tbody>
        </table>
      </div>

      {menuOpen && (
        <div className="border-t border-line px-3 py-2">
          <p className="mb-1.5 text-[11px] uppercase tracking-wider text-ink-faint">
            {t('entity.provenance')}
          </p>
          <div className="flex flex-wrap gap-3">
            {columns.map((column) => (
              <label key={column.name} className="flex items-center gap-1.5 text-xs text-ink-muted">
                <input
                  type="checkbox"
                  checked={shown.has(column.name)}
                  onChange={() => toggle(column.name)}
                  className="accent-[var(--color-accent)]"
                />
                {t(column.headerKey)}
              </label>
            ))}
          </div>
        </div>
      )}
    </div>
  );
}

/**
 * A row's actions, behind one button.
 *
 * Opened by clicking the button, closed by choosing something, by clicking
 * anywhere else, or by pressing Escape, which is what a menu has to do to feel
 * like one.
 */
function RowMenu<Row>({
  open,
  onToggle,
  onClose,
  actions,
  row,
  label,
}: {
  readonly open: boolean;
  readonly onToggle: () => void;
  readonly onClose: () => void;
  readonly actions: readonly RowAction<Row>[];
  readonly row: Row;
  readonly label: string;
}): ReactNode {
  return (
    <div className="relative">
      <button
        type="button"
        onClick={(event) => {
          // The row itself opens the record, and the menu must not do that too.
          event.stopPropagation();
          onToggle();
        }}
        aria-label={label}
        aria-expanded={open}
        aria-haspopup="menu"
        className="rounded p-1 text-ink-faint hover:bg-surface-hover hover:text-ink"
      >
        <svg viewBox="0 0 16 16" className="size-3.5" aria-hidden>
          <circle cx="8" cy="3.5" r="1.2" fill="currentColor" />
          <circle cx="8" cy="8" r="1.2" fill="currentColor" />
          <circle cx="8" cy="12.5" r="1.2" fill="currentColor" />
        </svg>
      </button>

      {open && (
        <>
          <button
            type="button"
            aria-hidden
            tabIndex={-1}
            className="fixed inset-0 z-10 cursor-default"
            onClick={(event) => {
              event.stopPropagation();
              onClose();
            }}
          />
          <ul
            role="menu"
            className="absolute right-0 z-20 mt-1 w-40 overflow-hidden rounded-md border border-line bg-surface-overlay py-1 shadow-lg"
          >
            {actions.map((action) => (
              <li key={action.id} role="none">
                <button
                  type="button"
                  role="menuitem"
                  onClick={(event) => {
                    event.stopPropagation();
                    onClose();
                    action.onSelect(row);
                  }}
                  className={cx(
                    'flex w-full items-center gap-2 px-3 py-1.5 text-left text-sm',
                    action.danger
                      ? 'text-red-400 hover:bg-surface-hover'
                      : 'text-ink-muted hover:bg-surface-hover hover:text-ink',
                  )}
                >
                  {action.icon !== undefined && <MaskIcon name={action.icon} className="size-3.5" />}
                  {action.label}
                </button>
              </li>
            ))}
          </ul>
        </>
      )}
    </div>
  );
}

/**
 * A flag.
 *
 * Flags are not a separate concept: a country carries an image identifier and the
 * image happens to be a flag. The bytes come through the BFF, since the browser
 * never reaches NATS.
 *
 * A record with no image gets the same quiet placeholder as any other empty cell,
 * so a missing flag reads as absent data rather than as a broken image.
 */
function Flag({ imageId }: { readonly imageId?: string | null }): ReactNode {
  if (imageId === undefined || imageId === null || imageId.length === 0) {
    return <span className="inline-block size-4 shrink-0" aria-hidden />;
  }
  return (
    <img
      src={`/api/images/${encodeURIComponent(imageId)}`}
      alt=""
      aria-hidden
      loading="lazy"
      className="h-3.5 w-5 shrink-0 rounded-[2px] object-cover"
    />
  );
}

/** Horizontal alignment and font, from the declared style. */
function alignment(style: ColumnStyle): string {
  switch (style) {
    case 'text_center':
    case 'mono_center':
    case 'mono_bold_center':
    case 'icon_centered':
    case 'badge_centered':
      return 'text-center';
    case 'mono_right':
      return 'text-right font-mono tabular-nums';
    case 'mono_left':
    case 'mono_bold_left':
      return 'text-left font-mono tabular-nums';
    case 'icon_text_left':
      return 'text-left';
    default:
      return 'text-left';
  }
}

function isMono(style: ColumnStyle): boolean {
  return style.startsWith('mono');
}

function Cell({
  column,
  value,
  flagId,
}: {
  readonly column: ColumnMeta;
  readonly value: unknown;
  readonly flagId?: string | null;
}): ReactNode {
  const text = value === null || value === undefined ? '' : String(value);

  if (text.length === 0) {
    // A blank is never left blank: it is said, so it is not mistaken for a
    // failure to load.
    return <span className="text-ink-faint">—</span>;
  }

  if (column.flag === true) {
    return (
      <span className="flex items-center gap-2">
        <Flag {...(flagId === undefined ? {} : { imageId: flagId })} />
        <span className={cx(isMono(column.style) && 'font-mono')}>{text}</span>
      </span>
    );
  }

  if (column.codeDomain !== undefined) {
    return (
      <span className="inline-flex rounded-full border border-line px-2 py-px text-xs text-ink-muted">
        {text}
      </span>
    );
  }

  if (column.temporal === true) {
    return <span className="font-mono text-xs tabular-nums">{text}</span>;
  }

  return <span className={cx(isMono(column.style) && 'font-mono tabular-nums')}>{text}</span>;
}
