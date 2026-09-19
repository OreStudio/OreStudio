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
import { useTranslation } from '../i18n/Provider.js';
import { MaskIcon } from '../ui/icons/MaskIcon.js';
import { Notice, cx } from '../ui/Primitives.js';
import { ConfirmDialog } from './ConfirmDialog.js';
import { diffValues, type Segment } from './diff.js';
import type { ColumnMeta, EntityMeta } from '../ui-contract.js';

/**
 * The version history of one record.
 *
 * A timeline on the left and a comparison on the right, which is the shape the
 * Qt client settled on and the right one: the timeline answers "what happened",
 * and the comparison answers "what exactly changed".
 *
 * Provenance fields are excluded from the comparison and shown on the timeline
 * entry instead, because the version and the actor differ by definition and
 * diffing them is noise that hides the real change.
 */
export interface HistoryVersion {
  /** The version number, which is the key. */
  readonly version: number;
  readonly modifiedBy: string;
  readonly performedBy: string;
  readonly recordedAt: string;
  readonly changeReasonCode: string;
  readonly changeCommentary: string;
  /** The record's own fields, by wire name, for the diff. */
  readonly values: Readonly<Record<string, unknown>>;
  /**
   * The record as the service sent it, for anything that has to write it back.
   *
   * Carried rather than looked up again, because a version number repeats when a
   * record is deleted and created again, so a number does not identify a version.
   */
  readonly wire?: unknown;
}

export interface EntityHistoryPageProps {
  readonly meta: EntityMeta;
  readonly title: string;
  readonly versions: readonly HistoryVersion[];
  readonly loading: boolean;
  readonly failed: boolean;
  readonly onRetry: () => void;
  readonly onOpenVersion: (version: number) => void;
  /**
   * Puts the record back to a version, by writing it again.
   *
   * Absent where the entity cannot be written, in which case no action is shown.
   */
  readonly onRevert?: (version: HistoryVersion) => void;
  /** What the record is called, for the confirmation. */
  readonly recordName?: string;
}

export function EntityHistoryPage({
  meta,
  title,
  versions,
  loading,
  failed,
  onRetry,
  onOpenVersion,
  onRevert,
  recordName,
}: EntityHistoryPageProps): ReactNode {
  const { t } = useTranslation();
  // Newest first, so the default selection is the two most recent versions.
  /*
   * Two versions, chosen independently.
   *
   * Comparing only neighbours is the smallest case of the thing people actually
   * want, which is "this one against that one" — the record as it stands against
   * how it stood five versions ago. So the pair is chosen rather than implied,
   * and stepping moves both.
   *
   * Indexes into the newest-first list, so the older version has the larger
   * index of the two.
   */
  const [newerIndex, setNewerIndex] = useState(0);
  const [olderIndex, setOlderIndex] = useState(1);
  const selected = newerIndex;
  const [showAll, setShowAll] = useState(false);
  const [confirmingRevert, setConfirmingRevert] = useState(false);


  /*
   * One row per history entry.
   *
   * The identity is a version and its content, and neither alone will do.
   *
   * Version alone collapses the generations: a record that was deleted and
   * created again starts numbering at one, so version alone made an old
   * comparison look like the current one.
   *
   * Version and time together keep the duplicates the service returns — the same
   * version written twice with different timestamps, which is a data problem to
   * fix at the source — so the screen compared a row with itself and reported no
   * changes for a record that had changed.
   *
   * Version and content is right: two entries with the same version and the same
   * values are one entry, whoever wrote them and whenever, and two entries with
   * the same version and different values are different entries.
   */
  const entries = useMemo(() => {
    const seen = new Set<string>();
    return versions.filter((version) => {
      const key = entryKey(version, meta.columns);
      if (seen.has(key)) return false;
      seen.add(key);
      return true;
    });
  }, [versions, meta.columns]);

  /*
   * A window of versions rather than all of them.
   *
   * A hundred versions in a list is a scrollbar again, just a taller one: the
   * arrows would walk to the end of the visible few and then a person is back to
   * dragging a thumb. Showing a fixed handful and letting the arrows move the
   * window means every version is two keys away however many there are.
   *
   * Derived from the selection rather than remembered, so the window and the
   * selection cannot disagree.
   */
  const WINDOW = 7;
  const half = Math.floor(WINDOW / 2);
  const windowStart = Math.max(0, Math.min(entries.length - WINDOW, newerIndex - half));
  const windowed = entries.slice(windowStart, windowStart + WINDOW);

  const newer = entries[newerIndex];
  const older = entries[olderIndex];

  /** Moves the pair one version, keeping them the same distance apart. */
  function step(direction: number): void {
    const span = olderIndex - newerIndex;
    const nextNewer = Math.min(Math.max(0, newerIndex + direction), entries.length - 1);
    const nextOlder = Math.min(entries.length - 1, nextNewer + Math.max(1, span));
    if (nextNewer === newerIndex) return;
    setNewerIndex(nextNewer);
    setOlderIndex(nextOlder);
  }
  // Two versions can only be compared one way round, and saying so is better
  // than showing a diff that reads backwards.
  const outOfOrder = older !== undefined && newer !== undefined && olderIndex <= newerIndex;

  const rows = useMemo(
    () => (older === undefined || newer === undefined || outOfOrder ? [] : diff(older, newer, meta.columns)),
    [older, newer, meta.columns, outOfOrder],
  );
  const shown = showAll ? rows : rows.filter((row) => row.changed);

  if (failed) {
    return (
      <div className="mx-auto max-w-[680px] px-5 py-16 text-center">
        <Notice tone="error">
          {t('feedback.unreachable')}{' '}
          <button type="button" onClick={onRetry} className="underline hover:text-ink">
            {t('feedback.retry')}
          </button>
        </Notice>
      </div>
    );
  }

  return (
    <div className="mx-auto max-w-[1100px] px-5 py-7">
      <header className="mb-5">
        <h1 className="text-xl font-semibold tracking-tight">{title}</h1>
        <p className="mt-1 text-sm text-ink-muted">{t('history.description')}</p>
      </header>

      {loading ? (
        <p className="text-sm text-ink-faint">{t('entity.loading')}</p>
      ) : entries.length === 0 ? (
        <p className="text-sm text-ink-muted">{t('history.empty')}</p>
      ) : (
        /*
         * Each pane scrolls on its own within the height of the window.
         *
         * A record can have hundreds of versions, and letting the timeline set
         * the page height means the fields scroll away exactly when there is
         * most to compare — the reader loses the thing they are reading in order
         * to move between the things they are reading it against.
         */
        /*
         * One scrollbar, and it is the page's.
         *
         * Two panes scrolling inside a scrolling page is three scrollbars where
         * one will do, and the inner one is small, awkward and unnecessary. The
         * list flows with the page; the comparison is pinned, so it stays where
         * the reader is looking while the list moves past it.
         */
        <div
          className="grid items-start gap-5 lg:grid-cols-[minmax(170px,210px)_1fr]"
        >
          <section>
            <div className="mb-2 flex items-center gap-2">
              <h2 className="text-sm font-medium text-ink-muted">
                {t('history.timeline')}
                {/* A window hides the rest, so it says how much there is. */}
                <span className="ml-1.5 font-normal tabular-nums text-ink-faint">
                  {newerIndex + 1}/{entries.length}
                </span>
              </h2>
              {/*
                Stepping, because a scrollbar makes a person hunt.
                Going from v12 to v11 by aiming at a scrollbar is work; by
                pressing a key it is not, and the comparison follows.
              */}
              <div className="ml-auto flex items-center gap-1">
                <StepButton
                  label={t('history.newer')}
                  disabled={newerIndex === 0}
                  onClick={() => step(-1)}
                  direction="up"
                />
                <StepButton
                  label={t('history.older')}
                  disabled={olderIndex >= entries.length - 1}
                  onClick={() => step(1)}
                  direction="down"
                />
              </div>
            </div>
            <ol
              // Focusable so the arrows work without a mouse, and so a person
              // tabbing through the screen lands somewhere useful.
              tabIndex={0}
              onKeyDown={(event) => {
                if (event.key === 'ArrowDown' || event.key === 'j') {
                  event.preventDefault();
                  step(1);
                } else if (event.key === 'ArrowUp' || event.key === 'k') {
                  event.preventDefault();
                  step(-1);
                }
              }}
              aria-label={t('history.timeline')}
              className="space-y-1.5 focus:outline-none focus-visible:ring-2 focus-visible:ring-accent"
            >
              {windowed.map((version, offset) => {
                const index = windowStart + offset;
                return (
                // Keyed by the same identity the deduplication uses: a version
                // alone can legitimately appear twice, once per life of the
                // record after a delete and a re-create.
                <li key={entryKey(version, meta.columns)}>
                  <button
                    type="button"
                    // Choosing a version previews it against the one before, and
                    // the selects can then take it anywhere.
                    onClick={() => {
                      setNewerIndex(index);
                      setOlderIndex(Math.min(entries.length - 1, index + 1));
                    }}
                    aria-current={index === selected}
                    className={cx(
                      'w-full rounded-[var(--radius-card)] border px-3 py-2 text-left transition-colors',
                      index === selected
                        ? 'border-accent bg-surface-overlay'
                        : 'border-line bg-bg-secondary hover:border-line-strong',
                    )}
                  >
                    <span className="flex items-center gap-2">
                      <span className="font-mono text-xs text-ink">
                        v{version.version}
                      </span>
                      {index === 0 && (
                        <span className="rounded-full border border-line px-1.5 py-px text-[10px] uppercase tracking-wide text-ink-faint">
                          {t('history.current')}
                        </span>
                      )}
                    </span>
                    <span className="mt-0.5 block font-mono text-xs tabular-nums text-ink-muted">
                      {version.recordedAt}
                    </span>
                    <span className="mt-1 block truncate text-xs text-ink-muted">
                      {version.modifiedBy.length > 0 ? version.modifiedBy : t('account.nobody')}
                    </span>
                    {version.changeReasonCode.length > 0 && (
                      <span className="mt-1 inline-flex rounded-full border border-line px-1.5 py-px text-[10px] text-ink-faint">
                        {version.changeReasonCode}
                      </span>
                    )}
                    {version.changeCommentary.length > 0 && (
                      <span className="mt-1 block text-xs italic text-ink-faint">
                        “{version.changeCommentary}”
                      </span>
                    )}
                  </button>
                </li>
                );
              })}
            </ol>
          </section>

          <section className="min-w-0 lg:sticky lg:top-4">
            <div className="mb-2 flex flex-wrap items-center gap-3">
              {/*
                Any two versions, not only neighbours.
                The question a history gets asked is "what is different now from
                how it was then", and "then" is rarely the version before.
              */}
              <div className="flex items-center gap-1.5 text-sm text-ink-muted">
                <label className="flex items-center gap-1">
                  <span className="text-xs">{t('history.from')}</span>
                  <select
                    value={olderIndex}
                    onChange={(event) => setOlderIndex(Number(event.target.value))}
                    aria-label={t('history.from')}
                    className="h-7 rounded-md border border-line bg-bg-secondary px-1.5 text-xs text-ink focus:border-line-strong focus:outline-none"
                  >
                    {entries.map((version, index) => (
                      <option key={`from-${index}`} value={index}>
                        v{version.version}
                        {index === 0 ? ` (${t('history.current')})` : ''}
                      </option>
                    ))}
                  </select>
                </label>
                <span className="text-ink-faint">→</span>
                <label className="flex items-center gap-1">
                  <span className="text-xs">{t('history.to')}</span>
                  <select
                    value={newerIndex}
                    onChange={(event) => setNewerIndex(Number(event.target.value))}
                    aria-label={t('history.to')}
                    className="h-7 rounded-md border border-line bg-bg-secondary px-1.5 text-xs text-ink focus:border-line-strong focus:outline-none"
                  >
                    {entries.map((version, index) => (
                      <option key={`to-${index}`} value={index}>
                        v{version.version}
                        {index === 0 ? ` (${t('history.current')})` : ''}
                      </option>
                    ))}
                  </select>
                </label>
              </div>
              <div className="ml-auto flex items-center gap-1 rounded-md border border-line p-0.5">
                {([false, true] as const).map((all) => (
                  <button
                    key={String(all)}
                    type="button"
                    onClick={() => setShowAll(all)}
                    className={cx(
                      'rounded px-2 py-1 text-xs transition-colors',
                      showAll === all ? 'bg-surface-overlay text-ink' : 'text-ink-muted hover:text-ink',
                    )}
                  >
                    {all ? t('history.allFields') : t('history.onlyChanges')}
                  </button>
                ))}
              </div>
            </div>

            {outOfOrder ? (
              <p className="rounded-[var(--radius-card)] border border-line bg-bg-secondary px-4 py-6 text-center text-sm text-ink-muted">
                {t('history.outOfOrder')}
              </p>
            ) : rows.length === 0 ? (
              <p className="rounded-[var(--radius-card)] border border-line bg-bg-secondary px-4 py-6 text-center text-sm text-ink-muted">
                {t('history.noChanges')}
              </p>
            ) : (
              <div className="overflow-hidden rounded-[var(--radius-card)] border border-line bg-bg-secondary">
                <table className="w-full border-collapse text-sm">
                  <thead>
                    <tr className="border-b border-line text-xs text-ink-muted">
                      <th scope="col" className="px-3 py-2 text-left font-medium">{t('history.field')}</th>
                      <th scope="col" className="px-3 py-2 text-left font-medium">{t('history.before')}</th>
                      <th scope="col" className="px-3 py-2 text-left font-medium">{t('history.after')}</th>
                    </tr>
                  </thead>
                  <tbody>
                    {shown.map((row) => (
                      <tr key={row.name} className="border-b border-line/60 last:border-0">
                        <td className="px-3 py-2 text-ink-muted">{t(row.headerKey)}</td>
                        {/* The cell carries a wash and the run carries the mark,
                            so a scan finds the field and reading finds the
                            characters. */}
                        <td
                          className={cx(
                            'px-3 py-2 font-mono text-xs',
                            row.changed && 'bg-red-500/[0.06]',
                          )}
                        >
                          <Value segments={row.before} side="before" />
                        </td>
                        <td
                          className={cx(
                            'px-3 py-2 font-mono text-xs',
                            row.changed && 'bg-emerald-500/[0.06]',
                          )}
                        >
                          <Value segments={row.after} side="after" />
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            )}

            {/*
              Reverting writes the record again from this version, so the newest
              version has nothing to revert to and offers nothing.
            */}
            {onRevert !== undefined && newer !== undefined && older !== undefined && (
              <button
                type="button"
                onClick={() => setConfirmingRevert(true)}
                className="inline-flex items-center gap-1.5 rounded-md border border-line px-2.5 py-1 text-sm text-ink-muted transition-colors hover:border-line-strong hover:text-ink"
              >
                <MaskIcon name="arrowRotateCounterclockwise" className="size-3.5" />
                {t('history.revert')}
              </button>
            )}

            {newer !== undefined && (
              <button
                type="button"
                onClick={() => onOpenVersion(newer.version)}
                className="mt-3 inline-flex items-center gap-1.5 text-sm text-accent hover:text-accent-bright"
              >
                <MaskIcon name="edit" className="size-3.5" />
                {t('history.openVersion')}
              </button>
            )}
          </section>
        </div>
      )}
      {confirmingRevert && newer !== undefined && older !== undefined && (
        <ConfirmDialog
          title={t('history.revert')}
          body={t('history.revertBody', {
            name: recordName ?? '',
            from: newer.version,
            to: older.version,
          })}
          confirmLabel={t('history.revert')}
          pending={false}
          onCancel={() => setConfirmingRevert(false)}
          onConfirm={() => {
            setConfirmingRevert(false);
            onRevert?.(older);
          }}
        />
      )}

    </div>
  );
}

/**
 * A value, with the part that differs marked.
 *
 * Red on the side it left and green on the side it arrived, which is the
 * convention a person already reads in a code host, and the reason the changed
 * characters are found rather than the whole value being coloured: marking the
 * whole cell says "something here is different", and marking the run says what.
 *
 * The runs are the only place colour carries meaning here, so the two are the
 * same green and red used for success and failure elsewhere, and nothing else on
 * the screen uses them.
 */
/** One step through the versions, by button. */
function StepButton({
  label,
  disabled,
  onClick,
  direction,
}: {
  readonly label: string;
  readonly disabled: boolean;
  readonly onClick: () => void;
  readonly direction: 'up' | 'down';
}): ReactNode {
  return (
    <button
      type="button"
      onClick={onClick}
      disabled={disabled}
      title={label}
      aria-label={label}
      className="rounded-md border border-line p-1 text-ink-muted transition-colors hover:text-ink disabled:opacity-35 disabled:hover:text-ink-muted"
    >
      <svg viewBox="0 0 16 16" className="size-3" aria-hidden>
        <path
          d={direction === 'up' ? 'M8 11V5M4.5 8.5L8 5l3.5 3.5' : 'M8 5v6M4.5 7.5L8 11l3.5-3.5'}
          fill="none"
          stroke="currentColor"
          strokeWidth="1.6"
          strokeLinecap="round"
        />
      </svg>
    </button>
  );
}


function Value({
  segments,
  side,
}: {
  readonly segments: readonly Segment[];
  readonly side: 'before' | 'after';
}): ReactNode {
  if (segments.length === 0) {
    // A blank is said rather than shown, so it is not mistaken for a failure.
    return <span className="text-ink-faint">—</span>;
  }
  return (
    <span>
      {segments.map((segment, index) =>
        segment.changed ? (
          <mark
            key={index}
            className={
              side === 'before'
                ? 'rounded-sm bg-red-500/20 px-0.5 text-red-200'
                : 'rounded-sm bg-emerald-500/20 px-0.5 text-emerald-200'
            }
          >
            {segment.text}
          </mark>
        ) : (
          <span key={index} className="text-ink-muted">
            {segment.text}
          </span>
        ),
      )}
    </span>
  );
}

interface DiffRow {
  readonly name: string;
  readonly headerKey: string;
  /** The value split into what changed and what did not, per side. */
  readonly before: readonly Segment[];
  readonly after: readonly Segment[];
  readonly changed: boolean;
}

/**
 * The columns a comparison is about.
 *
 * The identity, the actor and the time are excluded: one cannot change, and the
 * others differ by definition, so including them would show a change on every
 * row of every comparison and hide the ones that matter.
 */
function comparedColumns(columns: readonly ColumnMeta[]): readonly ColumnMeta[] {
  return columns.filter(
    (column) =>
      column.name !== 'version' &&
      column.name !== 'modified_by' &&
      column.name !== 'recorded_at',
  );
}

/** What makes two history entries the same entry. */
function entryKey(version: HistoryVersion, columns: readonly ColumnMeta[]): string {
  const values = comparedColumns(columns).map((column) => text(version.values[column.name]));
  return `${version.version}\u0000${values.join('\u0000')}`;
}

/** Compares two versions across the entity's own columns. */
function diff(
  older: HistoryVersion,
  newer: HistoryVersion,
  columns: readonly ColumnMeta[],
): readonly DiffRow[] {
  return comparedColumns(columns)
    .map((column) => {
      const before = text(older.values[column.name]);
      const after = text(newer.values[column.name]);
      const diff = diffValues(before, after);
      return {
        name: column.name,
        headerKey: column.headerKey,
        before: diff.before,
        after: diff.after,
        changed: !diff.equal,
      };
    });
}

function text(value: unknown): string {
  return value === null || value === undefined ? '' : String(value);
}
