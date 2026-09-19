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
import { useTranslation } from '../i18n/Provider.js';
import { Button, Dialog, Notice } from '../ui/Primitives.js';
import {
  reasonsFor,
  type ChangeReason,
  type WriteOperation,
} from '../api/changeReasons.js';

/**
 * Why a change happened.
 *
 * Asked once, after the person has committed to the change and before it is
 * sent, so the flow is: press Save, choose why, done. Collected on every write,
 * including a delete, because a record whose reason is unknown is a record
 * nobody can account for later.
 *
 * The reasons offered depend on whether anything actually changed. That is the
 * rule worth the most care here: a person cannot file a material change as a
 * touch, nor a touch as a material change, and the set is computed from the diff
 * rather than left to judgement.
 */
export interface ChangeReasonResult {
  readonly reasonCode: string;
  readonly commentary: string;
}

export function ChangeReasonDialog({
  operation,
  hasChanges,
  reasons,
  pending,
  onConfirm,
  onCancel,
}: {
  readonly operation: WriteOperation;
  readonly hasChanges: boolean;
  readonly reasons: readonly ChangeReason[];
  readonly pending: boolean;
  readonly onConfirm: (result: ChangeReasonResult) => void;
  readonly onCancel: () => void;
}): ReactNode {
  const { t } = useTranslation();
  const offered = reasonsFor(reasons, operation, hasChanges);

  /*
   * The default is the first reason in the server's order.
   *
   * `display_order` is authored for exactly this: the reasons people reach for
   * most sit first, and the catch-alls carry a sentinel of 1000 so they sink.
   * So the first is the intended default and there is nothing to guess.
   */
  const [selected, setSelected] = useState<string>(() => offered[0]?.code ?? '');
  const [commentary, setCommentary] = useState('');

  const reason = offered.find((r) => r.code === selected);
  const commentaryRequired = reason?.requiresCommentary === true;
  const commentaryMissing = commentaryRequired && commentary.trim().length === 0;
  const canConfirm = selected.length > 0 && !commentaryMissing && !pending;

  const title =
    operation === 'create'
      ? t('audit.createTitle')
      : operation === 'amend'
        ? t('audit.amendTitle')
        : t('audit.deleteTitle');

  const prompt =
    operation === 'create'
      ? t('audit.createPrompt')
      : operation === 'amend'
        ? t('audit.amendPrompt')
        : t('audit.deletePrompt');

  const commitLabel =
    operation === 'create'
      ? t('audit.create')
      : operation === 'amend'
        ? t('entity.save')
        : t('audit.confirmDelete');

  return (
    <Dialog title={title} onClose={onCancel} wide>
      <p className="mb-4 text-sm text-ink-muted">{prompt}</p>

      {offered.length === 0 ? (
        // Saying so is better than an empty selector, and much better than
        // sending a write with no reason at all.
        <Notice tone="error">{t('audit.noReasons')}</Notice>
      ) : (
        <>
          {/*
            A combo, not a list of buttons.
            
            There are twenty-odd reasons for an amendment and the list grows, so a
            wall of buttons is a wall to read before every write. A combo shows
            one at a time and lets the browser's own keyboard handling do the
            finding, which is what it is good at.
          */}
          <label className="block">
            <span className="mb-1.5 block text-xs font-medium text-ink-muted">
              {t('audit.reason')}
            </span>
            <select
              value={selected}
              onChange={(event) => setSelected(event.target.value)}
              autoFocus
              className="h-9 w-full rounded-md border border-line bg-bg-secondary px-2.5 text-sm text-ink focus:border-line-strong focus:outline-none"
            >
              {offered.map((option) => (
                <option key={option.code} value={option.code}>
                  {option.description.length > 0 ? option.description : option.code}
                </option>
              ))}
            </select>
          </label>

          {/*
            The code and the description below the control, because the combo
            shows one of them and the audit trail records the other. Seeing both
            is what stops a person recording a reason they did not mean.
          */}
          {reason !== undefined && (
            <p className="mt-1.5 text-xs text-ink-faint">
              <span className="font-mono">{reason.code}</span>
              {reason.description.length > 0 && ` — ${reason.description}`}
            </p>
          )}

          <label className="mt-4 block">
            <span className="mb-1.5 flex items-center gap-1.5 text-xs font-medium text-ink-muted">
              {t('audit.commentary')}
              {commentaryRequired && <span className="text-ink-faint">*</span>}
            </span>
            <textarea
              value={commentary}
              rows={3}
              onChange={(event) => setCommentary(event.target.value)}
              placeholder={t('audit.commentaryPlaceholder')}
              className="w-full rounded-md border border-line bg-bg-secondary px-2.5 py-2 text-sm text-ink placeholder:text-ink-faint focus:border-line-strong focus:outline-none"
            />
            <span className="mt-1 block text-xs text-ink-faint">
              {/* Conditionally required, and said plainly either way. */}
              {commentaryRequired ? t('audit.commentaryRequired') : t('audit.commentaryOptional')}
            </span>
          </label>
        </>
      )}

      <div className="mt-5 flex justify-end gap-2">
        <Button variant="ghost" size="md" onClick={onCancel} disabled={pending}>
          {t('entity.cancel')}
        </Button>
        <Button
          variant={operation === 'delete' ? 'danger' : 'primary'}
          size="md"
          disabled={!canConfirm}
          pending={pending}
          pendingLabel={t('entity.saving')}
          onClick={() => onConfirm({ reasonCode: selected, commentary: commentary.trim() })}
        >
          {commitLabel}
        </Button>
      </div>
    </Dialog>
  );
}
