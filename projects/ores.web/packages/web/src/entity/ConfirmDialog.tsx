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
import { Button, Dialog } from '../ui/Primitives.js';

/**
 * Confirming a destructive action.
 *
 * Says what will be lost and names the record, because "are you sure" without
 * the name is a question a person cannot answer.
 */
export function ConfirmDialog({
  title,
  body,
  confirmLabel,
  pending,
  onConfirm,
  onCancel,
}: {
  readonly title: string;
  readonly body: string;
  readonly confirmLabel: string;
  readonly pending: boolean;
  readonly onConfirm: () => void;
  readonly onCancel: () => void;
}): ReactNode {
  const { t } = useTranslation();
  return (
    <Dialog title={title} onClose={onCancel}>
      <p className="text-sm text-ink-muted">{body}</p>
      <div className="mt-5 flex justify-end gap-2">
        <Button variant="ghost" size="md" onClick={onCancel} disabled={pending}>
          {t('entity.cancel')}
        </Button>
        <Button variant="danger" size="md" onClick={onConfirm} pending={pending} pendingLabel={t('entity.saving')}>
          {confirmLabel}
        </Button>
      </div>
    </Dialog>
  );
}
