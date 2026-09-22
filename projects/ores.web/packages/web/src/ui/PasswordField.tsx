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

import { useId, useState, type InputHTMLAttributes, type ReactNode } from 'react';
import { useTranslation } from '../i18n/Provider.js';
import { Field, Input, cx } from './Primitives.js';
import { PASSWORD_RULES, PASSWORD_SPECIAL_CHARS, MIN_PASSWORD_LENGTH, assessPassword } from './passwordPolicy.js';

/**
 * A password input with a show/hide toggle. Every password in the app uses it,
 * so revealing a password works and reads the same everywhere.
 */
export function PasswordInput({
  className,
  ...rest
}: Omit<InputHTMLAttributes<HTMLInputElement>, 'type'>): ReactNode {
  const { t } = useTranslation();
  const [reveal, setReveal] = useState(false);
  return (
    <div className="relative">
      <Input {...rest} type={reveal ? 'text' : 'password'} className={cx('pr-16', className)} />
      <button
        type="button"
        className="absolute inset-y-0 right-0 px-3 text-xs text-ink-faint hover:text-ink"
        aria-pressed={reveal}
        onClick={() => setReveal((value) => !value)}
      >
        {reveal ? t('password.hide') : t('password.show')}
      </button>
    </div>
  );
}

const STRENGTH_TONE = ['bg-line', 'bg-down', 'bg-warn', 'bg-up', 'bg-up'] as const;

/**
 * Choosing a new password: the policy's rules ticked off while typing, a
 * strength meter, and a confirmation that must match. `onChange` reports the
 * password and whether it may be submitted, which needs both.
 */
export function NewPasswordField({
  label,
  hint,
  value,
  onChange,
}: {
  readonly label?: string;
  readonly hint?: string;
  readonly value: string;
  readonly onChange: (password: string, acceptable: boolean) => void;
}): ReactNode {
  const { t } = useTranslation();
  const [confirm, setConfirm] = useState('');
  const rulesId = useId();
  const assessment = assessPassword(value);
  const mismatch = confirm.length > 0 && confirm !== value;

  const report = (password: string, confirmation: string): void =>
    onChange(password, assessPassword(password).valid && password === confirmation);

  return (
    <div className="space-y-3">
      <Field label={label ?? t('password.new')} {...(hint !== undefined && { hint })}>
        <PasswordInput
          value={value}
          autoComplete="new-password"
          aria-describedby={rulesId}
          onChange={(event) => report(event.target.value, confirm)}
        />
      </Field>

      <div id={rulesId} aria-live="polite">
        <div className="flex items-center gap-2">
          <div className="flex flex-1 gap-1" aria-hidden>
            {[1, 2, 3, 4].map((level) => (
              <span
                key={level}
                className={cx(
                  'h-1 flex-1 rounded-full',
                  assessment.strength >= level ? STRENGTH_TONE[assessment.strength] : 'bg-line',
                )}
              />
            ))}
          </div>
          <span className="w-16 text-right text-xs text-ink-muted">{t(`password.strength.${assessment.strength}`)}</span>
        </div>
        <ul className="mt-2 grid gap-x-4 gap-y-0.5 text-xs sm:grid-cols-2">
          {PASSWORD_RULES.map((rule) => {
            const met = assessment.met.has(rule);
            return (
              <li key={rule} className={met ? 'text-up' : 'text-ink-faint'}>
                <span aria-hidden>{met ? '✓' : '○'}</span>{' '}
                {t(`password.rule.${rule}`, { min: MIN_PASSWORD_LENGTH, chars: PASSWORD_SPECIAL_CHARS })}
                <span className="sr-only">{met ? t('password.ruleMet') : t('password.ruleNotMet')}</span>
              </li>
            );
          })}
        </ul>
      </div>

      <Field label={t('password.confirm')} {...(mismatch && { error: t('password.mismatch') })}>
        <PasswordInput
          value={confirm}
          autoComplete="new-password"
          aria-invalid={mismatch}
          onChange={(event) => {
            setConfirm(event.target.value);
            report(value, event.target.value);
          }}
        />
      </Field>
    </div>
  );
}
