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

/**
 * The password policy, as the server enforces it.
 *
 * Mirrors ores.security's password_validator: the server is the authority and
 * rejects a password that breaks a rule; this copy lets a person see the rules
 * while typing instead of after submitting. A change to the policy is a change
 * to both.
 */
export const MIN_PASSWORD_LENGTH = 12;
export const PASSWORD_SPECIAL_CHARS = '!@#$%^&*()_+-=[]{}|;:,.<>?';

export type PasswordRule = 'length' | 'upper' | 'lower' | 'digit' | 'special';

export const PASSWORD_RULES: readonly PasswordRule[] = ['length', 'upper', 'lower', 'digit', 'special'];

export interface PasswordAssessment {
  readonly met: ReadonlySet<PasswordRule>;
  readonly valid: boolean;
  /** 0 (empty) to 4 (strong). Meeting the policy is 3; length beyond it is 4. */
  readonly strength: 0 | 1 | 2 | 3 | 4;
}

export function assessPassword(password: string): PasswordAssessment {
  const met = new Set<PasswordRule>();
  if (password.length >= MIN_PASSWORD_LENGTH) met.add('length');
  if (/[A-Z]/.test(password)) met.add('upper');
  if (/[a-z]/.test(password)) met.add('lower');
  if (/[0-9]/.test(password)) met.add('digit');
  if ([...password].some((c) => PASSWORD_SPECIAL_CHARS.includes(c))) met.add('special');

  const valid = met.size === PASSWORD_RULES.length;
  let strength: PasswordAssessment['strength'];
  if (password.length === 0) strength = 0;
  else if (valid) strength = password.length >= MIN_PASSWORD_LENGTH + 4 ? 4 : 3;
  else strength = met.size >= 3 ? 2 : 1;

  return { met, valid, strength };
}
