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

import type { PasswordPolicy } from '@ores/wire-protocol/browser';

/**
 * The password policy, assessed against the record the server answered with.
 *
 * The rules live in ores.security's validator and reach a screen only through
 * the policy read, so nothing here is a copy of them: a rule the server changes
 * changes what this assesses, and a rule the server does not state is not shown
 * and not required. The server still refuses a password that breaks a rule; this
 * only says so while the person types instead of after they submit.
 */

export type PasswordRule = 'length' | 'upper' | 'lower' | 'digit' | 'special';

/**
 * The rules a policy declares, in the order a screen lists them.
 *
 * A rule the policy does not require is absent rather than shown as optional,
 * because a screen that listed every rule a server could have would describe
 * deployments other than this one.
 */
export function passwordRules(policy: PasswordPolicy): readonly PasswordRule[] {
    const rules: PasswordRule[] = ['length'];
    if (policy.requireUppercase) {
        rules.push('upper');
    }
    if (policy.requireLowercase) {
        rules.push('lower');
    }
    if (policy.requireDigit) {
        rules.push('digit');
    }
    if (policy.requireSpecial) {
        rules.push('special');
    }
    return rules;
}

export interface PasswordAssessment {
    readonly met: ReadonlySet<PasswordRule>;
    readonly valid: boolean;
    /** 0 (empty) to 4 (strong). Meeting the policy is 3; length beyond it is 4. */
    readonly strength: 0 | 1 | 2 | 3 | 4;
}

export function assessPassword(password: string, policy: PasswordPolicy): PasswordAssessment {
    const met = new Set<PasswordRule>();
    if (password.length >= policy.minLength) {
        met.add('length');
    }
    if (policy.requireUppercase && /[A-Z]/.test(password)) {
        met.add('upper');
    }
    if (policy.requireLowercase && /[a-z]/.test(password)) {
        met.add('lower');
    }
    if (policy.requireDigit && /[0-9]/.test(password)) {
        met.add('digit');
    }
    if (
        policy.requireSpecial &&
        policy.specialChars !== '' &&
        [...password].some((character) => policy.specialChars.includes(character))
    ) {
        met.add('special');
    }

    const rules = passwordRules(policy);
    const valid = rules.every((rule) => met.has(rule));
    let strength: PasswordAssessment['strength'];
    if (password.length === 0) {
        strength = 0;
    } else if (valid) {
        strength = password.length >= policy.minLength + 4 ? 4 : 3;
    } else {
        strength = met.size >= Math.ceil(rules.length / 2) ? 2 : 1;
    }

    return { met, valid, strength };
}
