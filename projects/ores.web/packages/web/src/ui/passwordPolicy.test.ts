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

import { describe, expect, it } from 'vitest';
import type { PasswordPolicy } from '@ores/wire-protocol/browser';
import { assessPassword, passwordRules } from './passwordPolicy.js';

/**
 * The assessment is the server's record, applied.
 *
 * The policy below is the one the deployment answers with, which is also the
 * rule set the validator enforces; what is asserted here is that the screen
 * assesses what the record says, and stops requiring a rule the record does
 * not state.
 */
const policy: PasswordPolicy = {
    success: true,
    message: '',
    minLength: 12,
    requireUppercase: true,
    requireLowercase: true,
    requireDigit: true,
    requireSpecial: true,
    specialChars: '!@#$%^&*()_+-=[]{}|;:,.<>?',
};

/** A policy that asks for length alone, as a deployment may. */
const lengthOnly: PasswordPolicy = {
    ...policy,
    requireUppercase: false,
    requireLowercase: false,
    requireDigit: false,
    requireSpecial: false,
};

describe('assessPassword', () => {
    it('rates an empty password zero', () => {
        expect(assessPassword('', policy)).toMatchObject({ valid: false, strength: 0 });
    });

    it('names each rule a password misses', () => {
        const result = assessPassword('abcdefghijkl', policy);
        expect(result.valid).toBe(false);
        expect([...result.met].sort()).toEqual(['length', 'lower']);
    });

    it('accepts a password that meets every rule the server stated', () => {
        expect(assessPassword('Abcdefgh123!', policy)).toMatchObject({ valid: true, strength: 3 });
    });

    it('rates a longer valid password strongest', () => {
        expect(assessPassword('Abcdefgh123!wxyz', policy).strength).toBe(4);
    });

    it('counts only the special characters the server listed', () => {
        expect(assessPassword('Abcdefgh1234~', policy).met.has('special')).toBe(false);
    });

    it('requires no rule the policy does not state', () => {
        expect(assessPassword('abcdefghijkl', lengthOnly)).toMatchObject({ valid: true });
        expect(passwordRules(lengthOnly)).toEqual(['length']);
    });

    it('states the rules in the order a screen lists them', () => {
        expect(passwordRules(policy)).toEqual(['length', 'upper', 'lower', 'digit', 'special']);
    });
});
