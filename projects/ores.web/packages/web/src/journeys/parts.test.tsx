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

import { describe, expect, it, vi } from 'vitest';
import { renderToStaticMarkup } from 'react-dom/server';
import type { ReactNode } from 'react';
import { TranslationProvider } from '../i18n/Provider.js';
import { FirstSignIn, signInComplete } from './FirstSignIn.js';
import { ProfileCards, RunStep, TenantForm, TenantSummary } from './parts.js';
import type { JourneyServer } from './server.js';
import { detailsFor, type TenantDetails } from './state.js';
import type { PasswordPolicy, SeedProfileChoice } from '@ores/wire-protocol/browser';

function render(node: ReactNode): string {
    return renderToStaticMarkup(<TranslationProvider>{node}</TranslationProvider>);
}

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

const profile: SeedProfileChoice = {
    code: 'empty_operational',
    name: 'Empty operational',
    summary: 'A production tenant with its own parties.',
    audience: 'Production',
    bullets: ['One root legal entity', 'Its counterparties'],
    tenant: {
        name: 'Northwind Capital',
        code: 'northwind',
        hostname: 'northwind.example.com',
        adminUsername: 'northwind_admin',
        adminEmail: 'admin@northwind.example.com',
    },
    inheritsAdminPassword: false,
    forcePasswordChange: true,
    order: 1,
    steps: [{ kind: 'provision_party', order: 1 }],
    parameters: [
        {
            name: 'root_lei',
            label: 'Root LEI',
            dataType: 'string',
            choices: [],
            defaultValue: '9695ACMEGROUP0000030',
            required: true,
            hint: '',
            order: 1,
        },
    ],
};

/** A profile that declares nothing, so the form opens on the person. */
const blank: SeedProfileChoice = {
    ...profile,
    code: 'empty_operational',
    tenant: { name: '', code: '', hostname: '', adminUsername: '', adminEmail: '' },
};

/**
 * The register-built profile. Its one setting chooses the entity the tenant is
 * built around, so the form states that setting before the tenant's own fields.
 */
const gleif: SeedProfileChoice = {
    ...profile,
    code: 'gleif_entity',
    name: 'GLEIF entity',
    tenant: { name: '', code: '', hostname: '', adminUsername: '', adminEmail: '' },
    parameters: [
        {
            name: 'root_lei',
            label: 'Parent legal entity',
            dataType: 'legal_entity',
            choices: [],
            defaultValue: '',
            required: true,
            hint: '',
            order: 1,
        },
    ],
};

/** The demo profile, whose code names the artwork the bundle carries. */
const acmeProfile: SeedProfileChoice = {
    ...profile,
    code: 'acme_demo',
    name: 'ACME demo',
};

function fakeServer(): JourneyServer {
    return {
        createAdministrator: vi.fn(async () => undefined),
        recheckBootstrap: vi.fn(async () => undefined),
        signIn: vi.fn(async () => ({ outcome: 'active', passwordResetRequired: false }) as const),
        chooseParty: vi.fn(async () => undefined),
        signOut: vi.fn(async () => undefined),
        passwordPolicy: vi.fn(async () => policy),
        seedProfiles: vi.fn(async () => [profile]),
        provision: vi.fn(async () => ({
            success: true,
            message: '',
            instanceId: '',
            tenantId: '',
            accountId: '',
        })),
        progress: vi.fn(async () => ({
            success: true,
            message: '',
            status: 'pending',
            error: '',
            step_count: 0,
            current_step_index: 0,
            steps: [],
        })),
        retry: vi.fn(async () => ({
            success: true,
            message: '',
            instanceId: '',
            stepIndex: 0,
            stepName: '',
        })),
        changePassword: vi.fn(async () => undefined),
    };
}

describe('the starting points', () => {
    it('states what the server said about each profile, and nothing of its own', () => {
        const html = render(
            <ProfileCards profiles={[profile]} selected={undefined} onSelect={() => undefined} />,
        );

        expect(html).toContain('Empty operational');
        expect(html).toContain('A production tenant with its own parties.');
        expect(html).toContain('One root legal entity');
        expect(html).toContain('1 settings · 1 steps');
    });

    it('shows the artwork the profile code names, and nothing when it names none', () => {
        const acme = render(
            <ProfileCards
                profiles={[acmeProfile]}
                selected={undefined}
                onSelect={() => undefined}
            />,
        );
        const plain = render(
            <ProfileCards profiles={[profile]} selected={undefined} onSelect={() => undefined} />,
        );

        expect(acme).toContain('<img');
        expect(acme).toContain('acme_demo');
        expect(plain).not.toContain('<img');
    });

    it('marks the profile the person chose', () => {
        const chosen = render(
            <ProfileCards
                profiles={[profile]}
                selected="empty_operational"
                onSelect={() => undefined}
            />,
        );
        const unchosen = render(
            <ProfileCards profiles={[profile]} selected={undefined} onSelect={() => undefined} />,
        );

        expect(chosen).toContain('aria-checked="true"');
        expect(unchosen).toContain('aria-checked="false"');
    });
});

describe('the tenant form', () => {
    it('states the settings the profile declares when the person fills it in', () => {
        const html = render(
            <TenantForm
                profile={blank}
                details={detailsFor(blank, '')}
                policy={policy}
                creatingPassword="Issued-Password-1"
                onChange={() => undefined}
                onPasswordAcceptable={() => undefined}
            />,
        );

        expect(html).toContain('Root LEI');
        expect(html).toContain('value="9695ACMEGROUP0000030"');
        expect(html).toContain('Name');
        expect(html).toContain('Hostname');
    });

    it('states the entity a tenant is built around before the tenant it names', () => {
        const html = render(
            <TenantForm
                server={fakeServer()}
                profile={gleif}
                details={detailsFor(gleif, '')}
                policy={policy}
                creatingPassword="Issued-Password-1"
                onChange={() => undefined}
                onPasswordAcceptable={() => undefined}
            />,
        );

        expect(html).toContain('Parent legal entity');
        expect(html.indexOf('Parent legal entity')).toBeLessThan(html.indexOf('Hostname'));
    });

    it('asks for an entity, and says what choosing one does', () => {
        const html = render(
            <TenantForm
                server={fakeServer()}
                profile={gleif}
                details={detailsFor(gleif, '')}
                policy={policy}
                creatingPassword="Issued-Password-1"
                onChange={() => undefined}
                onPasswordAcceptable={() => undefined}
            />,
        );

        expect(html).toContain('Search by name or LEI');
        expect(html).toContain('fills in the tenant below');
    });

    it('states the entity that was chosen instead of the letters that found it', () => {
        const chosen: TenantDetails = {
            ...detailsFor(gleif, ''),
            parameters: { root_lei: '213800LBQA1Y9L22JB70' },
        };
        const html = render(
            <TenantForm
                server={fakeServer()}
                profile={gleif}
                details={chosen}
                policy={policy}
                creatingPassword="Issued-Password-1"
                onChange={() => undefined}
                onPasswordAcceptable={() => undefined}
            />,
        );

        expect(html).toContain('213800LBQA1Y9L22JB70');
        expect(html).toContain('Change');
        // The box that found it is gone, because the choice is what the field
        // states: keeping the search text on screen says nothing about it.
        expect(html).not.toContain('Search by name or LEI');
    });

    it('states the tenant first when the person names it themselves', () => {
        const html = render(
            <TenantForm
                server={fakeServer()}
                profile={blank}
                details={detailsFor(blank, '')}
                policy={policy}
                creatingPassword="Issued-Password-1"
                onChange={() => undefined}
                onPasswordAcceptable={() => undefined}
            />,
        );

        expect(html.indexOf('Hostname')).toBeLessThan(html.indexOf('Root LEI'));
    });

    it('opens as a summary, and asks for no settings, when the profile states its own', () => {
        const html = render(
            <TenantForm
                profile={profile}
                details={detailsFor(profile, 'Issued-Password-1')}
                policy={policy}
                creatingPassword="Issued-Password-1"
                onChange={() => undefined}
                onPasswordAcceptable={() => undefined}
            />,
        );

        expect(html).toContain('Empty operational uses its standard settings.');
        expect(html).toContain('Northwind Capital (northwind)');
        expect(html).not.toContain('Root LEI');
    });

    it('states the forced change beside the password it applies to', () => {
        const html = render(
            <TenantForm
                profile={blank}
                details={detailsFor(blank, '')}
                policy={policy}
                creatingPassword="Issued-Password-1"
                onChange={() => undefined}
                onPasswordAcceptable={() => undefined}
            />,
        );

        expect(html).toContain('They must change it at first sign-in.');
    });

    it('asks for no rule the deployment does not state', () => {
        const lengthOnly: PasswordPolicy = {
            ...policy,
            requireUppercase: false,
            requireLowercase: false,
            requireDigit: false,
            requireSpecial: false,
        };
        const html = render(
            <TenantForm
                profile={blank}
                details={detailsFor(blank, '')}
                policy={lengthOnly}
                creatingPassword="Issued-Password-1"
                onChange={() => undefined}
                onPasswordAcceptable={() => undefined}
            />,
        );

        expect(html).toContain('At least 12 characters');
        expect(html).not.toContain('special character');
    });
});

describe('the review', () => {
    it('shows what is about to be created, and how much work it is', () => {
        const html = render(
            <TenantSummary
                profile={profile}
                details={detailsFor(profile, 'Issued-Password-1')}
                creatingPassword="Issued-Password-1"
            />,
        );

        expect(html).toContain('Empty operational');
        expect(html).toContain('Northwind Capital');
        expect(html).toContain('northwind.example.com');
        expect(html).toContain('Creating the tenant runs 1 steps.');
        expect(html).toContain('northwind_admin sets a password of their own at first sign-in.');
    });
});

describe('the first sign-in', () => {
    const server = fakeServer();

    it('asks a person who was handed the account for its credentials', () => {
        const html = render(
            <FirstSignIn
                server={server}
                policy={policy}
                entry={{
                    kind: 'sign-in',
                    principal: 'northwind_admin@northwind.example.com',
                    password: '',
                }}
                onDone={() => undefined}
            />,
        );

        expect(html).toContain('current-password');
        expect(html).toContain('northwind_admin@northwind.example.com');
    });

    it('offers the parties to work in when the account works in more than one', () => {
        const html = render(
            <FirstSignIn
                server={server}
                policy={policy}
                entry={{
                    kind: 'party',
                    principal: 'northwind_admin@northwind.example.com',
                    password: 'Chosen-Password-2',
                    resetRequired: false,
                    parties: [
                        {
                            id: '22222222-2222-2222-2222-222222222222',
                            name: 'Northwind Capital',
                            partyCategory: 'Operational',
                            businessCenterCode: 'GBLO',
                        },
                    ],
                }}
                onDone={() => undefined}
            />,
        );

        expect(html).toContain('Northwind Capital');
        expect(html).not.toContain('type="password"');
    });

    it('asks for a password of the person\u2019s own when the account must change it', () => {
        const html = render(
            <FirstSignIn
                server={server}
                policy={policy}
                entry={{
                    kind: 'active',
                    principal: 'northwind_admin@northwind.example.com',
                    password: 'Issued-Password-1',
                    resetRequired: true,
                }}
                onDone={() => undefined}
            />,
        );

        expect(html).toContain('This account sets a password of its own');
        expect(html).toContain('new-password');
    });

    it('shows no password field when no change is outstanding', () => {
        const html = render(
            <FirstSignIn
                server={server}
                policy={policy}
                entry={{
                    kind: 'active',
                    principal: 'northwind_admin@northwind.example.com',
                    password: 'Issued-Password-1',
                    resetRequired: false,
                }}
                onDone={() => undefined}
            />,
        );

        expect(html).toContain('Signed in as northwind_admin@northwind.example.com.');
        expect(html).not.toContain('type="password"');
    });
});

describe("a run's step", () => {
    const step = {
        id: '11111111-1111-1111-1111-111111111111',
        name: 'import_lei_hierarchy',
        label: 'Import the legal entities',
        description: 'Reads the legal entity the starting point names by its LEI.',
        status: 'completed',
        step_index: 2,
        created_at: '',
        started_at: null,
        completed_at: null,
        error: '',
        log: [],
    };

    it('states why a step finished with a warning, beside it', () => {
        const html = render(
            <RunStep
                step={{
                    ...step,
                    status: 'completed_with_warnings',
                    log: [
                        {
                            level: 'warn',
                            message:
                                'The starting point names no legal entity, so there was nothing to import.',
                            context: '',
                        },
                    ],
                }}
                words={(sentence) => sentence}
            />,
        );

        expect(html).toContain('there was nothing to import');
    });

    it('states the step in the words the server sent for it', () => {
        const html = render(<RunStep step={step} words={(sentence) => sentence} />);

        expect(html).toContain('Import the legal entities');
        expect(html).toContain('Reads the legal entity the starting point names by its LEI.');
    });

    it('falls back to the step\u2019s identity when its definition had no words', () => {
        const html = render(
            <RunStep step={{ ...step, label: '', description: '' }} words={(s) => s} />,
        );

        expect(html).toContain('import_lei_hierarchy');
    });

    it('translates the words through the catalogue it is handed', () => {
        const html = render(<RunStep step={step} words={(sentence) => `[${sentence}]`} />);

        expect(html).toContain('[Import the legal entities]');
    });
});

describe('when the first sign-in is finished', () => {
    const tenant = 'tenant_admin@northwind.example.com';

    it('is finished when the account is signed in and owes nothing', () => {
        expect(
            signInComplete(
                { kind: 'active', principal: tenant, password: 'p', resetRequired: false },
                false,
            ),
        ).toBe(true);
    });

    it('is not finished while a party is still to be chosen', () => {
        expect(
            signInComplete(
                {
                    kind: 'party',
                    principal: tenant,
                    password: 'p',
                    parties: [],
                    resetRequired: false,
                },
                false,
            ),
        ).toBe(false);
    });

    it('is not finished until a required password change has happened', () => {
        const active = {
            kind: 'active',
            principal: tenant,
            password: 'p',
            resetRequired: true,
        } as const;

        expect(signInComplete(active, false)).toBe(false);
        expect(signInComplete(active, true)).toBe(true);
    });
});
