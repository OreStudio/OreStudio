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

import { flatten, type SourceCatalogue } from '../translate.js';

/**
 * English, the source catalogue.
 *
 * This file is the key set. Every other language is checked against it, so a
 * message is added here first and then translated. Keys are grouped by area and
 * named for what the message is, not where it appears, so moving a control does
 * not move its key.
 */
export const en: SourceCatalogue = {
    app: {
        name: 'ORE Studio',
        tagline: 'Enterprise-grade risk analytics, in the browser.',
    },

    nav: {
        site: 'Site',
        accounts: 'Accounts',
        notifications: 'Notifications',
        alerts: 'Alerts',
        signOut: 'Sign out',
        signIn: 'Sign in',
        menu: 'Menu',
        closeMenu: 'Close menu',
        language: 'Language',
        iam: 'IAM',
        refdata: 'Reference data',
        trading: 'Trading',
        marketdata: 'Market data',
        reporting: 'Reporting',
        dataquality: 'Data quality',
        compute: 'Compute',
        workflow: 'Workflow',
        platform: 'Platform',
        collapse: 'Collapse',
        expand: 'Expand',
        home: 'Home',
        search: 'Search',
        searchHint: 'Search entities and actions',
        noResults: 'Nothing matches.',
        allEntities: 'All entities',
        mode: 'Mode',
        areas: 'Areas',
    },

    shell: {
        mode: {
            'system-administration': 'System administration',
            'tenant-administration': 'Tenant administration',
            application: 'Application',
        },
        menu: {
            home: 'Home',
            tenants: 'Tenants',
            parties: 'Parties',
            rescue: 'Rescue access',
            audit: 'Sign-ins',
        },
    },

    tenants: {
        title: 'Tenants',
        description: 'The tenants this deployment holds, and the state each one is in.',
        search: 'Search by code, name or hostname',
        noMatch: 'No tenant matches "{search}".',
        showing: {
            one: 'Showing {first}–{last} of {count} tenant',
            other: 'Showing {first}–{last} of {count} tenants',
        },
        filterType: 'Filter by type',
        filterStatus: 'Filter by status',
        allTypes: 'All types',
        allStatuses: 'All statuses',
        showTest: 'Show test tenants',
        hiddenTest: { one: '{count} test tenant hidden', other: '{count} test tenants hidden' },
        noneFiltered: 'No tenant matches these filters.',
        actions: 'Actions',
        actionsFor: 'Actions for {name}',
        open: 'Open',
        resumeSetup: 'Resume setup',
        add: 'Add tenant',
        notBuilt: 'Not in this release',
        retireOrReset: 'Retire or reset',
        code: 'Code',
        name: 'Name',
        hostname: 'Hostname',
        type: 'Type',
        status: 'Status',
        failed: 'The tenants could not be read.',
        setup: 'Setup',
        setupUnavailable: 'The provisioning runs could not be read, so the setup column is empty.',
        setupState: {
            in_progress: 'Step {step} of {count}',
            compensating: 'Rolling back',
            failed: 'Failed at step {step}',
            compensated: 'Rolled back',
        },
        run: {
            title: 'Setting up {name}',
            unknownTenant: 'Setting up a tenant',
            lead: 'The provisioning run for this tenant. It continues on the server while nobody watches.',
            back: 'Back to tenants',
            done: 'The run completed. The tenant is ready.',
        },
        detail: {
            viewOnly: 'View only',
            viewOnlyHint:
                "Changes to this tenant's parties and people are made by its own administrators.",
            tab: { overview: 'Overview', parties: 'Parties', people: 'People' },
            noParties: 'This tenant has no parties yet.',
            noPeople: 'Nobody can sign in to this tenant yet.',
            topOfGroup: 'Top of the group',
            person: 'Name',
            username: 'Username',
            email: 'Email',
            showingPeople: {
                one: 'Showing {first}–{last} of {count} person',
                other: 'Showing {first}–{last} of {count} people',
            },
            lead: 'One tenant of this deployment: what it is, and how its setup went.',
            back: 'Back to tenants',
            notFound: 'No tenant has this code.',
            failed: 'The tenant could not be read.',
            details: 'Details',
            description: 'Description',
            registrationDefault: 'Registration default',
            yes: 'Yes',
            no: 'No',
            provenance: 'Provenance',
            version: 'Version',
            modifiedBy: 'Changed by',
            performedBy: 'Written by',
            changeReason: 'Reason',
            changeCommentary: 'Commentary',
            recordedAt: 'Recorded',
            setupUnavailable: 'The provisioning runs could not be read.',
            noRun: 'No setup run is on record for this tenant.',
            runCompleted: 'The setup run completed.',
        },
        empty: {
            title: 'This deployment holds no tenant of its own yet.',
            body: 'The system administrator exists, so the deployment is set up. A tenant is the next step, and the first one is created the same way as any other.',
        },
    },

    parties: {
        title: 'Parties',
        description: 'The parties of this tenant, a page at a time.',
        failed: 'The parties could not be read.',
        code: 'Code',
        name: 'Name',
        category: 'Category',
        type: 'Type',
        status: 'Status',
        parent: 'Parent',
        parentElsewhere: 'Not one you can see',
        showing: {
            one: 'Showing {first}–{last} of {count} party',
            other: 'Showing {first}–{last} of {count} parties',
        },
    },

    landing: {
        heading: 'Enterprise-grade risk analytics — but visual and open-source.',
        // Split around the two product links, because the links are markup rather
        // than text and a translator must be able to move them within the sentence.
        introBefore: 'ORE Studio wraps the',
        introBetween: '(ORE) and',
        introAfter:
            'in an intuitive graphical interface — no Python or C++ required — built on a PostgreSQL-native, C++-performance backend.',
        signUp: 'Sign up',
        signIn: 'Sign in',
    },

    password: {
        show: 'Show',
        hide: 'Hide',
        new: 'Password',
        confirm: 'Confirm password',
        mismatch: 'The passwords do not match.',
        ruleMet: 'met',
        ruleNotMet: 'not met',
        strength: { 0: '', 1: 'Weak', 2: 'Fair', 3: 'Good', 4: 'Strong' },
        rule: {
            length: 'At least {min} characters',
            upper: 'An uppercase letter (A-Z)',
            lower: 'A lowercase letter (a-z)',
            digit: 'A digit (0-9)',
            special: 'A special character ({chars})',
        },
    },

    signIn: {
        title: 'Sign in',
        username: 'Username',
        password: 'Password',
        submit: 'Sign in',
        submitting: 'Signing in...',
        noAccount: 'No account?',
        createOne: 'Sign up',
        failed: 'Sign in failed.',
        chooseParty: 'Choose a party',
        choosePartyHint: 'This account works in more than one party.',
        partyCategory: 'Category',
    },

    signUp: {
        title: 'Sign up',
        haveAccount: 'Already have an account?',
        detailsTitle: 'Your details',
        detailsLead: 'The account you will sign in with.',
        principal: 'Username',
        email: 'Email',
        password: 'Password',
        confirmation: 'Confirm password',
        mismatch: 'The two passwords do not match.',
        review: 'Review',
        reviewTitle: 'Review',
        reviewLead: 'What will be created.',
        tenant: 'Tenant',
        party: 'Party',
        role: 'Role',
        noParty: 'Not yet. An administrator adds you to one.',
        usableNow: 'You can sign in as soon as the account exists.',
        pending: 'An administrator must add you to a party before you can sign in.',
        create: 'Create account',
        createFailed: 'The account was not created.',
        createdTitle: 'Account created',
        waitingTitle: 'Account waiting',
        createdLead: 'Your account exists. Sign in with the password you chose.',
        waitingLead: 'Your account exists and waits for an administrator.',
        waitingFor: 'It waits for a party to work in.',
        received: 'It holds the {role} role in {tenant}.',
        goToSignIn: 'Sign in',
        closedTitle: 'Registration is closed',
        closedFallback: 'This deployment does not accept registrations.',
        unavailable: 'The deployment could not say whether it accepts registrations.',
        tryAgain: 'Try again',
    },

    accounts: {
        title: 'Accounts',
        description: 'Identities that can sign in or act as a service, scoped to this tenant.',
        search: 'Search',
        searchPlaceholder: 'Username, name, or email',
        filterByType: 'Type',
        allTypes: 'All types',
        count: '{shown} of {total}',
        refreshing: 'refreshing',
        loading: 'Loading...',
        empty: 'No accounts match the current filter.',
        failed: 'Could not load accounts.',
    },

    account: {
        singular: 'account',
        // Column headers.
        colUsername: 'Username',
        colFullName: 'Full name',
        colEmail: 'Email',
        colType: 'Type',
        colRecorded: 'Recorded',
        // Field labels.
        fldFullName: 'Full name',
        fldEmail: 'Email',
        fldJobTitle: 'Job title',
        fldType: 'Type',
        fldDefaultParty: 'Default party',
        fldReportsTo: 'Reports to',
        fldVersion: 'Version',
        fldModifiedBy: 'Modified by',
        fldPerformedBy: 'Performed by',
        fldRecordedAt: 'Recorded at',
        fldChangeReason: 'Change reason',
        fldCommentary: 'Commentary',
        notRecorded: 'not recorded',
        notSet: 'not set',
        nobody: 'nobody',
        unknown: 'unknown',
        none: 'none',
    },

    entity: {
        add: 'Add',
        edit: 'Edit',
        delete: 'Delete',
        save: 'Save',
        saving: 'Saving...',
        close: 'Close',
        cancel: 'Cancel',
        changedBanner: 'This list has changed since you loaded it.',
        changed: 'This list has changed on the server. Reload to see it.',
        refresh: 'Reload',
        history: 'History',
        search: 'Search',
        filter: 'Filter',
        page: 'Page {page} of {pages}',
        pageSize: 'Page size',
        loadAll: 'Load all',
        emptyFiltered: 'Nothing matches that filter in these {collection}.',
        loadFailed: 'Could not load these {collection}.',
        noRecords: 'No records',
        loading: 'Loading...',
        first: 'First',
        previous: 'Previous',
        next: 'Next',
        last: 'Last',
        new: 'New',
        provenance: 'Provenance',
        asOf: 'As of',
        asOfNow: 'Now',
        general: 'General',
        related: 'Related',
    },

    audit: {
        createTitle: 'New Record Reason',
        amendTitle: 'Change Reason Required',
        deleteTitle: 'Deletion Reason Required',
        createPrompt: 'Please select a reason for creating this record:',
        amendPrompt: 'Please select a reason for this change:',
        deletePrompt: 'Please select a reason for this deletion:',
        reason: 'Reason',
        commentary: 'Commentary',
        commentaryPlaceholder: 'Enter explanation for this change...',
        commentaryRequired: 'Commentary is required for this reason.',
        commentaryOptional: 'Commentary is optional for this reason.',
        noReasons: 'No reasons are available for this operation. An administrator has to add one.',
        nonMaterial: 'no change',
        required: 'Required',
        create: 'Create',
        confirmDelete: 'Confirm Delete',
    },

    confirmation: {
        deleteTitle: 'Delete {singular}',
        deleteBody: "Are you sure you want to delete {singular} '{name}'?",
        unsavedTitle: 'Unsaved Changes',
        unsavedBody: 'You have unsaved changes. Close anyway?',
        yes: 'Yes',
        no: 'No',
    },

    feedback: {
        saved: '{name} saved',
        deleted: '{name} deleted',
        saveFailed: 'Save failed',
        createFailed: 'Create failed',
        deleteFailed: 'Delete failed',
        invalidInput: 'Invalid Input',
        requiredFields: 'Please fill in all required fields.',
        notConnected: 'Not connected to server. Please login.',
        sessionExpired: 'Your session has ended. Sign in again.',
        notFound: 'No record matches that identifier.',
        unreachable: 'Cannot reach the server.',
        retry: 'Retry',
    },

    status: {
        environment: 'Environment',
        connected: 'Connected',
        disconnected: 'Disconnected',
        development: 'development',
        notSignedIn: 'Not signed in',
        copyright: '© 2026 ORE Studio contributors.',
    },

    deployment: {
        title: 'Deployment',
        description:
            'Read-only. Everything here is decided when the process starts, not in the browser.',
        environment: 'This environment',
        name: 'Name',
        identifier: 'Identifier',
        kind: 'Kind',
        production: 'production',
        notProduction: 'not production',
        natsServer: 'NATS server',
        namespace: 'Subject namespace',
        httpServer: 'HTTP server',
        notConfigured: 'not configured',
        configuration: 'Configuration',
        configFile: 'File',
        everyEnvironment: 'Every environment declared',
        serving: 'serving',
        switchHint: 'Switch by restarting with a different --env.',
        unavailable: 'This deployment does not offer the developer surface.',
    },

    home: {
        welcome: 'Welcome, {name}',
        system: {
            lead: {
                one: 'This deployment runs {count} tenant.',
                other: 'This deployment runs {count} tenants.',
            },
            addTenant: 'Add tenant',
            manageTenants: 'Manage tenants',
            health: 'System health',
            inService: 'In service',
            onEvaluation: 'On evaluation',
            settingUp: 'Setting up',
            allClear: 'Everything is running',
            needAttention: {
                one: '{count} tenant needs attention',
                other: '{count} tenants need attention',
            },
            recentActivity: 'Recent activity',
            noActivity: 'No tenant has been set up yet.',
            activityUnavailable: 'The setup history could not be read.',
            attention: 'Needs attention',
            setupFailed: 'Setup stopped at step {step} of {count}.',
            suspended: 'Suspended: nobody in it can sign in.',
            seeFailure: 'See what failed',
            open: 'Open',
            tenants: 'Tenants',
            tenantsLead: 'The organisations this deployment serves, and the state each one is in.',
            showing: {
                one: 'Showing {shown} of {count} tenant',
                other: 'Showing {shown} of {count} tenants',
            },
            seeAll: 'See all',
            noTenants: 'No tenant yet. Add the first one to get started.',
            name: 'Name',
            hostname: 'Sign-in address',
            type: 'Type',
            status: 'Status',
            activity: {
                completed: '{tenant} finished setting up',
                in_progress: '{tenant} is setting up, step {step} of {count}',
                failed: '{tenant}: setup stopped at step {step} of {count}',
                compensating: '{tenant}: setup is being undone',
                compensated: '{tenant}: setup was undone',
            },
        },
        tenant: {
            lead: 'Signed in as {name}',
            parties: 'Parties',
            partiesBody: 'The companies and branches in your group, and how they relate.',
            newParty: 'Add party',
            newPartyBody: 'Add a company or a branch to your group.',
            rescue: 'Lock or unlock an account',
            rescueBody: 'Help someone who cannot sign in, or stop an account from being used.',
            audit: 'Sign-ins',
            auditBody: 'Who is signed in now, and the sign-ins that failed.',
            security: 'Your password and sign-ins',
            securityBody: 'Change your password and see where you are signed in.',
        },
        party: {
            lead: 'Working for {party}',
            note: 'The trading screens are not in this release yet. The areas below show what is coming.',
            comingLater: 'Coming later',
            refdata: 'Reference data',
            refdataBody: 'Currencies, calendars, conventions and the parties you trade with.',
            marketdata: 'Market data',
            marketdataBody: 'Curves, surfaces and fixings, and where they came from.',
            trading: 'Trading',
            tradingBody: 'Book, amend and review trades.',
            reporting: 'Reporting',
            reportingBody: 'Risk and P&L reports for your books.',
            security: 'Your password and sign-ins',
            securityBody: 'Change your password and see where you are signed in.',
        },
    },

    component: {
        entities: 'Entities',
        shortcuts: 'Common tasks',
        noEntities: 'No entities are declared for this component yet.',
    },

    shortcut: {
        accounts: {
            title: 'Accounts',
            description: 'Add, amend and lock the accounts that can sign in.',
        },
        orgChart: { title: 'Org chart', description: 'See who reports to whom.' },
        onboardTenant: {
            title: 'Onboard tenant',
            description: 'Provision a new tenant and its first party.',
        },
        parties: {
            title: 'Parties',
            description: 'The organisations you trade with and belong to.',
        },
        currencies: {
            title: 'Currencies',
            description: 'Currency codes, rounding and market tiers.',
        },
        books: { title: 'Books', description: 'The books trades are recorded against.' },
        trades: { title: 'Trades', description: 'Captured trades and their lifecycle events.' },
        portfolios: { title: 'Portfolios', description: 'Hierarchies of books and positions.' },
        marketSeries: {
            title: 'Market series',
            description: 'Time series of market observations.',
        },
        fixings: { title: 'Fixings', description: 'Published fixings and their details.' },
        reportDefinitions: {
            title: 'Report definitions',
            description: 'What can be run, and with what parameters.',
        },
        reportInstances: {
            title: 'Report instances',
            description: 'Reports that have been generated.',
        },
        catalog: { title: 'Catalog', description: 'Every dataset and where it came from.' },
        codingSchemes: {
            title: 'Coding schemes',
            description: 'Controlled vocabularies and their codes.',
        },
        computeDashboard: {
            title: 'Dashboard',
            description: 'What is running and what is queued.',
        },
        queues: { title: 'Queues', description: 'Work waiting to be picked up.' },
        workflowDefinitions: {
            title: 'Definitions',
            description: 'The workflows that can be started.',
        },
        scheduler: { title: 'Scheduler', description: 'Jobs and when they next run.' },
    },

    card: {
        planned: 'Planned',
        notBuilt: 'Not built yet',
    },

    breadcrumb: {
        home: 'Home',
    },

    country: {
        title: 'Countries',
        singular: 'country',
        newTitle: 'New country',
        invalidAlpha2: 'An alpha-2 code is two letters.',
        invalidAlpha3: 'An alpha-3 code is three letters.',
        invalidNumeric: 'An ISO numeric code is three digits.',
        description: 'Countries, their ISO codes and their official names.',
        colAlpha2Code: 'Alpha-2 code',
        colAlpha3Code: 'Alpha-3 code',
        colNumericCode: 'Numeric code',
        colName: 'Name',
        colOfficialName: 'Official name',
        colVersion: 'Version',
        colModifiedBy: 'Modified by',
        colRecordedAt: 'Recorded at',
        searchPlaceholder: 'Alpha-2, alpha-3, numeric code or name',
        fldAlpha2Code: 'Alpha-2 code',
        fldAlpha3Code: 'Alpha-3 code',
        fldNumericCode: 'Numeric code',
        fldName: 'Name',
        fldOfficialName: 'Official name',
        alpha2CodePh: 'Enter country alpha2 code',
        alpha3CodePh: 'Enter country alpha3 code',
        numericCodePh: 'Enter ISO numeric code',
        namePh: 'Enter display name',
        officialNamePh: 'Enter official country name',
    },

    tenant_type: {
        title: 'Tenant types',
        singular: 'tenant type',
        newTitle: 'New tenant type',
        description: 'Tenant type classifications and the order they are shown in.',
        colType: 'Type',
        colName: 'Name',
        colDescription: 'Description',
        colDisplayOrder: 'Display order',
        colVersion: 'Version',
        colModifiedBy: 'Modified by',
        colRecordedAt: 'Recorded at',
        searchPlaceholder: 'Type, name or description',
        fldType: 'Type',
        fldName: 'Name',
        fldDescription: 'Description',
        typePh: 'Enter the tenant type code',
        namePh: 'Enter the display name',
        descriptionPh: 'Enter the description',
    },

    table: {
        chooseColumns: 'Choose columns',
        rowActions: 'Actions',
        searchedSoFar: 'Searched the first {shown} of {total}',
    },
    history: {
        description: 'Every version of this record, newest first.',
        newer: 'Newer',
        older: 'Older',
        revert: 'Revert',
        revertBody:
            "Are you sure you want to revert '{name}' from version {from} back to version {to}? This will create a new version with the data from version {to}.",
        from: 'From',
        to: 'To',
        outOfOrder: 'The From version must be older than the To version.',
        timeline: 'Timeline',
        current: 'current',
        empty: 'This record has no history.',
        initial: 'Initial version',
        comparing: 'Comparing v{from} with v{to}',
        allFields: 'All fields',
        onlyChanges: 'Only changes',
        field: 'Field',
        before: 'Before',
        after: 'After',
        noChanges: 'No field changes between these versions.',
        openVersion: 'Open this version',
    },

    validation: {
        required: 'This field is required.',
    },

    image: {
        choose: 'Choose image',
        change: 'Change image',
        remove: 'Remove',
        pick: 'Choose an image',
        search: 'Search images',
        none: 'No image',
        noneAvailable: 'No images are available in this tenant.',
    },

    gate: {
        unreachable:
            'The server did not answer, so the interface cannot tell whether this installation still needs setting up. {reason}',
        retry: 'Try again',
    },

    journey: {
        steps: 'Steps',
        actionFailed: 'The step failed: {message}',
        policyFailed:
            'The password rules could not be read, so no password can be set here. {message}',

        welcome: {
            title: 'Welcome to ORE Studio',
            lead: "Set up a new installation: the administrator that owns it, the first tenant, and the tenant administrator's first sign-in.",
            start: 'Get started',
            stage: {
                admin: 'Create the administrator',
                adminBody: 'The account that owns this installation.',
                tenant: 'Create the first tenant',
                tenantBody: 'The first tenant, built from a starting point on the server.',
                signIn: 'Sign in',
                signInBody:
                    "The tenant administrator's first sign-in, with a password of their own.",
            },
        },

        admin: {
            title: 'Create the administrator',
            lead: 'This installation has no administrator, and nobody can sign in yet.',
            bootstrap:
                'The administrator owns the installation. The deployment stops being in bootstrap mode when this account exists.',
            username: 'Administrator username',
            email: 'Administrator email',
            password: 'Administrator password',
            create: 'Create administrator',
            resumeTitle: 'Sign in as the administrator',
            resumeLead:
                'This installation already has its administrator. Sign in as that account to carry on. The password is also what the tenant takes when its profile shares yours.',
            signIn: 'Sign in and continue',
            partyChoice:
                'The administrator works in more than one party. Sign in and choose one first.',
        },

        profile: {
            title: 'Choose a starting point',
            lead: 'A starting point is a profile the server holds. It states the settings, the steps and the tenant it creates.',
            counts: '{settings} settings · {steps} steps',
            installed: 'Installed',
        },

        details: {
            title: 'Describe the tenant',
            lead: 'Name the tenant and create its administrator.',
            tenant: 'Tenant',
            name: 'Name',
            code: 'Code',
            codeHint:
                'Lowercase letters, digits and underscores, starting with a letter. It names the tenant in a username.',
            hostname: 'Hostname',
            settings: '{profile} settings',
            administrator: 'Tenant administrator',
            username: 'Username',
            email: 'Email',
            useMyPassword: 'Use my password',
            adminPassword: 'Administrator password',
            passwordForced: 'They must change it at first sign-in.',
            standard: '{profile} uses its standard settings.',
            leiSearch: 'Search by name or LEI',
            leiHowItWorks:
                'Choosing an entity fills in the tenant below, and every field stays editable.',
            leiChange: 'Change',
            leiKeep: 'Keep',
            leiParties: {
                one: '1 party in its hierarchy',
                other: '{count} parties in its hierarchy',
            },
            leiNoMatch: 'No entity matches that.',
            leiReadFailed: 'The legal entities could not be read. {message}',
            changeSettings: 'Change settings',
            noCreatingPassword:
                'The administrator who creates the tenant must set a password first.',
            rows: {
                tenant: 'Tenant',
                hostname: 'Hostname',
                administrator: 'Administrator',
                password: 'Password',
            },
            passwordMine: 'Same as mine',
            passwordTyped: 'Set here',
        },

        review: {
            title: 'Review',
            lead: 'Nothing is created until you confirm.',
            startingPoint: 'Starting point',
            tenant: 'Tenant',
            hostname: 'Hostname',
            administrator: 'Administrator',
            password: 'Password',
            passwordMine: 'The same as yours',
            passwordSet: 'The one you typed',
            steps: 'Creating the tenant runs {steps} steps.',
            forcedChange: '{principal} sets a password of their own at first sign-in.',
            noPassword: 'The tenant administrator has no password yet.',
            create: 'Create tenant',
            noRun: 'The server created the tenant but named no run to follow.',
        },

        provisioning: {
            title: 'Provisioning',
            lead: 'You can leave this page and come back.',
            readFailed: 'The progress could not be read. {message}',
            retry: 'Retry from the failed step',
            retryKeeps: 'The steps that completed are kept.',
            retrying: 'The run resumed at {step}.',
            rolledBack:
                'The run rolled back the steps it had completed, so it left nothing behind.',
        },

        handOff: {
            title: 'Hand off',
            lead: 'The tenant is ready. Its administrator signs in next.',
            administrator: 'Its administrator is {principal}.',
            continue: 'Continue as tenant admin',
            continueHint: 'Sign in as {principal} now.',
            elsewhere: 'Hand off to someone else',
            elsewhereHint: 'Sign out and pass the username and the password on.',
            elsewhereHintForced:
                'Sign out and pass the username on. They set their own password at first sign-in.',
            partyChoice:
                'The tenant administrator works in more than one party, so the one to open cannot be chosen here. Sign in as them and choose one.',
        },

        signIn: {
            title: 'First sign-in',
            lead: 'The tenant administrator signs in for the first time.',
            username: 'Username',
            password: 'Password',
            submit: 'Sign in',
            submitting: 'Signing in...',
            choosePartyHint: 'Choose the party to work in.',
            done: 'Signed in as {principal}.',
        },

        change: {
            required: 'This account sets a password of its own before it goes on.',
            new: 'New password',
            submit: 'Set password',
            submitting: 'Setting...',
            done: 'Your password is set, and you are signed in as {principal}.',
        },

        ready: {
            title: 'Ready',
            lead: 'The installation is set up, and {principal} is signed in.',
            home: 'Go home',
        },

        party: {
            noLei: 'No LEI',
            find: {
                title: 'Find the legal entity',
                lead: 'The party is a legal entity. Search the ones this deployment holds, or name one it does not.',
                search: 'A legal entity we hold',
                searchHint: 'Search by name or LEI.',
                byHand: 'It is not one of them',
                byHandHint: 'Name a party the deployment holds no legal entity for.',
                name: 'Legal name',
                nameHint: 'The name the party is registered under.',
                lei: 'Legal entity',
                leiHint: 'Choosing an entity names the party after it.',
            },
            describe: {
                title: 'Describe the party',
                lead: 'Give it the short code this tenant will know it by.',
                shortCode: 'Short code',
                shortCodeHint:
                    'A mnemonic unique to this tenant, proposed from the legal name. It identifies the party in commands and reports.',
            },
            review: {
                title: 'Review',
                lead: 'Nothing is created until you confirm.',
                legalName: 'Legal name',
                lei: 'LEI',
                shortCode: 'Short code',
                data: "The party's data is the tenant's standard data, which the run publishes.",
                bornInactive:
                    'The party is created inactive. The run publishes its data, activates it and joins you to it.',
                create: 'Add party',
                noRun: 'The server created the party but named no run to follow.',
            },
            provisioning: {
                title: 'Provisioning',
                lead: 'This runs on the server. You can leave this page and come back.',
            },
            next: {
                title: 'Next steps',
                lead: '{code} is active.',
                work: 'Work in it now',
                workHint: 'Work as this party for the rest of this session.',
                another: 'Add another party',
                anotherHint: 'Add another one the same way.',
                done: 'Done',
                doneHint: 'Go back to where you started.',
                joined: 'You are joined to the party, so it is one of the ones you can work in.',
            },
        },
    },

    version: {
        client: 'client {version}',
        server: 'server {version}',
        serverUnknown: 'server version unknown',
    },

    server: {
        'Publish the reference data': 'Publish the reference data',
        'Publishes the reference data the tenant works from: the bundles the starting point orders, and the datasets those bundles name.':
            'Publishes the reference data the tenant works from: the bundles the starting point orders, and the datasets those bundles name.',
        'Import the legal entities': 'Import the legal entities',
        "Reads the legal entity the starting point names by its LEI and the entities it consolidates, and publishes them as the tenant's parties.":
            "Reads the legal entity the starting point names by its LEI and the entities it consolidates, and publishes them as the tenant's parties.",
        'Provision the parties': 'Provision the parties',
        "Publishes each party's reference data, records the legal entity it was built from, activates it, marks its onboarding complete and joins the caller to it.":
            "Publishes each party's reference data, records the legal entity it was built from, activates it, marks its onboarding complete and joins the caller to it.",
        'Load the staff': 'Load the staff',
        'Creates an account for each person the starting point lists, each in its own party.':
            'Creates an account for each person the starting point lists, each in its own party.',
        'Attach the photographs': 'Attach the photographs',
        "Gives the accounts their photographs, and the tenant's party its logo.":
            "Gives the accounts their photographs, and the tenant's party its logo.",
        'Start the market feeds': 'Start the market feeds',
        "Starts the synthetic market data the tenant's curves and prices are built from.":
            "Starts the synthetic market data the tenant's curves and prices are built from.",
        Finish: 'Finish',
        'Marks the tenant ready: it stops bootstrapping and becomes active.':
            'Marks the tenant ready: it stops bootstrapping and becomes active.',
    },

    common: {
        loading: 'Loading...',
        all: 'All',
        close: 'Close',
        open: 'Open',
        revert: 'Revert',
        apply: 'Apply',
        back: 'Back',
        continue: 'Continue',
    },
};

/** The English catalogue, flattened to dot paths. */
export const enFlat = flatten(en);
