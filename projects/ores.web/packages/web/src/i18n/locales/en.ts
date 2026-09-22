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
    notAvailable:
      'Accounts are created by an administrator, or through the provisioning wizard in the desktop client. Self-service sign-up is not available yet.',
    haveAccount: 'Already have an account?',
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
    saved: "{name} saved",
    deleted: "{name} deleted",
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
    description: 'Read-only. Everything here is decided when the process starts, not in the browser.',
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
    greeting: 'Signed in as {name}',
    quickActions: 'Quick actions',
    components: 'Components',
    entities: 'entities',
    planned: 'planned',
    whatIs: 'What is here',
  },

  component: {
    entities: 'Entities',
    shortcuts: 'Common tasks',
    noEntities: 'No entities are declared for this component yet.',
  },

  shortcut: {
    accounts: { title: 'Accounts', description: 'Add, amend and lock the accounts that can sign in.' },
    orgChart: { title: 'Org chart', description: 'See who reports to whom.' },
    onboardTenant: { title: 'Onboard tenant', description: 'Provision a new tenant and its first party.' },
    parties: { title: 'Parties', description: 'The organisations you trade with and belong to.' },
    currencies: { title: 'Currencies', description: 'Currency codes, rounding and market tiers.' },
    books: { title: 'Books', description: 'The books trades are recorded against.' },
    trades: { title: 'Trades', description: 'Captured trades and their lifecycle events.' },
    portfolios: { title: 'Portfolios', description: 'Hierarchies of books and positions.' },
    marketSeries: { title: 'Market series', description: 'Time series of market observations.' },
    fixings: { title: 'Fixings', description: 'Published fixings and their details.' },
    reportDefinitions: { title: 'Report definitions', description: 'What can be run, and with what parameters.' },
    reportInstances: { title: 'Report instances', description: 'Reports that have been generated.' },
    catalog: { title: 'Catalog', description: 'Every dataset and where it came from.' },
    codingSchemes: { title: 'Coding schemes', description: 'Controlled vocabularies and their codes.' },
    computeDashboard: { title: 'Dashboard', description: 'What is running and what is queued.' },
    queues: { title: 'Queues', description: 'Work waiting to be picked up.' },
    workflowDefinitions: { title: 'Definitions', description: 'The workflows that can be started.' },
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
    revertBody: "Are you sure you want to revert '{name}' from version {from} back to version {to}? This will create a new version with the data from version {to}.",
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

  common: {
    loading: 'Loading...',
    all: 'All',
    close: 'Close',
    open: 'Open',
    revert: 'Revert',
    apply: 'Apply',
    back: 'Back',
  },
};

/** The English catalogue, flattened to dot paths. */
export const enFlat = flatten(en);
