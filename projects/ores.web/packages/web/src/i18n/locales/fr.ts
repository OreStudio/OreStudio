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

import { catalogueSchema, flatten, type SourceCatalogue } from '../translate.js';
import { en as source } from './en.js';

/** French. Typed against the English catalogue, so a stray key is a compile error. */
const fr: SourceCatalogue = {
  app: {
    name: 'ORE Studio',
    tagline: 'Analyse de risque de niveau entreprise, dans le navigateur.',
  },

  nav: {
    site: 'Site',
    accounts: 'Comptes',
    notifications: 'Notifications',
    alerts: 'Alertes',
    signOut: 'Se déconnecter',
    signIn: 'Se connecter',
    menu: 'Menu',
    closeMenu: 'Fermer le menu',
    language: 'Langue',
     iam: 'IAM',
    refdata: 'Données de référence',
    trading: 'Négociation',
    marketdata: 'Données de marché',
    reporting: 'Rapports',
    dataquality: 'Qualité des données',
    compute: 'Calcul',
    workflow: 'Flux de travail',
    platform: 'Plateforme',
    collapse: 'Réduire',
    expand: 'Développer',
    home: 'Accueil',
    search: 'Rechercher',
    searchHint: 'Rechercher entités et actions',
    noResults: 'Aucun résultat.',
    allEntities: 'Toutes les entités',
  },

  landing: {
    heading: 'Analyse de risque de niveau entreprise — mais visuelle et open-source.',
    introBefore: 'ORE Studio enveloppe',
    introBetween: '(ORE) et',
    introAfter:
      "dans une interface graphique intuitive — sans Python ni C++ — sur un backend natif PostgreSQL aux performances C++.",
    signUp: "S'inscrire",
    signIn: 'Se connecter',
  },

  signIn: {
    title: 'Se connecter',
    username: "Nom d'utilisateur",
    password: 'Mot de passe',
    show: 'Afficher',
    hide: 'Masquer',
    submit: 'Se connecter',
    submitting: 'Connexion...',
    noAccount: 'Pas de compte ?',
    createOne: "S'inscrire",
    failed: 'Échec de la connexion.',
    chooseParty: 'Choisir une entité',
    choosePartyHint: 'Ce compte fonctionne dans plusieurs entités.',
    partyCategory: 'Catégorie',
  },

  signUp: {
    title: "S'inscrire",
    notAvailable:
      "Les comptes sont créés par un administrateur ou via l'assistant de provisioning du client de bureau. L'inscription en libre-service n'est pas encore disponible.",
    haveAccount: 'Vous avez déjà un compte ?',
  },

  accounts: {
    title: 'Comptes',
    description: 'Identités pouvant se connecter ou agir en tant que service, dans ce locataire.',
    search: 'Rechercher',
    searchPlaceholder: "Nom d'utilisateur, nom ou email",
    filterByType: 'Type',
    allTypes: 'Tous les types',
    count: '{shown} sur {total}',
    refreshing: 'actualisation',
    loading: 'Chargement...',
    empty: 'Aucun compte ne correspond au filtre actuel.',
    failed: 'Impossible de charger les comptes.',
  },

  account: {
    singular: 'compte',
    colUsername: "Nom d'utilisateur",
    colFullName: 'Nom complet',
    colEmail: 'Email',
    colType: 'Type',
    colRecorded: 'Enregistré',
    fldFullName: 'Nom complet',
    fldEmail: 'Email',
    fldJobTitle: 'Intitulé du poste',
    fldType: 'Type',
    fldDefaultParty: 'Entité par défaut',
    fldReportsTo: 'Rend compte à',
    fldVersion: 'Version',
    fldModifiedBy: 'Modifié par',
    fldPerformedBy: 'Exécuté par',
    fldRecordedAt: 'Enregistré le',
    fldChangeReason: 'Motif du changement',
    fldCommentary: 'Commentaire',
    notRecorded: 'non enregistré',
    notSet: 'non défini',
    nobody: 'personne',
    unknown: 'inconnu',
    none: 'aucun',
  },

  entity: {
    add: 'Ajouter',
    edit: 'Modifier',
    delete: 'Supprimer',
    save: 'Enregistrer',
    saving: 'Enregistrement...',
    close: 'Fermer',
    cancel: 'Annuler',
    changedBanner: 'Cette liste a changé depuis votre chargement.',
    changed: 'Cette liste a changé sur le serveur. Rechargez pour la voir.',
    refresh: 'Recharger',
    history: 'Historique',
    search: 'Rechercher',
    filter: 'Filtrer',
    page: 'Page {page} sur {pages}',
    pageSize: 'Éléments par page',
    loadAll: 'Tout charger',
    emptyFiltered: 'Rien ne correspond à ce filtre dans ces {collection}.',
    loadFailed: 'Impossible de charger ces {collection}.',
    noRecords: 'Aucun enregistrement',
    loading: 'Chargement...',
    first: 'Première',
    previous: 'Précédente',
    next: 'Suivante',
    last: 'Dernière',
    new: 'Nouveau',
    provenance: 'Provenance',
    general: 'Général',
    related: 'Associé',
  },

  audit: {
    createTitle: 'Motif du nouvel enregistrement',
    amendTitle: 'Motif du changement requis',
    deleteTitle: 'Motif de la suppression requis',
    createPrompt: 'Veuillez choisir un motif pour créer cet enregistrement :',
    amendPrompt: 'Veuillez choisir un motif pour ce changement :',
    deletePrompt: 'Veuillez choisir un motif pour cette suppression :',
    reason: 'Motif',
    commentary: 'Commentaire',
    commentaryPlaceholder: 'Saisissez une explication pour ce changement...',
    commentaryRequired: 'Le commentaire est obligatoire pour ce motif.',
    commentaryOptional: 'Le commentaire est facultatif pour ce motif.',
    noReasons: "Aucun motif n'est disponible pour cette opération. Un administrateur doit en ajouter un.",
    nonMaterial: 'sans changement',
    required: 'Obligatoire',
    create: 'Créer',
    confirmDelete: 'Confirmer la suppression',
  },

  confirmation: {
    deleteTitle: 'Supprimer {singular}',
    deleteBody: "Voulez-vous vraiment supprimer {singular} '{name}' ?",
    unsavedTitle: 'Modifications non enregistrées',
    unsavedBody: 'Vous avez des modifications non enregistrées. Fermer quand même ?',
    yes: 'Oui',
    no: 'Non',
  },

  feedback: {
    saved: '{name} enregistré',
    deleted: '{name} supprimé',
    saveFailed: "Échec de l'enregistrement",
    createFailed: 'Échec de la création',
    deleteFailed: 'Échec de la suppression',
    invalidInput: 'Saisie invalide',
    requiredFields: 'Veuillez remplir tous les champs obligatoires.',
    notConnected: 'Non connecté au serveur. Veuillez vous connecter.',
    sessionExpired: 'Votre session a pris fin. Connectez-vous à nouveau.',
    notFound: 'Aucun enregistrement ne correspond à cet identifiant.',
    unreachable: 'Impossible de joindre le serveur.',
    retry: 'Réessayer',
  },

  status: {
    environment: 'Environnement',
    connected: 'Connecté',
    disconnected: 'Déconnecté',
    development: 'développement',
    notSignedIn: 'Non connecté',
    copyright: '© 2026 Contributeurs ORE Studio.',
  },

  deployment: {
    title: 'Déploiement',
    description:
      "Lecture seule. Tout ici est décidé au démarrage du processus, pas dans le navigateur.",
    environment: 'Cet environnement',
    name: 'Nom',
    identifier: 'Identifiant',
    kind: 'Type',
    production: 'production',
    notProduction: 'pas en production',
    natsServer: 'Serveur NATS',
    namespace: "Espace de noms d'objet",
    httpServer: 'Serveur HTTP',
    notConfigured: 'non configuré',
    configuration: 'Configuration',
    configFile: 'Fichier',
    everyEnvironment: 'Tous les environnements déclarés',
    serving: 'en service',
    switchHint: 'Changez en redémarrant avec un --env différent.',
    unavailable: "Ce déploiement n'expose pas l'espace développeur.",
  },

  home: {
    greeting: 'Connecté en tant que {name}',
    quickActions: 'Actions rapides',
    components: 'Composants',
    entities: 'entités',
    planned: 'prévues',
    whatIs: 'Ce qui se trouve ici',
  },

  component: {
    entities: 'Entités',
    shortcuts: 'Tâches courantes',
    noEntities: "Aucune entité n'est encore déclarée pour ce composant.",
  },

  shortcut: {
    accounts: { title: 'Comptes', description: 'Ajoutez, modifiez et verrouillez les comptes pouvant se connecter.' },
    orgChart: { title: 'Organigramme', description: 'Voir qui rend compte à qui.' },
    onboardTenant: { title: 'Provisionner un locataire', description: 'Créer un nouveau locataire et sa première entité.' },
    parties: { title: 'Entités', description: 'Les organisations avec lesquelles vous traitez.' },
    currencies: { title: 'Devises', description: 'Codes de devise, arrondi et paliers de marché.' },
    books: { title: 'Livres', description: 'Les livres sur lesquels les transactions sont enregistrées.' },
    trades: { title: 'Transactions', description: 'Transactions saisies et leurs événements de cycle de vie.' },
    portfolios: { title: 'Portefeuilles', description: 'Hiérarchies de livres et de positions.' },
    marketSeries: { title: 'Séries de marché', description: 'Séries temporelles d’observations de marché.' },
    fixings: { title: 'Fixings', description: 'Fixings publiés et leurs détails.' },
    reportDefinitions: { title: 'Définitions de rapport', description: 'Ce qui peut être exécuté, et avec quels paramètres.' },
    reportInstances: { title: 'Instances de rapport', description: 'Rapports déjà générés.' },
    catalog: { title: 'Catalogue', description: 'Chaque jeu de données et son origine.' },
    codingSchemes: { title: 'Schémas de codage', description: 'Vocabulaires contrôlés et leurs codes.' },
    computeDashboard: { title: 'Tableau de bord', description: 'Ce qui tourne et ce qui est en file.' },
    queues: { title: 'Files', description: 'Travail en attente de prise en charge.' },
    workflowDefinitions: { title: 'Définitions', description: 'Les flux de travail pouvant être lancés.' },
    scheduler: { title: 'Planificateur', description: 'Tâches et leur prochaine exécution.' },
  },

  card: {
    planned: 'Prévu',
    notBuilt: 'Pas encore construit',
  },

  breadcrumb: {
    home: 'Accueil',
  },

  country: {
    title: 'Pays',
    singular: 'pays',
    newTitle: 'Nouveau pays',
    invalidAlpha2: 'Un code alpha-2 comporte deux lettres.',
    invalidAlpha3: 'Un code alpha-3 comporte trois lettres.',
    invalidNumeric: 'Un code numérique ISO comporte trois chiffres.',
    description: 'Pays, leurs codes ISO et leurs noms officiels.',
    colAlpha2Code: 'Code alpha-2',
    colAlpha3Code: 'Code alpha-3',
    colNumericCode: 'Code numérique',
    colName: 'Nom',
    colOfficialName: 'Nom officiel',
    colVersion: 'Version',
    colModifiedBy: 'Modifié par',
    colRecordedAt: 'Enregistré le',
    searchPlaceholder: 'Code alpha-2, alpha-3, numérique ou nom',
    fldAlpha2Code: 'Code alpha-2',
    fldAlpha3Code: 'Code alpha-3',
    fldNumericCode: 'Code numérique',
    fldName: 'Nom',
    fldOfficialName: 'Nom officiel',
    alpha2CodePh: 'Saisissez le code alpha-2 du pays',
    alpha3CodePh: 'Saisissez le code alpha-3 du pays',
    numericCodePh: 'Saisissez le code numérique ISO',
    namePh: 'Saisissez le nom affiché',
    officialNamePh: 'Saisissez le nom officiel du pays',
  },

  tenant_type: {
    title: 'Types de locataire',
    singular: 'type de locataire',
    newTitle: 'Nouveau type de locataire',
    description: 'Classifications des types de locataire et leur ordre d’affichage.',
    colType: 'Type',
    colName: 'Nom',
    colDescription: 'Description',
    colDisplayOrder: 'Ordre d’affichage',
    colVersion: 'Version',
    colModifiedBy: 'Modifié par',
    colRecordedAt: 'Enregistré le',
    searchPlaceholder: 'Type, nom ou description',
    fldType: 'Type',
    fldName: 'Nom',
    fldDescription: 'Description',
    typePh: 'Saisissez le code du type de locataire',
    namePh: 'Saisissez le nom affiché',
    descriptionPh: 'Saisissez la description',
  },

  table: {
    chooseColumns: 'Choisir les colonnes',
    rowActions: 'Actions',
    searchedSoFar: 'Recherche sur les {shown} premiers sur {total}',
  },
  history: {
    description: 'Toutes les versions de cet enregistrement, la plus récente en premier.',
    newer: 'Plus récente',
    older: 'Plus ancienne',
    revert: 'Rétablir',
    revertBody: "Voulez-vous vraiment rétablir '{name}' de la version {from} à la version {to} ? Cela créera une nouvelle version avec les données de la version {to}.",
    from: 'De',
    to: 'Vers',
    outOfOrder: 'La version De doit être plus ancienne que la version Vers.',
    timeline: 'Chronologie',
    current: 'actuelle',
    empty: "Cet enregistrement n'a pas d'historique.",
    initial: 'Version initiale',
    comparing: 'Comparaison de v{from} avec v{to}',
    allFields: 'Tous les champs',
    onlyChanges: 'Changements seulement',
    field: 'Champ',
    before: 'Avant',
    after: 'Après',
    noChanges: 'Aucun changement de champ entre ces versions.',
    openVersion: 'Ouvrir cette version',
  },

  validation: {
    required: 'Ce champ est obligatoire.',
  },

  image: {
    choose: 'Choisir une image',
    change: "Changer d'image",
    remove: 'Retirer',
    pick: 'Choisir une image',
    search: 'Rechercher des images',
    none: 'Aucune image',
    noneAvailable: 'Aucune image n’est disponible dans ce locataire.',
  },

  common: {
    loading: 'Chargement...',
    all: 'Tous',
    close: 'Fermer',
    open: 'Ouvrir',
    revert: 'Rétablir',
    apply: 'Appliquer',
    back: 'Retour',
  },
};

catalogueSchema(flatten(source)).parse(flatten(fr));
export { fr };

/** The catalogue, flattened to dot paths. */
export const frFlat = flatten(fr);
