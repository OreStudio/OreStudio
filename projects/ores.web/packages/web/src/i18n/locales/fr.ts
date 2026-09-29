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
            'dans une interface graphique intuitive — sans Python ni C++ — sur un backend natif PostgreSQL aux performances C++.',
        signUp: "S'inscrire",
        signIn: 'Se connecter',
    },

    password: {
        show: 'Afficher',
        hide: 'Masquer',
        new: 'Mot de passe',
        confirm: 'Confirmer le mot de passe',
        mismatch: 'Les mots de passe ne correspondent pas.',
        ruleMet: 'respectée',
        ruleNotMet: 'non respectée',
        strength: { 0: '', 1: 'Faible', 2: 'Moyen', 3: 'Bon', 4: 'Fort' },
        rule: {
            length: 'Au moins {min} caractères',
            upper: 'Une majuscule (A-Z)',
            lower: 'Une minuscule (a-z)',
            digit: 'Un chiffre (0-9)',
            special: 'Un caractère spécial ({chars})',
        },
    },

    signIn: {
        title: 'Se connecter',
        username: "Nom d'utilisateur",
        password: 'Mot de passe',
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
        description:
            'Identités pouvant se connecter ou agir en tant que service, dans ce locataire.',
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
        asOf: 'À la date du',
        asOfNow: 'Maintenant',
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
        noReasons:
            "Aucun motif n'est disponible pour cette opération. Un administrateur doit en ajouter un.",
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
            'Lecture seule. Tout ici est décidé au démarrage du processus, pas dans le navigateur.',
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
        title: 'Connecté',
        next: 'Les parcours sont les écrans qui remplissent cette coque. Ils arrivent un groupe à la fois.',
        username: 'Compte',
        email: 'Courriel',
        tenant: 'Locataire',
        party: 'Partie',
    },

    component: {
        entities: 'Entités',
        shortcuts: 'Tâches courantes',
        noEntities: "Aucune entité n'est encore déclarée pour ce composant.",
    },

    shortcut: {
        accounts: {
            title: 'Comptes',
            description: 'Ajoutez, modifiez et verrouillez les comptes pouvant se connecter.',
        },
        orgChart: { title: 'Organigramme', description: 'Voir qui rend compte à qui.' },
        onboardTenant: {
            title: 'Provisionner un locataire',
            description: 'Créer un nouveau locataire et sa première entité.',
        },
        parties: {
            title: 'Entités',
            description: 'Les organisations avec lesquelles vous traitez.',
        },
        currencies: {
            title: 'Devises',
            description: 'Codes de devise, arrondi et paliers de marché.',
        },
        books: {
            title: 'Livres',
            description: 'Les livres sur lesquels les transactions sont enregistrées.',
        },
        trades: {
            title: 'Transactions',
            description: 'Transactions saisies et leurs événements de cycle de vie.',
        },
        portfolios: {
            title: 'Portefeuilles',
            description: 'Hiérarchies de livres et de positions.',
        },
        marketSeries: {
            title: 'Séries de marché',
            description: 'Séries temporelles d’observations de marché.',
        },
        fixings: { title: 'Fixings', description: 'Fixings publiés et leurs détails.' },
        reportDefinitions: {
            title: 'Définitions de rapport',
            description: 'Ce qui peut être exécuté, et avec quels paramètres.',
        },
        reportInstances: { title: 'Instances de rapport', description: 'Rapports déjà générés.' },
        catalog: { title: 'Catalogue', description: 'Chaque jeu de données et son origine.' },
        codingSchemes: {
            title: 'Schémas de codage',
            description: 'Vocabulaires contrôlés et leurs codes.',
        },
        computeDashboard: {
            title: 'Tableau de bord',
            description: 'Ce qui tourne et ce qui est en file.',
        },
        queues: { title: 'Files', description: 'Travail en attente de prise en charge.' },
        workflowDefinitions: {
            title: 'Définitions',
            description: 'Les flux de travail pouvant être lancés.',
        },
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
        revertBody:
            "Voulez-vous vraiment rétablir '{name}' de la version {from} à la version {to} ? Cela créera une nouvelle version avec les données de la version {to}.",
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

    gate: {
        unreachable:
            "Le serveur n'a pas répondu : l'interface ne peut pas savoir si cette installation doit encore être configurée. {reason}",
        retry: 'Réessayer',
    },

    journey: {
        steps: 'Étapes du parcours',
        actionFailed: "L'étape a échoué : {message}",
        policyFailed:
            "Les règles de mot de passe n'ont pas pu être lues, aucun mot de passe ne peut donc être défini ici. {message}",

        welcome: {
            title: 'Bienvenue dans ORE Studio',
            lead: 'Cette installation est vide. Configurez-la en trois étapes. La liste de gauche nomme chaque étape.',
            start: 'Commencer',
            stage: {
                admin: "Créer l'administrateur",
                adminBody: 'Le compte propriétaire de cette installation.',
                tenant: 'Créer le premier locataire',
                tenantBody:
                    'Le premier locataire, construit sur le serveur à partir d’un point de départ.',
                signIn: 'Se connecter',
                signInBody:
                    "La première connexion de l'administrateur du locataire, avec son propre mot de passe.",
            },
        },

        admin: {
            title: "Créer l'administrateur",
            lead: "Cette installation n'a aucun administrateur et personne ne peut encore se connecter.",
            bootstrap:
                "L'administrateur est propriétaire de l'installation. Le déploiement quitte le mode amorçage dès que ce compte existe.",
            username: "Nom d'utilisateur de l'administrateur",
            email: "Courriel de l'administrateur",
            password: "Mot de passe de l'administrateur",
            create: "Créer l'administrateur",
            resumeTitle: "Se connecter en tant qu'administrateur",
            resumeLead:
                "Cette installation a déjà son administrateur. Connectez-vous avec ce compte pour continuer. Le mot de passe est aussi celui que l'entité reprend lorsque son profil partage le vôtre.",
            signIn: 'Se connecter et continuer',
            partyChoice:
                "L'administrateur travaille dans plusieurs entités. Connectez-vous et choisissez-en une d'abord.",
        },

        profile: {
            title: 'Choisir un point de départ',
            lead: "Un point de départ est un profil détenu par le serveur. Il indique les paramètres, les étapes et le locataire qu'il crée.",
            counts: '{settings} paramètres · {steps} étapes',
        },

        details: {
            title: 'Décrire le locataire',
            lead: 'Nommez le locataire et créez son administrateur.',
            tenant: 'Locataire',
            name: 'Nom',
            code: 'Code',
            codeHint: "Court et unique. Il nomme le locataire dans un nom d'utilisateur.",
            hostname: "Nom d'hôte",
            settings: '{profile} paramètres',
            administrator: 'Administrateur du locataire',
            username: "Nom d'utilisateur",
            email: 'Courriel',
            useMyPassword: 'Utiliser mon mot de passe',
            adminPassword: "Mot de passe de l'administrateur",
            passwordForced: 'Il devra le changer à la première connexion.',
            standard: '{profile} utilise ses paramètres standard.',
            leiSearch: 'Rechercher par nom ou LEI',
            leiParties: {
                one: '1 entité dans sa hiérarchie',
                other: '{count} entités dans sa hiérarchie',
            },
            leiNoMatch: 'Aucune entité ne correspond.',
            leiReadFailed: "Les entités juridiques n'ont pas pu être lues. {message}",
            changeSettings: 'Modifier les paramètres',
            noCreatingPassword:
                "L'administrateur qui crée le locataire doit d'abord définir un mot de passe.",
            rows: {
                tenant: 'Locataire',
                hostname: "Nom d'hôte",
                administrator: 'Administrateur',
                password: 'Mot de passe',
            },
            passwordMine: 'Identique au mien',
            passwordTyped: 'Défini ici',
        },

        review: {
            title: 'Vérification',
            lead: "Rien n'est créé tant que vous n'avez pas confirmé.",
            startingPoint: 'Point de départ',
            tenant: 'Locataire',
            hostname: "Nom d'hôte",
            administrator: 'Administrateur',
            password: 'Mot de passe',
            passwordMine: 'Le même que le vôtre',
            passwordSet: 'Celui que vous avez saisi',
            steps: 'La création du locataire exécute {steps} étapes.',
            forcedChange: '{principal} définit son propre mot de passe à la première connexion.',
            noPassword: "L'administrateur du locataire n'a pas encore de mot de passe.",
            create: 'Créer le locataire',
            noRun: "Le serveur a créé le locataire mais n'a désigné aucune exécution à suivre.",
        },

        provisioning: {
            title: 'Provisionnement',
            lead: 'Vous pouvez quitter cette page et revenir.',
            readFailed: "La progression n'a pas pu être lue. {message}",
            retry: "Reprendre à l'étape échouée",
            retryKeeps: 'Les étapes terminées sont conservées.',
            retrying: "L'exécution a repris à {step}.",
            rolledBack:
                "L'exécution a annulé les étapes qu'elle avait terminées, elle ne laisse donc rien derrière elle.",
        },

        handOff: {
            title: 'Passation',
            lead: 'Le locataire est prêt. Son administrateur se connecte ensuite.',
            administrator: 'Son administrateur est {principal}.',
            continue: "Continuer en tant qu'administrateur du locataire",
            continueHint: 'Connectez-vous en tant que {principal} maintenant.',
            elsewhere: 'Passer la main à quelqu’un d’autre',
            elsewhereHint:
                "Déconnectez-vous et transmettez le nom d'utilisateur et le mot de passe.",
            elsewhereHintForced:
                "Déconnectez-vous et transmettez le nom d'utilisateur. La personne définit son propre mot de passe à la première connexion.",
        },

        signIn: {
            title: 'Première connexion',
            lead: "L'administrateur du locataire se connecte pour la première fois.",
            username: "Nom d'utilisateur",
            password: 'Mot de passe',
            submit: 'Se connecter',
            submitting: 'Connexion...',
            choosePartyHint: "Choisissez l'entité dans laquelle travailler.",
            done: 'Connecté en tant que {principal}.',
        },

        change: {
            required: 'Ce compte définit son propre mot de passe avant de continuer.',
            new: 'Nouveau mot de passe',
            submit: 'Définir le mot de passe',
            submitting: 'Définition...',
            done: 'Votre mot de passe est défini et vous êtes connecté en tant que {principal}.',
        },

        ready: {
            title: 'Prêt',
            lead: "L'installation est configurée et {principal} est connecté.",
            home: "Aller à l'accueil",
        },
    },

    version: {
        client: 'client {version}',
        server: 'serveur {version}',
        serverUnknown: 'version du serveur inconnue',
    },

    server: {
        'Publish the reference data': 'Publier les données de référence',
        'Publishes the reference data the tenant works from: the bundles the starting point orders, and the datasets those bundles name.':
            'Publie les données de référence à partir desquelles le locataire travaille : les ensembles que le point de départ commande, et les jeux de données que ces ensembles nomment.',
        'Import the legal entities': 'Importer les entités juridiques',
        "Reads the legal entity the starting point names by its LEI and the entities it consolidates, and publishes them as the tenant's parties.":
            "Lit l'entité juridique que le point de départ nomme par son LEI et les entités qu'elle consolide, puis les publie comme entités du locataire.",
        "Create the tenant's parties": 'Créer les entités du locataire',
        'Creates the party that represents the tenant itself, with the reference data a party needs.':
            "Crée l'entité qui représente le locataire lui-même, avec les données de référence dont une entité a besoin.",
        'Load the staff': 'Charger le personnel',
        'Creates an account for each person the starting point lists, each in its own party.':
            'Crée un compte pour chaque personne que le point de départ liste, chacune dans sa propre entité.',
        'Attach the photographs': 'Joindre les photographies',
        "Gives the accounts their photographs, and the tenant's party its logo.":
            "Donne aux comptes leurs photographies et à l'entité du locataire son logo.",
        'Start the market feeds': 'Démarrer les flux de marché',
        "Starts the synthetic market data the tenant's curves and prices are built from.":
            'Démarre les données de marché synthétiques à partir desquelles les courbes et les prix du locataire sont construits.',
        Finish: 'Terminer',
        'Marks the tenant ready: it stops bootstrapping and becomes active.':
            'Marque le locataire comme prêt : il quitte le mode amorçage et devient actif.',
    },

    common: {
        loading: 'Chargement...',
        all: 'Tous',
        close: 'Fermer',
        open: 'Ouvrir',
        revert: 'Rétablir',
        apply: 'Appliquer',
        back: 'Retour',
        continue: 'Continuer',
    },
};

catalogueSchema(flatten(source)).parse(flatten(fr));
export { fr };

/** The catalogue, flattened to dot paths. */
export const frFlat = flatten(fr);
