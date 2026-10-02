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

/**
 * Portuguese.
 *
 * Typed against the English catalogue, so a key that does not exist is a compile
 * error and a missing key is caught by the schema at the bottom of this file.
 */
const pt: SourceCatalogue = {
    app: {
        name: 'ORE Studio',
        tagline: 'Análise de risco de nível empresarial, no navegador.',
    },

    nav: {
        site: 'Site',
        accounts: 'Contas',
        notifications: 'Notificações',
        alerts: 'Alertas',
        signOut: 'Terminar sessão',
        signIn: 'Iniciar sessão',
        menu: 'Menu',
        closeMenu: 'Fechar menu',
        language: 'Idioma',
        iam: 'IAM',
        refdata: 'Dados de referência',
        trading: 'Negociação',
        marketdata: 'Dados de mercado',
        reporting: 'Relatórios',
        dataquality: 'Qualidade de dados',
        compute: 'Cálculo',
        workflow: 'Fluxos de trabalho',
        platform: 'Plataforma',
        collapse: 'Recolher',
        expand: 'Expandir',
        home: 'Início',
        search: 'Pesquisar',
        searchHint: 'Pesquisar entidades e ações',
        noResults: 'Nada corresponde.',
        allEntities: 'Todas as entidades',
        mode: 'Modo',
        areas: 'Áreas',
    },

    shell: {
        mode: {
            'system-administration': 'Administração do sistema',
            'tenant-administration': 'Administração do inquilino',
            application: 'Aplicação',
        },
        journeyCount: { one: '{count} jornada', other: '{count} jornadas' },
        notBuilt: 'Ainda não construído',
        area: {
            tenants: 'Inquilinos',
        },
        journey: {
            seeTenants: 'Ver os inquilinos',
            newTenant: 'Novo inquilino',
            retireTenant: 'Retirar ou reiniciar um inquilino',
        },
    },

    tenants: {
        title: 'Inquilinos',
        description: 'Os inquilinos deste ambiente e o estado de cada um.',
        search: 'Pesquisar por código, nome ou nome do anfitrião',
        noMatch: 'Nenhum inquilino corresponde a "{search}".',
        showing: {
            one: '{first}–{last} de {count} inquilino',
            other: '{first}–{last} de {count} inquilinos',
        },
        filterType: 'Filtrar por tipo',
        filterStatus: 'Filtrar por estado',
        allTypes: 'Todos os tipos',
        allStatuses: 'Todos os estados',
        showTest: 'Mostrar inquilinos de teste',
        hiddenTest: {
            one: '{count} inquilino de teste oculto',
            other: '{count} inquilinos de teste ocultos',
        },
        noneFiltered: 'Nenhum inquilino corresponde a estes filtros.',
        previous: 'Anterior',
        next: 'Seguinte',
        code: 'Código',
        name: 'Nome',
        hostname: 'Nome do anfitrião',
        type: 'Tipo',
        status: 'Estado',
        failed: 'Não foi possível ler os inquilinos.',
        setup: 'Configuração',
        setupUnavailable:
            'Não foi possível ler as execuções de aprovisionamento, por isso a coluna de configuração está vazia.',
        setupState: {
            in_progress: 'Passo {step} de {count}',
            compensating: 'A reverter',
            failed: 'Falhou no passo {step}',
            compensated: 'Revertido',
        },
        run: {
            title: 'A configurar {name}',
            unknownTenant: 'A configurar um inquilino',
            lead: 'A execução de aprovisionamento deste inquilino. Continua no servidor mesmo sem ninguém a acompanhar.',
            back: 'Voltar aos inquilinos',
            done: 'A execução terminou. O inquilino está pronto.',
        },
        empty: {
            title: 'Esta instalação ainda não tem um inquilino próprio.',
            body: 'O administrador do sistema existe, portanto a instalação está configurada. Um inquilino é o passo seguinte, e o primeiro cria-se como qualquer outro.',
        },
    },

    landing: {
        heading: 'Análise de risco de nível empresarial — mas visual e de código aberto.',
        introBefore: 'O ORE Studio envolve o',
        introBetween: '(ORE) e o',
        introAfter:
            'numa interface gráfica intuitiva — sem Python nem C++ — sobre um backend nativo de PostgreSQL com desempenho de C++.',
        signUp: 'Criar conta',
        signIn: 'Iniciar sessão',
    },

    password: {
        show: 'Mostrar',
        hide: 'Ocultar',
        new: 'Palavra-passe',
        confirm: 'Confirmar palavra-passe',
        mismatch: 'As palavras-passe não coincidem.',
        ruleMet: 'cumprida',
        ruleNotMet: 'não cumprida',
        strength: { 0: '', 1: 'Fraca', 2: 'Razoável', 3: 'Boa', 4: 'Forte' },
        rule: {
            length: 'Pelo menos {min} caracteres',
            upper: 'Uma letra maiúscula (A-Z)',
            lower: 'Uma letra minúscula (a-z)',
            digit: 'Um algarismo (0-9)',
            special: 'Um carácter especial ({chars})',
        },
    },

    signIn: {
        title: 'Iniciar sessão',
        username: 'Nome de utilizador',
        password: 'Palavra-passe',
        submit: 'Iniciar sessão',
        submitting: 'A iniciar sessão...',
        noAccount: 'Não tem conta?',
        createOne: 'Criar conta',
        failed: 'Não foi possível iniciar sessão.',
        chooseParty: 'Escolha uma entidade',
        choosePartyHint: 'Esta conta trabalha em mais do que uma entidade.',
        partyCategory: 'Categoria',
    },

    signUp: {
        title: 'Criar conta',
        haveAccount: 'Já tem uma conta?',
        detailsTitle: 'Os seus dados',
        detailsLead: 'A conta com que vai iniciar sessão.',
        principal: 'Nome de utilizador',
        email: 'Correio eletrónico',
        password: 'Palavra-passe',
        confirmation: 'Confirmar palavra-passe',
        mismatch: 'As duas palavras-passe não coincidem.',
        review: 'Rever',
        reviewTitle: 'Revisão',
        reviewLead: 'O que vai ser criado.',
        tenant: 'Inquilino',
        party: 'Parte',
        role: 'Perfil',
        noParty: 'Ainda não. Um administrador acrescenta uma.',
        usableNow: 'Poderá iniciar sessão assim que a conta existir.',
        pending: 'Um administrador tem de o adicionar a uma parte antes de poder iniciar sessão.',
        create: 'Criar conta',
        createFailed: 'A conta não foi criada.',
        createdTitle: 'Conta criada',
        waitingTitle: 'Conta em espera',
        createdLead: 'A sua conta existe. Inicie sessão com a palavra-passe que escolheu.',
        waitingLead: 'A sua conta existe e aguarda um administrador.',
        waitingFor: 'Aguarda uma parte onde trabalhar.',
        received: 'Detém o perfil {role} em {tenant}.',
        goToSignIn: 'Iniciar sessão',
        closedTitle: 'O registo está fechado',
        closedFallback: 'Esta implantação não aceita registos.',
        unavailable: 'A implantação não conseguiu dizer se aceita registos.',
        tryAgain: 'Tentar de novo',
    },

    accounts: {
        title: 'Contas',
        description: 'Identidades que podem iniciar sessão ou atuar como serviço, neste inquilino.',
        search: 'Pesquisar',
        searchPlaceholder: 'Nome de utilizador, nome ou email',
        filterByType: 'Tipo',
        allTypes: 'Todos os tipos',
        count: '{shown} de {total}',
        refreshing: 'a atualizar',
        loading: 'A carregar...',
        empty: 'Nenhuma conta corresponde ao filtro atual.',
        failed: 'Não foi possível carregar as contas.',
    },

    account: {
        singular: 'conta',
        colUsername: 'Nome de utilizador',
        colFullName: 'Nome completo',
        colEmail: 'Email',
        colType: 'Tipo',
        colRecorded: 'Registado',
        fldFullName: 'Nome completo',
        fldEmail: 'Email',
        fldJobTitle: 'Cargo',
        fldType: 'Tipo',
        fldDefaultParty: 'Entidade predefinida',
        fldReportsTo: 'Reporta a',
        fldVersion: 'Versão',
        fldModifiedBy: 'Modificado por',
        fldPerformedBy: 'Executado por',
        fldRecordedAt: 'Registado em',
        fldChangeReason: 'Motivo da alteração',
        fldCommentary: 'Comentário',
        notRecorded: 'não registado',
        notSet: 'não definido',
        nobody: 'ninguém',
        unknown: 'desconhecido',
        none: 'nenhum',
    },

    entity: {
        add: 'Adicionar',
        edit: 'Editar',
        delete: 'Eliminar',
        save: 'Guardar',
        saving: 'A guardar...',
        close: 'Fechar',
        cancel: 'Cancelar',
        changedBanner: 'Esta lista foi alterada desde que a carregou.',
        changed: 'Esta lista foi alterada no servidor. Atualize para a ver.',
        refresh: 'Recarregar',
        history: 'Histórico',
        search: 'Pesquisar',
        filter: 'Filtrar',
        page: 'Página {page} de {pages}',
        pageSize: 'Itens por página',
        loadAll: 'Carregar tudo',
        emptyFiltered: 'Nada corresponde a esse filtro nestes {collection}.',
        loadFailed: 'Não foi possível carregar estes {collection}.',
        noRecords: 'Sem registos',
        loading: 'A carregar...',
        first: 'Primeira',
        previous: 'Anterior',
        next: 'Seguinte',
        last: 'Última',
        new: 'Novo',
        provenance: 'Proveniência',
        asOf: 'A partir de',
        asOfNow: 'Agora',
        general: 'Geral',
        related: 'Relacionado',
    },

    audit: {
        createTitle: 'Motivo do novo registo',
        amendTitle: 'Motivo da alteração obrigatório',
        deleteTitle: 'Motivo da eliminação obrigatório',
        createPrompt: 'Escolha um motivo para criar este registo:',
        amendPrompt: 'Escolha um motivo para esta alteração:',
        deletePrompt: 'Escolha um motivo para esta eliminação:',
        reason: 'Motivo',
        commentary: 'Comentário',
        commentaryPlaceholder: 'Introduza uma explicação para esta alteração...',
        commentaryRequired: 'O comentário é obrigatório para este motivo.',
        commentaryOptional: 'O comentário é opcional para este motivo.',
        noReasons:
            'Não há motivos disponíveis para esta operação. Um administrador tem de adicionar um.',
        nonMaterial: 'sem alteração',
        required: 'Obrigatório',
        create: 'Criar',
        confirmDelete: 'Confirmar eliminação',
    },

    confirmation: {
        deleteTitle: 'Eliminar {singular}',
        deleteBody: "Tem a certeza de que pretende eliminar {singular} '{name}'?",
        unsavedTitle: 'Alterações não guardadas',
        unsavedBody: 'Tem alterações não guardadas. Fechar mesmo assim?',
        yes: 'Sim',
        no: 'Não',
    },

    feedback: {
        saved: '{name} guardado',
        deleted: '{name} eliminado',
        saveFailed: 'Falha ao guardar',
        createFailed: 'Falha ao criar',
        deleteFailed: 'Falha ao eliminar',
        invalidInput: 'Dados inválidos',
        requiredFields: 'Preencha todos os campos obrigatórios.',
        notConnected: 'Sem ligação ao servidor. Inicie sessão.',
        sessionExpired: 'A sua sessão terminou. Inicie sessão novamente.',
        notFound: 'Nenhum registo corresponde a esse identificador.',
        unreachable: 'Não é possível contactar o servidor.',
        retry: 'Tentar novamente',
    },

    status: {
        environment: 'Ambiente',
        connected: 'Ligado',
        disconnected: 'Desligado',
        development: 'desenvolvimento',
        notSignedIn: 'Sessão não iniciada',
        copyright: '© 2026 Contribuidores do ORE Studio.',
    },

    deployment: {
        title: 'Implementação',
        description:
            'Apenas de leitura. Tudo aqui é decidido quando o processo arranca, não no navegador.',
        environment: 'Este ambiente',
        name: 'Nome',
        identifier: 'Identificador',
        kind: 'Tipo',
        production: 'produção',
        notProduction: 'não é produção',
        natsServer: 'Servidor NATS',
        namespace: 'Espaço de nomes',
        httpServer: 'Servidor HTTP',
        notConfigured: 'não configurado',
        configuration: 'Configuração',
        configFile: 'Ficheiro',
        everyEnvironment: 'Todos os ambientes declarados',
        serving: 'em uso',
        switchHint: 'Altere reiniciando com um --env diferente.',
        unavailable: 'Esta implementação não disponibiliza a área de programador.',
    },

    home: {
        greeting: 'Sessão iniciada como {name}',
        quickActions: 'Ações rápidas',
        components: 'Componentes',
        entities: 'entidades',
        planned: 'planeadas',
        whatIs: 'O que existe aqui',
        title: 'Sessão iniciada',
        next: 'Os percursos são os ecrãs que enchem esta estrutura. Chegam um grupo de cada vez.',
        username: 'Conta',
        email: 'Email',
        tenant: 'Inquilino',
        party: 'Parte',
        newParty: 'Nova parte',
    },

    component: {
        entities: 'Entidades',
        shortcuts: 'Tarefas comuns',
        noEntities: 'Ainda não há entidades declaradas para este componente.',
    },

    shortcut: {
        accounts: {
            title: 'Contas',
            description: 'Adicione, altere e bloqueie as contas que podem iniciar sessão.',
        },
        orgChart: { title: 'Organograma', description: 'Veja quem reporta a quem.' },
        onboardTenant: {
            title: 'Aprovisionar inquilino',
            description: 'Crie um novo inquilino e a sua primeira entidade.',
        },
        parties: {
            title: 'Entidades',
            description: 'As organizações com quem negoceia e a que pertence.',
        },
        currencies: {
            title: 'Moedas',
            description: 'Códigos de moeda, arredondamento e escalões de mercado.',
        },
        books: {
            title: 'Carteiras',
            description: 'As carteiras onde as operações são registadas.',
        },
        trades: {
            title: 'Operações',
            description: 'Operações capturadas e os seus eventos de ciclo de vida.',
        },
        portfolios: { title: 'Portfólios', description: 'Hierarquias de carteiras e posições.' },
        marketSeries: {
            title: 'Séries de mercado',
            description: 'Séries temporais de observações de mercado.',
        },
        fixings: { title: 'Fixings', description: 'Fixings publicados e os seus detalhes.' },
        reportDefinitions: {
            title: 'Definições de relatório',
            description: 'O que pode ser executado e com que parâmetros.',
        },
        reportInstances: {
            title: 'Instâncias de relatório',
            description: 'Relatórios já gerados.',
        },
        catalog: { title: 'Catálogo', description: 'Todos os conjuntos de dados e a sua origem.' },
        codingSchemes: {
            title: 'Esquemas de codificação',
            description: 'Vocabulários controlados e os seus códigos.',
        },
        computeDashboard: {
            title: 'Painel',
            description: 'O que está a correr e o que está em fila.',
        },
        queues: { title: 'Filas', description: 'Trabalho à espera de ser recolhido.' },
        workflowDefinitions: {
            title: 'Definições',
            description: 'Os fluxos de trabalho que podem ser iniciados.',
        },
        scheduler: { title: 'Agendador', description: 'Tarefas e quando correm a seguir.' },
    },

    card: {
        planned: 'Planeado',
        notBuilt: 'Ainda não construído',
    },

    breadcrumb: {
        home: 'Início',
    },

    country: {
        title: 'Países',
        singular: 'país',
        newTitle: 'Novo país',
        invalidAlpha2: 'Um código alfa-2 tem duas letras.',
        invalidAlpha3: 'Um código alfa-3 tem três letras.',
        invalidNumeric: 'Um código numérico ISO tem três dígitos.',
        description: 'Países, os seus códigos ISO e os seus nomes oficiais.',
        colAlpha2Code: 'Código alfa-2',
        colAlpha3Code: 'Código alfa-3',
        colNumericCode: 'Código numérico',
        colName: 'Nome',
        colOfficialName: 'Nome oficial',
        colVersion: 'Versão',
        colModifiedBy: 'Modificado por',
        colRecordedAt: 'Registado em',
        searchPlaceholder: 'Código alfa-2, alfa-3, numérico ou nome',
        fldAlpha2Code: 'Código alfa-2',
        fldAlpha3Code: 'Código alfa-3',
        fldNumericCode: 'Código numérico',
        fldName: 'Nome',
        fldOfficialName: 'Nome oficial',
        alpha2CodePh: 'Introduza o código alfa-2 do país',
        alpha3CodePh: 'Introduza o código alfa-3 do país',
        numericCodePh: 'Introduza o código numérico ISO',
        namePh: 'Introduza o nome a apresentar',
        officialNamePh: 'Introduza o nome oficial do país',
    },

    tenant_type: {
        title: 'Tipos de inquilino',
        singular: 'tipo de inquilino',
        newTitle: 'Novo tipo de inquilino',
        description: 'Classificações de tipos de inquilino e a ordem em que são apresentadas.',
        colType: 'Tipo',
        colName: 'Nome',
        colDescription: 'Descrição',
        colDisplayOrder: 'Ordem de apresentação',
        colVersion: 'Versão',
        colModifiedBy: 'Modificado por',
        colRecordedAt: 'Registado em',
        searchPlaceholder: 'Tipo, nome ou descrição',
        fldType: 'Tipo',
        fldName: 'Nome',
        fldDescription: 'Descrição',
        typePh: 'Introduza o código do tipo de inquilino',
        namePh: 'Introduza o nome a apresentar',
        descriptionPh: 'Introduza a descrição',
    },

    table: {
        chooseColumns: 'Escolher colunas',
        rowActions: 'Ações',
        searchedSoFar: 'Pesquisados os primeiros {shown} de {total}',
    },
    history: {
        description: 'Todas as versões deste registo, da mais recente para a mais antiga.',
        newer: 'Mais recente',
        older: 'Mais antiga',
        revert: 'Reverter',
        revertBody:
            "Tem a certeza de que pretende reverter '{name}' da versão {from} para a versão {to}? Isto criará uma nova versão com os dados da versão {to}.",
        from: 'De',
        to: 'Para',
        outOfOrder: 'A versão De tem de ser mais antiga do que a versão Para.',
        timeline: 'Cronologia',
        current: 'atual',
        empty: 'Este registo não tem histórico.',
        initial: 'Versão inicial',
        comparing: 'A comparar a v{from} com a v{to}',
        allFields: 'Todos os campos',
        onlyChanges: 'Apenas alterações',
        field: 'Campo',
        before: 'Antes',
        after: 'Depois',
        noChanges: 'Sem alterações de campos entre estas versões.',
        openVersion: 'Abrir esta versão',
    },

    validation: {
        required: 'Este campo é obrigatório.',
    },

    image: {
        choose: 'Escolher imagem',
        change: 'Alterar imagem',
        remove: 'Remover',
        pick: 'Escolher uma imagem',
        search: 'Pesquisar imagens',
        none: 'Sem imagem',
        noneAvailable: 'Não existem imagens disponíveis neste inquilino.',
    },

    gate: {
        unreachable:
            'O servidor não respondeu, por isso a interface não sabe se esta instalação ainda precisa de ser configurada. {reason}',
        retry: 'Tentar de novo',
    },

    journey: {
        steps: 'Passos do percurso',
        actionFailed: 'O passo falhou: {message}',
        policyFailed:
            'Não foi possível ler as regras da palavra-passe, por isso não é possível definir aqui nenhuma. {message}',

        welcome: {
            title: 'Bem-vindo ao ORE Studio',
            lead: 'Esta instalação está vazia. Configure-a em três etapas. A lista à esquerda indica cada passo.',
            start: 'Começar',
            stage: {
                admin: 'Criar o administrador',
                adminBody: 'A conta proprietária desta instalação.',
                tenant: 'Criar o primeiro inquilino',
                tenantBody:
                    'O primeiro inquilino, criado no servidor a partir de um ponto de partida.',
                signIn: 'Iniciar sessão',
                signInBody:
                    'O primeiro início de sessão do administrador do inquilino, com uma palavra-passe própria.',
            },
        },

        admin: {
            title: 'Criar o administrador',
            lead: 'Esta instalação não tem administrador e ninguém pode iniciar sessão ainda.',
            bootstrap:
                'O administrador é proprietário da instalação. A implementação sai do modo de arranque quando esta conta existir.',
            username: 'Nome de utilizador do administrador',
            email: 'Email do administrador',
            password: 'Palavra-passe do administrador',
            create: 'Criar administrador',
            resumeTitle: 'Iniciar sessão como administrador',
            resumeLead:
                'Esta instalação já tem o seu administrador. Inicie sessão com essa conta para continuar. A palavra-passe é também a que a entidade recebe quando o perfil dela partilha a sua.',
            signIn: 'Iniciar sessão e continuar',
            partyChoice:
                'O administrador trabalha em mais do que uma entidade. Inicie sessão e escolha uma primeiro.',
        },

        profile: {
            title: 'Escolher um ponto de partida',
            lead: 'Um ponto de partida é um perfil que o servidor guarda. Indica as definições, os passos e o inquilino que cria.',
            counts: '{settings} definições · {steps} passos',
            installed: 'Instalado',
        },

        details: {
            title: 'Descrever o inquilino',
            lead: 'Dê um nome ao inquilino e crie o seu administrador.',
            tenant: 'Inquilino',
            name: 'Nome',
            code: 'Código',
            codeHint:
                'Letras minúsculas, dígitos e underscores, começando por uma letra. Dá nome ao inquilino num nome de utilizador.',
            hostname: 'Nome do anfitrião',
            settings: '{profile} definições',
            administrator: 'Administrador do inquilino',
            username: 'Nome de utilizador',
            email: 'Email',
            useMyPassword: 'Usar a minha palavra-passe',
            adminPassword: 'Palavra-passe do administrador',
            passwordForced: 'Terá de a alterar no primeiro início de sessão.',
            standard: 'O {profile} usa as suas definições padrão.',
            leiSearch: 'Pesquisar por nome ou LEI',
            leiHowItWorks:
                'Escolher uma entidade preenche o locatário abaixo, e cada campo continua editável.',
            leiChange: 'Alterar',
            leiKeep: 'Manter',
            leiParties: {
                one: '1 entidade na sua hierarquia',
                other: '{count} entidades na sua hierarquia',
            },
            leiNoMatch: 'Nenhuma entidade corresponde.',
            leiReadFailed: 'Não foi possível ler as entidades jurídicas. {message}',
            changeSettings: 'Alterar definições',
            noCreatingPassword:
                'O administrador que cria o inquilino tem de definir primeiro uma palavra-passe.',
            rows: {
                tenant: 'Inquilino',
                hostname: 'Nome do anfitrião',
                administrator: 'Administrador',
                password: 'Palavra-passe',
            },
            passwordMine: 'Igual à minha',
            passwordTyped: 'Definida aqui',
        },

        review: {
            title: 'Revisão',
            lead: 'Nada é criado antes de confirmar.',
            startingPoint: 'Ponto de partida',
            tenant: 'Inquilino',
            hostname: 'Nome do anfitrião',
            administrator: 'Administrador',
            password: 'Palavra-passe',
            passwordMine: 'A mesma que a sua',
            passwordSet: 'A que introduziu',
            steps: 'A criação do inquilino executa {steps} passos.',
            forcedChange:
                '{principal} define a sua própria palavra-passe no primeiro início de sessão.',
            noPassword: 'O administrador do inquilino ainda não tem palavra-passe.',
            create: 'Criar inquilino',
            noRun: 'O servidor criou o inquilino mas não indicou nenhuma execução a seguir.',
        },

        provisioning: {
            title: 'Aprovisionamento',
            lead: 'Pode sair desta página e voltar.',
            readFailed: 'Não foi possível ler o progresso. {message}',
            retry: 'Retomar a partir do passo falhado',
            retryKeeps: 'Os passos concluídos são mantidos.',
            retrying: 'A execução retomou em {step}.',
            rolledBack:
                'A execução anulou os passos que tinha concluído, por isso não deixa nada atrás.',
        },

        handOff: {
            title: 'Passagem',
            lead: 'O inquilino está pronto. O seu administrador inicia sessão a seguir.',
            administrator: 'O seu administrador é {principal}.',
            continue: 'Continuar como administrador do inquilino',
            continueHint: 'Inicie sessão como {principal} agora.',
            elsewhere: 'Passar a vez a outra pessoa',
            elsewhereHint: 'Termine a sessão e entregue o nome de utilizador e a palavra-passe.',
            elsewhereHintForced:
                'Termine a sessão e entregue o nome de utilizador. A pessoa define a sua própria palavra-passe no primeiro início de sessão.',
            partyChoice:
                'O administrador do inquilino trabalha em mais de uma parte, por isso a parte a abrir não pode ser escolhida aqui. Inicie sessão como ele e escolha uma.',
        },

        signIn: {
            title: 'Primeiro início de sessão',
            lead: 'O administrador do inquilino inicia sessão pela primeira vez.',
            username: 'Nome de utilizador',
            password: 'Palavra-passe',
            submit: 'Iniciar sessão',
            submitting: 'A iniciar sessão...',
            choosePartyHint: 'Escolha a entidade em que quer trabalhar.',
            done: 'Sessão iniciada como {principal}.',
        },

        change: {
            required: 'Esta conta define a sua própria palavra-passe antes de continuar.',
            new: 'Nova palavra-passe',
            submit: 'Definir palavra-passe',
            submitting: 'A definir...',
            done: 'A sua palavra-passe está definida e tem a sessão iniciada como {principal}.',
        },

        ready: {
            title: 'Pronto',
            lead: 'A instalação está configurada e {principal} tem a sessão iniciada.',
            home: 'Ir para o início',
        },

        party: {
            noLei: 'Sem LEI',
            find: {
                title: 'Encontrar a entidade legal',
                lead: 'A parte é uma entidade legal. Procure as que esta instalação detém, ou nomeie uma que ela não detém.',
                search: 'Uma entidade legal que detemos',
                searchHint: 'Procure por nome ou por LEI.',
                byHand: 'Não é uma delas',
                byHandHint:
                    'Nomeie uma parte para a qual a instalação não detém nenhuma entidade legal.',
                name: 'Designação legal',
                nameHint: 'O nome pelo qual a parte está registada.',
                lei: 'Entidade legal',
                leiHint: 'Escolher uma entidade dá à parte o nome dela.',
            },
            describe: {
                title: 'Descrever a parte',
                lead: 'Dê-lhe o código curto pelo qual este inquilino a conhecerá.',
                shortCode: 'Código curto',
                shortCodeHint:
                    'Um mnemónico único deste inquilino, proposto a partir da designação legal. Identifica a parte nos comandos e nos relatórios.',
            },
            review: {
                title: 'Revisão',
                lead: 'Nada é criado antes de confirmar.',
                legalName: 'Designação legal',
                lei: 'LEI',
                shortCode: 'Código curto',
                data: 'Os dados da parte são os dados padrão do inquilino, que a execução publica.',
                bornInactive:
                    'A parte é criada inativa. A execução publica os seus dados, ativa-a e liga-o a ela.',
                create: 'Adicionar parte',
                noRun: 'O servidor criou a parte mas não indicou nenhuma execução a seguir.',
            },
            provisioning: {
                title: 'Aprovisionamento',
                lead: 'Isto corre no servidor. Pode sair desta página e voltar.',
            },
            next: {
                title: 'Passos seguintes',
                lead: '{code} está ativa.',
                work: 'Trabalhar nela agora',
                workHint: 'Trabalhe como esta parte pelo resto desta sessão.',
                another: 'Adicionar outra parte',
                anotherHint: 'Comece este percurso de novo.',
                done: 'Concluído',
                doneHint: 'Volte ao ecrã de onde partiu.',
                joined: 'Está ligado à parte, por isso é uma daquelas em que pode trabalhar.',
            },
        },
    },

    version: {
        client: 'cliente {version}',
        server: 'servidor {version}',
        serverUnknown: 'versão do servidor desconhecida',
    },

    server: {
        'Publish the reference data': 'Publicar os dados de referência',
        'Publishes the reference data the tenant works from: the bundles the starting point orders, and the datasets those bundles name.':
            'Publica os dados de referência com que o inquilino trabalha: os pacotes que o ponto de partida encomenda e os conjuntos de dados que esses pacotes nomeiam.',
        'Import the legal entities': 'Importar as entidades jurídicas',
        "Reads the legal entity the starting point names by its LEI and the entities it consolidates, and publishes them as the tenant's parties.":
            'Lê a entidade jurídica que o ponto de partida indica pelo seu LEI e as entidades que esta consolida, e publica-as como entidades do inquilino.',
        'Provision the parties': 'Aprovisionar as partes',
        "Publishes each party's reference data, records the legal entity it was built from, activates it, marks its onboarding complete and joins the caller to it.":
            'Publica os dados de referência de cada parte, registra a entidade legal de que foi criada, ativa-a, marca a sua integração como concluída e liga o chamador a ela.',
        'Load the staff': 'Carregar o pessoal',
        'Creates an account for each person the starting point lists, each in its own party.':
            'Cria uma conta para cada pessoa que o ponto de partida lista, cada uma na sua própria entidade.',
        'Attach the photographs': 'Anexar as fotografias',
        "Gives the accounts their photographs, and the tenant's party its logo.":
            'Dá as fotografias às contas e o logótipo à entidade do inquilino.',
        'Start the market feeds': 'Iniciar os fluxos de mercado',
        "Starts the synthetic market data the tenant's curves and prices are built from.":
            'Inicia os dados de mercado sintéticos a partir dos quais as curvas e os preços do inquilino são construídos.',
        Finish: 'Concluir',
        'Marks the tenant ready: it stops bootstrapping and becomes active.':
            'Marca o inquilino como pronto: sai do modo de arranque e fica ativo.',
    },

    common: {
        loading: 'A carregar...',
        all: 'Todos',
        close: 'Fechar',
        open: 'Abrir',
        revert: 'Reverter',
        apply: 'Aplicar',
        back: 'Voltar',
        continue: 'Continuar',
    },
};

// Checked at import time against the English key set: a missing or renamed key
// fails here rather than silently falling back to English in front of a
// Portuguese speaker.
catalogueSchema(flatten(source)).parse(flatten(pt));
export { pt };

/** The catalogue, flattened to dot paths. */
export const ptFlat = flatten(pt);
