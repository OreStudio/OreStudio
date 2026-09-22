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
    notAvailable:
      'As contas são criadas por um administrador ou através do assistente de aprovisionamento no cliente de ambiente de trabalho. O registo autónomo ainda não está disponível.',
    haveAccount: 'Já tem uma conta?',
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
    noReasons: 'Não há motivos disponíveis para esta operação. Um administrador tem de adicionar um.',
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
  },

  component: {
    entities: 'Entidades',
    shortcuts: 'Tarefas comuns',
    noEntities: 'Ainda não há entidades declaradas para este componente.',
  },

  shortcut: {
    accounts: { title: 'Contas', description: 'Adicione, altere e bloqueie as contas que podem iniciar sessão.' },
    orgChart: { title: 'Organograma', description: 'Veja quem reporta a quem.' },
    onboardTenant: { title: 'Aprovisionar inquilino', description: 'Crie um novo inquilino e a sua primeira entidade.' },
    parties: { title: 'Entidades', description: 'As organizações com quem negoceia e a que pertence.' },
    currencies: { title: 'Moedas', description: 'Códigos de moeda, arredondamento e escalões de mercado.' },
    books: { title: 'Carteiras', description: 'As carteiras onde as operações são registadas.' },
    trades: { title: 'Operações', description: 'Operações capturadas e os seus eventos de ciclo de vida.' },
    portfolios: { title: 'Portfólios', description: 'Hierarquias de carteiras e posições.' },
    marketSeries: { title: 'Séries de mercado', description: 'Séries temporais de observações de mercado.' },
    fixings: { title: 'Fixings', description: 'Fixings publicados e os seus detalhes.' },
    reportDefinitions: { title: 'Definições de relatório', description: 'O que pode ser executado e com que parâmetros.' },
    reportInstances: { title: 'Instâncias de relatório', description: 'Relatórios já gerados.' },
    catalog: { title: 'Catálogo', description: 'Todos os conjuntos de dados e a sua origem.' },
    codingSchemes: { title: 'Esquemas de codificação', description: 'Vocabulários controlados e os seus códigos.' },
    computeDashboard: { title: 'Painel', description: 'O que está a correr e o que está em fila.' },
    queues: { title: 'Filas', description: 'Trabalho à espera de ser recolhido.' },
    workflowDefinitions: { title: 'Definições', description: 'Os fluxos de trabalho que podem ser iniciados.' },
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
    revertBody: "Tem a certeza de que pretende reverter '{name}' da versão {from} para a versão {to}? Isto criará uma nova versão com os dados da versão {to}.",
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

  common: {
    loading: 'A carregar...',
    all: 'Todos',
    close: 'Fechar',
    open: 'Abrir',
    revert: 'Reverter',
    apply: 'Aplicar',
    back: 'Voltar',
  },
};

// Checked at import time against the English key set: a missing or renamed key
// fails here rather than silently falling back to English in front of a
// Portuguese speaker.
catalogueSchema(flatten(source)).parse(flatten(pt));
export { pt };

/** The catalogue, flattened to dot paths. */
export const ptFlat = flatten(pt);
