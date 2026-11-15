// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { version } from './version.js';

const docsRoot = 'https://orestudio.github.io/OreStudio/doc/';
const repoRoot = 'https://github.com/OreStudio/OreStudio/blob/main/doc/';
const sections = [
    {
        title: 'Create a book in the web app',
        topics: 'book books',
        keywords: 'add new create set up structure web browser',
        excerpt:
            'As a tenant administrator, open **Shape the book structure**. In the book tree, select the portfolio that will contain the book. Enter the book name and functional currency, complete its classification, then review the changes before confirming.',
        source: 'knowledge/journeys/books/journey_shape_the_book_structure',
    },
    {
        title: 'Create a portfolio in the web app',
        topics: 'portfolio portfolios',
        keywords: 'add new create set up parent hierarchy tree structure web browser',
        excerpt:
            'Open **Shape the book structure** and select **New portfolio** in the tree. Enter its name, purpose and aggregation currency. Choose a parent if this is a sub-portfolio. This workflow creates the portfolio as part of adding its first book. Complete the book details and review the changes before confirming.',
        source: 'knowledge/journeys/books/journey_shape_the_book_structure',
    },
    {
        title: 'Find and create books in the shell',
        topics: 'book books',
        keywords: 'shell cli terminal command list find get add new create',
        excerpt:
            'Use `books list` to read a page of books and `books get <name>` for an exact name. Use `books add` to create a book. The recipe lists the required arguments, including the party, parent portfolio, classification and change reason.',
        source: 'recipes/shell/books/book',
    },
    {
        title: 'Find and create portfolios in the shell',
        topics: 'portfolio portfolios',
        keywords: 'shell cli terminal command list find get add new create',
        excerpt:
            'Use `portfolios list` to read a page of portfolios and `portfolios get <name>` for an exact name. Use `portfolios add` to create a portfolio. The recipe lists the required arguments, including the party, purpose, aggregation currency and change reason.',
        source: 'recipes/shell/portfolios/portfolio',
    },
    {
        title: 'Sign in and choose a party in the web app',
        topics: 'sign login log signin authenticate password party',
        keywords: 'choose select web browser',
        excerpt:
            'Open **Sign in** and enter your username and password. If OreStudio asks you to choose a party, select the party you want to work in. If the party choice times out, sign in again.',
        source: 'knowledge/journeys/entry/journey_sign_in',
    },
    {
        title: 'Run a report in the web app',
        topics: 'report reports npv',
        keywords: 'run runs start trigger execute launch now demand web browser',
        excerpt:
            'Open **Reporting** and choose the report definition. Check its scope and run configuration, select **Run now**, then confirm. OreStudio opens the new run so you can follow its status.',
        source: 'knowledge/journeys/reporting/journey_run_a_report_and_watch_it_finish',
    },
    {
        title: 'View or download report results in the web app',
        topics: 'report reports run runs result results',
        keywords:
            'view find read see check open download export output archive status error errors npv web browser',
        excerpt:
            'Open the report run from **Reporting**. Use **Details** for the run state, **Results** for recorded results, **Errors** for problems and **Downloads** for available output files.',
        source: 'knowledge/journeys/reporting/journey_read_what_a_run_produced',
    },
    {
        title: 'Book a trade in OreStudio',
        topics: 'trade trades booking book',
        keywords:
            'book booking capture enter new trade trades deal draft confirm counterparty instrument terms workflow',
        excerpt:
            'The booking workflow starts with the trade type, party, book and counterparty. Record the execution details and the terms required by the instrument. Choose whether to save a draft or book the live trade, then review and confirm. The workflow guide also lists gaps in the implementation.',
        source: 'knowledge/journeys/trades/journey_book_a_trade',
    },
    {
        title: 'Where Acme demo market data comes from',
        topics: 'market data series observations quotes feed catalogue',
        keywords:
            'acme demo seeded synthetic simulator subscriptions subscribed provider source ore reference imported sample',
        excerpt:
            'The Acme demo combines series from subscribed feeds with selected historical inputs imported from ORE example files. Coverage depends on the party and provider. Use a market data lookup to check what is stored for the selected party and observation date.',
        source: 'knowledge/demo/acme_demo_market_data_catalogue',
    },
    {
        title: 'View and compare book history in the desktop app',
        topics: 'book books history',
        keywords:
            'trading view find open audit compare version versions changes historical revert restore previous old qt desktop',
        excerpt:
            'In the desktop app, open **Trading → Books**. Right-click the book and choose **History**, or open the book details and select the history icon. Pick two versions to compare. Reverting records a new version and preserves the audit trail.',
        source: 'manual/user_guide/chapter_books',
    },
    {
        title: 'How portfolios group books',
        topics: 'portfolio portfolios book books',
        keywords:
            'organize organise group hierarchy structure parent child desk risk aggregation currency meaning difference',
        excerpt:
            'A portfolio groups books within a party’s hierarchy. A portfolio can sit under another portfolio and can specify the currency used to aggregate results. Individual trades are booked to books.',
        source: 'manual/user_guide/chapter_portfolios',
    },
    {
        title: 'View currencies in the desktop app',
        topics: 'currency currencies',
        keywords:
            'open view list find lookup reference data codes iso symbol symbols details qt desktop',
        excerpt:
            'Open **Reference Data → Currencies** in the desktop app. The list shows currency codes, names and symbols. Use the page controls to browse and open a row for its formatting, rounding and provenance details.',
        source: 'manual/user_guide/chapter_currencies',
    },
    {
        title: 'Currency pair conventions and pip factor',
        topics: 'currency pair pairs pip pips fx',
        keywords:
            'convention conventions points forward base quote tick size decimal pricing factor yen scale convert',
        excerpt:
            'A currency pair identifies the base and quote currencies. Its separate convention record holds the pip factor, tick size, display precision and date rules. The pip factor converts a number of pips into a rate change. Use the convention recorded for the pair and applicable date.',
        source: 'manual/user_guide/chapter_currency_pairs',
    },
    {
        title: 'Find a counterparty in the desktop app',
        topics: 'counterparty counterparties',
        keywords:
            'open view list find lookup reference data short code legal contact identifier identifiers qt desktop',
        excerpt:
            'Open **Reference Data → Counterparties** in the desktop app. The list includes each counterparty’s short code, name and status. Open a row to see its identifiers, contact information, hierarchy and provenance.',
        source: 'manual/user_guide/chapter_counterparties',
    },
];

const stopWords = new Set(
    'a an and are can could do does for from guide help how i in is it me my of on or orestudio ore studio please should the to use using we what where with would you'.split(
        ' ',
    ),
);

function words(text: string) {
    return new Set(
        (text.toLowerCase().match(/[a-z0-9]+/g) ?? []).filter((word) => !stopWords.has(word)),
    );
}

const index = sections.map(({ keywords, topics, title, excerpt, source }) => ({
    title,
    excerpt,
    url: source.startsWith('recipes/') ? repoRoot + source + '.org' : docsRoot + source + '.html',
    topics: words(topics),
    terms: words(`${topics} ${keywords}`),
    titleTerms: words(title),
}));

export const helpTool = {
    description:
        'Find cited OreStudio documentation about workflows, navigation, terminology or shell syntax. “How do I run a report in the web app?” → search_help. “Show me completed runs” → find_report_runs for live stored records.',
    schema: z
        .object({
            query: z
                .string()
                .trim()
                .min(1)
                .max(16000)
                .describe(
                    'Short task or concept, plus web, desktop or shell only if the user specified it.',
                ),
        })
        .strict(),
};

export function helpQuery(message: string) {
    const explicit =
        /^(?:(?:please\s+)?(?:use|call|invoke)\s+)?`?search_help`?(?::\s*|\s+(?:for\s+|to\s+(?:find|look up|search for)\s+))(.+)$/i.exec(
            message,
        );
    if (explicit) return explicit[1]!.trim();
    if (
        /^(?:please\s+)?(?:how\s+(?:(?:do|can|could|should|would)\s+(?:i|we)\b|to\b)|(?:show|tell|teach)\s+me\s+how\b|where\s+(?:do|can|should)\s+(?:i|we)\b|(?:find|search)\s+(?:(?:the|for)\s+)?(?:orestudio\s+)?(?:help|docs|documentation)\b|(?:help|docs|documentation|how-to)\b)/i.test(
            message,
        )
    )
        return message;
}

export function searchHelp(query: string) {
    const terms = [...words(query)];
    const hits = index
        .filter(({ topics }) => terms.some((term) => topics.has(term)))
        .map(({ title, excerpt, url, terms: indexed, titleTerms }) => {
            const coverage = terms.filter((term) => indexed.has(term)).length;
            return {
                title,
                excerpt,
                url,
                coverage,
                score: coverage + terms.filter((term) => titleTerms.has(term)).length,
            };
        })
        .filter(({ coverage }) => coverage >= Math.ceil(terms.length * 0.6))
        .sort((a, b) => b.score - a.score);
    const matches = hits
        .filter(({ score }) => score === hits[0]?.score)
        .slice(0, 3)
        .map(({ title, excerpt, url }) => ({ title, excerpt, url }));
    return {
        tool: 'search_help',
        status: matches.length ? 'ok' : 'not_found',
        version,
        matches,
    };
}

export function formatHelp(result: ReturnType<typeof searchHelp>) {
    if (!result.matches.length)
        return 'I found no matching instructions in the OreStudio help for this release.';
    return result.matches
        .map(({ title, excerpt, url }) => `**${title}**\n\n${excerpt}\n\n[Source](${url})`)
        .join('\n\n');
}
