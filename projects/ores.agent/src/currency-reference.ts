// SPDX-License-Identifier: GPL-3.0-or-later
import { z } from 'zod';
import { AppError, type Backend, type Operation } from './backend.js';
import type { Session } from './auth.js';
import { markdownTable, markdownText } from './markdown.js';
import { readPages } from './read-pages.js';

export const currencyInput = z
    .object({
        limit: z.number().int().min(1).max(50).default(10),
        offset: z.number().int().min(0).max(10000).default(0),
        codePrefix: z
            .string()
            .trim()
            .regex(/^[a-z]{1,3}$/i)
            .toUpperCase()
            .optional()
            .describe('ISO currency-code prefix if requested. For a country use country instead.'),
        country: z
            .string()
            .trim()
            .min(1)
            .max(100)
            .optional()
            .describe(
                'Literal country name or ISO country code. The tool finds its linked currency records.',
            ),
    })
    .strict();

const currency = z.object({
    iso_code: z.string(),
    name: z.string(),
    symbol: z.string().optional(),
});
const country = z.object({
    alpha2_code: z.string(),
    alpha3_code: z.string(),
    name: z.string(),
    official_name: z.string(),
});
const countryCurrency = z.object({
    country_alpha2_code: z.string(),
    currency_iso_code: z.string(),
});
const cataloguePage = z.object({
    result: z.object({ outcome: z.string() }),
    total: z.number().int().nonnegative(),
});
const policy = {
    code: 'incomplete_currency_catalogue',
    message: 'The complete OreStudio currency catalogue could not be verified.',
    maxRows: 1000,
};

export function currencySelectors(message: string) {
    const prefix =
        /^(?:(?:please\s+)?(?:show|list|find)\s+)?currencies(?:\s+with\s+(?:ISO\s+)?codes?)?\s+(?:beginning|starting)\s+with\s+(?:the\s+letter\s+)?["']?([a-z]{1,3})["']?[.?!]?$/i.exec(
            message,
        );
    if (prefix) return { codePrefix: prefix[1]!.toUpperCase(), limit: 50 };
    const place =
        /^(?:(?:what\s+is|what's|show|find|list)\s+)?(?:the\s+)?currenc(?:y|ies)\s+(?:for|of|used\s+in)\s+(?:the\s+)?([^?.!]{1,100})[.?!]?$/i.exec(
            message,
        );
    if (place) return { country: place[1]!.trim(), limit: 50 };
}

export async function readCurrencies(
    backend: Backend,
    request: z.infer<typeof currencyInput>,
    session: Session,
    signal?: AbortSignal,
) {
    const { limit, offset, codePrefix } = request;
    const fetchPage = async <T>(
        operation: Operation,
        key: string,
        schema: z.ZodType<T>,
        offset: number,
        limit: number,
        filter: unknown = null,
    ) => {
        const raw = await backend.request(
            operation,
            {
                offset,
                limit,
                order: { field: '', descending: false },
                filter,
                ...(operation === 'currencyCountries' ? {} : { as_of: null }),
            },
            session,
            signal,
        );
        signal?.throwIfAborted();
        if (!session.active) throw new AppError('unauthorized', 'Session was closed.', 401);
        const { result, total } = cataloguePage.parse(raw);
        if (result.outcome !== 'ok')
            throw new AppError(
                'backend_denied',
                'Backend did not authorize or complete this read.',
                403,
            );
        return { total, rows: z.object({ [key]: z.array(schema).max(limit) }).parse(raw)[key]! };
    };
    const currencyPage = (start: number, size: number, filter?: unknown) =>
        fetchPage('currencies', 'currencies', currency, start, size, filter);
    let selectedCountry: z.infer<typeof country> | undefined;
    let page = { total: 0, rows: [] as z.infer<typeof currency>[] };
    if (request.country) {
        const countries = await readPages(
            (start, size) => fetchPage('countries', 'countries', country, start, size),
            (row) => row.alpha2_code,
            policy,
        );
        const selector = request.country.toLowerCase();
        const matches = countries.filter((row) =>
            [row.alpha2_code, row.alpha3_code, row.name, row.official_name].some(
                (value) => value.toLowerCase() === selector,
            ),
        );
        if (matches.length === 1) {
            selectedCountry = matches[0]!;
            const links = await readPages(
                (start, size) =>
                    fetchPage(
                        'currencyCountries',
                        'currency_countries',
                        countryCurrency,
                        start,
                        size,
                    ),
                (row) => row.country_alpha2_code + ':' + row.currency_iso_code,
                policy,
            );
            const codes = links
                .filter((row) => row.country_alpha2_code === selectedCountry!.alpha2_code)
                .map((row) => row.currency_iso_code)
                .filter((code) => !codePrefix || code.startsWith(codePrefix));
            if (codes.length) page = await currencyPage(offset, limit, { iso_code_one_of: codes });
        }
    } else if (codePrefix) {
        const all = await readPages(currencyPage, (row) => row.iso_code, policy);
        const matches = all.filter((row) => row.iso_code.startsWith(codePrefix));
        page = { total: matches.length, rows: matches.slice(offset, offset + limit) };
    } else {
        page = await currencyPage(offset, limit);
    }
    return {
        tool: 'list_currencies',
        ...page,
        offset,
        source: 'OreStudio',
        retrievedAt: new Date().toISOString(),
        ...(codePrefix ? { codePrefix } : {}),
        ...(request.country
            ? {
                  country: selectedCountry?.name ?? request.country,
                  countryFound: Boolean(selectedCountry),
              }
            : {}),
    };
}

export function formatCurrencies(raw: unknown): string | undefined {
    const parsed = z
        .object({
            rows: z.array(currency),
            total: z.number().int().nonnegative(),
            offset: z.number().int().nonnegative(),
            source: z.string(),
            retrievedAt: z.string(),
            codePrefix: z.string().optional(),
            country: z.string().optional(),
            countryFound: z.boolean().optional(),
        })
        .safeParse(raw);
    if (!parsed.success) return;
    const result = parsed.data;
    const scope =
        (result.country ? ` for ${markdownText(result.country)}` : '') +
        (result.codePrefix
            ? ` with ISO codes beginning with ${markdownText(result.codePrefix)}`
            : '');
    const summary =
        result.countryFound === false
            ? `No configured country matched ${markdownText(result.country)}. Try its exact name or ISO country code.`
            : result.total === 0
              ? `No configured currencies found${scope}.`
              : `${result.total} configured ${result.total === 1 ? 'currency' : 'currencies'}${scope}. Showing ${result.rows.length} from offset ${result.offset}.`;
    return [
        summary,
        markdownTable(
            ['ISO code', 'Currency', 'Symbol'],
            result.rows.map((row) => [row.iso_code, row.name, row.symbol ?? '']),
        ),
        `Source: ${markdownText(result.source)}, retrieved ${markdownText(result.retrievedAt)}.`,
    ]
        .filter(Boolean)
        .join('\n\n');
}
