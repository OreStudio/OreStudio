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
 */
import { randomUUID } from 'node:crypto';
import type { FastifyInstance, FastifyRequest } from 'fastify';
import { z } from 'zod';
import { SYSTEM_TENANT_ID } from '@ores/wire-protocol';
import {
    subjects as feedSubjects,
    type CalibrateRequest,
    type ReviewVarianceCalibrationRequest,
} from '@ores/wire-protocol/generated/synthetic/protocol/feed_config_protocol';
import {
    subjects as policySubjects,
    type CalibrationPolicyWrite,
    type ListCalibrationPoliciesResponse,
} from '@ores/wire-protocol/generated/synthetic/protocol/calibration_policy_protocol';
import {
    subjects as orderSubjects,
    type ListCalibrationProviderOrdersResponse,
} from '@ores/wire-protocol/generated/synthetic/protocol/calibration_provider_order_protocol';
import {
    subjects as itemSubjects,
    type ListFxSpotFixingItemsResponse,
} from '@ores/wire-protocol/generated/synthetic/protocol/fx_spot_fixing_item_protocol';
import {
    subjects as irConfigSubjects,
    type ListIrCurveGenerationConfigsResponse,
} from '@ores/wire-protocol/generated/synthetic/protocol/ir_curve_generation_config_protocol';
import {
    subjects as irBootstrapSubjects,
    type ListIrCurveBootstrapConfigsResponse,
} from '@ores/wire-protocol/generated/refdata/protocol/ir_curve_bootstrap_config_protocol';
import {
    subjects as publishedCurveSubjects,
    type GetPublishedCurveSetsRequest,
} from '@ores/wire-protocol/generated/marketdata/protocol/published_curves_operations_protocol';
import { HttpFailure, notFound, notPermitted } from './errors.js';
import { input, intentSchema, paramId, readAll, refusal, write } from './refdata-calls.js';
import type { LiveSession } from './sessions.js';
import { feedStartSchema, operationReplySchema, upstreamFailure } from './synthetic.js';

/** A calibration read: every feed of the class, or the one feed it names. */
const readQuerySchema = z.object({ configId: z.uuid().optional() });

/** A calibration run: every feed of the class, or the one feed it names. */
const runBodySchema = z.object({ configId: z.uuid().optional() }).strict().default({});

/** An asset class as its refdata code names it. */
const assetClassSchema = z
    .string()
    .trim()
    .regex(/^[a-z0-9_]{1,40}$/);

/** The policy a class calibrates under, as the service states it. */
const calibrationPolicySchema = z.object({
    from_policy_row: z.boolean().default(false),
    window_days: z.int().default(0),
    variance_window_days: z.int().default(0),
    variance_approval_mode: z.string().default('manual'),
    variance_min_returns: z.int().default(0),
    variance_require_starts_agree: z.boolean().default(false),
    variance_min_persistence: z.number().default(0),
    variance_max_persistence: z.number().default(0),
    variance_min_reaction: z.number().default(0),
    allow_inverse: z.boolean().default(false),
    fallback_code: z.string().default(''),
    provider_order: z.array(z.string()).default([]),
});

/** One candidate a feed read, and what became of it. */
const candidateOutcomeSchema = z.object({
    item: z.string(),
    provider_code: z.string(),
    inverted: z.boolean().default(false),
    rank: z.int().default(0),
    state: z.enum(['chosen', 'skipped', 'not_reached']),
    reason: z.string().default(''),
});

/** What a calibration run made of a feed's daily variance. */
const varianceCalibrationSummarySchema = z.object({
    input_id: z.string().default(''),
    calibration_id: z.string().default(''),
    method: z.string().default(''),
    state: z.string().default(''),
    state_reason: z.string().default(''),
    item: z.string().default(''),
    provider_code: z.string().default(''),
    inverted: z.boolean().default(false),
    window_days: z.int().default(0),
    window_end: z.string().default(''),
    point_count: z.int().default(0),
    used_count: z.int().default(0),
    alpha: z.number().nullish(),
    gamma: z.number().nullish(),
    beta: z.number().nullish(),
    persistence: z.number().nullish(),
    starting_variance_ratio: z.number().nullish(),
    starts: z.int().default(0),
    starts_agreed: z.boolean().default(false),
});

export type VarianceCalibrationSummary = z.infer<typeof varianceCalibrationSummarySchema>;

/** How one feed of the class resolved under the policy. */
const calibratedFeedSchema = z.object({
    config_id: z.string(),
    source_name: z.string(),
    base_currency_code: z.string().default(''),
    quote_currency_code: z.string().default(''),
    explicit_items: z.boolean().default(false),
    window_days_override: z.int().nullish(),
    refused: z.boolean().default(false),
    start: feedStartSchema,
    candidates: z.array(candidateOutcomeSchema).default([]),
    running: z.boolean().default(false),
    outcome: z.string().default(''),
    outcome_message: z.string().default(''),
    variance: varianceCalibrationSummarySchema.nullish(),
    current_variance: varianceCalibrationSummarySchema.nullish(),
});

export type CalibratedFeed = z.infer<typeof calibratedFeedSchema>;

/**
 * The counts a class's calibration covers: how many feeds start from a fixing, keep their
 * fixed level, are refused, and override the policy.
 */
export function calibrationCoverage(feeds: readonly CalibratedFeed[]) {
    return {
        total: feeds.length,
        fromFixing: feeds.filter((f) => f.start.from_fixing).length,
        keptFixed: feeds.filter((f) => !f.start.from_fixing && !f.refused).length,
        refused: feeds.filter((f) => f.refused).length,
        overridden: feeds.filter((f) => f.explicit_items || f.window_days_override != null).length,
    };
}

/** The fields of a resolved feed that say where its currency's rate came from. */
export interface IrCoverageFeed {
    config_id: string;
    refused: boolean;
    start: {
        from_fixing: boolean;
        provider_code: string;
        note: string;
    };
}

/** The fields of an IR generation config that name a currency's source. */
export interface IrCoverageConfig {
    id: string;
    currency_code: string;
    source_name: string;
    price_source?: string;
}

/** The fields of a bootstrap config that name the curve it builds. */
export interface IrCoverageBootstrapConfig {
    id: string;
    currency_code: string;
    name: string;
    output_series_id: string;
}

/** The fields of a published set that say when it was published and where it stands. */
export interface IrCoveragePublish {
    as_of: string;
    state_code: string;
}

/** One currency's IR curve, and the gap that keeps it from being covered. */
export interface IrCurveCoverageRow {
    currency_code: string;
    config_id: string;
    source_name: string;
    provider_code: string;
    window_days: number;
    bootstrap_config_id: string;
    bootstrap_config_name: string;
    published_as_of: string;
    published_state: string;
    gap: IrCurveGap;
    gap_reason: string;
}

/** The first fact a currency is missing, or empty when its curve is covered. */
export type IrCurveGap =
    '' | 'no_feed' | 'no_source' | 'no_curve_config' | 'no_publish' | 'not_live' | 'stale_publish';

export interface IrCurveCoverageInput {
    readonly feeds: readonly IrCoverageFeed[];
    readonly generationConfigs: readonly IrCoverageConfig[];
    readonly bootstrapConfigs: readonly IrCoverageBootstrapConfig[];
    readonly publishedBySeries: ReadonlyMap<string, IrCoveragePublish | undefined>;
    readonly windowDays: number;
    readonly now: Date;
}

/** The IR generation config fields the coverage reads. */
const irCoverageConfigSchema = z.looseObject({
    id: z.string(),
    currency_code: z.string(),
    source_name: z.string().default(''),
    price_source: z.string().default(''),
});

const irCoverageBootstrapSchema = z.looseObject({
    id: z.string(),
    currency_code: z.string(),
    name: z.string().default(''),
    output_series_id: z.string(),
});

const publishedSetSchema = z.looseObject({
    as_of: z.string(),
    state_code: z.string().default(''),
});

/** A curve older than two republish firings states a gap: the job runs every fifteen minutes. */
const STALE_PUBLISH_MS = 30 * 60 * 1000;

const NO_FEED_REASON = 'No synthetic feed holds this currency.';
const NO_SOURCE_REASON =
    'The feed keeps a fixed level, because no public source publishes this currency.';
const REFUSED_REASON = 'The class policy refuses to start the feed without a fixing.';
const NO_CURVE_CONFIG_REASON = 'No bootstrap config holds this currency.';
const NO_PUBLISH_REASON = 'The curve has never been published.';
const STALE_PUBLISH_REASON = 'The latest curve was published more than 30 minutes ago.';
const NOT_LIVE_REASON = 'The latest set is not the set in use.';

/**
 * One row per supported currency: its synthetic source, its bootstrap config and its latest
 * published curve, with the first gap that stops it being covered and the reason recorded for
 * it. The currency universe is the union of the feed configs and the bootstrap configs.
 */
export function irCurveCoverage(input: IrCurveCoverageInput): IrCurveCoverageRow[] {
    const feeds = new Map(input.feeds.map((feed) => [feed.config_id, feed]));
    const configs = groupByCurrency(input.generationConfigs);
    const bootstraps = groupByCurrency(input.bootstrapConfigs);
    const currencies = [...new Set([...configs.keys(), ...bootstraps.keys()])].sort((a, b) =>
        a.localeCompare(b),
    );
    return currencies.map((currency) =>
        coverageRow(
            input,
            currency,
            configs.get(currency) ?? [],
            bootstraps.get(currency) ?? [],
            feeds,
        ),
    );
}

/** The rows of a list, grouped by the currency each names. */
function groupByCurrency<T extends { currency_code: string }>(
    items: readonly T[],
): Map<string, T[]> {
    const byCurrency = new Map<string, T[]>();
    for (const item of items) {
        const held = byCurrency.get(item.currency_code);
        if (held === undefined) {
            byCurrency.set(item.currency_code, [item]);
        } else {
            held.push(item);
        }
    }
    return byCurrency;
}

function coverageRow(
    input: IrCurveCoverageInput,
    currency: string,
    configs: readonly IrCoverageConfig[],
    bootstrapConfigs: readonly IrCoverageBootstrapConfig[],
    feeds: ReadonlyMap<string, IrCoverageFeed>,
): IrCurveCoverageRow {
    const config = publicConfigOf(configs, feeds);
    const feed = config === undefined ? undefined : feeds.get(config.id);
    const bootstrap = newestPublishedOf(bootstrapConfigs, input.publishedBySeries);
    const published =
        bootstrap === undefined
            ? undefined
            : input.publishedBySeries.get(bootstrap.output_series_id);
    return {
        currency_code: currency,
        config_id: config?.id ?? '',
        source_name: config?.source_name ?? '',
        provider_code: feed?.start.provider_code ?? '',
        window_days: input.windowDays,
        bootstrap_config_id: bootstrap?.id ?? '',
        bootstrap_config_name: bootstrap?.name ?? '',
        published_as_of: published?.as_of ?? '',
        published_state: published?.state_code ?? '',
        ...gapOf(feed, bootstrap, published, input.now),
    };
}

/** The feed config a currency reads through: the first, by source name, that takes a public rate. */
function publicConfigOf(
    configs: readonly IrCoverageConfig[],
    feeds: ReadonlyMap<string, IrCoverageFeed>,
): IrCoverageConfig | undefined {
    const ordered = [...configs].sort((a, b) => a.source_name.localeCompare(b.source_name));
    return ordered.find((config) => feeds.get(config.id)?.start.from_fixing === true) ?? ordered[0];
}

/** The curve config whose newest publish is the latest, else the first by name. */
function newestPublishedOf(
    configs: readonly IrCoverageBootstrapConfig[],
    publishedBySeries: ReadonlyMap<string, IrCoveragePublish | undefined>,
): IrCoverageBootstrapConfig | undefined {
    const ordered = [...configs].sort((a, b) => a.name.localeCompare(b.name));
    let best: IrCoverageBootstrapConfig | undefined;
    let bestAt = Number.NEGATIVE_INFINITY;
    for (const config of ordered) {
        const at = Date.parse(publishedBySeries.get(config.output_series_id)?.as_of ?? '');
        if (!Number.isNaN(at) && at > bestAt) {
            best = config;
            bestAt = at;
        }
    }
    return best ?? ordered[0];
}

/** The first gap, in the order a currency falls short of a published curve. */
function gapOf(
    feed: IrCoverageFeed | undefined,
    bootstrap: IrCoverageBootstrapConfig | undefined,
    published: IrCoveragePublish | undefined,
    now: Date,
): { gap: IrCurveGap; gap_reason: string } {
    if (feed === undefined) {
        return { gap: 'no_feed', gap_reason: NO_FEED_REASON };
    }
    if (!feed.start.from_fixing) {
        return { gap: 'no_source', gap_reason: fixedLevelReason(feed) };
    }
    if (bootstrap === undefined) {
        return { gap: 'no_curve_config', gap_reason: NO_CURVE_CONFIG_REASON };
    }
    if (published === undefined) {
        return { gap: 'no_publish', gap_reason: NO_PUBLISH_REASON };
    }
    if (published.state_code !== '' && published.state_code !== 'published') {
        return {
            gap: 'not_live',
            gap_reason: `${NOT_LIVE_REASON} It is ${published.state_code}.`,
        };
    }
    const age = now.getTime() - Date.parse(published.as_of);
    if (!(age <= STALE_PUBLISH_MS)) {
        return { gap: 'stale_publish', gap_reason: STALE_PUBLISH_REASON };
    }
    return { gap: '', gap_reason: '' };
}

/** The reason the system recorded for a feed with no public rate, else a stated sentence. */
function fixedLevelReason(feed: IrCoverageFeed): string {
    if (feed.start.note !== '') {
        return feed.start.note;
    }
    return feed.refused ? REFUSED_REASON : NO_SOURCE_REASON;
}

/** Each curve's latest published set, read by the output series its bootstrap config names. */
async function latestPublishedSets(
    session: LiveSession,
    configs: readonly IrCoverageBootstrapConfig[],
): Promise<Map<string, IrCoveragePublish>> {
    const entries = await Promise.all(
        configs.map(async (config) => {
            const reply = await session.client.callAuthenticated(
                publishedCurveSubjects.get_published_curve_sets_request,
                {
                    series_id: config.output_series_id,
                    limit: 1,
                    include_candidates: false,
                } satisfies GetPublishedCurveSetsRequest,
                operationReplySchema.extend({
                    sets: z.array(publishedSetSchema).default([]),
                }),
            );
            if (!reply.success) {
                throw upstreamFailure(reply.message);
            }
            return [config.output_series_id, reply.sets.at(-1)] as const;
        }),
    );
    const published = new Map<string, IrCoveragePublish>();
    for (const [seriesId, set] of entries) {
        if (set !== undefined) {
            published.set(seriesId, set);
        }
    }
    return published;
}

/** The IR coverage of the class: its generation configs, its bootstrap configs and their publishes. */
async function interestRateCoverage(
    session: LiveSession,
    feeds: readonly CalibratedFeed[],
    windowDays: number,
): Promise<IrCurveCoverageRow[]> {
    const [configRows, bootstrapRows] = await Promise.all([
        readAll<ListIrCurveGenerationConfigsResponse>(
            session,
            irConfigSubjects.list_ir_curve_generation_configs_request,
            'ir_curve_generation_configs',
            { filter: null, as_of: null },
        ),
        readAll<ListIrCurveBootstrapConfigsResponse>(
            session,
            irBootstrapSubjects.list_ir_curve_bootstrap_configs_request,
            'bootstrap_configs',
            { filter: null, as_of: null },
        ),
    ]);
    const generationConfigs = configRows.map((row) => irCoverageConfigSchema.parse(row));
    const bootstrapConfigs = bootstrapRows.map((row) => irCoverageBootstrapSchema.parse(row));
    return irCurveCoverage({
        feeds,
        generationConfigs,
        bootstrapConfigs,
        publishedBySeries: await latestPublishedSets(session, bootstrapConfigs),
        windowDays,
        now: new Date(),
    });
}

/** A class's policy row, as stored, when it has one. */
/**
 * The policy's fields the policy screen does not edit: the variance window and the approval
 * rules. A save writes them back as they are stored, or as the column defaults for a class with
 * no row, so editing the window or the order never resets them.
 */
const keptPolicyFields = {
    variance_window_days: z.int().default(1826),
    variance_approval_mode: z.enum(['manual', 'auto']).default('manual'),
    variance_min_returns: z.int().default(250),
    variance_require_starts_agree: z.boolean().default(true),
    variance_min_persistence: z.number().default(0.8),
    variance_max_persistence: z.number().default(0.995),
    variance_min_reaction: z.number().default(0.01),
};

const policyRowSchema = z.looseObject({
    id: z.string(),
    version: z.int(),
    asset_class_code: z.string(),
    ...keptPolicyFields,
});

const orderRowSchema = z.looseObject({
    id: z.string(),
    version: z.int(),
    policy_id: z.string(),
    provider_code: z.string(),
    rank: z.int(),
});

/** The policy a person states for a class: the window, the inverse rule, the order, the fallback. */
const policyBodySchema = z.object({
    intent: intentSchema,
    version: z.int().nonnegative().nullable(),
    window_days: z.int().min(2).max(180),
    allow_inverse: z.boolean(),
    fallback_code: z.enum(['keep_fixed', 'refuse']),
    provider_order: z.array(z.string().trim().min(1).max(100)).max(50),
});

const removeBodySchema = z.object({ intent: intentSchema });

/** The HTTP answer to each refusal the review service states. */
const REVIEW_REFUSALS: Readonly<
    Record<string, { status: number; code: 'not-found' | 'conflict' | 'invalid-request' }>
> = {
    not_found: { status: 404, code: 'not-found' },
    decided: { status: 409, code: 'conflict' },
    invalid: { status: 400, code: 'invalid-request' },
};

const reviewBodySchema = z
    .object({
        decision: z.enum(['approve', 'reject']),
        reason: z.string().trim().min(1).max(2000),
    })
    .strict();

async function policyRowOf(session: LiveSession, assetClass: string) {
    const rows = await readAll<ListCalibrationPoliciesResponse>(
        session,
        policySubjects.list_calibration_policies_request,
        'policies',
        {
            filter: null,
            as_of: null,
        },
    );
    const found = rows.find((r) => r['asset_class_code'] === assetClass);
    return found === undefined ? undefined : policyRowSchema.parse(found);
}

async function orderRowsOf(session: LiveSession, policyId: string) {
    const rows = await readAll<ListCalibrationProviderOrdersResponse>(
        session,
        orderSubjects.list_calibration_provider_orders_request,
        'provider_orders',
        {
            filter: {
                policy_id: policyId,
                provider_code: null,
                id_one_of: null,
                policy_id_one_of: null,
                provider_code_one_of: null,
            },
            as_of: null,
        },
    );
    return rows.map((r) => orderRowSchema.parse(r));
}

/** A write that did not succeed, as the HTTP failure it means. */
function ensure(result: Awaited<ReturnType<typeof write>>): void {
    if (result.outcome !== 'ok') {
        throw refusal(result);
    }
}

/** The policy is the installation's: only the system tenant changes it. */
function requireSystem(session: LiveSession): void {
    if (session.tenantId !== SYSTEM_TENANT_ID) {
        throw notPermitted(
            'A calibration policy belongs to the installation, so only the system tenant changes it.',
        );
    }
}

function assetClassOf(request: FastifyRequest): string {
    return input(assetClassSchema, (request.params as { assetClass: string }).assetClass);
}

/** The items a feed names, when it names its own instead of deriving them. */
async function itemRowsOf(session: LiveSession, configId: string) {
    const rows = await readAll<ListFxSpotFixingItemsResponse>(
        session,
        itemSubjects.list_fx_spot_fixing_items_request,
        'fx_spot_fixing_items',
        {
            filter: {
                fx_spot_config_id: configId,
                id_one_of: null,
                fx_spot_config_id_one_of: null,
            },
            as_of: null,
        },
    );
    return rows.map((r) => z.looseObject({ id: z.string(), version: z.int() }).parse(r));
}

async function removeItems(
    session: LiveSession,
    intent: z.infer<typeof intentSchema>,
    rows: readonly { readonly id: string }[],
): Promise<void> {
    if (rows.length === 0) {
        return;
    }
    ensure(
        await write(session, itemSubjects.delete_many_fx_spot_fixing_items_request, {
            intent,
            removals: rows.map((r) => ({
                key: { id: r.id },
                precondition: { kind: 'any', version: null },
            })),
        }),
    );
}

/** One candidate a person pins on a feed, as the exception to its policy. */
const pinBodySchema = z.object({
    intent: intentSchema,
    item: z
        .string()
        .trim()
        .max(500)
        .regex(/^oresmd:\/\/fx\/[A-Z]{3}\?(?:.*&)?type=fixing(?:&|$)/),
    provider_code: z
        .string()
        .trim()
        .regex(/^[a-z0-9_]{1,100}$/),
    window_days: z.int().min(2).max(180),
});

export function registerCalibrationRoutes(
    server: FastifyInstance,
    requireSession: (request: FastifyRequest) => LiveSession,
): void {
    /**
     * Calibrate an asset class: the policy it calibrates under, how every feed of it
     * resolves with its candidates, and the coverage counts. Nothing starts; every feed is
     * resolved.
     */
    server.get('/api/synthetic/calibration/:assetClass', async (request) => {
        const session = requireSession(request);
        const assetClass = assetClassOf(request);
        const { configId } = input(readQuerySchema, request.query ?? {});
        const reply = await session.client.callAuthenticated(
            feedSubjects.calibrate_request,
            {
                asset_class_code: assetClass,
                config_id: configId ?? '',
                apply: false,
            } satisfies CalibrateRequest,
            operationReplySchema.extend({
                policy: calibrationPolicySchema,
                feeds: z.array(calibratedFeedSchema).default([]),
            }),
        );
        if (!reply.success) {
            throw upstreamFailure(reply.message);
        }
        const row = reply.policy.from_policy_row
            ? await policyRowOf(session, assetClass)
            : undefined;
        const composed = {
            assetClass,
            policy: { ...reply.policy, version: row?.version ?? null },
            policyWritable: session.tenantId === SYSTEM_TENANT_ID,
            feeds: reply.feeds,
            coverage: calibrationCoverage(reply.feeds),
        };
        if (assetClass !== 'interest_rates') {
            return composed;
        }
        return {
            ...composed,
            ir_curves: await interestRateCoverage(session, reply.feeds, reply.policy.window_days),
        };
    });

    /**
     * Calibrates and stores: each feed's resolved level and deviations are written as a new
     * version of its config, under the reason system.calibration, when they moved. A screen
     * names one feed a call so it can report each feed as it is done. A running feed takes
     * the stored values at its next start.
     */
    server.post('/api/synthetic/calibration/:assetClass/run', async (request) => {
        const session = requireSession(request);
        const assetClass = assetClassOf(request);
        const body = input(runBodySchema, request.body ?? {});
        const reply = await session.client.callAuthenticated(
            feedSubjects.calibrate_request,
            {
                asset_class_code: assetClass,
                config_id: body.configId ?? '',
                apply: true,
            } satisfies CalibrateRequest,
            operationReplySchema.extend({
                feeds: z.array(calibratedFeedSchema).default([]),
            }),
        );
        if (!reply.success) {
            throw upstreamFailure(reply.message);
        }
        if (body.configId !== undefined && reply.feeds.length === 0) {
            throw notFound(`No feed ${body.configId} calibrates in ${assetClass}.`);
        }
        return { assetClass, feeds: reply.feeds };
    });

    /**
     * Approves or rejects one proposed variance calibration, with the reviewer's reason.
     * Approving supersedes the pair's earlier approval and points the pair's GJR-GARCH feeds
     * at the new calibration; the service refuses a calibration that is not proposed.
     */
    server.post('/api/synthetic/calibration/variance/:id/review', async (request) => {
        const session = requireSession(request);
        requireSystem(session);
        const id = input(z.uuid(), (request.params as { id: string }).id);
        const body = input(reviewBodySchema, request.body);
        const reply = await session.client.callAuthenticated(
            feedSubjects.review_variance_calibration_request,
            {
                calibration_id: id,
                decision: body.decision,
                reason: body.reason,
            } satisfies ReviewVarianceCalibrationRequest,
            operationReplySchema.extend({
                refusal: z.string().default(''),
                state: z.string().default(''),
                superseded_id: z.string().default(''),
                repointed_feeds: z.array(z.string()).default([]),
            }),
        );
        // The service says why it refused: a calibration that does not exist, one already decided,
        // or a malformed review. Any other failure is the service's own, not the reviewer's.
        if (!reply.success) {
            const status = REVIEW_REFUSALS[reply.refusal];
            if (status === undefined) {
                throw upstreamFailure(reply.message);
            }
            throw new HttpFailure(status.status, { code: status.code, message: reply.message });
        }
        return {
            state: reply.state,
            supersededId: reply.superseded_id,
            repointedFeeds: reply.repointed_feeds,
            message: reply.message,
        };
    });

    /**
     * States a class's policy. The policy row is written against the version read. When the
     * provider order differs from the stored one, the stored rungs are removed and the list
     * is written from rank 1: a rank is unique within a policy, so rungs are not moved in
     * place. Each write converges, so a save that fails part way is completed by saving again.
     */
    server.put('/api/synthetic/calibration/:assetClass/policy', async (request, reply) => {
        const session = requireSession(request);
        requireSystem(session);
        const assetClass = assetClassOf(request);
        const body = input(policyBodySchema, request.body);
        if (new Set(body.provider_order).size !== body.provider_order.length) {
            throw new HttpFailure(400, {
                code: 'invalid-request',
                message: 'The provider order names a provider twice.',
            });
        }

        const existing = await policyRowOf(session, assetClass);
        const policyId = existing?.id ?? randomUUID();
        const kept = existing ?? z.object(keptPolicyFields).parse({});
        ensure(
            await write(session, policySubjects.put_many_calibration_policies_request, {
                intent: body.intent,
                changes: [
                    {
                        write: {
                            id: policyId,
                            asset_class_code: assetClass,
                            window_days: body.window_days,
                            variance_window_days: kept.variance_window_days,
                            variance_approval_mode: kept.variance_approval_mode,
                            variance_min_returns: kept.variance_min_returns,
                            variance_require_starts_agree: kept.variance_require_starts_agree,
                            variance_min_persistence: kept.variance_min_persistence,
                            variance_max_persistence: kept.variance_max_persistence,
                            variance_min_reaction: kept.variance_min_reaction,
                            allow_inverse: body.allow_inverse,
                            fallback_code: body.fallback_code,
                        } satisfies CalibrationPolicyWrite,
                        precondition:
                            body.version === null
                                ? { kind: 'must_not_exist', version: null }
                                : { kind: 'must_match_version', version: body.version },
                    },
                ],
            }),
        );

        const orders = existing === undefined ? [] : await orderRowsOf(session, policyId);
        const current = [...orders].sort((x, y) => x.rank - y.rank).map((o) => o.provider_code);
        const unchanged =
            current.length === body.provider_order.length &&
            current.every((code, index) => code === body.provider_order[index]);
        if (!unchanged) {
            if (orders.length > 0) {
                ensure(
                    await write(
                        session,
                        orderSubjects.delete_many_calibration_provider_orders_request,
                        {
                            intent: body.intent,
                            removals: orders.map((o) => ({
                                key: { id: o.id },
                                precondition: { kind: 'any', version: null },
                            })),
                        },
                    ),
                );
            }
            if (body.provider_order.length > 0) {
                ensure(
                    await write(
                        session,
                        orderSubjects.put_many_calibration_provider_orders_request,
                        {
                            intent: body.intent,
                            changes: body.provider_order.map((provider, index) => ({
                                write: {
                                    id: randomUUID(),
                                    policy_id: policyId,
                                    provider_code: provider,
                                    rank: index + 1,
                                },
                                precondition: { kind: 'must_not_exist', version: null },
                            })),
                        },
                    ),
                );
            }
        }
        return reply.code(204).send();
    });

    /** Returns a class to automatic calibration: its provider order and its policy row go. */
    server.delete('/api/synthetic/calibration/:assetClass/policy', async (request, reply) => {
        const session = requireSession(request);
        requireSystem(session);
        const assetClass = assetClassOf(request);
        const body = input(removeBodySchema, request.body);
        const existing = await policyRowOf(session, assetClass);
        if (existing === undefined) {
            return reply.code(204).send();
        }
        const orders = await orderRowsOf(session, existing.id);
        if (orders.length > 0) {
            ensure(
                await write(
                    session,
                    orderSubjects.delete_many_calibration_provider_orders_request,
                    {
                        intent: body.intent,
                        removals: orders.map((o) => ({
                            key: { id: o.id },
                            precondition: { kind: 'any', version: null },
                        })),
                    },
                ),
            );
        }
        ensure(
            await write(session, policySubjects.delete_calibration_policy_request, {
                intent: body.intent,
                removal: {
                    key: { id: existing.id },
                    precondition: { kind: 'must_match_version', version: existing.version },
                },
            }),
        );
        return reply.code(204).send();
    });

    /**
     * Pins one candidate on a feed: the feed's own items become that one item, read on or
     * before the clock over the window given. The feed then overrides its class policy. A
     * feed belongs to its tenant, so any tenant pins its own feeds. The old items go first:
     * a write that then fails leaves the feed following its policy, and pinning again
     * completes it.
     */
    server.put('/api/synthetic/calibration/feeds/:id/pin', async (request, reply) => {
        const session = requireSession(request);
        const configId = paramId(request);
        const body = input(pinBodySchema, request.body);
        await removeItems(session, body.intent, await itemRowsOf(session, configId));
        ensure(
            await write(session, itemSubjects.put_many_fx_spot_fixing_items_request, {
                intent: body.intent,
                changes: [
                    {
                        write: {
                            id: randomUUID(),
                            fx_spot_config_id: configId,
                            rank: 1,
                            item: body.item,
                            provider_code: body.provider_code,
                            policy: 'latest_on_or_before_clock',
                            window_days: body.window_days,
                        },
                        precondition: { kind: 'must_not_exist', version: null },
                    },
                ],
            }),
        );
        return reply.code(204).send();
    });

    /** Clears a feed's own items, so it derives its candidates from the policy again. */
    server.delete('/api/synthetic/calibration/feeds/:id/pin', async (request, reply) => {
        const session = requireSession(request);
        const configId = paramId(request);
        const body = input(removeBodySchema, request.body);
        await removeItems(session, body.intent, await itemRowsOf(session, configId));
        return reply.code(204).send();
    });
}
