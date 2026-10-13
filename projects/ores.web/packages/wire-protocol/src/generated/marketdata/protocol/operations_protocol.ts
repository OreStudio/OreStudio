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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: ts_protocol.ts.mustache
 * To modify, update the template and regenerate.
 */
import type { MarketSeries } from '../domain/market_series.js';

/**
 * @brief One typed identity field of a series, as the caller gives it.
 *
 * The name is an oresmd field code, such as ccy or curve_id, and the text is
 * what the caller believes that field holds. The pair is the request's spelling
 * of the identity, not a URI: the server composes the URI through the codec.
 */
export interface SeriesIdentityField {
    name: string;
    text: string;
}

/**
 * @brief Requests the series a typed identity names.
 *
 * The caller states the identity as typed fields and the server resolves it to
 * the series rows that carry it, so no caller builds an oresmd_uri and no
 * caller matches on one. The asset class, the scope, the instrument type and
 * the quote type are stated on their own because the URI grammar puts them in a
 * fixed position; every other identity field travels in `fields`.
 *
 * `party_id` states whose series is wanted. An empty one asks across the
 * parties the caller can see, which is why the answer is a list.
 */
export interface ResolveSeriesIdentityRequest {
    asset: string;
    scope: string;
    instrument_type: string;
    quote_type: string;
    party_id: string;
    /**
     * @brief The identity's remaining fields, such as ccy or curve_id.
     *
     * The other fields the type's schema row declares. A field the caller does not
     * state is left unconstrained, so the read narrows by what it is given.
     */
    fields: SeriesIdentityField[];
}

/**
 * @brief The series the identity resolves to.
 *
 * One row per owning party that holds the identity. Empty when nothing carries
 * it, which is not an error: a scope may hold no series of that kind yet.
 */
export interface ResolveSeriesIdentityResponse {
    series: MarketSeries[];
    success: boolean;
    message: string;
}

/**
 * @brief One field of a schema row.
 *
 * The role says whether the field names the series or a point within it. A field
 * that may be none can be left out of a URI. `codes` holds the tokens a code field
 * accepts and is empty for any other kind.
 */
export interface OresmdFieldSpec {
    name: string;
    role: string;
    may_be_none: boolean;
    kind: string;
    codes: string[];
}

/**
 * @brief One instrument type of the schema, with its fields in key order.
 *
 * `code` is the text a URI writes under `instrument=`. `subject` is the field a
 * URI writes as its path.
 */
export interface OresmdInstrument {
    code: string;
    ore_name: string;
    title: string;
    asset: string;
    subject: string;
    fields: OresmdFieldSpec[];
}

/** @brief One field of an index family, which a fixing URI writes in the row's order. */
export interface OresmdIndexField {
    name: string;
    optional: boolean;
}

/**
 * @brief One family of index.
 *
 * A fixing URI writes `type=fixing` and `index=` with the family's code. The path
 * is the subject and the query holds the other fields in the row's order.
 */
export interface OresmdIndexFamily {
    code: string;
    asset: string;
    subject: string;
    fields: OresmdIndexField[];
}

/**
 * @brief Asks for the oresmd schema.
 *
 * The request carries nothing. The schema is the codec's own table, and it is the
 * same for every caller.
 */
export interface DescribeOresmdSchemaRequest {}

/**
 * @brief The oresmd schema, read from the codec's own tables.
 *
 * `instruments` follows the order of ORE's instrument types and each row's fields
 * follow the order its key writes them. Every list is in a stable order.
 */
export interface DescribeOresmdSchemaResponse {
    assets: string[];
    instruments: OresmdInstrument[];
    quotes: string[];
    quote_titles: string[];
    indexes: OresmdIndexFamily[];
    success: boolean;
    message: string;
}

/**
 * @brief One field of an oresmd URI: its name and the text it holds.
 *
 * The text is in the spelling a URI writes. The path of a URI is the subject, and
 * it is not one of these.
 */
export interface OresmdUriField {
    name: string;
    text: string;
}

/**
 * @brief Asks the codec to read an oresmd URI.
 *
 * The codec's reader is strict, so a URI that is not in its own spelling is
 * refused with the rule that failed. A URI longer than the codec's limit is
 * refused without being read.
 */
export interface ParseOresmdUriRequest {
    uri: string;
}

/**
 * @brief The parts of an oresmd URI, or why the codec refuses it.
 *
 * `kind` is quote, series or fixing. For a quote or a series `instrument` is the
 * text under `instrument=` and `quote` is the text under `quote=`. For a fixing
 * `instrument` is the index family and `quote` is empty. `subject` is the path.
 * `fields` holds the other fields the URI states, in schema order, and leaves
 * out one the URI leaves out.
 */
export interface ParseOresmdUriResponse {
    kind: string;
    instrument: string;
    quote: string;
    asset: string;
    subject: string;
    fields: OresmdUriField[];
    success: boolean;
    message: string;
}

/**
 * @brief Asks the codec to write an oresmd URI from its parts.
 *
 * A field with no text is a field the URI leaves out. The codec decides whether
 * the parts make a URI: a required field that is missing, a field the row does
 * not list, a field given twice and a value of the wrong form are each refused.
 */
export interface ComposeOresmdUriRequest {
    kind: string;
    instrument: string;
    quote: string;
    subject: string;
    fields: OresmdUriField[];
}

/**
 * @brief The oresmd URI the codec writes, or why the parts make none.
 *
 * The URI is the codec's canonical text, so a stored URI compares as text.
 */
export interface ComposeOresmdUriResponse {
    uri: string;
    success: boolean;
    message: string;
}

/**
 * @brief Requests the typed identity of each series the caller can see.
 *
 * A party reads only its own series, so the list is the caller's view and not the
 * tenant's. An empty `party_id` asks across the parties the caller can see.
 */
export interface ListSeriesIdentitiesRequest {
    party_id: string;
}

/**
 * @brief One series in the list, with the typed identity that names it.
 *
 * The identity carries the owning party, so passing it to the snapshot, slice or
 * evolution read names this series and no other party's series of the same kind.
 */
export interface ListedSeries {
    series_id: string;
    identity: ResolveSeriesIdentityRequest;
    /** The declared coordinate fields in order. Empty when no shape is declared. */
    axes: string[];
}

/**
 * @brief The series the caller can see, in a stable order.
 *
 * Only series a curve read can name are listed: a series whose URI carries an
 * instrument type and a quote type. A fixing series is not.
 */
export interface ListSeriesIdentitiesResponse {
    series: ListedSeries[];
    truncated: boolean;
    success: boolean;
    message: string;
}

export interface CrmRateItem {
    crm_name: string;
    base_currency_code: string;
    quote_currency_code: string;
    rate: number;
    /** "fresh" | "stale" | "unavailable" -- see
     * ores.analytics.quant::domain::rate_status.
     */
    status: string;
    /** ISO-8601 UTC, empty when status is "unavailable". */
    as_of: string;
    /** True when this pair has no direct quote and rate is computed as
     * 1/rate from the reverse pair -- only ever set when the request's own
     * `reciprocal` flag was true.
     */
    reciprocal: boolean;
    /** %-change vs. the last value served for this exact pair (as displayed,
     * i.e. post-reciprocal) within this (tenant, party, crm_name) scope --
     * shared across every session polling that same CRM, not per-connection.
     * Unset for the first observation of a pair, or when status is
     * "unavailable".
     */
    delta_pct: number | null;
}

/**
 * @brief Request a single CRM rate (direct or derived) for a party from
 * one specific named CRM.
 *
 * Pull-only, computed on demand from that CRM's live rate_engine -- see
 * the CRM story's architecture decision to never broadcast the full
 * derived set. party_id is explicit (not derived from session) because
 * a tenant may have many parties and no single "current party" concept
 * exists at the request-context layer today. crm_name is required (not
 * optional/defaulted) because a party may have more than one enabled
 * CRM and the same pair can resolve to a different rate in each.
 */
export interface GetCrmRateRequest {
    party_id: string;
    crm_name: string;
    base_currency_code: string;
    quote_currency_code: string;
}

export interface GetCrmRateResponse {
    success: boolean;
    message: string;
    rate: CrmRateItem;
}

/**
 * @brief Request every currently-configured CRM rate for a party in one
 * call -- the union of that party's enabled driver pairs and enabled
 * derived pairs, resolved via a single rate_engine::rates() batch (one
 * atomic snapshot load, not N single-pair round trips).
 *
 * When crm_name is empty, returns rates from *every* enabled CRM the
 * party has, each item tagged with which CRM it came from. When
 * crm_name is set, scopes to just that one CRM.
 */
export interface GetCrmRatesRequest {
    party_id: string;
    crm_name: string;
    /** When true, a configured pair with no direct quote but whose reverse
     * pair *is* configured is backfilled with the reverse's computed
     * reciprocal (1/rate) -- see
     * ores.analytics.quant::service::rate_reciprocator. When the reverse
     * pair is itself also directly configured, its own direct rate is
     * served instead of a synthesised reciprocal.
     */
    reciprocal: boolean;
}

export interface GetCrmRatesResponse {
    success: boolean;
    message: string;
    rates: CrmRateItem[];
}

/**
 * @brief Request to bootstrap and republish an ir_curve_bootstrap_config's
 * output as of a given snapshot -- the on-demand trigger for
 * curve_republish_service::republish(), callable identically from the shell,
 * an HTTP caller, or a scheduler job. No review/approval gate here; that is a
 * separate, later layer on top of this always-auto-publishing mechanism.
 */
export interface RepublishCurveRequest {
    bootstrap_config_id: string;
    as_of: string;
}

export interface RepublishCurveResponse {
    success: boolean;
    message: string;
}

/**
 * @brief Request to bootstrap (but not publish) an
 * ir_curve_bootstrap_config's output as of a given snapshot -- the on-demand
 * trigger for curve_republish_service::compute(), the preview half of the
 * compute/publish split. Same shape as republish_curve_request; kept as its
 * own request/subject rather than an "options" flag on that one, since the
 * two are genuinely different actions with different permission codes (a
 * compute-only preview vs. a write that publishes new market_observations).
 */
export interface ComputeCurveRequest {
    bootstrap_config_id: string;
    as_of: string;
}

/**
 * @brief One computed point, echoed back from
 * curve_republish_service::compute() -- unlike a published
 * market_observation, this never touches the database.
 */
export interface ComputedCurvePoint {
    point_id: string;
    date: string;
    discount_factor: number;
}

export interface ComputeCurveResponse {
    success: boolean;
    message: string;
    points: ComputedCurvePoint[];
}

/**
 * @brief One instant of a slice: the object as it stood then.
 */
export interface SeriesSliceInstant {
    as_of: string;
    /**
     * @brief The value at each declared coordinate, index for index with the
     * response's coordinates.
     *
     * The empty string is a hole: the shape declares the coordinate and no point
     * holds a value for it at this instant. A short vector would be read as a
     * shorter object, which is the mistake the aligned shape exists to prevent.
     */
    values: string[];
}

/**
 * @brief Requests one component of a composite object over a range of
 * instants.
 *
 * The object is named the way the resolver names one, so no caller builds an
 * oresmd URI and no caller matches on one. The component is the value the
 * object holds on one of its axes; an object with a single axis has one
 * component, the whole object, and leaves the component field empty.
 */
export interface GetSeriesSliceRequest {
    /** The object, as the typed identity the resolver already takes. */
    identity: ResolveSeriesIdentityRequest;
    /**
     * @brief The axis the component fixes, an oresmd field code such as
     * strike_label.
     *
     * Empty for an object with one coordinate axis, whose one component is the
     * whole object.
     */
    component_field: string;
    /**
     * @brief The value the component fixes on that axis, such as ATM, in the
     * spelling the shape stores.
     */
    component_value: string;
    /** The first market instant of the range, inclusive. */
    from_instant: string;
    /** The last market instant of the range, inclusive. */
    to_instant: string;
}

/**
 * @brief The component at each instant in the range.
 *
 * The ladder is stated once, because it belongs to the shape and not to the
 * points: coordinates holds the declared values of the axis the component
 * varies over, in the order the shape stores them, and every instant's values
 * are index for index with it.
 */
export interface GetSeriesSliceResponse {
    success: boolean;
    /**
     * @brief Why the read was refused, when it was.
     *
     * A component the shape does not declare is refused here, naming the axis or
     * the value that is missing.
     */
    message: string;
    /**
     * @brief The axis the component varies over, an oresmd field code such as
     * expiry.
     *
     * Empty when the response holds no instant.
     */
    coordinate_field: string;
    /**
     * @brief The declared values of the coordinate field, in the order the shape
     * stores them.
     *
     * Every instant's values are index for index with this.
     */
    coordinates: string[];
    /** Oldest first, one entry per distinct market instant in the range. */
    instants: SeriesSliceInstant[];
}

/**
 * @brief One node of a composite object: its coordinate labels and the change
 * it shows over the range.
 */
export interface EvolutionNode {
    /**
     * @brief One label per declared axis, index for index with the response's
     * coordinate_fields.
     */
    coordinates: string[];
    /**
     * @brief The change the node shows over the range.
     *
     * One of never_quoted, persistent, added, dropped or intermittent, which is
     * the first of those that holds. never_quoted is a node the shape declares and
     * no instant in the range carries, which is what tells it apart from a node the
     * object does not have: that node is not in the response at all.
     */
    status: string;
}

/**
 * @brief One instant of the evolution: the object as it stood then.
 */
export interface EvolutionInstant {
    as_of: string;
    /**
     * @brief The value at each node, index for index with the response's nodes.
     *
     * The empty string is a hole: the shape declares the node and no point held a
     * value for it at this instant.
     */
    values: string[];
}

/**
 * @brief Requests a whole composite object as instants by nodes.
 *
 * The object is named the way the resolver names one, so no caller builds an
 * oresmd URI and no caller matches on one. The instants are either the ones the
 * object holds between two instants or the set the caller states.
 */
export interface GetSeriesEvolutionRequest {
    /** The object, as the typed identity the resolver already takes. */
    identity: ResolveSeriesIdentityRequest;
    /** The first market instant of the range, inclusive, when instants is empty. */
    from_instant: string;
    /** The last market instant of the range, inclusive, when instants is empty. */
    to_instant: string;
    /**
     * @brief The instants the caller states, which replace the range when the
     * vector is not empty.
     *
     * Each one is read as the object stood at it, whether or not the object holds a
     * point at exactly that instant. A caller that wants the object at the month
     * ends states them; a caller that wants everything it holds between two
     * instants states the range and leaves this empty.
     */
    instants: string[];
}

/**
 * @brief The object at each instant, against one grid of declared nodes.
 *
 * The grid is stated once, because it belongs to the shape and not to the
 * points: nodes holds every combination of the declared axis values, each with
 * its labels and the change it shows over the range, and every instant's values
 * are index for index with it. A node the shape declares and no instant carries
 * is a cell all the same, which is what tells it apart from a node the object
 * does not have.
 */
export interface GetSeriesEvolutionResponse {
    success: boolean;
    /** Why the read was refused, when it was. */
    message: string;
    /**
     * @brief The axis names, in the order the shape stores them.
     *
     * Every node's coordinates are index for index with this.
     */
    coordinate_fields: string[];
    /**
     * @brief The declared grid, with the last axis varying fastest.
     *
     * Every combination of the declared axis values is a node, whether or not any
     * instant carries it.
     */
    nodes: EvolutionNode[];
    /** Oldest first, one entry per distinct market instant read. */
    instants: EvolutionInstant[];
}

/**
 * @brief Where one value of a snapshot came from.
 */
export interface PointProvenance {
    /**
     * @brief One of quoted, derived or manual.
     *
     * A point with no annex row is quoted. The empty string marks a hole, which has
     * no source.
     */
    source_kind: string;
    /** Who set the point. Empty for a quoted point and for a hole. */
    modified_by: string;
    /** Why the point was set. Empty for a quoted point and for a hole. */
    change_reason_code: string;
    /** The free-text reason for the change. Empty when none was given. */
    change_commentary: string;
    /** When the annex row was recorded. The epoch for a quoted point and for a hole. */
    recorded_at: string;
    /** The recipe that built a derived point. Empty unless the point is derived. */
    derivation_config_id: string;
    /** The recipe version the point was built under. Zero unless the point is derived. */
    derivation_config_version: number;
    /** The as-of of the upstream data the derivation read. The epoch unless the point is derived. */
    source_as_of: string;
}

/**
 * @brief Requests a composite object as it stands now, as one instant by nodes.
 *
 * The object is named the way the resolver names one, as the slice read and the
 * evolution read name it, so the three cannot disagree about what the object is.
 * The instant is always now; there is no as-of-in-the-past parameter, because the
 * evolution read answers that question.
 */
export interface GetCurveSnapshotRequest {
    /** The object, as the typed identity the resolver already takes. */
    identity: ResolveSeriesIdentityRequest;
    /**
     * @brief Whether the response states where each value came from.
     *
     * The annex is read only when this is set, so a caller that does not ask pays
     * nothing for it. The provenance is the current annex row of each point shown, the
     * same generation the values come from, not the row as it stood at the instant.
     */
    include_provenance: boolean;
}

/**
 * @brief The object at one instant, against the same grid of declared nodes the
 * evolution read states.
 *
 * The evolution read of that object gives the same coordinate fields and the same
 * nodes. The snapshot carries the latest value of a node forward from before the
 * instant, which the evolution does not, and adds the age of each value, because a
 * composite of several market states must not be taken for one.
 */
export interface GetCurveSnapshotResponse {
    success: boolean;
    /** Why the read was refused, when it was. */
    message: string;
    /**
     * @brief The instant the snapshot is as of, which is what an age is measured
     * from.
     *
     * Without it a reader measures an age from its own clock, so clock skew shows
     * up as staleness that is not in the curve.
     */
    as_of: string;
    /** The axis names, in the order the shape stores them. */
    coordinate_fields: string[];
    /**
     * @brief The declared grid, with the last axis varying fastest.
     *
     * The status of a node is empty: one instant shows no change.
     */
    nodes: EvolutionNode[];
    /**
     * @brief The value at each node, index for index with nodes.
     *
     * The empty string is a hole: the shape declares the node and no point held a
     * value for it.
     */
    values: string[];
    /** When each point was recorded, index-for-index with observations: the
     * bitemporal valid_from of the row it was read from. Two ages are readable
     * from a point and they answer different questions: the snapshot instant
     * minus the point's instant is market staleness, the age of the market state
     * it belongs to; now minus its record time is record staleness, how long ago
     * the value arrived.
     */
    recorded_at: string[];
    /**
     * @brief Where each value came from, index for index with nodes.
     *
     * Empty when the request did not ask for provenance.
     */
    provenance: PointProvenance[];
    /**
     * @brief The age of the snapshot's oldest point, in seconds.
     *
     * Market staleness, not record staleness: the age of the market state the
     * point belongs to, which is what decides whether a drawn curve is one market
     * read or a stitching of several. Zero for an empty snapshot.
     */
    oldest_age_seconds: number;
    /**
     * @brief The spread between the oldest and the newest point, in seconds.
     *
     * The number that measures the mixing: points that share one instant have a
     * spread of zero however old that instant is, and a curve stitched across
     * market horizons does not. A point whose instant is after the snapshot
     * instant carries no age at all, and one exactly at it is age zero and counts.
     */
    spread_seconds: number;
    /**
     * @brief Whether the snapshot is mixed enough that a view must not draw it
     * silently.
     *
     * The threshold is stated by the evolution journey, not by this message: the
     * read reports the crossing, and the view decides what to show.
     */
    warning: boolean;
}

/**
 * @brief Request to import ORE market.txt and/or fixings.txt content.
 *
 * The service parses both files, names each key and index through the ORE
 * codecs, upserts market series catalog entries, and bulk-inserts
 * observations and fixings. Either content field may be empty if only one
 * file type is being imported.
 */
export interface ImportMarketDataRequest {
    /**
     * @brief Full text content of an ORE market.txt file.
     *
     * Supports both YYYYMMDD and YYYY-MM-DD date formats, and whitespace or
     * comma delimiters. Empty string means no market observations to import.
     */
    market_data_content: string;
    /**
     * @brief Full text content of an ORE fixings.txt file.
     *
     * Same format rules as market_data_content. Empty string means no fixings
     * to import.
     */
    fixings_content: string;
    /**
     * @brief Tag stamped on every imported market_observation's source field.
     *
     * Distinguishes where an observation came from (e.g. "ore.reference" for a
     * named ORE example vintage) so downstream consumers can query for a specific
     * import rather than an untagged mix. Empty string (the default) preserves
     * the pre-existing untagged behaviour.
     */
    source: string;
    /**
     * @brief How a (date, key) pair repeated within market_data_content or
     * fixings_content is handled.
     *
     * false (default): the later occurrence wins (last-line-wins), each repeat is
     * reported in @c warnings, and the import proceeds -- it matches real ORE
     * example data, which does contain such repeats.
     *
     * true: same de-duplication, but each repeat is reported in @c errors
     * instead, and the affected content (market data and/or fixings, whichever
     * contained repeats) is not persisted -- a strict mode for callers that want
     * repeats treated as a hard failure.
     */
    duplicates_are_errors: boolean;
}

export interface ImportMarketDataResponse {
    success: boolean;
    message: string;
    series_count: number;
    observation_count: number;
    fixing_count: number;
    /**
     * @brief Non-fatal issues from de-duplicating repeated (date, key) pairs.
     *
     * One string per repeat, e.g. "market data line 12: duplicate key '...' --
     * superseded by line 34". Populated regardless of duplicates_are_errors;
     * which of warnings/errors they land in depends on it.
     */
    warnings: string[];
    /**
     * @brief Same shape as warnings, populated only when duplicates_are_errors
     * is true.
     *
     * A non-empty errors means the content with repeats (market data and/or
     * fixings) was not persisted.
     */
    errors: string[];
}

/**
 * @brief Per-kind outcome counts of one folder start/stop request, keyed
 * by the producer kind string (e.g. "fx_spot", "ir_curve") -- see
 * ores.synthetic.api's feed_factory kind constants. The protocol names no
 * asset class: a new producer kind appears as a new map key.
 */
export interface FeedKindCounts {
    started: number;
    already_running: number;
    skipped: number;
}

/**
 * @brief Request to start every feed under a synthetic folder subtree.
 *
 * folder_id may name a Root, Collection, asset-class, or instrument-type
 * folder -- the service resolves the whole subtree server-side (via
 * ores.synthetic.folder's hierarchy_fn) and starts every feed row whose
 * folder_id falls anywhere in it, of every asset class, dispatching through
 * the producer factory. One request starts the subtree, so a caller need
 * not enumerate every pair; it works the same from ores.shell, an HTTP
 * caller, or a workflow step.
 */
export interface StartFeedsUnderFolderRequest {
    folder_id: string;
}

export interface StartFeedsUnderFolderResponse {
    success: boolean;
    message: string;
    started: number;
    already_running: number;
    skipped: number;
    /** The per-kind breakdown of the three counts above, which aggregate across
     * every kind and are the summary clients print. Both are always populated.
     */
    by_kind: Record<string, FeedKindCounts>;
}

/**
 * @brief Request to stop every running feed under a synthetic folder
 * subtree. Same folder_id semantics as start_feeds_under_folder_request.
 */
export interface StopFeedsUnderFolderRequest {
    folder_id: string;
}

export interface StopFeedsUnderFolderResponse {
    success: boolean;
    message: string;
    stopped: number;
    stopped_by_kind: Record<string, number>;
}

/**
 * @brief Vintage-availability status for one feed config of any asset class,
 * computed live (not stored) against market_observation.
 */
export interface VintageValidityEntry {
    /** The feed config row's own id: fx_spot_generation_config::id for an FX
     * feed, ir_curve_generation_config::id for an IR curve feed.
     */
    config_id: string;
    /** The registered feed kind the row belongs to, so a caller can tell the
     * asset classes apart without probing.
     */
    kind: string;
    /** False for price_source = "fixed" feeds -- the vintage check does not
     * apply to them, so `valid` is meaningless and should not be shown.
     */
    applicable: boolean;
    valid: boolean;
}

/**
 * @brief Request the vintage-availability status of every feed visible to
 * the caller, computed at read time (no persisted/cached column) so it is
 * always accurate -- including immediately after a data import that added
 * no new feed rows, only new market_observation ones.
 */
export interface GetVintageValidityRequest {}

export interface GetVintageValidityResponse {
    success: boolean;
    message: string;
    entries: VintageValidityEntry[];
}

/**
 * @brief Exports the tenant's market data to object storage as ORE text.
 *
 * The engine reads a market data file its run document names, and nothing else
 * can fill that file, so what is written here is that body and no other form
 * of the data.
 */
export interface ExportMarketDataToStorageRequest {
    storage_bucket: string;
    /**
     * @brief Where the market data body goes.
     */
    storage_key: string;
    /**
     * @brief Where the fixings body goes.
     *
     * The engine reads fixings from a file of their own, so the body goes to an
     * object of its own. An empty body is written too, because a run whose run
     * document names the file must find it.
     */
    fixings_storage_key: string;
}

export interface ExportMarketDataToStorageResponse {
    success: boolean;
    message: string;
    series_count: number;
    storage_key: string;
    /**
     * @brief Where the fixings body was written.
     */
    fixings_storage_key: string;
}

/**
 * @brief Projects the identity of every series whose stored row is stale.
 *
 * The projection is written by the series write path, so only rows that
 * predate it, that were written outside it, or that an earlier projector
 * spelled differently are stale. This compares each series' fresh projection
 * with the stored row and writes only the difference.
 */
export interface BackfillSeriesIdentityRequest {
    /**
     * @brief The owning party to narrow to, or empty for every party the caller
     * can see.
     */
    party_id: string;
}

export interface BackfillSeriesIdentityResponse {
    success: boolean;
    message: string;
    /**
     * @brief How many series were projected by this call.
     *
     * A series whose stored row already matched the fresh projection is not
     * counted: only the rows this call wrote are.
     */
    projected_count: number;
    /**
     * @brief How many current series already carried the projection this call
     * would write.
     */
    already_projected_count: number;
    /**
     * @brief How many of the projected series carried a URI neither codec reads.
     *
     * Their rows are written with their kind and no field value, so the series is
     * findable and never mistaken for one whose identity was read. The count is
     * what says how many identities the database could not state.
     */
    unreadable_count: number;
}

/**
 * @brief Keys a manual point on a node of a named object.
 *
 * The coordinates are checked against the shape the object declares, so a node
 * the shape does not hold is refused. The point is stamped at the instant the
 * server receives the request, and the caller is its author.
 */
export interface KeyManualPointRequest {
    /** The object, as the typed identity the reads take. */
    identity: ResolveSeriesIdentityRequest;
    /** The node: one coordinate per declared axis, in the shape's order. */
    coordinates: string[];
    /** The value to key, as a number written in text. */
    value: string;
    /** The change reason code that says why the point is keyed. */
    change_reason_code: string;
    /** The free-text reason for the correction. */
    change_commentary: string;
}

export interface KeyManualPointResponse {
    success: boolean;
    message: string;
}

/**
 * @brief Clears the manual point on a node of a named object.
 *
 * Every manual point the node holds is released, whatever instant it was keyed
 * at. The history keeps the value it replaced.
 */
export interface ClearManualPointRequest {
    /** The object, as the typed identity the reads take. */
    identity: ResolveSeriesIdentityRequest;
    /** The node: one coordinate per declared axis, in the shape's order. */
    coordinates: string[];
}

export interface ClearManualPointResponse {
    success: boolean;
    message: string;
}

/**
 * @brief Request to write the tenant's market data back out as ORE text.
 *
 * Takes no arguments: the export is the whole tenant's, because that is the
 * scope the files have. An ORE market.txt is a snapshot of everything a run
 * needs, not a slice of one series, and an export that could name a subset
 * would produce a file that does not reproduce anything.
 */
export interface ExportMarketDataRequest {}

/**
 * @brief The two file bodies, as the caller's own text to write.
 *
 * The service returns content rather than writing it: the caller owns the file
 * system, and the same bodies are what the round-trip test compares against
 * the file it imported.
 */
export interface ExportMarketDataResponse {
    success: boolean;
    message: string;
    /**
     * @brief The market.txt body: one DATE<TAB>KEY<TAB>VALUE line per
     * observation, empty when the tenant has no observations.
     */
    market_data_content: string;
    /**
     * @brief The fixings.txt body: one DATE<TAB>INDEX<TAB>VALUE line per
     * fixing, empty when the tenant has no fixings.
     */
    fixings_content: string;
    series_count: number;
    observation_count: number;
    fixing_count: number;
}

/**
 * @brief One live observation of one market datum.
 *
 * The datum is named by its oresmd quote URI, which carries the asset class,
 * the instrument and every field, so no asset class has a tick type of its
 * own. A curve is the set of ticks one producer publishes with one
 * observation time.
 */
export interface MarketTick {
    /** The quote URI of the datum observed, under the strict contract. */
    oresmd_uri: string;
    /** The observed value as decimal text, so it keeps the producer's digits. */
    value: string;
    /** When the value was observed. */
    observation_time: string;
    /** The producer, unique within a tenant; its feed bindings name the consumers. */
    source: string;
}

/**
 * @brief Asks what each feed binding of the caller has written.
 *
 * The request carries nothing. A party reads only its own bindings and its own
 * observations, so the answer is the caller's view.
 */
export interface ListFeedBindingActivityRequest {}

/**
 * @brief The newest observation a binding's source has written.
 *
 * A binding names a source and not a series, and a source writes one series, so the
 * series of the source's newest observation is the series the binding feeds. A
 * source that has written nothing for the party has neither a series nor a tick,
 * and both are empty.
 */
export interface FeedBindingActivity {
    source_name: string;
    series_id: string;
    last_tick_at: string;
}

/**
 * @brief The activity of each feed binding of the caller.
 *
 * One entry for each binding, whether or not its source has written anything, so a
 * binding with no tick stays in the answer and the screen can say so.
 */
export interface ListFeedBindingActivityResponse {
    activities: FeedBindingActivity[];
    success: boolean;
    message: string;
}

export const subjects = {
    resolve_series_identity_request: 'marketdata.v1.ops.resolve_series_identity',
    describe_oresmd_schema_request: 'marketdata.v1.ops.describe_oresmd_schema',
    parse_oresmd_uri_request: 'marketdata.v1.ops.parse_oresmd_uri',
    compose_oresmd_uri_request: 'marketdata.v1.ops.compose_oresmd_uri',
    list_series_identities_request: 'marketdata.v1.ops.list_series_identities',
    get_crm_rate_request: 'marketdata.v1.ops.get_crm_rate',
    get_crm_rates_request: 'marketdata.v1.ops.get_crm_rates',
    republish_curve_request: 'marketdata.v1.ops.republish_curve',
    compute_curve_request: 'marketdata.v1.ops.compute_curve',
    get_series_slice_request: 'marketdata.v1.ops.get_series_slice',
    get_series_evolution_request: 'marketdata.v1.ops.get_series_evolution',
    get_curve_snapshot_request: 'marketdata.v1.curve_snapshot.get',
    import_market_data_request: 'marketdata.v1.ops.import_market_data',
    start_feeds_under_folder_request: 'marketdata.v1.ops.start_feeds_under_folder',
    stop_feeds_under_folder_request: 'marketdata.v1.ops.stop_feeds_under_folder',
    get_vintage_validity_request: 'marketdata.v1.ops.get_vintage_validity',
    export_market_data_to_storage_request: 'marketdata.v1.ops.export_market_data_to_storage',
    backfill_series_identity_request: 'marketdata.v1.ops.backfill_series_identity',
    key_manual_point_request: 'marketdata.v1.ops.key_manual_point',
    clear_manual_point_request: 'marketdata.v1.ops.clear_manual_point',
    export_market_data_request: 'marketdata.v1.ops.export_market_data',
    market_tick: 'marketdata.v1.ops.market_tick',
    list_feed_binding_activity_request: 'marketdata.v1.ops.list_feed_binding_activity',
} as const;
/**
 * Whether a message needs an established session first. An operation that
 * produces the session cannot present one, so a client reads this rather than
 * assuming every call carries a token.
 */
export const requiresSession = {
    resolve_series_identity_request: true,
    describe_oresmd_schema_request: true,
    parse_oresmd_uri_request: true,
    compose_oresmd_uri_request: true,
    list_series_identities_request: true,
    get_crm_rate_request: true,
    get_crm_rates_request: true,
    republish_curve_request: true,
    compute_curve_request: true,
    get_series_slice_request: true,
    get_series_evolution_request: true,
    get_curve_snapshot_request: true,
    import_market_data_request: true,
    start_feeds_under_folder_request: true,
    stop_feeds_under_folder_request: true,
    get_vintage_validity_request: true,
    export_market_data_to_storage_request: true,
    backfill_series_identity_request: true,
    key_manual_point_request: true,
    clear_manual_point_request: true,
    export_market_data_request: true,
    market_tick: false,
    list_feed_binding_activity_request: true,
} as const;
