/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
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
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_MARKETDATA_API_MESSAGING_OPERATIONS_PROTOCOL_HPP
#define ORES_MARKETDATA_API_MESSAGING_OPERATIONS_PROTOCOL_HPP

#include "ores.marketdata.api/domain/market_observation.hpp"
#include <chrono>
#include <cstdint>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::messaging {

struct crm_rate_item {
    std::string crm_name;
    std::string base_currency_code;
    std::string quote_currency_code;
    double rate = 0.0;
    /** "fresh" | "stale" | "unavailable" -- see
     * ores.analytics.quant::domain::rate_status.
     */
    std::string status;
    /** ISO-8601 UTC, empty when status is "unavailable". */
    std::string as_of;
    /** True when this pair has no direct quote and rate is computed as
     * 1/rate from the reverse pair -- only ever set when the request's own
     * `reciprocal` flag was true.
     */
    bool reciprocal = false;
    /** %-change vs. the last value served for this exact pair (as displayed,
     * i.e. post-reciprocal) within this (tenant, party, crm_name) scope --
     * shared across every session polling that same CRM, not per-connection.
     * Unset for the first observation of a pair, or when status is
     * "unavailable".
     */
    std::optional<double> delta_pct;
};

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
struct get_crm_rate_request {
    using response_type = struct get_crm_rate_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.crm.rate";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string party_id;
    std::string crm_name;
    std::string base_currency_code;
    std::string quote_currency_code;
};

struct get_crm_rate_response {
    bool success = true;
    std::string message;
    crm_rate_item rate;
};

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
struct get_crm_rates_request {
    using response_type = struct get_crm_rates_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.crm.rates";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string party_id;
    std::string crm_name;
    /** When true, a configured pair with no direct quote but whose reverse
     * pair *is* configured is backfilled with the reverse's computed
     * reciprocal (1/rate) -- see
     * ores.analytics.quant::service::rate_reciprocator. When the reverse
     * pair is itself also directly configured, its own direct rate is
     * served instead of a synthesised reciprocal.
     */
    bool reciprocal = false;
};

struct get_crm_rates_response {
    bool success = true;
    std::string message;
    std::vector<crm_rate_item> rates;
};

/**
 * @brief Request to bootstrap and republish an ir_curve_bootstrap_config's
 * output as of a given snapshot -- the on-demand trigger for
 * curve_republish_service::republish(), callable identically from the shell,
 * an HTTP caller, or a scheduler job. No review/approval gate here; that is a
 * separate, later layer on top of this always-auto-publishing mechanism.
 */
struct republish_curve_request {
    using response_type = struct republish_curve_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.curve_bootstrap.republish";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string bootstrap_config_id;
    std::chrono::system_clock::time_point as_of;
};

struct republish_curve_response {
    bool success = false;
    std::string message;
};

/**
 * @brief Request to bootstrap (but not publish) an
 * ir_curve_bootstrap_config's output as of a given snapshot -- the on-demand
 * trigger for curve_republish_service::compute(), the preview half of the
 * compute/publish split. Same shape as republish_curve_request; kept as its
 * own request/subject rather than an "options" flag on that one, since the
 * two are genuinely different actions with different permission codes (a
 * compute-only preview vs. a write that publishes new market_observations).
 */
struct compute_curve_request {
    using response_type = struct compute_curve_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.curve_bootstrap.compute";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string bootstrap_config_id;
    std::chrono::system_clock::time_point as_of;
};

/**
 * @brief One computed point, echoed back from
 * curve_republish_service::compute() -- unlike a published
 * market_observation, this never touches the database.
 */
struct computed_curve_point {
    std::string point_id;
    std::chrono::year_month_day date = {};
    double discount_factor = 0.0;
};

struct compute_curve_response {
    bool success = false;
    std::string message;
    std::vector<computed_curve_point> points;
};

/**
 * @brief Requests the latest-as-of snapshot of a curve/grid series: one
 * observation per point_id, reconstructed from independently-ticking
 * market_observation rows.
 *
 * oresmd_uri identifies the series, the same identity every market_series
 * row is keyed by and the one the feed's ticks land under -- the caller does
 * not need to know the internal series_id, and it does not need the
 * registry's decomposition of the key either. Always "latest" (now) for the
 * as-of time; no as-of-in-the-past parameter yet.
 */
struct get_curve_snapshot_request {
    using response_type = struct get_curve_snapshot_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.curve-snapshot.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string oresmd_uri;
};

struct get_curve_snapshot_response {
    std::vector<ores::marketdata::domain::market_observation> observations;
    bool success = false;
    std::string message;
};

/**
 * @brief Requests a curve-evolution view: bucket_count as-of snapshots, one
 * every bucket_seconds, ending now.
 */
struct get_curve_snapshot_buckets_request {
    using response_type = struct get_curve_snapshot_buckets_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.curve-snapshot.buckets";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string oresmd_uri;
    std::int64_t bucket_seconds = 1800;
    std::uint32_t bucket_count = 5;
};

struct get_curve_snapshot_buckets_response {
    /** Oldest to newest, one entry per bucket; a bucket with no observations
     * at/before its boundary is an empty vector.
     */
    std::vector<std::vector<ores::marketdata::domain::market_observation>> buckets;
    bool success = false;
    std::string message;
};

/**
 * @brief Request to import ORE market.txt and/or fixings.txt content.
 *
 * The service parses both files, names each key and index through the ORE
 * codecs, upserts market series catalog entries, and bulk-inserts
 * observations and fixings. Either content field may be empty if only one
 * file type is being imported.
 */
struct import_market_data_request {
    using response_type = struct import_market_data_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.import";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief Full text content of an ORE market.txt file.
     *
     * Supports both YYYYMMDD and YYYY-MM-DD date formats, and whitespace or
     * comma delimiters. Empty string means no market observations to import.
     */
    std::string market_data_content;
    /**
     * @brief Full text content of an ORE fixings.txt file.
     *
     * Same format rules as market_data_content. Empty string means no fixings
     * to import.
     */
    std::string fixings_content;
    /**
     * @brief Tag stamped on every imported market_observation's source field.
     *
     * Distinguishes where an observation came from (e.g. "ore.reference" for a
     * named ORE example vintage) so downstream consumers can query for a specific
     * import rather than an untagged mix. Empty string (the default) preserves
     * the pre-existing untagged behaviour.
     */
    std::string source;
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
    bool duplicates_are_errors = false;
};

struct import_market_data_response {
    bool success = false;
    std::string message;
    int series_count = 0;
    int observation_count = 0;
    int fixing_count = 0;
    /**
     * @brief Non-fatal issues from de-duplicating repeated (date, key) pairs.
     *
     * One string per repeat, e.g. "market data line 12: duplicate key '...' --
     * superseded by line 34". Populated regardless of duplicates_are_errors;
     * which of warnings/errors they land in depends on it.
     */
    std::vector<std::string> warnings;
    /**
     * @brief Same shape as warnings, populated only when duplicates_are_errors
     * is true.
     *
     * A non-empty errors means the content with repeats (market data and/or
     * fixings) was not persisted.
     */
    std::vector<std::string> errors;
};

/**
 * @brief Per-kind outcome counts of one folder start/stop request, keyed
 * by the producer kind string (e.g. "fx_spot", "ir_curve") -- see
 * ores.synthetic.api's feed_factory kind constants. The protocol names no
 * asset class: a new producer kind appears as a new map key.
 */
struct feed_kind_counts {
    int started = 0;
    int already_running = 0;
    int skipped = 0;
};

/**
 * @brief Request to start every feed under a synthetic folder subtree.
 *
 * folder_id may name a Root, Collection, asset-class, or instrument-type
 * folder -- the service resolves the whole subtree server-side (via
 * ores.synthetic.folder's hierarchy_fn) and starts every feed row whose
 * folder_id falls anywhere in it, of every asset class, dispatching through
 * the producer factory. One request expresses what used to require
 * client-side enumeration of every pair; works identically from ores.shell,
 * an HTTP caller, or a workflow step.
 */
struct start_feeds_under_folder_request {
    using response_type = struct start_feeds_under_folder_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_feed_configs.start_folder";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string folder_id;
};

struct start_feeds_under_folder_response {
    bool success = false;
    std::string message;
    int started = 0;
    int already_running = 0;
    int skipped = 0;
    /** The per-kind breakdown of the three counts above, which aggregate across
     * every kind and are the summary clients print. Both are always populated.
     */
    std::map<std::string, feed_kind_counts> by_kind;
};

/**
 * @brief Request to stop every running feed under a synthetic folder
 * subtree. Same folder_id semantics as start_feeds_under_folder_request.
 */
struct stop_feeds_under_folder_request {
    using response_type = struct stop_feeds_under_folder_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_feed_configs.stop_folder";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string folder_id;
};

struct stop_feeds_under_folder_response {
    bool success = false;
    std::string message;
    int stopped = 0;
    std::map<std::string, int> stopped_by_kind;
};

/**
 * @brief Vintage-availability status for one fx_spot_generation_config,
 * computed live (not stored) against market_observation.
 */
struct vintage_validity_entry {
    std::string fx_spot_generation_config_id;
    /** False for price_source = "fixed" feeds -- the vintage check does not
     * apply to them, so `valid` is meaningless and should not be shown.
     */
    bool applicable = false;
    bool valid = false;
};

/**
 * @brief Request the vintage-availability status of every feed visible to
 * the caller, computed at read time (no persisted/cached column) so it is
 * always accurate -- including immediately after a data import that added
 * no new feed rows, only new market_observation ones.
 */
struct get_vintage_validity_request {
    using response_type = struct get_vintage_validity_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.market_feed_configs.vintage_validity";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

struct get_vintage_validity_response {
    bool success = false;
    std::string message;
    std::vector<vintage_validity_entry> entries;
};

/**
 * @brief Exports all market data series to object storage.
 *
 * Fetches all active market series for the tenant, serialises to JSON,
 * compresses with gzip, and uploads to storage. Returns the storage key
 * on success.
 */
struct export_market_data_to_storage_request {
    using response_type = struct export_market_data_to_storage_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.series.export-to-storage";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string storage_bucket;
    std::string storage_key;
};

struct export_market_data_to_storage_response {
    bool success = false;
    std::string message;
    int series_count = 0;
    std::string storage_key;
};

/**
 * @brief Request to write the tenant's market data back out as ORE text.
 *
 * Takes no arguments: the export is the whole tenant's, because that is the
 * scope the files have. An ORE market.txt is a snapshot of everything a run
 * needs, not a slice of one series, and an export that could name a subset
 * would produce a file that does not reproduce anything.
 */
struct export_market_data_request {
    using response_type = struct export_market_data_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.export";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
};

/**
 * @brief The two file bodies, as the caller's own text to write.
 *
 * The service returns content rather than writing it: the caller owns the file
 * system, and the same bodies are what the round-trip test compares against
 * the file it imported.
 */
struct export_market_data_response {
    bool success = false;
    std::string message;
    /**
     * @brief The market.txt body: one DATE<TAB>KEY<TAB>VALUE line per
     * observation, empty when the tenant has no observations.
     */
    std::string market_data_content;
    /**
     * @brief The fixings.txt body: one DATE<TAB>INDEX<TAB>VALUE line per
     * fixing, empty when the tenant has no fixings.
     */
    std::string fixings_content;
    int series_count = 0;
    int observation_count = 0;
    int fixing_count = 0;
};

/**
 * @brief One live observation of one market datum.
 *
 * The datum is named by its oresmd quote URI, which carries the asset class,
 * the instrument and every field, so no asset class has a tick type of its
 * own. A curve is the set of ticks one producer publishes with one
 * observation time.
 */
struct market_tick {
    static constexpr std::string_view nats_subject = "marketdata.v1.tick";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = false;
    /** The quote URI of the datum observed, under the strict contract. */
    std::string oresmd_uri;
    /** The observed value as decimal text, so it keeps the producer's digits. */
    std::string value;
    /** When the value was observed. */
    std::chrono::system_clock::time_point observation_time;
    /** The producer, unique within a tenant; its feed bindings name the consumers. */
    std::string source;
};

}

#endif
