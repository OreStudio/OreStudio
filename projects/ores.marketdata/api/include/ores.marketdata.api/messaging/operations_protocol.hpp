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
#include "ores.marketdata.api/domain/market_series.hpp"
#include <chrono>
#include <cstdint>
#include <map>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace ores::marketdata::messaging {

/**
 * @brief One typed identity field of a series, as the caller gives it.
 *
 * The name is an oresmd field code, such as ccy or curve_id, and the text is
 * what the caller believes that field holds. The pair is the request's spelling
 * of the identity, not a URI: the server composes the URI through the codec.
 */
struct series_identity_field {
    std::string name;
    std::string text;
};

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
struct resolve_series_identity_request {
    using response_type = struct resolve_series_identity_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.resolve_series_identity";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string asset;
    std::string scope;
    std::string instrument_type;
    std::string quote_type;
    std::string party_id;
    /**
     * @brief The identity's remaining fields, such as ccy or curve_id.
     *
     * The other fields the type's schema row declares. A field the caller does not
     * state is left unconstrained, so the read narrows by what it is given.
     */
    std::vector<series_identity_field> fields;
};

/**
 * @brief The series the identity resolves to.
 *
 * One row per owning party that holds the identity. Empty when nothing carries
 * it, which is not an error: a scope may hold no series of that kind yet.
 */
struct resolve_series_identity_response {
    std::vector<ores::marketdata::domain::market_series> series;
    bool success = false;
    std::string message;
};

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
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.get_crm_rate";
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
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.get_crm_rates";
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
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.republish_curve";
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
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.compute_curve";
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
 * @brief One instant of a slice: the object as it stood then.
 */
struct series_slice_instant {
    std::chrono::system_clock::time_point as_of;
    /**
     * @brief The value at each declared coordinate, index for index with the
     * response's coordinates.
     *
     * The empty string is a hole: the shape declares the coordinate and no point
     * holds a value for it at this instant. A short vector would be read as a
     * shorter object, which is the mistake the aligned shape exists to prevent.
     */
    std::vector<std::string> values;
};

/**
 * @brief Requests one component of a composite object over a range of
 * instants.
 *
 * The object is named the way the resolver names one, so no caller builds an
 * oresmd URI and no caller matches on one. The component is the value the
 * object holds on one of its axes; an object with a single axis has one
 * component, the whole object, and leaves the component field empty.
 */
struct get_series_slice_request {
    using response_type = struct get_series_slice_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.get_series_slice";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** The object, as the typed identity the resolver already takes. */
    resolve_series_identity_request identity;
    /**
     * @brief The axis the component fixes, an oresmd field code such as
     * strike_label.
     *
     * Empty for an object with one coordinate axis, whose one component is the
     * whole object.
     */
    std::string component_field;
    /**
     * @brief The value the component fixes on that axis, such as ATM, in the
     * spelling the shape stores.
     */
    std::string component_value;
    /** The first market instant of the range, inclusive. */
    std::chrono::system_clock::time_point from_instant;
    /** The last market instant of the range, inclusive. */
    std::chrono::system_clock::time_point to_instant;
};

/**
 * @brief The component at each instant in the range.
 *
 * The ladder is stated once, because it belongs to the shape and not to the
 * points: coordinates holds the declared values of the axis the component
 * varies over, in the order the shape stores them, and every instant's values
 * are index for index with it.
 */
struct get_series_slice_response {
    bool success = false;
    /**
     * @brief Why the read was refused, when it was.
     *
     * A component the shape does not declare is refused here, naming the axis or
     * the value that is missing.
     */
    std::string message;
    /**
     * @brief The axis the component varies over, an oresmd field code such as
     * expiry.
     *
     * Empty when the response holds no instant.
     */
    std::string coordinate_field;
    /**
     * @brief The declared values of the coordinate field, in the order the shape
     * stores them.
     *
     * Every instant's values are index for index with this.
     */
    std::vector<std::string> coordinates;
    /** Oldest first, one entry per distinct market instant in the range. */
    std::vector<series_slice_instant> instants;
};

/**
 * @brief One node of a composite object: its coordinate labels and the change
 * it shows over the range.
 */
struct evolution_node {
    /**
     * @brief One label per declared axis, index for index with the response's
     * coordinate_fields.
     */
    std::vector<std::string> coordinates;
    /**
     * @brief The change the node shows over the range.
     *
     * One of never_quoted, persistent, added, dropped or intermittent, which is
     * the first of those that holds. never_quoted is a node the shape declares and
     * no instant in the range carries, which is what tells it apart from a node the
     * object does not have: that node is not in the response at all.
     */
    std::string status;
};

/**
 * @brief One instant of the evolution: the object as it stood then.
 */
struct evolution_instant {
    std::chrono::system_clock::time_point as_of;
    /**
     * @brief The value at each node, index for index with the response's nodes.
     *
     * The empty string is a hole: the shape declares the node and no point held a
     * value for it at this instant.
     */
    std::vector<std::string> values;
};

/**
 * @brief Requests a whole composite object as instants by nodes.
 *
 * The object is named the way the resolver names one, so no caller builds an
 * oresmd URI and no caller matches on one. The instants are either the ones the
 * object holds between two instants or the set the caller states.
 */
struct get_series_evolution_request {
    using response_type = struct get_series_evolution_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.get_series_evolution";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** The object, as the typed identity the resolver already takes. */
    resolve_series_identity_request identity;
    /** The first market instant of the range, inclusive, when instants is empty. */
    std::chrono::system_clock::time_point from_instant;
    /** The last market instant of the range, inclusive, when instants is empty. */
    std::chrono::system_clock::time_point to_instant;
    /**
     * @brief The instants the caller states, which replace the range when the
     * vector is not empty.
     *
     * Each one is read as the object stood at it, whether or not the object holds a
     * point at exactly that instant. A caller that wants the object at the month
     * ends states them; a caller that wants everything it holds between two
     * instants states the range and leaves this empty.
     */
    std::vector<std::chrono::system_clock::time_point> instants;
};

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
struct get_series_evolution_response {
    bool success = false;
    /** Why the read was refused, when it was. */
    std::string message;
    /**
     * @brief The axis names, in the order the shape stores them.
     *
     * Every node's coordinates are index for index with this.
     */
    std::vector<std::string> coordinate_fields;
    /**
     * @brief The declared grid, with the last axis varying fastest.
     *
     * Every combination of the declared axis values is a node, whether or not any
     * instant carries it.
     */
    std::vector<evolution_node> nodes;
    /** Oldest first, one entry per distinct market instant read. */
    std::vector<evolution_instant> instants;
};

/**
 * @brief Where one value of a snapshot came from.
 */
struct point_provenance {
    /**
     * @brief One of quoted, derived or manual.
     *
     * A point with no annex row is quoted. The empty string marks a hole, which has
     * no source.
     */
    std::string source_kind;
    /** Who set the point. Empty for a quoted point and for a hole. */
    std::string modified_by;
    /** Why the point was set. Empty for a quoted point and for a hole. */
    std::string change_reason_code;
    /** The free-text reason for the change. Empty when none was given. */
    std::string change_commentary;
    /** When the annex row was recorded. The epoch for a quoted point and for a hole. */
    std::chrono::system_clock::time_point recorded_at;
};

/**
 * @brief Requests a composite object as it stands now, as one instant by nodes.
 *
 * The object is named the way the resolver names one, as the slice read and the
 * evolution read name it, so the three cannot disagree about what the object is.
 * The instant is always now; there is no as-of-in-the-past parameter, because the
 * evolution read answers that question.
 */
struct get_curve_snapshot_request {
    using response_type = struct get_curve_snapshot_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.curve_snapshot.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** The object, as the typed identity the resolver already takes. */
    resolve_series_identity_request identity;
    /**
     * @brief Whether the response states where each value came from.
     *
     * The annex is read only when this is set, so a caller that does not ask pays
     * nothing for it. The provenance is the current annex row of each point shown, the
     * same generation the values come from, not the row as it stood at the instant.
     */
    bool include_provenance = false;
};

/**
 * @brief The object at one instant, against the same grid of declared nodes the
 * evolution read states.
 *
 * The evolution read of that object gives the same coordinate fields and the same
 * nodes. The snapshot carries the latest value of a node forward from before the
 * instant, which the evolution does not, and adds the age of each value, because a
 * composite of several market states must not be taken for one.
 */
struct get_curve_snapshot_response {
    bool success = false;
    /** Why the read was refused, when it was. */
    std::string message;
    /**
     * @brief The instant the snapshot is as of, which is what an age is measured
     * from.
     *
     * Without it a reader measures an age from its own clock, so clock skew shows
     * up as staleness that is not in the curve.
     */
    std::chrono::system_clock::time_point as_of;
    /** The axis names, in the order the shape stores them. */
    std::vector<std::string> coordinate_fields;
    /**
     * @brief The declared grid, with the last axis varying fastest.
     *
     * The status of a node is empty: one instant shows no change.
     */
    std::vector<evolution_node> nodes;
    /**
     * @brief The value at each node, index for index with nodes.
     *
     * The empty string is a hole: the shape declares the node and no point held a
     * value for it.
     */
    std::vector<std::string> values;
    /** When each point was recorded, index-for-index with observations: the
     * bitemporal valid_from of the row it was read from. Two ages are readable
     * from a point and they answer different questions: the snapshot instant
     * minus the point's instant is market staleness, the age of the market state
     * it belongs to; now minus its record time is record staleness, how long ago
     * the value arrived.
     */
    std::vector<std::chrono::system_clock::time_point> recorded_at;
    /**
     * @brief Where each value came from, index for index with nodes.
     *
     * Empty when the request did not ask for provenance.
     */
    std::vector<point_provenance> provenance;
    /**
     * @brief The age of the snapshot's oldest point, in seconds.
     *
     * Market staleness, not record staleness: the age of the market state the
     * point belongs to, which is what decides whether a drawn curve is one market
     * read or a stitching of several. Zero for an empty snapshot.
     */
    std::int64_t oldest_age_seconds = 0;
    /**
     * @brief The spread between the oldest and the newest point, in seconds.
     *
     * The number that measures the mixing: points that share one instant have a
     * spread of zero however old that instant is, and a curve stitched across
     * market horizons does not. A point whose instant is after the snapshot
     * instant carries no age at all, and one exactly at it is age zero and counts.
     */
    std::int64_t spread_seconds = 0;
    /**
     * @brief Whether the snapshot is mixed enough that a view must not draw it
     * silently.
     *
     * The threshold is stated by the evolution journey, not by this message: the
     * read reports the crossing, and the view decides what to show.
     */
    bool warning = false;
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
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.import_market_data";
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
 * the producer factory. One request starts the subtree, so a caller need
 * not enumerate every pair; it works the same from ores.shell, an HTTP
 * caller, or a workflow step.
 */
struct start_feeds_under_folder_request {
    using response_type = struct start_feeds_under_folder_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.start_feeds_under_folder";
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
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.stop_feeds_under_folder";
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
 * @brief Vintage-availability status for one feed config of any asset class,
 * computed live (not stored) against market_observation.
 */
struct vintage_validity_entry {
    /** The feed config row's own id: fx_spot_generation_config::id for an FX
     * feed, ir_curve_generation_config::id for an IR curve feed.
     */
    std::string config_id;
    /** The registered feed kind the row belongs to, so a caller can tell the
     * asset classes apart without probing.
     */
    std::string kind;
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
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.get_vintage_validity";
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
 * @brief Exports the tenant's market data to object storage as ORE text.
 *
 * The engine reads a market data file its run document names, and nothing else
 * can fill that file, so what is written here is that body and no other form
 * of the data.
 */
struct export_market_data_to_storage_request {
    using response_type = struct export_market_data_to_storage_response;
    static constexpr std::string_view nats_subject =
        "marketdata.v1.ops.export_market_data_to_storage";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string storage_bucket;
    /**
     * @brief Where the market data body goes.
     */
    std::string storage_key;
    /**
     * @brief Where the fixings body goes.
     *
     * The engine reads fixings from a file of their own, so the body goes to an
     * object of its own. An empty body is written too, because a run whose run
     * document names the file must find it.
     */
    std::string fixings_storage_key;
};

struct export_market_data_to_storage_response {
    bool success = false;
    std::string message;
    int series_count = 0;
    std::string storage_key;
    /**
     * @brief Where the fixings body was written.
     */
    std::string fixings_storage_key;
};

/**
 * @brief Projects the identity of every series whose stored row is stale.
 *
 * The projection is written by the series write path, so only rows that
 * predate it, that were written outside it, or that an earlier projector
 * spelled differently are stale. This compares each series' fresh projection
 * with the stored row and writes only the difference.
 */
struct backfill_series_identity_request {
    using response_type = struct backfill_series_identity_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.backfill_series_identity";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The owning party to narrow to, or empty for every party the caller
     * can see.
     */
    std::string party_id;
};

struct backfill_series_identity_response {
    bool success = false;
    std::string message;
    /**
     * @brief How many series were projected by this call.
     *
     * A series whose stored row already matched the fresh projection is not
     * counted: only the rows this call wrote are.
     */
    int projected_count = 0;
    /**
     * @brief How many current series already carried the projection this call
     * would write.
     */
    int already_projected_count = 0;
    /**
     * @brief How many of the projected series carried a URI neither codec reads.
     *
     * Their rows are written with their kind and no field value, so the series is
     * findable and never mistaken for one whose identity was read. The count is
     * what says how many identities the database could not state.
     */
    int unreadable_count = 0;
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
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.export_market_data";
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
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.market_tick";
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
