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
#include "ores.marketdata.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <chrono>
#include <cstddef>
#include <cstdint>
#include <map>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
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

    friend bool operator==(const series_identity_field&, const series_identity_field&) = default;
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
 *
 * `provider_code` states which provider's series is wanted. A party holds one
 * series of an identity for each provider, so an empty one can match several.
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
    std::string provider_code;
    /**
     * @brief The identity's remaining fields, such as ccy or curve_id.
     *
     * The other fields the type's schema row declares. A field the caller does not
     * state is left unconstrained, so the read narrows by what it is given.
     */
    std::vector<series_identity_field> fields;

    friend bool operator==(const resolve_series_identity_request&,
                           const resolve_series_identity_request&) = default;
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

    friend bool operator==(const resolve_series_identity_response&,
                           const resolve_series_identity_response&) = default;
};

/**
 * @brief One field of a schema row.
 *
 * The role says whether the field names the series or a point within it. A field
 * that may be none can be left out of a URI. `codes` holds the tokens a code field
 * accepts and is empty for any other kind.
 */
struct oresmd_field_spec {
    std::string name;
    std::string role;
    bool may_be_none = false;
    std::string kind;
    std::vector<std::string> codes;

    friend bool operator==(const oresmd_field_spec&, const oresmd_field_spec&) = default;
};

/**
 * @brief One instrument type of the schema, with its fields in key order.
 *
 * `code` is the text a URI writes under `instrument=`. `subject` is the field a
 * URI writes as its path.
 */
struct oresmd_instrument {
    std::string code;
    std::string ore_name;
    std::string title;
    std::string asset;
    std::string subject;
    std::vector<oresmd_field_spec> fields;

    friend bool operator==(const oresmd_instrument&, const oresmd_instrument&) = default;
};

/** @brief One field of an index family, which a fixing URI writes in the row's order. */
struct oresmd_index_field {
    std::string name;
    bool optional = false;

    friend bool operator==(const oresmd_index_field&, const oresmd_index_field&) = default;
};

/**
 * @brief One family of index.
 *
 * A fixing URI writes `type=fixing` and `index=` with the family's code. The path
 * is the subject and the query holds the other fields in the row's order.
 */
struct oresmd_index_family {
    std::string code;
    std::string asset;
    std::string subject;
    std::vector<oresmd_index_field> fields;

    friend bool operator==(const oresmd_index_family&, const oresmd_index_family&) = default;
};

/**
 * @brief Asks for the oresmd schema.
 *
 * The request carries nothing. The schema is the codec's own table, and it is the
 * same for every caller.
 */
struct describe_oresmd_schema_request {
    using response_type = struct describe_oresmd_schema_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.describe_oresmd_schema";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;

    friend bool operator==(const describe_oresmd_schema_request&,
                           const describe_oresmd_schema_request&) = default;
};

/**
 * @brief The oresmd schema, read from the codec's own tables.
 *
 * `instruments` follows the order of ORE's instrument types and each row's fields
 * follow the order its key writes them. Every list is in a stable order.
 */
struct describe_oresmd_schema_response {
    std::vector<std::string> assets;
    std::vector<oresmd_instrument> instruments;
    std::vector<std::string> quotes;
    std::vector<std::string> quote_titles;
    std::vector<oresmd_index_family> indexes;
    bool success = false;
    std::string message;

    friend bool operator==(const describe_oresmd_schema_response&,
                           const describe_oresmd_schema_response&) = default;
};

/**
 * @brief One field of an oresmd URI: its name and the text it holds.
 *
 * The text is in the spelling a URI writes. The path of a URI is the subject, and
 * it is not one of these.
 */
struct oresmd_uri_field {
    std::string name;
    std::string text;

    friend bool operator==(const oresmd_uri_field&, const oresmd_uri_field&) = default;
};

/**
 * @brief Asks the codec to read an oresmd URI.
 *
 * The codec's reader is strict, so a URI that is not in its own spelling is
 * refused with the rule that failed. A URI longer than the codec's limit is
 * refused without being read.
 */
struct parse_oresmd_uri_request {
    using response_type = struct parse_oresmd_uri_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.parse_oresmd_uri";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string uri;

    friend bool operator==(const parse_oresmd_uri_request&,
                           const parse_oresmd_uri_request&) = default;
};

/**
 * @brief The parts of an oresmd URI, or why the codec refuses it.
 *
 * `kind` is quote, series or fixing. For a quote or a series `instrument` is the
 * text under `instrument=` and `quote` is the text under `quote=`. For a fixing
 * `instrument` is the index family and `quote` is empty. `subject` is the path.
 * `fields` holds the other fields the URI states, in schema order, and leaves
 * out one the URI leaves out.
 */
struct parse_oresmd_uri_response {
    std::string kind;
    std::string instrument;
    std::string quote;
    std::string asset;
    std::string subject;
    std::vector<oresmd_uri_field> fields;
    bool success = false;
    std::string message;

    friend bool operator==(const parse_oresmd_uri_response&,
                           const parse_oresmd_uri_response&) = default;
};

/**
 * @brief Asks the codec to write an oresmd URI from its parts.
 *
 * A field with no text is a field the URI leaves out. The codec decides whether
 * the parts make a URI: a required field that is missing, a field the row does
 * not list, a field given twice and a value of the wrong form are each refused.
 */
struct compose_oresmd_uri_request {
    using response_type = struct compose_oresmd_uri_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.compose_oresmd_uri";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string kind;
    std::string instrument;
    std::string quote;
    std::string subject;
    std::vector<oresmd_uri_field> fields;

    friend bool operator==(const compose_oresmd_uri_request&,
                           const compose_oresmd_uri_request&) = default;
};

/**
 * @brief The oresmd URI the codec writes, or why the parts make none.
 *
 * The URI is the codec's canonical text, so a stored URI compares as text.
 */
struct compose_oresmd_uri_response {
    std::string uri;
    bool success = false;
    std::string message;

    friend bool operator==(const compose_oresmd_uri_response&,
                           const compose_oresmd_uri_response&) = default;
};

/**
 * @brief Requests the typed identity of each series the caller can see.
 *
 * A party reads only its own series, so the list is the caller's view and not the
 * tenant's. An empty `party_id` asks across the parties the caller can see.
 */
struct list_series_identities_request {
    using response_type = struct list_series_identities_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.list_series_identities";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string party_id;

    friend bool operator==(const list_series_identities_request&,
                           const list_series_identities_request&) = default;
};

/**
 * @brief One series in the list, with the typed identity that names it.
 *
 * The identity carries the owning party, so passing it to the snapshot, slice or
 * evolution read names this series and no other party's series of the same kind.
 */
struct listed_series {
    std::string series_id;
    resolve_series_identity_request identity;
    /** The declared coordinate fields in order. Empty when no shape is declared. */
    std::vector<std::string> axes;

    friend bool operator==(const listed_series&, const listed_series&) = default;
};

/**
 * @brief The series the caller can see, in a stable order.
 *
 * Only series a curve read can name are listed: a series whose URI carries an
 * instrument type and a quote type. A fixing series is not.
 */
struct list_series_identities_response {
    std::vector<listed_series> series;
    bool truncated = false;
    bool success = false;
    std::string message;

    friend bool operator==(const list_series_identities_response&,
                           const list_series_identities_response&) = default;
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

    friend bool operator==(const crm_rate_item&, const crm_rate_item&) = default;
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

    friend bool operator==(const get_crm_rate_request&, const get_crm_rate_request&) = default;
};

struct get_crm_rate_response {
    bool success = true;
    std::string message;
    crm_rate_item rate;

    friend bool operator==(const get_crm_rate_response&, const get_crm_rate_response&) = default;
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

    friend bool operator==(const get_crm_rates_request&, const get_crm_rates_request&) = default;
};

struct get_crm_rates_response {
    bool success = true;
    std::string message;
    std::vector<crm_rate_item> rates;

    friend bool operator==(const get_crm_rates_response&, const get_crm_rates_response&) = default;
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

    friend bool operator==(const republish_curve_request&,
                           const republish_curve_request&) = default;
};

struct republish_curve_response {
    bool success = false;
    std::string message;

    friend bool operator==(const republish_curve_response&,
                           const republish_curve_response&) = default;
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

    friend bool operator==(const compute_curve_request&, const compute_curve_request&) = default;
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

    friend bool operator==(const computed_curve_point&, const computed_curve_point&) = default;
};

struct compute_curve_response {
    bool success = false;
    std::string message;
    std::vector<computed_curve_point> points;

    friend bool operator==(const compute_curve_response&, const compute_curve_response&) = default;
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

    friend bool operator==(const series_slice_instant&, const series_slice_instant&) = default;
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

    friend bool operator==(const get_series_slice_request&,
                           const get_series_slice_request&) = default;
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

    friend bool operator==(const get_series_slice_response&,
                           const get_series_slice_response&) = default;
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

    friend bool operator==(const evolution_node&, const evolution_node&) = default;
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

    friend bool operator==(const evolution_instant&, const evolution_instant&) = default;
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

    friend bool operator==(const get_series_evolution_request&,
                           const get_series_evolution_request&) = default;
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

    friend bool operator==(const get_series_evolution_response&,
                           const get_series_evolution_response&) = default;
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
    /** The recipe that built a derived point. Empty unless the point is derived. */
    std::string derivation_config_id;
    /** The recipe version the point was built under. Zero unless the point is derived. */
    int derivation_config_version;
    /** The as-of of the upstream data the derivation read. The epoch unless the point is derived.
     */
    std::chrono::system_clock::time_point source_as_of;

    friend bool operator==(const point_provenance&, const point_provenance&) = default;
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

    friend bool operator==(const get_curve_snapshot_request&,
                           const get_curve_snapshot_request&) = default;
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
    /**
     * @brief When a person last published this series, or absent when nobody has.
     *
     * The values the snapshot serves are the latest observations, published or
     * not. This is what tells a reader which of the two it is looking at, and a
     * series no person published answers absent rather than a plausible instant.
     */
    std::optional<std::chrono::system_clock::time_point> published_at;
    /**
     * @brief The account that published the set, or empty when nobody has.
     *
     * The decider and the publisher are two people: the approval records who
     * answered the request, and this records who applied the answer.
     */
    std::string published_by;
    /**
     * @brief The market instant the published set was bootstrapped against.
     *
     * The epoch when nobody has published the series. The snapshot's own instant
     * is now, and the values carry their own ages; this is the as-of the published
     * set was built at, which is what a delta against the live set is measured
     * from.
     */
    std::chrono::system_clock::time_point published_as_of;
    /**
     * @brief The approval request that authorised the published set, or empty.
     *
     * The request a decider answered, so a reader sees which decision the set in
     * use stands on and can read its decisions, its reason and its state.
     */
    std::string approval_request_id;

    friend bool operator==(const get_curve_snapshot_response&,
                           const get_curve_snapshot_response&) = default;
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

    friend bool operator==(const import_market_data_request&,
                           const import_market_data_request&) = default;
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

    friend bool operator==(const import_market_data_response&,
                           const import_market_data_response&) = default;
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

    friend bool operator==(const feed_kind_counts&, const feed_kind_counts&) = default;
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

    friend bool operator==(const start_feeds_under_folder_request&,
                           const start_feeds_under_folder_request&) = default;
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

    friend bool operator==(const start_feeds_under_folder_response&,
                           const start_feeds_under_folder_response&) = default;
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

    friend bool operator==(const stop_feeds_under_folder_request&,
                           const stop_feeds_under_folder_request&) = default;
};

struct stop_feeds_under_folder_response {
    bool success = false;
    std::string message;
    int stopped = 0;
    std::map<std::string, int> stopped_by_kind;

    friend bool operator==(const stop_feeds_under_folder_response&,
                           const stop_feeds_under_folder_response&) = default;
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
    /**
     * @brief The valuation date, as an ISO 8601 date, or empty for the whole stored history.
     *
     * A date adds one line for each datum, dated at it, with the value the datum had at the
     * close of that day, and the history of the other dates stays. The ticks of a live feed are
     * in that line. They are not in the history.
     */
    std::optional<std::string> valuation_date;
    /**
     * @brief The oldest value, in days before the valuation date, that the line of that date
     * may carry forward. Empty means the service default.
     *
     * A datum whose latest value is older than this has no line of its own at the date, so the
     * run does not use a stale quote as a current one. Its history lines stay. The answer counts
     * these datums.
     */
    std::optional<int> max_age_days;

    friend bool operator==(const export_market_data_to_storage_request&,
                           const export_market_data_to_storage_request&) = default;
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
    /**
     * @brief The datums whose line at the valuation date carries a value from an earlier day.
     */
    int carried_forward_count = 0;
    /**
     * @brief The datums left out of the valuation date because their latest value is older
     * than the limit.
     */
    int stale_count = 0;

    friend bool operator==(const export_market_data_to_storage_response&,
                           const export_market_data_to_storage_response&) = default;
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

    friend bool operator==(const backfill_series_identity_request&,
                           const backfill_series_identity_request&) = default;
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

    friend bool operator==(const backfill_series_identity_response&,
                           const backfill_series_identity_response&) = default;
};

/**
 * @brief Keys a manual point on a node of a named object.
 *
 * The coordinates are checked against the shape the object declares, so a node
 * the shape does not hold is refused. The point is stamped at the instant the
 * server receives the request, and the caller is its author.
 */
struct key_manual_point_request {
    using response_type = struct key_manual_point_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.key_manual_point";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** The object, as the typed identity the reads take. */
    resolve_series_identity_request identity;
    /** The node: one coordinate per declared axis, in the shape's order. */
    std::vector<std::string> coordinates;
    /** The value to key, as a number written in text. */
    std::string value;
    /** The change reason code that says why the point is keyed. */
    std::string change_reason_code;
    /** The free-text reason for the correction. */
    std::string change_commentary;

    friend bool operator==(const key_manual_point_request&,
                           const key_manual_point_request&) = default;
};

struct key_manual_point_response {
    bool success = false;
    std::string message;

    friend bool operator==(const key_manual_point_response&,
                           const key_manual_point_response&) = default;
};

/**
 * @brief Clears the manual point on a node of a named object.
 *
 * Every manual point the node holds is released, whatever instant it was keyed
 * at. The history keeps the value it replaced.
 */
struct clear_manual_point_request {
    using response_type = struct clear_manual_point_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.clear_manual_point";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /** The object, as the typed identity the reads take. */
    resolve_series_identity_request identity;
    /** The node: one coordinate per declared axis, in the shape's order. */
    std::vector<std::string> coordinates;

    friend bool operator==(const clear_manual_point_request&,
                           const clear_manual_point_request&) = default;
};

struct clear_manual_point_response {
    bool success = false;
    std::string message;

    friend bool operator==(const clear_manual_point_response&,
                           const clear_manual_point_response&) = default;
};

/**
 * @brief Request to write the tenant's market data back out as ORE text.
 *
 * The export is the whole tenant's, because that is the scope the files have.
 * An ORE market.txt is a snapshot of everything a run needs, not a slice of
 * one series, and an export that could name a subset would produce a file that
 * does not reproduce anything. The one thing a caller states is the date.
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
    /**
     * @brief The valuation date, as an ISO 8601 date, or empty for the whole stored history.
     *
     * A date adds one line for each datum, dated at it, with the value the datum had at the
     * close of that day, and the history of the other dates stays. The ticks of a live feed are
     * in that line. They are not in the history.
     */
    std::optional<std::string> valuation_date;
    /**
     * @brief The oldest value, in days before the valuation date, that the line of that date
     * may carry forward. Empty means the service default.
     *
     * A datum whose latest value is older than this has no line of its own at the date, so the
     * run does not use a stale quote as a current one. Its history lines stay. The answer counts
     * these datums.
     */
    std::optional<int> max_age_days;

    friend bool operator==(const export_market_data_request&,
                           const export_market_data_request&) = default;
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
    /**
     * @brief The datums whose line at the valuation date carries a value from an earlier day.
     */
    int carried_forward_count = 0;
    /**
     * @brief The datums left out of the valuation date because their latest value is older
     * than the limit.
     */
    int stale_count = 0;

    friend bool operator==(const export_market_data_response&,
                           const export_market_data_response&) = default;
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
    /** The producer, unique within a tenant; its routes name the consumers. */
    std::string source;

    friend bool operator==(const market_tick&, const market_tick&) = default;
};

/**
 * @brief Asks for every tick of one series in a span of time.
 *
 * The span is half open: it holds a tick at from_instant and none at to_instant. The
 * request has no count and no page, because a span of time means the same on every tier
 * that answers it. A span that holds more ticks than the ceiling is refused, never cut.
 */
struct get_series_ticks_request {
    using response_type = struct get_series_ticks_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.get_series_ticks";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    resolve_series_identity_request identity;
    std::chrono::system_clock::time_point from_instant;
    std::optional<std::chrono::system_clock::time_point> to_instant;
    std::vector<std::string> coordinates;

    friend bool operator==(const get_series_ticks_request&,
                           const get_series_ticks_request&) = default;
};

/**
 * @brief Every tick of a span of one series.
 *
 * The coordinates come first, once each. The times follow as the first instant and a gap in
 * microseconds from the previous tick, because every instant has one wire shape and a text
 * instant for each tick would take more than twice the bytes of a gap. The three arrays have
 * the same length, and the order is by time and then by coordinate.
 */
struct get_series_ticks_response {
    ores::utility::domain::result result;
    std::vector<std::string> coordinates;
    std::optional<std::chrono::system_clock::time_point> first_instant;
    std::vector<std::int64_t> gaps_us;
    std::vector<int> coordinate_indexes;
    std::vector<double> values;

    friend bool operator==(const get_series_ticks_response&,
                           const get_series_ticks_response&) = default;
};

/**
 * @brief Asks this component to keep the market state a run priced against.
 *
 * The state is the output files the run wrote, and those files are far too
 * large for a message: a cube states a column for every curve and tenor the run
 * built. The node uploads each file as its own object with a manifest that names
 * them, so the caller names each result's manifest and lends the token that reads
 * them — the run's own, the same one the run's other steps use — and this fetches
 * the market-state files rather than receiving them.
 *
 * A manifest names each file by its storage location, so this learns no other
 * component's layout. A file whose family is not a market state is not fetched:
 * the run's results belong to reporting.
 */
struct store_market_state_request {
    using response_type = struct store_market_state_response;
    static constexpr std::string_view nats_subject = "marketdata.v1.ops.store_market_state";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::string party_id;
    std::string report_instance_id;
    std::string report_definition_id;
    std::string as_of;
    std::string storage_token;
    std::vector<std::string> manifest_uris;
    std::vector<std::string> skipped_families;

    friend bool operator==(const store_market_state_request&,
                           const store_market_state_request&) = default;
};

struct store_market_state_response {
    ores::utility::domain::result result;
    std::vector<std::string> families;
    int files = 0;
    std::int64_t rows = 0;
    std::vector<std::string> left_alone;
    std::vector<std::string> refusals;

    friend bool operator==(const store_market_state_response&,
                           const store_market_state_response&) = default;
};

/**
 * @brief The members of series_identity_field, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<series_identity_field>) {
    return std::tuple{
        ::ores::utility::codec::field{"name", &series_identity_field::name},
        ::ores::utility::codec::field{"text", &series_identity_field::text},
    };
}

/**
 * @brief The members of resolve_series_identity_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<resolve_series_identity_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"asset", &resolve_series_identity_request::asset},
        ::ores::utility::codec::field{"scope", &resolve_series_identity_request::scope},
        ::ores::utility::codec::field{"instrument_type",
                                      &resolve_series_identity_request::instrument_type},
        ::ores::utility::codec::field{"quote_type", &resolve_series_identity_request::quote_type},
        ::ores::utility::codec::field{"party_id", &resolve_series_identity_request::party_id},
        ::ores::utility::codec::field{"provider_code",
                                      &resolve_series_identity_request::provider_code},
        ::ores::utility::codec::field{"fields", &resolve_series_identity_request::fields},
    };
}

/**
 * @brief The members of resolve_series_identity_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<resolve_series_identity_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"series", &resolve_series_identity_response::series},
        ::ores::utility::codec::field{"success", &resolve_series_identity_response::success},
        ::ores::utility::codec::field{"message", &resolve_series_identity_response::message},
    };
}

/**
 * @brief The members of oresmd_field_spec, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<oresmd_field_spec>) {
    return std::tuple{
        ::ores::utility::codec::field{"name", &oresmd_field_spec::name},
        ::ores::utility::codec::field{"role", &oresmd_field_spec::role},
        ::ores::utility::codec::field{"may_be_none", &oresmd_field_spec::may_be_none},
        ::ores::utility::codec::field{"kind", &oresmd_field_spec::kind},
        ::ores::utility::codec::field{"codes", &oresmd_field_spec::codes},
    };
}

/**
 * @brief The members of oresmd_instrument, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<oresmd_instrument>) {
    return std::tuple{
        ::ores::utility::codec::field{"code", &oresmd_instrument::code},
        ::ores::utility::codec::field{"ore_name", &oresmd_instrument::ore_name},
        ::ores::utility::codec::field{"title", &oresmd_instrument::title},
        ::ores::utility::codec::field{"asset", &oresmd_instrument::asset},
        ::ores::utility::codec::field{"subject", &oresmd_instrument::subject},
        ::ores::utility::codec::field{"fields", &oresmd_instrument::fields},
    };
}

/**
 * @brief The members of oresmd_index_field, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<oresmd_index_field>) {
    return std::tuple{
        ::ores::utility::codec::field{"name", &oresmd_index_field::name},
        ::ores::utility::codec::field{"optional", &oresmd_index_field::optional},
    };
}

/**
 * @brief The members of oresmd_index_family, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<oresmd_index_family>) {
    return std::tuple{
        ::ores::utility::codec::field{"code", &oresmd_index_family::code},
        ::ores::utility::codec::field{"asset", &oresmd_index_family::asset},
        ::ores::utility::codec::field{"subject", &oresmd_index_family::subject},
        ::ores::utility::codec::field{"fields", &oresmd_index_family::fields},
    };
}

/**
 * @brief The members of describe_oresmd_schema_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<describe_oresmd_schema_request>) {
    return std::tuple{};
}

/**
 * @brief The members of describe_oresmd_schema_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<describe_oresmd_schema_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"assets", &describe_oresmd_schema_response::assets},
        ::ores::utility::codec::field{"instruments", &describe_oresmd_schema_response::instruments},
        ::ores::utility::codec::field{"quotes", &describe_oresmd_schema_response::quotes},
        ::ores::utility::codec::field{"quote_titles",
                                      &describe_oresmd_schema_response::quote_titles},
        ::ores::utility::codec::field{"indexes", &describe_oresmd_schema_response::indexes},
        ::ores::utility::codec::field{"success", &describe_oresmd_schema_response::success},
        ::ores::utility::codec::field{"message", &describe_oresmd_schema_response::message},
    };
}

/**
 * @brief The members of oresmd_uri_field, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<oresmd_uri_field>) {
    return std::tuple{
        ::ores::utility::codec::field{"name", &oresmd_uri_field::name},
        ::ores::utility::codec::field{"text", &oresmd_uri_field::text},
    };
}

/**
 * @brief The members of parse_oresmd_uri_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<parse_oresmd_uri_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"uri", &parse_oresmd_uri_request::uri},
    };
}

/**
 * @brief The members of parse_oresmd_uri_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<parse_oresmd_uri_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"kind", &parse_oresmd_uri_response::kind},
        ::ores::utility::codec::field{"instrument", &parse_oresmd_uri_response::instrument},
        ::ores::utility::codec::field{"quote", &parse_oresmd_uri_response::quote},
        ::ores::utility::codec::field{"asset", &parse_oresmd_uri_response::asset},
        ::ores::utility::codec::field{"subject", &parse_oresmd_uri_response::subject},
        ::ores::utility::codec::field{"fields", &parse_oresmd_uri_response::fields},
        ::ores::utility::codec::field{"success", &parse_oresmd_uri_response::success},
        ::ores::utility::codec::field{"message", &parse_oresmd_uri_response::message},
    };
}

/**
 * @brief The members of compose_oresmd_uri_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<compose_oresmd_uri_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"kind", &compose_oresmd_uri_request::kind},
        ::ores::utility::codec::field{"instrument", &compose_oresmd_uri_request::instrument},
        ::ores::utility::codec::field{"quote", &compose_oresmd_uri_request::quote},
        ::ores::utility::codec::field{"subject", &compose_oresmd_uri_request::subject},
        ::ores::utility::codec::field{"fields", &compose_oresmd_uri_request::fields},
    };
}

/**
 * @brief The members of compose_oresmd_uri_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<compose_oresmd_uri_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"uri", &compose_oresmd_uri_response::uri},
        ::ores::utility::codec::field{"success", &compose_oresmd_uri_response::success},
        ::ores::utility::codec::field{"message", &compose_oresmd_uri_response::message},
    };
}

/**
 * @brief The members of list_series_identities_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_series_identities_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"party_id", &list_series_identities_request::party_id},
    };
}

/**
 * @brief The members of listed_series, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<listed_series>) {
    return std::tuple{
        ::ores::utility::codec::field{"series_id", &listed_series::series_id},
        ::ores::utility::codec::field{"identity", &listed_series::identity},
        ::ores::utility::codec::field{"axes", &listed_series::axes},
    };
}

/**
 * @brief The members of list_series_identities_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_series_identities_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"series", &list_series_identities_response::series},
        ::ores::utility::codec::field{"truncated", &list_series_identities_response::truncated},
        ::ores::utility::codec::field{"success", &list_series_identities_response::success},
        ::ores::utility::codec::field{"message", &list_series_identities_response::message},
    };
}

/**
 * @brief The members of crm_rate_item, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<crm_rate_item>) {
    return std::tuple{
        ::ores::utility::codec::field{"crm_name", &crm_rate_item::crm_name},
        ::ores::utility::codec::field{"base_currency_code", &crm_rate_item::base_currency_code},
        ::ores::utility::codec::field{"quote_currency_code", &crm_rate_item::quote_currency_code},
        ::ores::utility::codec::field{"rate", &crm_rate_item::rate},
        ::ores::utility::codec::field{"status", &crm_rate_item::status},
        ::ores::utility::codec::field{"as_of", &crm_rate_item::as_of},
        ::ores::utility::codec::field{"reciprocal", &crm_rate_item::reciprocal},
        ::ores::utility::codec::field{"delta_pct", &crm_rate_item::delta_pct},
    };
}

/**
 * @brief The members of get_crm_rate_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_crm_rate_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"party_id", &get_crm_rate_request::party_id},
        ::ores::utility::codec::field{"crm_name", &get_crm_rate_request::crm_name},
        ::ores::utility::codec::field{"base_currency_code",
                                      &get_crm_rate_request::base_currency_code},
        ::ores::utility::codec::field{"quote_currency_code",
                                      &get_crm_rate_request::quote_currency_code},
    };
}

/**
 * @brief The members of get_crm_rate_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_crm_rate_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &get_crm_rate_response::success},
        ::ores::utility::codec::field{"message", &get_crm_rate_response::message},
        ::ores::utility::codec::field{"rate", &get_crm_rate_response::rate},
    };
}

/**
 * @brief The members of get_crm_rates_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_crm_rates_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"party_id", &get_crm_rates_request::party_id},
        ::ores::utility::codec::field{"crm_name", &get_crm_rates_request::crm_name},
        ::ores::utility::codec::field{"reciprocal", &get_crm_rates_request::reciprocal},
    };
}

/**
 * @brief The members of get_crm_rates_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_crm_rates_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &get_crm_rates_response::success},
        ::ores::utility::codec::field{"message", &get_crm_rates_response::message},
        ::ores::utility::codec::field{"rates", &get_crm_rates_response::rates},
    };
}

/**
 * @brief The members of republish_curve_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<republish_curve_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"bootstrap_config_id",
                                      &republish_curve_request::bootstrap_config_id},
        ::ores::utility::codec::field{"as_of", &republish_curve_request::as_of},
    };
}

/**
 * @brief The members of republish_curve_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<republish_curve_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &republish_curve_response::success},
        ::ores::utility::codec::field{"message", &republish_curve_response::message},
    };
}

/**
 * @brief The members of compute_curve_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<compute_curve_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"bootstrap_config_id",
                                      &compute_curve_request::bootstrap_config_id},
        ::ores::utility::codec::field{"as_of", &compute_curve_request::as_of},
    };
}

/**
 * @brief The members of computed_curve_point, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<computed_curve_point>) {
    return std::tuple{
        ::ores::utility::codec::field{"point_id", &computed_curve_point::point_id},
        ::ores::utility::codec::field{"date", &computed_curve_point::date},
        ::ores::utility::codec::field{"discount_factor", &computed_curve_point::discount_factor},
    };
}

/**
 * @brief The members of compute_curve_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<compute_curve_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &compute_curve_response::success},
        ::ores::utility::codec::field{"message", &compute_curve_response::message},
        ::ores::utility::codec::field{"points", &compute_curve_response::points},
    };
}

/**
 * @brief The members of series_slice_instant, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<series_slice_instant>) {
    return std::tuple{
        ::ores::utility::codec::field{"as_of", &series_slice_instant::as_of},
        ::ores::utility::codec::field{"values", &series_slice_instant::values},
    };
}

/**
 * @brief The members of get_series_slice_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_series_slice_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"identity", &get_series_slice_request::identity},
        ::ores::utility::codec::field{"component_field",
                                      &get_series_slice_request::component_field},
        ::ores::utility::codec::field{"component_value",
                                      &get_series_slice_request::component_value},
        ::ores::utility::codec::field{"from_instant", &get_series_slice_request::from_instant},
        ::ores::utility::codec::field{"to_instant", &get_series_slice_request::to_instant},
    };
}

/**
 * @brief The members of get_series_slice_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_series_slice_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &get_series_slice_response::success},
        ::ores::utility::codec::field{"message", &get_series_slice_response::message},
        ::ores::utility::codec::field{"coordinate_field",
                                      &get_series_slice_response::coordinate_field},
        ::ores::utility::codec::field{"coordinates", &get_series_slice_response::coordinates},
        ::ores::utility::codec::field{"instants", &get_series_slice_response::instants},
    };
}

/**
 * @brief The members of evolution_node, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<evolution_node>) {
    return std::tuple{
        ::ores::utility::codec::field{"coordinates", &evolution_node::coordinates},
        ::ores::utility::codec::field{"status", &evolution_node::status},
    };
}

/**
 * @brief The members of evolution_instant, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<evolution_instant>) {
    return std::tuple{
        ::ores::utility::codec::field{"as_of", &evolution_instant::as_of},
        ::ores::utility::codec::field{"values", &evolution_instant::values},
    };
}

/**
 * @brief The members of get_series_evolution_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_series_evolution_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"identity", &get_series_evolution_request::identity},
        ::ores::utility::codec::field{"from_instant", &get_series_evolution_request::from_instant},
        ::ores::utility::codec::field{"to_instant", &get_series_evolution_request::to_instant},
        ::ores::utility::codec::field{"instants", &get_series_evolution_request::instants},
    };
}

/**
 * @brief The members of get_series_evolution_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_series_evolution_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &get_series_evolution_response::success},
        ::ores::utility::codec::field{"message", &get_series_evolution_response::message},
        ::ores::utility::codec::field{"coordinate_fields",
                                      &get_series_evolution_response::coordinate_fields},
        ::ores::utility::codec::field{"nodes", &get_series_evolution_response::nodes},
        ::ores::utility::codec::field{"instants", &get_series_evolution_response::instants},
    };
}

/**
 * @brief The members of point_provenance, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<point_provenance>) {
    return std::tuple{
        ::ores::utility::codec::field{"source_kind", &point_provenance::source_kind},
        ::ores::utility::codec::field{"modified_by", &point_provenance::modified_by},
        ::ores::utility::codec::field{"change_reason_code", &point_provenance::change_reason_code},
        ::ores::utility::codec::field{"change_commentary", &point_provenance::change_commentary},
        ::ores::utility::codec::field{"recorded_at", &point_provenance::recorded_at},
        ::ores::utility::codec::field{"derivation_config_id",
                                      &point_provenance::derivation_config_id},
        ::ores::utility::codec::field{"derivation_config_version",
                                      &point_provenance::derivation_config_version},
        ::ores::utility::codec::field{"source_as_of", &point_provenance::source_as_of},
    };
}

/**
 * @brief The members of get_curve_snapshot_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_curve_snapshot_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"identity", &get_curve_snapshot_request::identity},
        ::ores::utility::codec::field{"include_provenance",
                                      &get_curve_snapshot_request::include_provenance},
    };
}

/**
 * @brief The members of get_curve_snapshot_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_curve_snapshot_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &get_curve_snapshot_response::success},
        ::ores::utility::codec::field{"message", &get_curve_snapshot_response::message},
        ::ores::utility::codec::field{"as_of", &get_curve_snapshot_response::as_of},
        ::ores::utility::codec::field{"coordinate_fields",
                                      &get_curve_snapshot_response::coordinate_fields},
        ::ores::utility::codec::field{"nodes", &get_curve_snapshot_response::nodes},
        ::ores::utility::codec::field{"values", &get_curve_snapshot_response::values},
        ::ores::utility::codec::field{"recorded_at", &get_curve_snapshot_response::recorded_at},
        ::ores::utility::codec::field{"provenance", &get_curve_snapshot_response::provenance},
        ::ores::utility::codec::field{"oldest_age_seconds",
                                      &get_curve_snapshot_response::oldest_age_seconds},
        ::ores::utility::codec::field{"spread_seconds",
                                      &get_curve_snapshot_response::spread_seconds},
        ::ores::utility::codec::field{"warning", &get_curve_snapshot_response::warning},
        ::ores::utility::codec::field{"published_at", &get_curve_snapshot_response::published_at},
        ::ores::utility::codec::field{"published_by", &get_curve_snapshot_response::published_by},
        ::ores::utility::codec::field{"published_as_of",
                                      &get_curve_snapshot_response::published_as_of},
        ::ores::utility::codec::field{"approval_request_id",
                                      &get_curve_snapshot_response::approval_request_id},
    };
}

/**
 * @brief The members of import_market_data_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<import_market_data_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"market_data_content",
                                      &import_market_data_request::market_data_content},
        ::ores::utility::codec::field{"fixings_content",
                                      &import_market_data_request::fixings_content},
        ::ores::utility::codec::field{"source", &import_market_data_request::source},
        ::ores::utility::codec::field{"duplicates_are_errors",
                                      &import_market_data_request::duplicates_are_errors},
    };
}

/**
 * @brief The members of import_market_data_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<import_market_data_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &import_market_data_response::success},
        ::ores::utility::codec::field{"message", &import_market_data_response::message},
        ::ores::utility::codec::field{"series_count", &import_market_data_response::series_count},
        ::ores::utility::codec::field{"observation_count",
                                      &import_market_data_response::observation_count},
        ::ores::utility::codec::field{"fixing_count", &import_market_data_response::fixing_count},
        ::ores::utility::codec::field{"warnings", &import_market_data_response::warnings},
        ::ores::utility::codec::field{"errors", &import_market_data_response::errors},
    };
}

/**
 * @brief The members of feed_kind_counts, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<feed_kind_counts>) {
    return std::tuple{
        ::ores::utility::codec::field{"started", &feed_kind_counts::started},
        ::ores::utility::codec::field{"already_running", &feed_kind_counts::already_running},
        ::ores::utility::codec::field{"skipped", &feed_kind_counts::skipped},
    };
}

/**
 * @brief The members of start_feeds_under_folder_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<start_feeds_under_folder_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"folder_id", &start_feeds_under_folder_request::folder_id},
    };
}

/**
 * @brief The members of start_feeds_under_folder_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<start_feeds_under_folder_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &start_feeds_under_folder_response::success},
        ::ores::utility::codec::field{"message", &start_feeds_under_folder_response::message},
        ::ores::utility::codec::field{"started", &start_feeds_under_folder_response::started},
        ::ores::utility::codec::field{"already_running",
                                      &start_feeds_under_folder_response::already_running},
        ::ores::utility::codec::field{"skipped", &start_feeds_under_folder_response::skipped},
        ::ores::utility::codec::field{"by_kind", &start_feeds_under_folder_response::by_kind},
    };
}

/**
 * @brief The members of stop_feeds_under_folder_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stop_feeds_under_folder_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"folder_id", &stop_feeds_under_folder_request::folder_id},
    };
}

/**
 * @brief The members of stop_feeds_under_folder_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<stop_feeds_under_folder_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &stop_feeds_under_folder_response::success},
        ::ores::utility::codec::field{"message", &stop_feeds_under_folder_response::message},
        ::ores::utility::codec::field{"stopped", &stop_feeds_under_folder_response::stopped},
        ::ores::utility::codec::field{"stopped_by_kind",
                                      &stop_feeds_under_folder_response::stopped_by_kind},
    };
}

/**
 * @brief The members of export_market_data_to_storage_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<export_market_data_to_storage_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"storage_bucket",
                                      &export_market_data_to_storage_request::storage_bucket},
        ::ores::utility::codec::field{"storage_key",
                                      &export_market_data_to_storage_request::storage_key},
        ::ores::utility::codec::field{"fixings_storage_key",
                                      &export_market_data_to_storage_request::fixings_storage_key},
        ::ores::utility::codec::field{"valuation_date",
                                      &export_market_data_to_storage_request::valuation_date},
        ::ores::utility::codec::field{"max_age_days",
                                      &export_market_data_to_storage_request::max_age_days},
    };
}

/**
 * @brief The members of export_market_data_to_storage_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<export_market_data_to_storage_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &export_market_data_to_storage_response::success},
        ::ores::utility::codec::field{"message", &export_market_data_to_storage_response::message},
        ::ores::utility::codec::field{"series_count",
                                      &export_market_data_to_storage_response::series_count},
        ::ores::utility::codec::field{"storage_key",
                                      &export_market_data_to_storage_response::storage_key},
        ::ores::utility::codec::field{"fixings_storage_key",
                                      &export_market_data_to_storage_response::fixings_storage_key},
        ::ores::utility::codec::field{
            "carried_forward_count",
            &export_market_data_to_storage_response::carried_forward_count},
        ::ores::utility::codec::field{"stale_count",
                                      &export_market_data_to_storage_response::stale_count},
    };
}

/**
 * @brief The members of backfill_series_identity_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<backfill_series_identity_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"party_id", &backfill_series_identity_request::party_id},
    };
}

/**
 * @brief The members of backfill_series_identity_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<backfill_series_identity_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &backfill_series_identity_response::success},
        ::ores::utility::codec::field{"message", &backfill_series_identity_response::message},
        ::ores::utility::codec::field{"projected_count",
                                      &backfill_series_identity_response::projected_count},
        ::ores::utility::codec::field{"already_projected_count",
                                      &backfill_series_identity_response::already_projected_count},
        ::ores::utility::codec::field{"unreadable_count",
                                      &backfill_series_identity_response::unreadable_count},
    };
}

/**
 * @brief The members of key_manual_point_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<key_manual_point_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"identity", &key_manual_point_request::identity},
        ::ores::utility::codec::field{"coordinates", &key_manual_point_request::coordinates},
        ::ores::utility::codec::field{"value", &key_manual_point_request::value},
        ::ores::utility::codec::field{"change_reason_code",
                                      &key_manual_point_request::change_reason_code},
        ::ores::utility::codec::field{"change_commentary",
                                      &key_manual_point_request::change_commentary},
    };
}

/**
 * @brief The members of key_manual_point_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<key_manual_point_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &key_manual_point_response::success},
        ::ores::utility::codec::field{"message", &key_manual_point_response::message},
    };
}

/**
 * @brief The members of clear_manual_point_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<clear_manual_point_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"identity", &clear_manual_point_request::identity},
        ::ores::utility::codec::field{"coordinates", &clear_manual_point_request::coordinates},
    };
}

/**
 * @brief The members of clear_manual_point_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<clear_manual_point_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &clear_manual_point_response::success},
        ::ores::utility::codec::field{"message", &clear_manual_point_response::message},
    };
}

/**
 * @brief The members of export_market_data_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<export_market_data_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"valuation_date",
                                      &export_market_data_request::valuation_date},
        ::ores::utility::codec::field{"max_age_days", &export_market_data_request::max_age_days},
    };
}

/**
 * @brief The members of export_market_data_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<export_market_data_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &export_market_data_response::success},
        ::ores::utility::codec::field{"message", &export_market_data_response::message},
        ::ores::utility::codec::field{"market_data_content",
                                      &export_market_data_response::market_data_content},
        ::ores::utility::codec::field{"fixings_content",
                                      &export_market_data_response::fixings_content},
        ::ores::utility::codec::field{"series_count", &export_market_data_response::series_count},
        ::ores::utility::codec::field{"observation_count",
                                      &export_market_data_response::observation_count},
        ::ores::utility::codec::field{"fixing_count", &export_market_data_response::fixing_count},
        ::ores::utility::codec::field{"carried_forward_count",
                                      &export_market_data_response::carried_forward_count},
        ::ores::utility::codec::field{"stale_count", &export_market_data_response::stale_count},
    };
}

/**
 * @brief The members of market_tick, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<market_tick>) {
    return std::tuple{
        ::ores::utility::codec::field{"oresmd_uri", &market_tick::oresmd_uri},
        ::ores::utility::codec::field{"value", &market_tick::value},
        ::ores::utility::codec::field{"observation_time", &market_tick::observation_time},
        ::ores::utility::codec::field{"source", &market_tick::source},
    };
}

/**
 * @brief The members of get_series_ticks_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_series_ticks_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"identity", &get_series_ticks_request::identity},
        ::ores::utility::codec::field{"from_instant", &get_series_ticks_request::from_instant},
        ::ores::utility::codec::field{"to_instant", &get_series_ticks_request::to_instant},
        ::ores::utility::codec::field{"coordinates", &get_series_ticks_request::coordinates},
    };
}

/**
 * @brief The members of get_series_ticks_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_series_ticks_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &get_series_ticks_response::result},
        ::ores::utility::codec::field{"coordinates", &get_series_ticks_response::coordinates},
        ::ores::utility::codec::field{"first_instant", &get_series_ticks_response::first_instant},
        ::ores::utility::codec::field{"gaps_us", &get_series_ticks_response::gaps_us},
        ::ores::utility::codec::field{"coordinate_indexes",
                                      &get_series_ticks_response::coordinate_indexes},
        ::ores::utility::codec::field{"values", &get_series_ticks_response::values},
    };
}

/**
 * @brief The members of store_market_state_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<store_market_state_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"party_id", &store_market_state_request::party_id},
        ::ores::utility::codec::field{"report_instance_id",
                                      &store_market_state_request::report_instance_id},
        ::ores::utility::codec::field{"report_definition_id",
                                      &store_market_state_request::report_definition_id},
        ::ores::utility::codec::field{"as_of", &store_market_state_request::as_of},
        ::ores::utility::codec::field{"storage_token", &store_market_state_request::storage_token},
        ::ores::utility::codec::field{"manifest_uris", &store_market_state_request::manifest_uris},
        ::ores::utility::codec::field{"skipped_families",
                                      &store_market_state_request::skipped_families},
    };
}

/**
 * @brief The members of store_market_state_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<store_market_state_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"result", &store_market_state_response::result},
        ::ores::utility::codec::field{"families", &store_market_state_response::families},
        ::ores::utility::codec::field{"files", &store_market_state_response::files},
        ::ores::utility::codec::field{"rows", &store_market_state_response::rows},
        ::ores::utility::codec::field{"left_alone", &store_market_state_response::left_alone},
        ::ores::utility::codec::field{"refusals", &store_market_state_response::refusals},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::series_identity_field> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::series_identity_field& v);
    static ores::marketdata::messaging::series_identity_field read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::series_identity_field& v);
    static ores::marketdata::messaging::series_identity_field
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::resolve_series_identity_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::resolve_series_identity_request& v);
    static ores::marketdata::messaging::resolve_series_identity_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::resolve_series_identity_request& v);
    static ores::marketdata::messaging::resolve_series_identity_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::resolve_series_identity_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::resolve_series_identity_response& v);
    static ores::marketdata::messaging::resolve_series_identity_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::resolve_series_identity_response& v);
    static ores::marketdata::messaging::resolve_series_identity_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::oresmd_field_spec> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::oresmd_field_spec& v);
    static ores::marketdata::messaging::oresmd_field_spec read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::oresmd_field_spec& v);
    static ores::marketdata::messaging::oresmd_field_spec
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::oresmd_instrument> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::oresmd_instrument& v);
    static ores::marketdata::messaging::oresmd_instrument read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::oresmd_instrument& v);
    static ores::marketdata::messaging::oresmd_instrument
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::oresmd_index_field> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::oresmd_index_field& v);
    static ores::marketdata::messaging::oresmd_index_field read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::oresmd_index_field& v);
    static ores::marketdata::messaging::oresmd_index_field
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::oresmd_index_family> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::oresmd_index_family& v);
    static ores::marketdata::messaging::oresmd_index_family read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::oresmd_index_family& v);
    static ores::marketdata::messaging::oresmd_index_family
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::describe_oresmd_schema_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::describe_oresmd_schema_request& v);
    static ores::marketdata::messaging::describe_oresmd_schema_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::describe_oresmd_schema_request& v);
    static ores::marketdata::messaging::describe_oresmd_schema_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::describe_oresmd_schema_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::describe_oresmd_schema_response& v);
    static ores::marketdata::messaging::describe_oresmd_schema_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::describe_oresmd_schema_response& v);
    static ores::marketdata::messaging::describe_oresmd_schema_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::oresmd_uri_field> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::oresmd_uri_field& v);
    static ores::marketdata::messaging::oresmd_uri_field read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::oresmd_uri_field& v);
    static ores::marketdata::messaging::oresmd_uri_field
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::parse_oresmd_uri_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::parse_oresmd_uri_request& v);
    static ores::marketdata::messaging::parse_oresmd_uri_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::parse_oresmd_uri_request& v);
    static ores::marketdata::messaging::parse_oresmd_uri_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::parse_oresmd_uri_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::parse_oresmd_uri_response& v);
    static ores::marketdata::messaging::parse_oresmd_uri_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::parse_oresmd_uri_response& v);
    static ores::marketdata::messaging::parse_oresmd_uri_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::compose_oresmd_uri_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::compose_oresmd_uri_request& v);
    static ores::marketdata::messaging::compose_oresmd_uri_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::compose_oresmd_uri_request& v);
    static ores::marketdata::messaging::compose_oresmd_uri_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::compose_oresmd_uri_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::compose_oresmd_uri_response& v);
    static ores::marketdata::messaging::compose_oresmd_uri_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::compose_oresmd_uri_response& v);
    static ores::marketdata::messaging::compose_oresmd_uri_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_series_identities_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_series_identities_request& v);
    static ores::marketdata::messaging::list_series_identities_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_series_identities_request& v);
    static ores::marketdata::messaging::list_series_identities_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::listed_series> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::listed_series& v);
    static ores::marketdata::messaging::listed_series read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::listed_series& v);
    static ores::marketdata::messaging::listed_series read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::list_series_identities_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::list_series_identities_response& v);
    static ores::marketdata::messaging::list_series_identities_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::list_series_identities_response& v);
    static ores::marketdata::messaging::list_series_identities_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::crm_rate_item> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::crm_rate_item& v);
    static ores::marketdata::messaging::crm_rate_item read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::crm_rate_item& v);
    static ores::marketdata::messaging::crm_rate_item read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::get_crm_rate_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_crm_rate_request& v);
    static ores::marketdata::messaging::get_crm_rate_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::get_crm_rate_request& v);
    static ores::marketdata::messaging::get_crm_rate_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::get_crm_rate_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_crm_rate_response& v);
    static ores::marketdata::messaging::get_crm_rate_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::get_crm_rate_response& v);
    static ores::marketdata::messaging::get_crm_rate_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::get_crm_rates_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_crm_rates_request& v);
    static ores::marketdata::messaging::get_crm_rates_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::get_crm_rates_request& v);
    static ores::marketdata::messaging::get_crm_rates_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::get_crm_rates_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_crm_rates_response& v);
    static ores::marketdata::messaging::get_crm_rates_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::get_crm_rates_response& v);
    static ores::marketdata::messaging::get_crm_rates_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::republish_curve_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::republish_curve_request& v);
    static ores::marketdata::messaging::republish_curve_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::republish_curve_request& v);
    static ores::marketdata::messaging::republish_curve_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::republish_curve_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::republish_curve_response& v);
    static ores::marketdata::messaging::republish_curve_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::republish_curve_response& v);
    static ores::marketdata::messaging::republish_curve_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::compute_curve_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::compute_curve_request& v);
    static ores::marketdata::messaging::compute_curve_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::compute_curve_request& v);
    static ores::marketdata::messaging::compute_curve_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::computed_curve_point> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::computed_curve_point& v);
    static ores::marketdata::messaging::computed_curve_point read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::computed_curve_point& v);
    static ores::marketdata::messaging::computed_curve_point
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::compute_curve_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::compute_curve_response& v);
    static ores::marketdata::messaging::compute_curve_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::compute_curve_response& v);
    static ores::marketdata::messaging::compute_curve_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::series_slice_instant> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::series_slice_instant& v);
    static ores::marketdata::messaging::series_slice_instant read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::series_slice_instant& v);
    static ores::marketdata::messaging::series_slice_instant
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_series_slice_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_series_slice_request& v);
    static ores::marketdata::messaging::get_series_slice_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_series_slice_request& v);
    static ores::marketdata::messaging::get_series_slice_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_series_slice_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_series_slice_response& v);
    static ores::marketdata::messaging::get_series_slice_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_series_slice_response& v);
    static ores::marketdata::messaging::get_series_slice_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::evolution_node> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::evolution_node& v);
    static ores::marketdata::messaging::evolution_node read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::evolution_node& v);
    static ores::marketdata::messaging::evolution_node
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::evolution_instant> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::evolution_instant& v);
    static ores::marketdata::messaging::evolution_instant read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::evolution_instant& v);
    static ores::marketdata::messaging::evolution_instant
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_series_evolution_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_series_evolution_request& v);
    static ores::marketdata::messaging::get_series_evolution_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_series_evolution_request& v);
    static ores::marketdata::messaging::get_series_evolution_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_series_evolution_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_series_evolution_response& v);
    static ores::marketdata::messaging::get_series_evolution_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_series_evolution_response& v);
    static ores::marketdata::messaging::get_series_evolution_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::point_provenance> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::point_provenance& v);
    static ores::marketdata::messaging::point_provenance read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::point_provenance& v);
    static ores::marketdata::messaging::point_provenance
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_curve_snapshot_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_curve_snapshot_request& v);
    static ores::marketdata::messaging::get_curve_snapshot_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_curve_snapshot_request& v);
    static ores::marketdata::messaging::get_curve_snapshot_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_curve_snapshot_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::get_curve_snapshot_response& v);
    static ores::marketdata::messaging::get_curve_snapshot_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_curve_snapshot_response& v);
    static ores::marketdata::messaging::get_curve_snapshot_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::import_market_data_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::import_market_data_request& v);
    static ores::marketdata::messaging::import_market_data_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::import_market_data_request& v);
    static ores::marketdata::messaging::import_market_data_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::import_market_data_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::import_market_data_response& v);
    static ores::marketdata::messaging::import_market_data_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::import_market_data_response& v);
    static ores::marketdata::messaging::import_market_data_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::feed_kind_counts> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::feed_kind_counts& v);
    static ores::marketdata::messaging::feed_kind_counts read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::feed_kind_counts& v);
    static ores::marketdata::messaging::feed_kind_counts
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::start_feeds_under_folder_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::start_feeds_under_folder_request& v);
    static ores::marketdata::messaging::start_feeds_under_folder_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::start_feeds_under_folder_request& v);
    static ores::marketdata::messaging::start_feeds_under_folder_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::start_feeds_under_folder_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::start_feeds_under_folder_response& v);
    static ores::marketdata::messaging::start_feeds_under_folder_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::start_feeds_under_folder_response& v);
    static ores::marketdata::messaging::start_feeds_under_folder_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::stop_feeds_under_folder_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::stop_feeds_under_folder_request& v);
    static ores::marketdata::messaging::stop_feeds_under_folder_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::stop_feeds_under_folder_request& v);
    static ores::marketdata::messaging::stop_feeds_under_folder_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::stop_feeds_under_folder_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::stop_feeds_under_folder_response& v);
    static ores::marketdata::messaging::stop_feeds_under_folder_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::stop_feeds_under_folder_response& v);
    static ores::marketdata::messaging::stop_feeds_under_folder_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::export_market_data_to_storage_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::export_market_data_to_storage_request& v);
    static ores::marketdata::messaging::export_market_data_to_storage_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::export_market_data_to_storage_request& v);
    static ores::marketdata::messaging::export_market_data_to_storage_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::export_market_data_to_storage_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::export_market_data_to_storage_response& v);
    static ores::marketdata::messaging::export_market_data_to_storage_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::export_market_data_to_storage_response& v);
    static ores::marketdata::messaging::export_market_data_to_storage_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::backfill_series_identity_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::backfill_series_identity_request& v);
    static ores::marketdata::messaging::backfill_series_identity_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::backfill_series_identity_request& v);
    static ores::marketdata::messaging::backfill_series_identity_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::backfill_series_identity_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::backfill_series_identity_response& v);
    static ores::marketdata::messaging::backfill_series_identity_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::backfill_series_identity_response& v);
    static ores::marketdata::messaging::backfill_series_identity_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::key_manual_point_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::key_manual_point_request& v);
    static ores::marketdata::messaging::key_manual_point_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::key_manual_point_request& v);
    static ores::marketdata::messaging::key_manual_point_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::key_manual_point_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::key_manual_point_response& v);
    static ores::marketdata::messaging::key_manual_point_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::key_manual_point_response& v);
    static ores::marketdata::messaging::key_manual_point_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::clear_manual_point_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::clear_manual_point_request& v);
    static ores::marketdata::messaging::clear_manual_point_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::clear_manual_point_request& v);
    static ores::marketdata::messaging::clear_manual_point_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::clear_manual_point_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::clear_manual_point_response& v);
    static ores::marketdata::messaging::clear_manual_point_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::clear_manual_point_response& v);
    static ores::marketdata::messaging::clear_manual_point_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::export_market_data_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::export_market_data_request& v);
    static ores::marketdata::messaging::export_market_data_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::export_market_data_request& v);
    static ores::marketdata::messaging::export_market_data_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::export_market_data_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::export_market_data_response& v);
    static ores::marketdata::messaging::export_market_data_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::export_market_data_response& v);
    static ores::marketdata::messaging::export_market_data_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT wire_traits<ores::marketdata::messaging::market_tick> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::market_tick& v);
    static ores::marketdata::messaging::market_tick read_json(std::string_view text);
    static std::string write_msgpack(const ores::marketdata::messaging::market_tick& v);
    static ores::marketdata::messaging::market_tick read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_series_ticks_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_series_ticks_request& v);
    static ores::marketdata::messaging::get_series_ticks_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_series_ticks_request& v);
    static ores::marketdata::messaging::get_series_ticks_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::get_series_ticks_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::get_series_ticks_response& v);
    static ores::marketdata::messaging::get_series_ticks_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::get_series_ticks_response& v);
    static ores::marketdata::messaging::get_series_ticks_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::store_market_state_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::marketdata::messaging::store_market_state_request& v);
    static ores::marketdata::messaging::store_market_state_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::store_market_state_request& v);
    static ores::marketdata::messaging::store_market_state_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_MARKETDATA_API_EXPORT
    wire_traits<ores::marketdata::messaging::store_market_state_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::marketdata::messaging::store_market_state_response& v);
    static ores::marketdata::messaging::store_market_state_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::marketdata::messaging::store_market_state_response& v);
    static ores::marketdata::messaging::store_market_state_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
