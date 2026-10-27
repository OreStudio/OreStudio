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
 * Template: cpp_domain_type_class.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_MARKETDATA_API_DOMAIN_CURVE_PUBLISH_HPP
#define ORES_MARKETDATA_API_DOMAIN_CURVE_PUBLISH_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief The subject an approval request for a curve publish names, and the publisher and time the
 * apply records.
 *
 * The detail row of one marketdata.curve_publish approval request: the subject
 * the request authorises, and the record of the act once the subject is published.
 *
 * A request is a header. What it is about lives here, keyed by the request's own
 * id, exactly as a proposed book change lives in ores.refdata.book_change and is
 * keyed by its request. The subject is three facts and no more: the curve
 * configuration whose output is published, the as-of instant the bootstrap reads
 * the market at, and the party the configuration belongs to (party_id). The curve
 * is named by its configuration because that is what the publish call names. The
 * series the output is written to (output_series_id) is carried beside it because
 * the reader that serves a published curve starts from a series and not from a
 * configuration.
 *
 * published_at and published_by are the publication record. Both are empty
 * while the request waits, and both are written in one act when the approved
 * subject is published: the timestamp is the instant the published set moved, and
 * the username is the account that applied it. The row is bitemporal, so the
 * applied version is a new version of the same row and the history holds the
 * unapplied subject beside it. request_id is the natural key, so a request
 * authorises one publish and no second apply can find a row to write.
 *
 * A set ends once, and the end is two more facts on the same row rather than a
 * second entity. withdrawn_at, withdrawn_by, withdrawn_reason_code and
 * withdrawal_request_id are the withdrawal: the instant, the account, the reason
 * and the request that authorised it, written as a new version of this row. The
 * withdrawal is not a deletion: the published facts stay, so the set a desk priced
 * from is still readable afterwards. superseded_at,
 * superseded_by_request_id and superseded_as_of are the supersession: a later
 * publish replaced this set, and the link names the successor's request and the
 * as-of it was built at. A supersession is a publish and not a second act, so the
 * successor's own apply writes it on this row.
 *
 * The state a reader sees is a function of these facts and is stored nowhere:
 * withdrawn when withdrawn_at holds an instant, superseded when superseded_at
 * does, published when published_at does, and a candidate otherwise.
 *
 * The row is not a client's to write: the raise writes it, the gate reads it, and
 * the apply stamps it. A client reads it to decide a request or to audit a curve.
 */
struct curve_publish final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID identifying this publish record.
     */
    boost::uuids::uuid id;

    /**
     * @brief The approval request this record is the subject of.
     *
     * One request, one publish: the row is written when the request is raised and stamped when it
     * is applied, so a request that authorised one publish has no second row to authorise another.
     */
    boost::uuids::uuid request_id;

    /**
     * @brief The party the published configuration belongs to.
     *
     * The subject names it because the same curve code in two parties is two curves, and a publish
     * of one must not authorise the other.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The ir_curve_bootstrap_config whose output the publish writes.
     *
     * The publish names the configuration and not a set of points, because the configuration is
     * what the bootstrap recomputes from the market.
     */
    boost::uuids::uuid bootstrap_config_id;

    /**
     * @brief The market series the configuration's output is written to.
     *
     * Carried beside the configuration so a reader that starts from a series -- the snapshot read,
     * a consumer of the curve -- finds the publish record for the series it holds without resolving
     * the configuration first.
     */
    boost::uuids::uuid output_series_id;

    /**
     * @brief The instant the bootstrap reads the market at.
     *
     * The as-of is part of the subject: the same curve published against two market instants is two
     * different published sets, and one request authorises one of them.
     */
    std::chrono::system_clock::time_point as_of;

    /**
     * @brief The username of the account that applied the request, or empty while it waits.
     *
     * The account is the publisher, which is not the decider: the approval records who answered,
     * and this records who made the published set move.
     */
    std::string published_by = "";

    /**
     * @brief When the published set moved, or null while the request waits.
     *
     * This is the published marker: a series is published when a record for it holds an instant
     * here, and the latest such instant is the version in use.
     */
    std::optional<std::chrono::system_clock::time_point> published_at;

    /**
     * @brief The username of the account that took the set out of use, or empty while the set is in
     * use or ended by a successor.
     *
     * The account is the withdrawer, which is not the publisher: the journey states that the
     * publisher of a set has no special right to withdraw it, and the reason is the error and not
     * the person.
     */
    std::string withdrawn_by = "";

    /**
     * @brief When the set left use, or null while it is in use.
     *
     * The instant is the end of the act, and it is written in the same version as the withdrawer
     * and the reason, so a reader never sees a withdrawal with no account behind it.
     */
    std::optional<std::chrono::system_clock::time_point> withdrawn_at;

    /**
     * @brief Why the set left use, as a change reason code, or empty while it is in use.
     *
     * The journey keeps the reason against the act, so the code travels with the instant and the
     * account. The screen offers the reasons a curve leaves use and the record holds the one
     * chosen.
     */
    std::string withdrawn_reason_code = "";

    /**
     * @brief The withdrawal request that authorised the act, or null.
     *
     * Null is not a gap: a tenant whose withdrawal kind requires no approval lets the trader
     * withdraw alone, and the act then names no request. The column is a soft reference to
     * ores_inbox_approval_requests_tbl rather than a foreign key, because the row states its own
     * authority and a hard key would refuse the count-zero path the journey allows.
     */
    std::optional<boost::uuids::uuid> withdrawal_request_id;

    /**
     * @brief When a later set took this one's place, or null while this set is in use or withdrawn.
     *
     * A supersession is a publish whose subject is the successor, so the instant is the successor's
     * published instant and no person signs it: the successor's own record names the publisher.
     */
    std::optional<std::chrono::system_clock::time_point> superseded_at;

    /**
     * @brief The publish request whose apply replaced this set, or null.
     *
     * A soft reference, as the withdrawal request is: the successor is published under its own
     * request and this row links to it so a reader follows one live set and one superseded set and
     * never two live sets.
     */
    std::optional<boost::uuids::uuid> superseded_by_request_id;

    /**
     * @brief The as-of the successor was built at, or null.
     *
     * Carried beside the successor's request so the history names the set that replaced this one
     * without a second read.
     */
    std::optional<std::chrono::system_clock::time_point> superseded_as_of;

    /**
     * @brief Username of the person who last modified this curve publish.
     */
    std::string modified_by;

    /**
     * @brief Username of the account that performed this action.
     */
    std::string performed_by;

    /**
     * @brief Code identifying the reason for the change.
     *
     * References change_reasons table (soft FK).
     */
    std::string change_reason_code;

    /**
     * @brief Free-text commentary explaining the change.
     */
    std::string change_commentary;

    /**
     * @brief Timestamp when this version of the record was recorded.
     *
     * The transaction-time window's start, which the store sets from its own
     * clock. It travels with the audit members because it is only ever read
     * with them: the history builder takes a version type that carries an
     * actor *and* this timestamp, so an entity without the actor has no use
     * for the timestamp either.
     */
    std::chrono::system_clock::time_point recorded_at;

    /**
     * @brief Value equality.
     *
     * Every generated domain type is a value: two of them are equal when their
     * members are, whatever the entity means. A test that round-trips one
     * through the wire asserts exactly that, so equality is part of the shape
     * rather than something each entity decides -- an entity without it cannot
     * be round-trip tested at all, which is why the omission went unnoticed
     * until the diff payloads were the first generated types to have a test.
     */
    friend bool operator==(const curve_publish&, const curve_publish&) = default;
};

/**
 * @brief Dispatch-key identifier for curve_publish, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const curve_publish&) {
    return "ores.marketdata.curve_publish";
}

}

#endif
