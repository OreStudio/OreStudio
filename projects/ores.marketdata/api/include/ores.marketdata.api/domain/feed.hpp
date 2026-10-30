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
#ifndef ORES_MARKETDATA_API_DOMAIN_FEED_HPP
#define ORES_MARKETDATA_API_DOMAIN_FEED_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief One publisher's data, on one service, through one session, with the publisher's own key.
 *
 * A feed binds a publisher to a session and a service. The publisher owns the
 * values and their terms; the session says how they arrive; the service says
 * whether they arrive live, delayed, at the end of the day or as a reference
 * set. The provider_key is the publisher's own name for the data it serves,
 * such as the ECB key Frankfurter reads.
 *
 * A publisher can have several feeds at once. A party reaches one of them through
 * a route, never through the publisher. One feed of each publisher is the default,
 * and a new subscription routes to it.
 *
 * The system tenant and the system party own the rows. Every tenant reads them and
 * only the system tenant writes them.
 */
struct feed final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Surrogate UUID uniquely identifying this feed.
     */
    boost::uuids::uuid id;

    /**
     * @brief The feed's readable label, such as ecb-frankfurter. Unique within the system tenant.
     * It is the name an adapter announces and a route names.
     */
    std::string name;

    /**
     * @brief References provider.code: the publisher whose ids this feed carries.
     */
    std::string publisher_code;

    /**
     * @brief References session.code: the session that carries this feed's values.
     */
    std::string session_code;

    /**
     * @brief What the feed offers: real_time, delayed, end_of_day, reference or historical. It
     * decides the feed's shape and, for end_of_day, its schedule.
     */
    std::string service;

    /**
     * @brief The publisher's own key for the data, such as ECB for the ECB through Frankfurter.
     * Empty when the adapter needs no key.
     */
    std::string provider_key = "";

    /**
     * @brief True for the one feed of the publisher that a new subscription routes to. At most one
     * feed of a publisher is the default.
     */
    bool is_default = false;

    /**
     * @brief What the feed is for: which publisher, which session and which service.
     */
    std::string description;

    /**
     * @brief Username of the person who last modified this market data feed.
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
    friend bool operator==(const feed&, const feed&) = default;
};

/**
 * @brief Dispatch-key identifier for feed, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const feed&) {
    return "ores.marketdata.feed";
}

}

#endif
