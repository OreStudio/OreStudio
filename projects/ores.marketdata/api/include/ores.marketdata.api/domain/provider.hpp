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
#ifndef ORES_MARKETDATA_API_DOMAIN_PROVIDER_HPP
#define ORES_MARKETDATA_API_DOMAIN_PROVIDER_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::marketdata::domain {

/**
 * @brief Market data provider that owns a namespace of vendor ids: a vendor, an exchange, a broker,
 * or a reference set.
 *
 * The catalogue of market data providers: data vendors such as Bloomberg and LSEG,
 * exchanges such as CME, brokers, and reference sets such as the ORE examples. A
 * provider owns a namespace of vendor ids. Every source belongs to one provider, and
 * every vendor instrument and mapping belongs to a provider, so a live and a
 * simulated source of one provider share their ids and their mapping. The name
 * carries no mark of any source kind.
 *
 * The system tenant and the system party own the rows. Every tenant reads them and
 * only the system tenant writes them.
 */
struct provider final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief Unique provider code: lower case letters, digits, underscores and hyphens, starting
     * with a letter.
     *
     * Examples: 'ore', 'bloomberg', 'lseg', 'ice'.
     */
    std::string code;

    /**
     * @brief The provider's name as a person knows it.
     */
    std::string name;

    /**
     * @brief What sort of provider it is: data_vendor, exchange, broker or reference.
     */
    std::string kind;

    /**
     * @brief What the provider supplies, in a sentence or two.
     */
    std::string description;

    /**
     * @brief Order for UI display purposes.
     */
    int display_order = 0;

    /**
     * @brief The page that states the provider's terms of use. Empty for a provider whose terms are
     * a contract outside the system, such as a vendor's licence, which the system does not check
     * before it takes data.
     */
    std::string terms_url = "";

    /**
     * @brief The credit the terms ask a copy of the data to carry, such as "Source: ECB
     * statistics".
     */
    std::string terms_credit = "";

    /**
     * @brief Whether the terms allow the provider's values to be stored. A provider whose terms do
     * not is not fetched, whatever its acceptance.
     */
    bool allows_storage = false;

    /**
     * @brief Where the key the provider needs is read from, such as
     * env:ORES_MARKETDATA_FRED_API_KEY. Never the key itself. Empty when no key is needed.
     */
    std::string credential_ref = "";

    /**
     * @brief The digest of the terms when they were accepted: the SHA-256 of the terms page, the
     * credit and the storage permission. The terms hold while it matches the digest of the terms as
     * they stand, so a change of any of them clears the acceptance with no further write. Empty
     * when the terms were never accepted.
     */
    std::string terms_accepted_digest = "";

    /**
     * @brief Who accepted the terms. Empty when they were never accepted.
     */
    std::string terms_accepted_by = "";

    /**
     * @brief When the terms were accepted. Empty when they never were.
     */
    std::optional<std::chrono::system_clock::time_point> terms_accepted_at;

    /**
     * @brief Username of the person who last modified this provider.
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
    friend bool operator==(const provider&, const provider&) = default;
};

/**
 * @brief Dispatch-key identifier for provider, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const provider&) {
    return "ores.marketdata.provider";
}

}

#endif
