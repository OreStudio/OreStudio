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
#ifndef ORES_REFDATA_API_DOMAIN_BUNDLE_IMPORT_ITEM_HPP
#define ORES_REFDATA_API_DOMAIN_BUNDLE_IMPORT_ITEM_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief One staged family payload belonging to a bundle import.
 *
 * One row per family a staging message delivers. The payload is that family's
 * own generated value type, already serialised, so a stage of one family never
 * builds a message that carries every family.
 *
 * The identity is the import, the family and the sequence together. A repeated
 * stage of the same three is refused rather than duplicated, so a retried stage
 * is safe. Each stage is its own local transaction, and nothing reaches the
 * live tables until a commit applies the rows.
 */
struct bundle_import_item final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The staged item's surrogate key. It is made when the row is written.
     */
    boost::uuids::uuid id;

    /**
     * @brief The import this staged item belongs to.
     */
    boost::uuids::uuid import_id;

    /**
     * @brief The convention family the payload carries, as the family registry names it. It decides
     * which generated type the payload decodes into at commit.
     */
    std::string family;

    /**
     * @brief The ordinal of this stage within its family. A family staged in more than one message
     * numbers its parts so a retry repeats one number rather than appending.
     */
    int sequence = 0;

    /**
     * @brief The party the staged item belongs to. The service stamps it from the session, so a
     * caller never states it. Row level security confines the item to that party, and a commit
     * reads only its own party's rows.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief The family's own generated value type, serialised as JSON. The column is jsonb and C++
     * holds the serialised form as a string, so no generated type has to carry every family's
     * shape.
     */
    std::string payload;

    /**
     * @brief The digest the caller computed over the payload, when it computed one. A commit may
     * compare it against the staged payload to tell a damaged transport from a good one.
     */
    std::optional<std::string> checksum;

    /**
     * @brief Username of the person who last modified this bundle import item.
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
    friend bool operator==(const bundle_import_item&, const bundle_import_item&) = default;
};

/**
 * @brief Dispatch-key identifier for bundle_import_item, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bundle_import_item&) {
    return "ores.refdata.bundle_import_item";
}

}

#endif
