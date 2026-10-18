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
#ifndef ORES_REFDATA_API_DOMAIN_BUNDLE_IMPORT_HPP
#define ORES_REFDATA_API_DOMAIN_BUNDLE_IMPORT_HPP

#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <optional>
#include <string>
#include <string_view>

namespace ores::refdata::domain {

/**
 * @brief The durable session a staged bundle belongs to.
 *
 * A bundle is applied in one unit_of_work, and the messages that
 * carry it cannot hold the whole bundle: a message carrying every family is the
 * aggregate that fails on MSVC. The import is the row the staging messages hang
 * off instead. It records that a bundle is being staged, who owns it, how many
 * families were promised, and when an abandoned session stops waiting.
 *
 * The caller mints id, so a repeated begin with a known id returns the import
 * that already exists rather than opening a second one. The service stamps
 * party_id, state and expires_at from the session it runs in.
 *
 * Nothing in this table is visible to a reader of the live tables
 * until a commit applies the staged rows.
 */
struct bundle_import final {
    /**
     * @brief Version number for optimistic locking and change tracking.
     */
    int version = 0;

    /**
     * @brief Tenant identifier for multi-tenancy isolation.
     */
    utility::uuid::tenant_id tenant_id = utility::uuid::tenant_id::system();

    /**
     * @brief The import's identity. The caller supplies it, so a retried begin names the same
     * import rather than opening a second one.
     */
    boost::uuids::uuid id;

    /**
     * @brief The party the staged bundle belongs to. The service stamps it from the session, so a
     * caller never states it. Row level security confines the import to that party.
     */
    boost::uuids::uuid party_id;

    /**
     * @brief A short description of where the bundle came from, such as the file name the caller
     * loaded it from. Nullable, because a caller that imports from a stream has no name to give.
     */
    std::optional<std::string> source;

    /**
     * @brief Where the import stands. A begun import is staging and a commit that applied every
     * family marks it applied. Those are the two states the service writes. The column permits
     * failed and expired, and nothing sets either yet, so a failed commit leaves its import staging
     * for a retry.
     */
    std::string state;

    /**
     * @brief How many families the caller promised in the begin. Nullable, because a caller that
     * does not know the count states none. Nothing reads it yet, so a commit accepts a bundle that
     * carries fewer families than were promised.
     */
    std::optional<int> families_expected;

    /**
     * @brief When an uncommitted import stops waiting. A sweep would mark the row expired and
     * release its staged items after this instant, and no sweep exists yet, so a staging import
     * waits indefinitely.
     */
    std::chrono::system_clock::time_point expires_at;

    /**
     * @brief Username of the person who last modified this bundle import.
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
    friend bool operator==(const bundle_import&, const bundle_import&) = default;
};

/**
 * @brief Dispatch-key identifier for bundle_import, e.g. for the
 * generic history-diff request and action registries. Single source
 * of truth: every call site spells entity_type_of(value) regardless
 * of which entity it holds.
 */
[[nodiscard]] constexpr std::string_view entity_type_of(const bundle_import&) {
    return "ores.refdata.bundle_import";
}

}

#endif
